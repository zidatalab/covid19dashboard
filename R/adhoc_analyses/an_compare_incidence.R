library(tidyverse)
library(lubridate)
library(readr)
library(DBI)
library("zicolors")
library("ggrepel")
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')


# data 
cases <- tbl(conn,"rki") %>% group_by(Refdatum,IdLandkreis,Altersgruppe) %>% 
  summarise(cases=sum(AnzahlFall)) %>% collect()

idnames <- tbl(conn,"params")  %>% select(id,name) %>% collect() 

meta <- tbl(conn,"params") %>% filter(ebene=="Kreis" | 
                                              (ebene=='Land' & name=="Berlin")) %>%
  mutate(id=ifelse(name=='Berlin',11000000,id)) %>%
  select(-contains('Jahre'),-EW_insgesamt,-contains('Einwohner')) %>%
  collect()


pop <- read_delim("https://raw.githubusercontent.com/zidatalab/covid19dashboard/master/data/Bev2019_Kreis_AG_rki_geschlecht.txt", 
                  ";", escape_double = FALSE, trim_ws = TRUE)

pop.rec <- pop %>% mutate(age=case_when(
  AG_rki==1 ~ "A00-A04" ,
  AG_rki==2 ~ "A05-A14" ,
  AG_rki==3 ~ "A15-A34" ,
  AG_rki==4 ~ "A35-A59" ,
  AG_rki==5 ~ "A60-A79" ,
  AG_rki==6 ~ "A80+" 
)) %>% filter(!is.na(AG_rki)) %>% select(id=Kreis,age,bev=ges)


cases.rec <- cases %>% ungroup() %>% 
  mutate(date=as.Date(Refdatum),
         id=as.numeric(IdLandkreis),
         age=as.character(Altersgruppe)) %>% 
  select(id,date,age,cases) %>%
  mutate(id=ifelse(id>=11000 & id<12000,11000,id))

cases.all <- cases.rec %>% select(id,date,age) %>% 
  expand(id,date,age) %>% 
  left_join(.,cases.rec, by = c("id", "date", "age")) %>% 
  mutate(cases=ifelse(is.na(cases),0,cases),
         KW=isoweek(date)) %>%
  group_by(id,KW,age) %>% 
  summarise(cases=sum(cases)) %>% ungroup() %>%
  left_join(.,pop.rec, by=c("id","age")) %>% filter(!is.na(bev)) 

cases.all <- bind_rows(
  cases.all,
  cases.all %>% group_by(id,KW) %>% summarise(cases=sum(cases),bev=sum(bev),age="Gesamt"))
cases.all <- cases.all %>% arrange(id,KW,age) %>% 
  mutate(Incidence=cases/(bev/100000))

plotdata <- cases.all %>% filter(age %in% c("A80+","Gesamt")) %>%
  select(id,KW,age,Incidence) %>% spread(age,Incidence) %>%
  group_by(id) %>% arrange(id,KW) %>% mutate(Gesamt_l2=lag(Gesamt,2)) %>%
  ungroup() %>% mutate(id=1000*id) %>%
  left_join(idnames ,by="id") %>% mutate(name=ifelse(id==11000000,"Berlin",name))
           
# Andata
andata <- plotdata %>% filter(KW>=isoweek(date("2020-09-01")) & KW<=49)
mymodel <- lm(`A80+`~ 1+ Gesamt_l2, data=andata)
broom::tidy(mymodel,conf.int=TRUE)
# Aggregate Info for Kreis after 1.9.2020
plotdata.condensed <- plotdata %>% filter(KW>=isoweek(date("2020-09-01")) & KW<=49) %>% 
  ungroup() %>% group_by(id,name) %>% 
  filter(`Gesamt_l2`>0) %>% 
  summarise(raw_Verhaeltnis=mean(`A80+`/`Gesamt_l2`,na.rm=T),
            Verhaeltnis=floor(raw_Verhaeltnis*15)/15,
            obs=n()) %>%
  group_by(Verhaeltnis) %>%
  mutate(pos=row_number(),
         schutz_80_plus=case_when(
           Verhaeltnis<0.5 ~"sehr stark",
           Verhaeltnis>=0.5 & Verhaeltnis<0.7  ~"stark",
           Verhaeltnis>=0.7 & Verhaeltnis<.9  ~"etwas",
           Verhaeltnis>=0.9 & Verhaeltnis<1.11  ~"kaum",
           Verhaeltnis>=1.11 ~"gar nicht",
           TRUE ~"NA"),
         schutz_80_plus=factor(schutz_80_plus,levels=c("sehr stark",
                                                       "stark","etwas",
                                                       "kaum","gar nicht"))
         )

# Meta Informationen zu den Kreisen  

more_meta <- readr::read_csv2("https://raw.githubusercontent.com/zidatalab/covid19causaleffects/master/data/inkar/kontextindikatoren_kreise.csv?token=AHZPFCZMK6PZH3B6LVQLWIK76WVBS") %>%
  mutate(id=Kennziffer*1000)

andata <- pop.rec %>% group_by(id) %>% mutate(ges=sum(bev)) %>% 
  spread(age,bev) %>% 
  mutate(
    Altenquotient=((`A60-A79`+`A80+`)/ges)) %>% 
  select(id,Altenquotient,"Einwohner"="ges") %>%
  mutate(id=id*1000) %>% left_join(meta) %>%
  mutate( Ant_Hoechstrisiko = Hoechstrisikopopulation_75plus/Einwohner) %>%
  left_join(.,plotdata %>% group_by(id) %>% summarise(mean_Inzidenz=mean(Gesamt,na.rm=T)),by="id") %>% ungroup() %>%
  left_join(plotdata.condensed,.,by="id") %>%
  mutate(Bundesland=floor(id/1000000)) %>%
  left_join(.,idnames %>% filter(id<17) %>% rename(Bundesland=id, BL_=name) %>% collect(),by="Bundesland") %>%
  mutate(BL_name=as.factor(BL_)) %>% 
  left_join(.,more_meta %>% select(-contains("Einw")),by="id") %>%
  mutate(z_BevDichte= scale(Einwohner/flaeche) )



#contrasts(andata$BL_ = contr.sum(16)

dasmodell <- andata %>%  MASS::glm.nb(Verhaeltnis ~ 1 +  
                                        mean_Inzidenz +
                                        #I(mean_Inzidenz^2) +
                                        # I(mean_Inzidenz^3 )+
                                        z_BevDichte +
                                       # I(mean_Inzidenz*BevDichte)+
                                          `Haushalte mit niedrigem Einkommen`+
                                        `Stimmenanteile AfD`
                                        ,data=.) 
broom::tidy( dasmodell ,exponentiate = TRUE) 
broom::glance( dasmodell) 
