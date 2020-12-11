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
  select(id,date,age,cases) 

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
  left_join(idnames,by="id")


myplot <- ggplot(plotdata %>% filter(KW<50),aes(x=`Gesamt_l2`,y=`A80+`)) + 
  geom_point(alpha=.05) + geom_smooth(method="lm") +
  scale_y_continuous(limits = c(0,250)) +
  scale_x_continuous(limits=c(0,250)) +
  facet_grid(.~case_when(
    KW<15 ~ "KW 0-14",
    KW>=15 & KW < 35 ~ "KW 15-34",
    KW>=35 & KW <= 49 ~ "KW 35-49"
  )) +
  geom_abline(aes(intercept=0,slope=1), color="black" , 
              linetype="dotted" ) +
  coord_fixed() +
  labs(y="Inz. 80+ je 100 Tsd.EW",
       x="Inz. je 100 Tsd.EW vorletzte KW",
       caption="Datenbasis: RKI Meldedaten bis 49. KW, eine Beobachtung je Kreis/KW, Stand: 10.12.20")

kreislines <- ggplot(plotdata %>% filter(KW<50),aes(x=`Gesamt_l2`,y=`A80+`)) + 
  geom_smooth(aes(group=id),method="lm",size=.1,color=zi_cols("zidarkblue"),alpha=.2,se = FALSE) +
  geom_abline(aes(intercept=0,slope=1), color="black" ) + 
  geom_smooth(method="lm",size=2,color=zi_cols("zigreen")) +
  geom_hline(yintercept = 0, color="black")+
  scale_y_continuous(limits = c(0,500)) +
  theme_zi_titels() + 
  labs(title="Wenige Kreise erreichen Entkopplung der Inzidenz der über 80-jährigen",
       y="Inzidenz in Altersgruppe 80+ J.",
       x="Inzidenz in der vorletzten KW")   

finalise_plot(kreislines, 
              save_filepath = "~/Desktop/Kreisvergleich_Schutz_älterer.png",
              width_cm=22,height_cm=22*(9/16),
              source_name = "Datenbasis: RKI 10.12.2020, Zusammenhang der Inzidenz je 100 Tsd. Einwohner auf Ebene der 402 Landkreise und kreisfreien Städte zwischen 1. und 49. KW")
                
                
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
                           

myplot <- ggplot(plotdata.condensed) +
  aes(x=Verhaeltnis,
      y=pos) +
  geom_point(show.legend = FALSE, aes(color=schutz_80_plus)) +
  geom_hline(yintercept = 0) +
  scale_x_log10(breaks=c(.1,.2,.5,1,2,5,10),limits=c(.1,10),
                labels=paste0(c("0,1","0,2","0,5","1","2","5","10"),":1")) +
  theme_zi() +
  scale_color_zi("bluegreen",reverse = TRUE) +
  geom_text_repel(data = . %>% 
                    arrange(Verhaeltnis) %>% ungroup() %>%
                    filter(row_number()<=5 | row_number()>=(n()-5)),
                  aes(label=str_wrap(name,10)),point.padding = NA,  nudge_y      = 15,
                  nudge_x = 0,
                  size=2,force=10,segment.colour = "lightgrey") +
  labs(title="Kreise unterscheiden sich beim Schutz Älterer in der 2. COVID-19 Welle stark",
         subtitle="Mittleres Verhältnis von Bevölkerungsinzidenz und Inzidenz 80+ Jahre nach 2 Wochen") +
annotate("text" ,x =.2:.75, y = 25, 
           label = "weniger betroffen",family="Calibri" , fontface =2)+
  annotate("text" ,x =1, y = 35, 
           label = "Ältere sind gleich betroffen",family="Calibri",fontface =2)+
  annotate("text", x =5, y = 25, 
                 label = "stärker betroffen",family="Calibri",fontface =2) +
  theme(axis.text.y = element_blank()) + scale_y_continuous(breaks = seq(0,30,5))

finalise_plot(myplot, 
              save_filepath = "~/Desktop/Kreisvergleich_Schutz_älterer.png",
              width_cm=22,height_cm=22*(9/16),
              source_name = "Datenbasis: RKI 11.12.2020, Meldungen der 36. bis 49. KW")



              