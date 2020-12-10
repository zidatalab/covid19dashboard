library(tidyverse)
library(lubridate)
library(readr)
library(DBI)

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
  group_by(id) %>% arrange(id,KW) %>% mutate(Gesamt_l2=lag(Gesamt,2))


myplot <- ggplot(plotdata,aes(x=`Gesamt_l2`,y=`A80+`)) + 
  geom_point(alpha=.05) + geom_smooth() +
  scale_y_continuous(limits = c(0,250)) +
  scale_x_continuous(limits=c(0,250)) +
  facet_grid(.~ifelse(KW>30,"ab KW 31.","bis KW 30")) +
  geom_abline(aes(intercept=0,slope=1), color="red") +
  coord_fixed()
