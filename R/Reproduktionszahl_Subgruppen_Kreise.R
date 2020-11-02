library(DBI)
library(forcats)
library(tidyverse)
library(lubridate)
library("EpiEstim")
library("zicolors")
library("ggrepel")
library("here")
# extrafont::loadfonts(device="win")
extrafont::loadfonts(device="win")


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = "covid19db",
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

brd_timeseries <- tbl(conn,"brd_timeseries")
prognosen <- tbl(conn,"prognosen")
rki <- tbl(conn,"rki")
strukturdaten <- tbl(conn,"strukturdaten") 
params <- tbl(conn,"params")
trends <- tbl(conn,"trends")
Datenstand <- tbl(conn,"Stand")

rki_local <- rki %>% select(contains("Id"),date=Meldedatum,
                            age=Altersgruppe,cases=AnzahlFall,deaths=AnzahlTodesfall) %>% collect() %>%
mutate(date=as_date(date))  
strukturdaten_local <- strukturdaten %>% select(id,name,ebene) %>% collect()
rki_w_id <- bind_rows(rki_local %>% select(date,cases,deaths,age) %>% mutate(id=0) %>% left_join(strukturdaten_local) ,
                      rki_local %>% select(id=IdBundesland,date,cases,deaths,age) %>% left_join(strukturdaten_local) ,
                      rki_local %>% select(id=IdLandkreis,date,cases,deaths,age) %>% 
                        mutate(id=as.numeric(id)*1000) %>% left_join(strukturdaten_local)) %>%
            mutate(id=ifelse(round(id/1000000)==11,11000000,id))

alldates <- date(min(rki_w_id$date)+days(seq(0,-as.numeric(min(rki_w_id$date)-max(rki_w_id$date)))))
andata <- tibble(expand.grid(id=unique(rki_w_id$id) ,date=alldates )) %>% 
    left_join(.,rki_w_id) %>% mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  select(ebene,id,name,date,age,cases)
andata <- andata %>% filter(date>=(now()-days(30))) %>%
  filter((id>=06000000 & id<07000000) | (id %in% c(0,6)))
## mitigation data generate
id_list <- rki_w_id %>%count(id) %>% pull(id)
Altersgruppen_liste <- rki_w_id %>% filter(age!="unbekannt") %>% count(age) %>% pull(age)

mitigation_data <- function(myid=0, agegrp="ALL"){
  df <- andata %>% filter(id==myid) 
  
  if (agegrp!="ALL"){
    df <- df %>% filter(id==myid & age==agegrp) 
  }
  df <- df %>% ungroup() %>% group_by(date) %>% 
    summarise(cases=sum(cases,na.rm=T)) %>% ungroup() %>% 
    filter(!is.na(cases))
  myconfig <- make_config(list(mean_si = 5,std_si = 4))
  df <- df %>% filter(date<max(df$date)-days(3))
  mindate <- min(df$date)
  res_parametric_si <- estimate_R(df$cases, method="parametric_si", config = myconfig)
  
  as_tibble(res_parametric_si$R) %>% select(t_start ,t_end, R_t=`Mean(R)` ) %>% 
    mutate(date=mindate+days(0.5*(t_start+t_end))) %>% select(date,R_t)
}  
  

# All Regions
theresult = tibble()
for (id in unique(andata$id)) {
  newdata <- tibble()
  tryCatch(newdata <- mitigation_data(myid=id) %>% mutate(id=id,age="Gesamt") )
  theresult <- bind_rows(theresult,newdata)

  for (ag in Altersgruppen_liste){
    newdata <- tibble()
    tryCatch(newdata <- mitigation_data(myid=id, agegrp = ag) %>% mutate(id=id,age=ag) )
    theresult <- bind_rows(theresult,newdata)
  }
    
}



hessen <- theresult %>%  mutate(kw=isoweek(date)) %>% filter(kw==43) %>% group_by(id,age) %>% 
  summarise(R_t=mean(R_t))   %>%
  left_join(strukturdaten_local) %>% mutate(KW=43) %>%  spread(age,R_t) %>% ungroup() %>% select(-id)


