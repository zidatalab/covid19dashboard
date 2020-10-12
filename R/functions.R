# Packages
library(dplyr)
library(glue)
library(lubridate)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(DT)
library(DBI)
library(forcats)
library(EpiEstim)
library(plotly)
library(zicolors)
library(deSolve)
library(jsonlite)
library(readxl)

# parameters from literature
icu_days <- 10.1 # aok/divi paper lancet
share_icu <- (449+17838)/303258 # divi intensivregister and rki daily report 6. oktober 2020
altersgruppen_bund <- tibble("unter 20"=18.4, "20 bis 40"=24.6,	"40 bis 60"=28.4,
                        "60 bis 80"=21.7,	"80+"=6.8)/100 # destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html

# Connect to DB
# conn <- dbConnect(RSQLite::SQLite(), "../covid-19/data/covid19db.sqlite")
conn <- DBI::dbConnect(RPostgres::Postgres(),
                          host   = Sys.getenv("DBHOST"),
                          dbname = Sys.getenv("DBNAME"),
                          user      =  Sys.getenv("DBUSER"),
                          password        = Sys.getenv("DBPASSWORD"),
                          port     = 5432,
                          sslmode = 'require')

# get data
## get data from db
brd_timeseries <- tbl(conn,"brd_timeseries")
vorwarndata <- brd_timeseries %>% filter(id==0) %>% collect()  %>%
  mutate(
    cases_rm=floor(zoo::rollmean(cases, 7, fill=NA)),
    cases=ifelse(is.na(cases_rm), cases, cases_rm),
    Infected=cases-lag(cases,15)) %>% # Wg. 10 Tage infektiös und symptomatisch + 5 Tage asymptomatisch
  mutate(Rt=(cases-lag(cases,10))/lag(Infected,10)) %>% filter(!is.na(Infected) & !is.na(Rt))  %>%
  mutate(date=date(date),
         Neue_faelle=cases-lag(cases),
         Neue_faelle_Anstieg = Neue_faelle/lag(Neue_faelle),
         Vorwarnzeit= log(16000/Neue_faelle)/log(Neue_faelle_Anstieg),
         Situation = case_when(Vorwarnzeit<0 ~ "grün",
                               (Vorwarnzeit>18 )  ~ "orange",
                               (Vorwarnzeit>=0 & Vorwarnzeit<18)   ~ "rot"),
         Situation=factor(Situation,levels=c("grün","orange","rot"),ordered=T),
         show_val=wday(date)==3) %>% filter(date>=date("2020-03-02"))
prognosen <- tbl(conn,"prognosen") %>% filter((Tage<=28) | Tage %in% c(30,60,90,120,150,180))
brdprognosen <- tbl(conn,"prognosen") %>% filter((Tage<=90) & ((ebene=="Kreis") | (name=="Berlin")) )
rki <- tbl(conn,"rki")
divi <- tbl(conn,"divi") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
rki_divi_n_alter <- rki %>% group_by(Meldedatum,Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T)) %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,"0-59")) %>%
  group_by(Meldedatum,Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T)) %>% 
  arrange(Meldedatum,Altersgruppe) %>% 
  pivot_wider(id_cols = Meldedatum,
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  left_join(divi_all %>% filter(id==0) %>% collect(), by=c('Meldedatum'='daten_stand')) %>%
  left_join(vorwarndata %>% select(date, Infected), by=c('Meldedatum'='date')) %>%
  mutate("Fälle gesamt"= `Fälle_0-59`+ `Fälle_60-79`+ `Fälle_80+` , 
         "Todesfälle gesamt" = `Todesfälle_0-59`+ `Todesfälle_60-79`+ `Todesfälle_80+`,
         "60+" = (`Fälle_80+` + `Fälle_60-79` )/ `Fälle gesamt`, 
         "itsfaelle"=`faelle_covid_aktuell`/`Infected`,
         'Todesfälle'= `Todesfälle gesamt`/ `Fälle gesamt`) 
rki_alter_destatis <- rki %>% 
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% # this takes long unfortunately...
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T)) %>% 
  # arrange(Meldedatum,Altersgruppe) %>%
  collect() %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                                      "60+",
                                      ifelse(Altersgruppe %in% c("00-04", "05-14"),
                                             "0-15", Altersgruppe))) %>%
  group_by(Meldedatum,Altersgruppe, id) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T)) %>% 
  # arrange(Meldedatum,Altersgruppe, id) %>% 
  pivot_wider(id_cols = c(Meldedatum, id),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum), blid=floor(id/1000000))
rki_alter_destatis <- bind_rows(rki_alter_destatis,
                                 rki_alter_destatis %>%
                                   group_by(Meldedatum, blid) %>%
                                   summarise(`Fälle_15-34`=sum(`Fälle_15-34`),
                                             `Fälle_35-59`=sum(`Fälle_35-59`),
                                             `Fälle_0-15`=sum(`Fälle_0-15`),
                                             `Fälle_60+`=sum(`Fälle_60+`),
                                             `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                             `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                             `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                             `Todesfälle_60+`=sum(`Todesfälle_60+`)) %>%
                                   mutate(id=blid),
                                 rki_alter_destatis %>% group_by(Meldedatum) %>%
                                  summarise(`Fälle_15-34`=sum(`Fälle_15-34`),
                                            `Fälle_35-59`=sum(`Fälle_35-59`),
                                            `Fälle_0-15`=sum(`Fälle_0-15`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                            `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                            `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                            `Todesfälle_60+`=sum(`Todesfälle_60+`)) %>%
                                   mutate(id=0))
rki_alter_bund <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T)) %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                                  Altersgruppe,
                                  "0-59")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T)) %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols =  c(Meldedatum),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum))
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
aktuell <- tbl(conn,"params") %>% collect()
trends <- tbl(conn,"trends")
brd_testungen <- tbl(conn,"brd_testungen") %>% collect()
Datenstand <- tbl(conn,"Stand") %>% collect()
bundprognose <- prognosen %>% filter(id==0) %>% collect() %>%
  filter(Szenario!="Trend D") %>%
  mutate(Szenario=ifelse(Szenario=="Trend lokal","aktueller Trend",Szenario),
         Szenario=ifelse(Szenario=="Worst Case","Worst Case (R=1,3)",Szenario),
         Datum=as.Date(Datum,format="%d.%m.%Y")) %>%
  filter(Datum<date(max(Datum)+weeks(12)))
# labordaten <- tbl(conn, "Labordaten")

## read and update RKI-R-estimates
RKI_R <- tryCatch(
  {
    mytemp = tempfile()
    rki_r_data = "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile"
    download.file(rki_r_data, mytemp, method = "curl")
    Nowcasting_Zahlen <- read_excel(mytemp,
                                    sheet = "Nowcast_R", col_types = c("date",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric",
                                                                       "numeric", "numeric", "numeric"))
    if (dim(Nowcasting_Zahlen)[2] != 13){
      stop("RKI changed their R table")
    } else {
      write_csv(Nowcasting_Zahlen, "./data/nowcasting_r_rki.csv")
      Nowcasting_Zahlen
    }
  },
  error=function(e) {
    # read old data
    Nowcasting_Zahlen <- read_csv("./data/nowcasting_r_rki.csv")
    return(Nowcasting_Zahlen)
  }
)
Nowcasting_Zahlen <- read_csv("./data/nowcasting_r_rki.csv") # fix for break
rkiall <-  rki %>% select(AnzahlFall,AnzahlTodesfall,Meldedatum,Datenstand,NeuerFall,NeuerTodesfall) %>%
  mutate(NeuerFall=ifelse(NeuerFall==1,"Neue Meldung","Alte Meldung")) %>%
  group_by(Meldedatum,NeuerFall) %>% summarise(AnzahlFall=sum(AnzahlFall,na.rm=T)) %>%
  ungroup() %>% collect() %>%
  mutate(Meldedatum=date(Meldedatum),
         Wochenende=ifelse(wday(Meldedatum)==1 |wday(Meldedatum)==7,"Wochenende",NA))

# Hilfsfunktionen
SIR <- function(time, state, parameters, ngesamt, gamma) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/ngesamt * I * S
    dI <- beta/ngesamt * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

sirmodel<- function(ngesamt,  S,   I,   R,  R0,  gamma,  horizont=365) {
  # Set parameters
  ## Infection parameter beta; gamma: recovery parameter
  params <- c("beta" = R0*gamma)
  ## Timeframe
  times      <- seq(0, horizont, by = 1)
  ## Initial numbers
  init       <- c("S"=S, "I"=I, "R"=R)
  ## Time frame
  times      <- seq(0, horizont, by = 1)

  # Solve using ode (General Solver for Ordinary Differential Equations)
  out <- ode(y = init, times = times, func = SIR, parms = params, ngesamt=ngesamt, gamma=gamma)

  # change to data frame and reformat
  out <- as.data.frame(out) %>% select(-time) %>% rename(S=1,I=2,R=3) %>%
    mutate_at(c("S","I","R"),round)
  ## Show data
  return(as_tibble(out))
}

# Funktion zur Vorwarnzeit bei festem Rt
vorwarnzeit_berechnen <- function(ngesamt,cases,faelle,Kapazitaet,Rt=1.3){
  gamma=1/10
  infected=faelle/gamma
  recovered= cases-infected
  mysir <- sirmodel(ngesamt = ngesamt,
                    S = ngesamt - infected - recovered,
                    I = infected,
                    R = recovered,
                    R0 = Rt,
                    gamma = gamma,
                    horizont = 365) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  myresult <- NA
  myresult <- mysir %>% mutate(Tage=row_number()-1) %>% filter(Neue_Faelle>=Kapazitaet) %>% head(1) %>% pull(Tage)
  return(myresult)
}

### Vorwarnzeit aktuell
letzte_7_tage <-  brd_timeseries %>% collect() %>% mutate(date=date(date)) %>%
  group_by(id) %>% arrange(id,-as.numeric(date)) %>%
  filter(row_number()<=8) %>% # fuer die anzahl der neuen faelle der letzten 7 tage muessen wir die letzten 8 tage ziehen
  summarise(Faelle_letzte_7_Tage=first(cases)-last(cases)) %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7))
letzte_7_tage_altersgruppen_bund <-  rki_alter_bund %>%
  mutate(date=date(Meldedatum)) %>%
  arrange(-as.numeric(date)) %>%
  filter(row_number()<=7) %>% # hier sind tatsächliche Fälle und nicht cumsum
  summarise(`Faelle_letzte_7_Tage_0-59`=sum(`Fälle_0-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`)) %>%
  # mutate(`Faelle_letzte_7_Tage_pro_Tag_0-59`=round(`Faelle_letzte_7_Tage_0-59`/7),
  #        `Faelle_letzte_7_Tage_pro_Tag_60-79`=round(`Faelle_letzte_7_Tage_60-79`/7),
  #        `Faelle_letzte_7_Tage_pro_Tag_80+`=round(`Faelle_letzte_7_Tage_80+`/7)) %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select_(., "`unter 20`:`40 bis 60`"))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)))

  # bind_cols(., strukturdaten %>% filter(id==0)) %>%
  # mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select_(., "`unter 3 Jahre`:`55 bis unter 60 Jahre`"))/100000)),
  #        `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(sum(select_(., "`unter 3 Jahre`:`55 bis unter 60 Jahre`"))/100000)),
  #        `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(sum(select_(., "`unter 3 Jahre`:`55 bis unter 60 Jahre`"))/100000)))
letzte_7_tage_altersgruppen_destatis <- rki_alter_destatis %>%
  mutate(date=date(Meldedatum)) %>%
  group_by(id) %>% arrange(id,-as.numeric(date)) %>%
  filter(row_number()<=7) %>% # hier sind tatsächliche Fälle und nicht cumsum
  summarise(`Faelle_letzte_7_Tage_0-14`=sum(`Fälle_0-15`),
            `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
            `Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
            `Faelle_letzte_7_Tage_60+`=sum(`Fälle_60+`)) %>%
  ungroup() %>%
  left_join(., strukturdaten, by="id") %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((`unter 3 Jahre`+`3 bis unter 6 Jahre`+`6 bis unter 10 Jahre`+`10 bis unter 15 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((`15 bis unter 18 Jahre`+`18 bis unter 20 Jahre`+`20 bis unter 25 Jahre`+`25 bis unter 30 Jahre`+`30 bis unter 35 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((`35 bis unter 40 Jahre`+`40 bis unter 45 Jahre`+`45 bis unter 50 Jahre`+`50 bis unter 55 Jahre`+`55 bis unter 60 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+`75 Jahre und mehr`)/100000)))

ausgangsdaten <- aktuell  %>%
  select(id,name,ICU_Betten,EW_insgesamt,ebene,
         cases,R0) %>% filter(ebene!="Staaten" & !is.na(ebene)) %>% select(-ebene) %>% collect() %>%
  left_join(.,letzte_7_tage,by="id") %>%
  left_join(., letzte_7_tage_altersgruppen_destatis %>% select(
    id,
    `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
    `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
    `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60+`), by="id") %>%
  mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/(EW_insgesamt/100000)),
         Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw))


# vorwarnzeit aktueller tag daten
vorwarnzeitergebnis <- ausgangsdaten %>%
  mutate(Handlungsgrenze_7_tage=50*(EW_insgesamt/100000),
         Handlungsgrenze_pro_Tag=round(Handlungsgrenze_7_tage/7),
         R0 = ifelse((R0>1) & (Faelle_letzte_7_Tage_pro_Tag==0),NA,R0),
         Kapazitaet=ICU_Betten*0.25/share_icu/icu_days,
         Auslastung_durch_Grenze=round(100*(Handlungsgrenze_pro_Tag/Kapazitaet)))

myTage <- vorwarnzeitergebnis %>% rowwise() %>%
  do(Tage = vorwarnzeit_berechnen(.$EW_insgesamt, .$cases,.$Faelle_letzte_7_Tage_pro_Tag,.$Kapazitaet,1.3)) %>%
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- vorwarnzeitergebnis %>%
  mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=Vorwarnzeit-21)

## vorwarnzeitverlauf daten
offline_timeseries <- brd_timeseries %>% collect() %>% mutate(date=as.Date(date))
horizont <- as.integer(date(max(offline_timeseries %>% pull(date))) - date("2020-03-13"))
# datevsvorwarnzeit <- matrix(0, nrow=horizont+1, ncol=2, dimnames=list(date=0:horizont, cols=c("stichtag", "vorwarnzeit")))
vorwarnzeitverlauf <- tibble()
for (h in 0:horizont) {
  stichtag <- date(max(offline_timeseries %>% pull(date)))-h
  letzte_7_tage_h <-  offline_timeseries %>% mutate(date=date(date)) %>%
    filter(date<=stichtag) %>%
    group_by(id) %>% arrange(id,-as.numeric(date)) %>%
    filter(row_number()<=8) %>% # genau wie oben, 8 statt 7!
    summarise(Faelle_letzte_7_Tage=first(cases)-last(cases)) %>%
    mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7))
  letzte_7_tage_altersgruppen_destatis_h <- rki_alter_destatis %>%
    mutate(date=date(Meldedatum)) %>%
    filter(date<=stichtag) %>%
    group_by(id) %>% arrange(id,-as.numeric(date)) %>%
    filter(row_number()<=7) %>% # hier sind tatsächliche Fälle und nicht cumsum
    summarise(`Faelle_letzte_7_Tage_0-14`=sum(`Fälle_0-15`),
              `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
              `Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
              `Faelle_letzte_7_Tage_60+`=sum(`Fälle_60+`)) %>%
    ungroup() %>%
    left_join(., strukturdaten, by="id") %>%
    mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((`unter 3 Jahre`+`3 bis unter 6 Jahre`+`6 bis unter 10 Jahre`+`10 bis unter 15 Jahre`)/100000)),
           `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((`15 bis unter 18 Jahre`+`18 bis unter 20 Jahre`+`20 bis unter 25 Jahre`+`25 bis unter 30 Jahre`+`30 bis unter 35 Jahre`)/100000)),
           `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((`35 bis unter 40 Jahre`+`40 bis unter 45 Jahre`+`45 bis unter 50 Jahre`+`50 bis unter 55 Jahre`+`55 bis unter 60 Jahre`)/100000)),
           `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+`75 Jahre und mehr`)/100000)))
  
  ausgangsdaten_h <- aktuell  %>%
    select(id,name,ICU_Betten,EW_insgesamt,ebene,
           cases,R0) %>% filter(ebene!="Staaten" & !is.na(ebene)) %>% select(-ebene) %>% collect() %>%
    left_join(.,letzte_7_tage_h,by="id") %>%
    left_join(., letzte_7_tage_altersgruppen_destatis_h %>% select(
      id,
      `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
      `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
      `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
      `Faelle_letzte_7_Tage_je100TsdEinw_60+`), by="id") %>%
    mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/(EW_insgesamt/100000)),
           Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw))

  vorwarnzeitergebnis_h <- ausgangsdaten_h %>%
    mutate(Handlungsgrenze_7_tage=50*(EW_insgesamt/100000),
           Handlungsgrenze_pro_Tag=round(Handlungsgrenze_7_tage/7),
           R0 = ifelse((R0>1) & (Faelle_letzte_7_Tage_pro_Tag==0),NA,R0),
           Kapazitaet=ICU_Betten*0.25/share_icu/icu_days,
           Auslastung_durch_Grenze=round(100*(Handlungsgrenze_pro_Tag/Kapazitaet))) %>%
    filter(id<=16)

  myTage <- vorwarnzeitergebnis_h %>% rowwise() %>%
    do(Tage = vorwarnzeit_berechnen(.$EW_insgesamt, .$cases,.$Faelle_letzte_7_Tage_pro_Tag,.$Kapazitaet,1.3)) %>%
    unnest(cols = c(Tage), keep_empty=TRUE)
  vorwarnzeitergebnis_h <- vorwarnzeitergebnis_h %>%
    mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=Vorwarnzeit-21, date=stichtag)
  # datevsvorwarnzeit[h+1, ] <- c(h, vorwarnzeitergebnis_h$Vorwarnzeit[1])
  vorwarnzeitverlauf <- bind_rows(vorwarnzeitverlauf, vorwarnzeitergebnis_h)
}

write_csv(vorwarnzeitverlauf, "./data/datevsvorwarnzeit.csv")

## mitigation data generate
mitigation_data <- function(myid=0){
  df <- brd_timeseries %>% filter(id==myid) %>% collect()
  if (nrow(df)==0) {
    df <- left_join(trends,strukturdaten %>% filter(id==myid) %>%
                      select(Country=name,id)) %>% filter(!is.na(id))
  }
  df <- df %>% mutate(date=date(date)) %>%
    mutate(I_cases=cases-lag(cases),I_dead=deaths-lag(deaths)) %>%
    filter(!is.na(I_cases) & (date<max(date)-days(3))) %>%
    filter(I_cases>=0 & I_dead>=0)
  mindate <- min(df$date)
  myconfig <- make_config(list(mean_si = 5,std_si = 4))
  res_parametric_si <- estimate_R(df$I_cases, method="parametric_si", config = myconfig)
  res_parametric_si_deaths <- estimate_R(df$I_dead, method="parametric_si", config = myconfig)
  result <- bind_rows(res_parametric_si$R %>% mutate(Merkmal="Fälle"),
                      res_parametric_si_deaths$R %>% mutate(Merkmal="Todesfälle"))
  as_tibble(result) %>% mutate(date=mindate+days(round(t_start+t_end)/2)+1) %>%
    select(date,Merkmal,R_Mean=`Mean(R)`,R_std= `Std(R)`) %>% left_join(.,df,by="date")
}

blmitidata <- tibble()
for (theid in seq(0,16,1)){
  thename<-strukturdaten %>% filter(id==theid) %>% collect() %>% head(1) %>% pull(name)
  blmitidata = bind_rows(blmitidata,mitigation_data(theid) %>% mutate(name=thename,id=theid, date=date+5) %>%
                           left_join(., vorwarnzeitverlauf %>% filter(id==theid) %>% select(date, Vorwarnzeit_effektiv), by="date"))
}

myblmitidata <- blmitidata %>%
  filter(Merkmal=="Fälle"  & R_Mean<10 & date>=date("2020-03-13"))

### ALLE Bundesländer
mitigationsplot_blvergleich <- function(){
  myplot <- ggplot(myblmitidata %>% rename(R=R_Mean) %>% mutate(R=round(R,digits = 1)),
                   aes(x=date,y=R,group=name,color=name=="Gesamt",
                       text=paste("Region: ",name,"<br>","Neue Fälle:",I_cases))) +
    geom_hline(yintercept = 1) +
    geom_line(data = . %>% filter(name!="Gesamt"),size=1,show.legend = F,color="lightgrey")+
    geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
    scale_color_zi()  +
    theme_minimal() + scale_x_date(date_labels = "%d.%m.", breaks="1 month") +
    labs(x="",y="Reproduktionszahl R(t)",caption="Vergleich Bund vs. Bundesländer") +
    geom_vline(aes(xintercept=date("2020-03-16")),color="grey") +
    geom_vline(aes(xintercept=date("2020-03-22")),color="grey") +
    geom_vline(aes(xintercept=date("2020-04-17")),color="grey") +
    annotate("text", x = date("2020-03-16"), y = 3.3, label = "Schulschließungen\n16.3.",color="black",size=3) +
    annotate("text", x = date("2020-03-22"), y = 2.5, label = "Kontakteinschränkungen\n22.3.",color="black",size=3) +
    annotate("text", x = date("2020-04-17"), y = 2.0, label = "Lockerung der \nMaßnahmen\n17.4.",color="black",size=3) +
    theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}

### Plot on Age of cases and case fatality 
age_plot_fatality <- function(){
myplot <- ggplot(rki_divi_n_alter %>%
                   select(Meldedatum,
                          "Alter 60+ an Fällen"=`60+`,
                          "ITS-Fälle an Fällen"=`itsfaelle`, 
                          "Todesfälle an Fällen"= `Todesfälle`) %>% 
                   gather(Merkmal,Anteil,2:4) %>% mutate(Anteil=round(Anteil*100,digits=2)) ,
                 aes(x=Meldedatum,y=Anteil,color=Merkmal)) + geom_line() + theme_minimal() + 
  scale_color_zi() + labs(y="Verhältnis in %",x="Datum",color="") + 
  scale_x_date(breaks="1 month", date_labels = "%d.%m.") + 
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())
myplot %>% ggplotly(tooltip = c("x", "y")) %>% 
  layout(legend = list(orientation = "h", x = 0.1, y = 1.2))
}

#### Einzelne Länder # Hier neue Datenreihe Vorwarnzeit!
range_r <- range(myblmitidata$R_Mean)
range_vwz <- range(myblmitidata$Vorwarnzeit_effektiv, na.rm = TRUE)
mitigationsplot_bl <- function(myid){
  myname <- myblmitidata %>% filter(id==myid) %>% head(1) %>% pull(name)
  my_r_vwz_data <- myblmitidata %>% filter(id==myid) %>% rename(Datum=date, R=R_Mean, Vorwarnzeit=Vorwarnzeit_effektiv) %>% mutate(R=round(R,digits = 1)) %>%
    pivot_longer(c("Vorwarnzeit", "R"), names_to="Variable", values_to="Wert") %>%
    mutate(y_min=ifelse(Variable=="R", range_r[1], range_vwz[1]),
           y_max=ifelse(Variable=="R", range_r[2], range_vwz[2]))
  myplot <- ggplot(my_r_vwz_data,
                   aes(x=Datum,y=Wert,group=name,color=Variable,
                       text=paste("Region: ",name,"<br>Neue Fälle:",I_cases))) +
    geom_hline(aes(yintercept=ifelse(Variable=="R",1, 0))) +
    geom_line(size=2, show.legend = F) +
    scale_x_date(date_labels = "%d.%m.", breaks="1 month") +
    # facet_grid(Variable~., scales = "free") +
    facet_wrap(~Variable, scales = "free") +
    geom_blank(aes(y = y_min)) +
    geom_blank(aes(y = y_max)) +
    scale_color_zi()  +
    theme_minimal() +
    labs(x="", y="") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position='none',
          panel.spacing = unit(2, "lines")) +
    ggtitle(myname)
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}

### Akute infizierte Fälle
akutinfiziert <- ggplot(vorwarndata,aes(x=date,y=Infected,group=1)) +
  geom_area(fill="#0086C530") +
  geom_vline(aes(xintercept=date("2020-03-16")),color="black",linetype ="dotted") +
  geom_vline(aes(xintercept=date("2020-03-22")),color="black",linetype ="dotted") +
  geom_vline(aes(xintercept=date("2020-04-17")),color="black",linetype ="dotted") +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_minimal() +
  scale_x_date(breaks = "1 month",date_labels = "%d.%m.") +
  annotate("text", x = date("2020-03-16"), y = 22000, label = "Schulschließungen",color="black",size=3) +
  annotate("text", x = date("2020-03-22"), y = 42000, label = "Kontakteinschränkungen",color="black",size=3) +
  annotate("text", x = date("2020-04-17"), y = 43500, label = "Lockerungsbeschluss",color="black",size=3) +
  labs(y="Anzahl akut infiziert",x = "Datum") +
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

## plotfunction for vorwarnzeitverlauf brd
vorwarnzeitverlauf_plot <- function(){
  # change to function to avoid full breaks
  rki_reformat_r_ts <- RKI_R %>%
    dplyr::select(contains("Datum"), contains("7-Tage-R Wertes")) %>% dplyr::select(contains("Datum"), contains("Punkt"))
  colnames(rki_reformat_r_ts) <-c("date","RKI-R-Wert")
  rki_reformat_r_ts <- rki_reformat_r_ts %>% mutate(date=as.Date(date)+5)
  zivwz_vs_rkir_verlauf <- inner_join(vorwarnzeitverlauf %>%
                                        filter(id==0) %>%
                                        mutate(Vorwarnzeit=Vorwarnzeit_effektiv),
                                      rki_reformat_r_ts,
                                      by=c("date")) %>%
    pivot_longer(c("Vorwarnzeit", "RKI-R-Wert"), names_to="Variable", values_to="Wert")

  # handle errors
  myplot <- ggplot()
  tryCatch(
  myplot <- ggplot(zivwz_vs_rkir_verlauf,
                                   aes(x=date, y=Wert, color=Variable)) +
    facet_wrap(~Variable, scales = "free_y") +
    geom_line(size=2) +
    ylim(0, NA) +
    scale_color_zi() +
    labs(subtitle="Zi-Vorwarnzeit und RKI-R-Wert im Zeitverlauf",x="",y="") +
    theme_minimal() +
    theme(legend.position='none'))
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}

#  functions for data generation

make_theoretischedaten <- function(myid=0) {
fall <- vorwarnzeitergebnis %>% filter(id==myid)
Rt <- seq(1.1, 2, 0.1)
Vorwarnzeit <- rep(0, length(Rt))
Anstieg <- rep(0, length(Rt))
Reaktionszeit <- 21
Belastungsgrenze <- fall$Kapazitaet
gamma=1/10
for (i in seq(Rt)) {
  mysir <- sirmodel(ngesamt = fall$EW_insgesamt,
                    S = fall$EW_insgesamt - fall$cases,
                    I = (fall$Faelle_letzte_7_Tage_pro_Tag)/gamma,
                    R = fall$cases - (fall$Faelle_letzte_7_Tage_pro_Tag)/gamma,
                    R0 = Rt[i],
                    gamma =gamma,
                    horizont = 365) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  Vorwarnzeit[i] <- which.max(mysir$Neue_Faelle>Belastungsgrenze)
}
as_tibble(cbind(Rt,Vorwarnzeit)) %>%
  mutate(id=myid,Effektive_Vorwarnzeit=Vorwarnzeit-Reaktionszeit) %>%
  select(id,Rt,Vorwarnzeit,Effektive_Vorwarnzeit)
}

rki_fallzahl_bl <- function(){
  df <- vorwarnzeitergebnis %>% filter(id<17) %>% mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
                                                         R0=round(R0,digits = 2))
  df %>% select(Bundesland=name,
                "R(t)"=R0,
                "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
                "Effektive Vorwarnzeit aktuell"=Vorwarnzeit_effektiv,
                "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
                "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
                "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
                "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
                "Fälle insgesamt"=cases,
                "Fälle je 100 Tsd. Einw."=cases_je_100Tsd,
                "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag
  )
}

rki_fallzahl_kreis <- function(){
  df <- vorwarnzeitergebnis %>% filter(id>17 | name=="Berlin") %>% mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
                                                         R0=round(R0,digits = 2))
  bundeslaender <- aktuell %>% filter(id>0 & id<17) %>% select(blid=id,Bundesland=name) %>% collect()
  df <- df %>% mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>% left_join(.,bundeslaender) %>% arrange(blid,id)
  df %>% select(Kreis=name,
                Bundesland,
                "R(t)"=R0,
                "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
                "Effektive Vorwarnzeit lokal*"=Vorwarnzeit_effektiv, # needs communication
                "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
                "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
                "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
                "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
                "Fälle insgesamt"=cases,
                "Fälle je 100 Tsd. Einw."=cases_je_100Tsd,
                "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag
                )
}

# plots direkt

Auslastungsplot<- ggplot(vorwarnzeitergebnis %>% filter(id<17),
                         aes(x=forcats::fct_reorder(name,Auslastung_durch_Grenze),y=Auslastung_durch_Grenze,
                             fill=name=="Gesamt")) +
  geom_bar(stat="identity",show.legend = F) + coord_flip() +
  scale_fill_zi() + labs(subtitle="Auslastung ICU",x="",y="") +
  theme_minimal() +
  geom_text(aes(y=5,label=paste0(round(Auslastung_durch_Grenze),"%")),size=2.5,color="white") +
  scale_y_continuous(breaks=seq(0,60,20), limits=c(0,65),
                     labels =  function(x) paste0(x,"%"))
Vorwarnzeitplot <- ggplot(vorwarnzeitergebnis %>% filter(id<17),aes(x=forcats::fct_reorder(name,Auslastung_durch_Grenze),y=Vorwarnzeit)) +
  geom_bar(stat="identity",show.legend = F,fill=zi_cols("ziorange")) + coord_flip() +
  geom_hline(yintercept = 0,color="black") +
  geom_text(aes(y=5,label=paste(Vorwarnzeit,"Tage")),size=2.5,color="white") +
  labs(subtitle="Vorwarnzeit ab Interventionsgrenze",x="",y="") + theme_zi() +
  scale_y_continuous(breaks=seq(0,50,5) #, labels =  function(x) paste0(x," Tage")
  )

mycolorbreaks <- c(14,30,90)
plotdata_Anstieg <- make_theoretischedaten(myid=0) %>% select(Rt,Vorwarnzeit,"Effektive Vorwarnzeit"=Effektive_Vorwarnzeit) %>% filter(`Effektive Vorwarnzeit`>=0)  %>% gather(Merkmal,Wert,2:3)
plot_Anstiegtheor <- ggplot(plotdata_Anstieg, aes(x=Rt, y=Wert,color=Merkmal)) +
  geom_line(size=1.5, show.legend = F)+
  geom_point(size=3, show.legend = F)+
  geom_hline(yintercept = 0) +
  theme_minimal() + scale_color_zi() +
  scale_x_continuous(labels =  function(x) paste0(format(x,decimal.mark = ",")),breaks=seq(1.1, 2, 0.1))  +
  labs(y=paste0("Vorwarnzeit in Tagen"))+
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())

