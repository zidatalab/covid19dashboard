##### Packages
library(DT)
library(DBI)
library(forcats)
library(EpiEstim)
library(plotly)
library(zicolors)
library(deSolve)
library(jsonlite)
library(readxl)
library(data.table)
library(dplyr)
library(glue)
library(lubridate)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(dtplyr)
library(zoo)
library(ISOcodes)

##### Source files
source("R/aux_functions.R")

##### parameters from literature
## AOK/DIVI/Busse Paper Lancet (Case characteristics...)
# icu_days <-  10.1 # 
# busselancet_altersgruppen_hospital <- tibble("Hosp059"=2896,
#                                              "Hosp6079"=1621+2158,
#                                              "Hosp80"=3346)
## Destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html
altersgruppen_bund <- tibble("unter 20"=18.4,
                             "20 bis 40"=24.6,
                             "40 bis 60"=28.4,
                             "60 bis 80"=21.7,
                             "80+"=6.8)/100
## icu-quoten nach altersgruppe
# dividay <- as_date("2020-11-16")
# divi_behandlungen_aktuell <- 26372/1.27+3436 # divi intensivregister on dividay
# icu_altersgruppen <- divi_behandlungen_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital)
## barmer
infektperiode <- 8
icu_days <- 21
infekt2icudays <- 14

##### Connect to DB
# conn <- dbConnect(RSQLite::SQLite(), "../covid-19/data/covid19db.sqlite")
conn <- DBI::dbConnect(RPostgres::Postgres(),
                          host   = Sys.getenv("DBHOST"),
                          dbname = Sys.getenv("DBNAME"),
                          user      =  Sys.getenv("DBUSER"),
                          password        = Sys.getenv("DBPASSWORD"),
                          port     = 5432,
                          sslmode = 'require')

##### get data
## get data from file
# its quoten
altersgruppen_beatmet <- as.vector(read_csv(file = "data/its_altersverteilung.csv") %>% 
                                     pivot_wider(names_from = Altersgruppe,
                                                 values_from = anzahl_beatmet))[c(2,1,3)]

itsquoten <- read_csv("data/itsquoten_final.csv") %>% 
  mutate(quote_its=quote_its/100) %>% 
  pivot_wider(id_cols = c(periode, beginn, ende),
              names_from = Altersgruppe,
              values_from = quote_its)
# einwohnende
kreise_regstat_alter <- read_delim("data/Bev2019_Kreis_AG_rki_geschlecht.txt", 
                                   ";",
                                   escape_double = FALSE,
                                   col_types = cols(X1 = col_skip()), 
                                   trim_ws = TRUE) %>%
  mutate(id=ifelse(Kreis==11000, 11, Kreis*1000)) %>%
  select(-Kreis, -m, -w) %>%
  pivot_wider(id_cols="id",
              names_from="AG_rki",
              names_prefix="ag_",
              values_from="ges")
kreise_regstat_alter <- bind_rows(tibble(id=0,
                                         ag_1=sum(kreise_regstat_alter$ag_1),
                                         ag_2=sum(kreise_regstat_alter$ag_2),
                                         ag_3=sum(kreise_regstat_alter$ag_3),
                                         ag_4=sum(kreise_regstat_alter$ag_4),
                                         ag_5=sum(kreise_regstat_alter$ag_5),
                                         ag_6=sum(kreise_regstat_alter$ag_6)),
                                  kreise_regstat_alter %>%
                                    mutate(blid=ifelse(id==11, 11, floor(id/1000000))) %>%
                                    group_by(blid) %>%
                                    summarise(ag_1=sum(ag_1, na.rm = TRUE),
                                              ag_2=sum(ag_2, na.rm = TRUE),
                                              ag_3=sum(ag_3, na.rm = TRUE),
                                              ag_4=sum(ag_4, na.rm = TRUE),
                                              ag_5=sum(ag_5, na.rm = TRUE),
                                              ag_6=sum(ag_6, na.rm = TRUE),
                                              .groups="drop") %>%
                                    mutate(id=blid) %>%
                                    filter(id!=11) %>%
                                    select(-blid),
                                  kreise_regstat_alter)
kreise_ror <- read_delim("data/kreise_2016_ror_kv_etc.csv", 
                                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                 grouping_mark = ".", encoding = "ISO-8859-1"), 
                                     trim_ws = TRUE) %>%
  select(krs17, ROR11)
pflegeheimbewohnende_2019_bundeslaender <- read_delim("data/pflegeheimbewohnende_2019_bundeslaender.csv", 
                                                      ";", escape_double = FALSE, trim_ws = TRUE)
## get data from db
brd_timeseries <- tbl(conn,"brd_timeseries") %>% collect() %>% mutate(date=base::as.Date(date))
rki <- tbl(conn,"rki") %>% collect()
# for assessment of meldeverzug
# write_csv(rki, paste0("data/rki_", max(as_date(rki$Meldedatum)), ".csv"))
# continue
divi <- tbl(conn,"divi") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
aktuell <- tbl(conn,"params") %>% collect()
jhu_germany <- tbl(conn, "trends") %>% filter(Country=="Germany") %>% select(date, cases, deaths, incident_cases) %>% collect()
vaccinations <- tbl(conn, "vaccinations") %>% collect() %>% mutate(datum=as_date(date),
                                                                   pubdatum=as_date(as_datetime(publication_date)))
# tt <- system.time({rki_dailycases <- tbl(conn, "rki_archive") %>%
#   filter(NeuerFall==1 | NeuerFall==-1) %>%
#   select(AnzahlFall, Datenstand) %>% 
#   group_by(Datenstand) %>%
#   summarise(newcases=sum(AnzahlFall)) %>%
#   collect()})

## read/update RKI-R-estimates
RKI_R <- tryCatch(
  {
    mytemp <- "data/rki_rwert_excel.xlsx"# tempfile()
    rki_r_data <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile"
    download.file(rki_r_data, mytemp, method = "curl")
    Nowcasting_Zahlen <- read_excel(mytemp,
                                    sheet = "Nowcast_R") %>% 
      mutate(datum=as_date(`Datum des Erkrankungsbeginns`, format="%d.%m.%Y"),
             r7tage=as.numeric(sub(",", ".", `Punktschätzer des 7-Tage-R Wertes`)))
    if (dim(Nowcasting_Zahlen)[2] != 15){
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

## read/update rki impf data from github
rki_vacc <- tryCatch(
  {
    mytemp <- tempfile()
    rki_vacc_data <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v2/all.csv"
    download.file(rki_vacc_data, mytemp, method = "curl")
    vacc_zahlen <- read_csv(mytemp)
    if (dim(vacc_zahlen)[2] != 5){
      stop("they changed the vacc table")
    } else {
      write_csv(vacc_zahlen, "./data/vacc_zahlen_ard.csv")
      vacc_zahlen
    }
  },
  error=function(e) {
    # read old data
    vacc_zahlen <- read_csv("./data/vacc_zahlen_ard.csv")
    return(vacc_zahlen)
  }
)
rki_vacc <- rki_vacc %>% 
  mutate(region=ifelse(region=="DE", "DE", paste0("DE-", region))) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(geo=ifelse(region=="DE", "Germany", geo),
         geotype=ifelse(region=="DE", "nation", "state"))
# rki_vacc <- tryCatch(
#   {
#     mytemp <- tempfile()
#     rki_vacc_data <- "https://raw.githubusercontent.com/n0rdlicht/rki-vaccination-scraper/main/data/de-vaccinations.csv"
#     download.file(rki_vacc_data, mytemp, method = "curl")
#     vacc_zahlen <- read_csv(mytemp)
#     if (dim(vacc_zahlen)[2] != 8){
#       stop("they changed the vacc table")
#     } else {
#       write_csv(vacc_zahlen, "./data/vacc_zahlen.csv")
#       vacc_zahlen
#     }
#   },
#   error=function(e) {
#     # read old data
#     vacc_zahlen <- read_csv("./data/vacc_zahlen.csv")
#     return(vacc_zahlen)
#   }
# )

##### make data for downstream analysis/plots
## daily cases from jhu
jhu_germany <- jhu_germany %>%
  arrange(date) %>%
  mutate(incident_deaths=deaths-lag(deaths),
         incident_mean7_deaths=round(rollapply(incident_deaths, 7, mean, align="right", fill=NA)), 
         date=as_date(date))
## if last divi report missing
maxdatum <- max(as_date(rki$Meldedatum))
lastsunday <- floor_date(maxdatum, "week")
sundaybeforelastsunday <- lastsunday-7
if (max(divi$daten_stand<maxdatum)) {
  divi_all <- bind_rows(divi_all, divi %>% mutate(daten_stand=as_date(maxdatum)))
}
## rki imputation because of delayed gesundheitsamt-meldungen
rki_original <- rki
rki <- rki %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)), maxdate=max(date(Meldedatum)))
rkiidkreise <- unique(rki$IdLandkreis)
rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
Kreise_allvalues <- expand_grid(Meldedatum=rkitimeframe$maxdate, IdLandkreis=rkiidkreise)
Kreise <- rki %>%
  mutate(Meldedatum=date(Meldedatum)) %>%
  full_join(., Kreise_allvalues, by=c("Meldedatum", "IdLandkreis"))
n_missingkreise <- 0
for (idkreis in rkiidkreise) { # take care of delayed reporting Landkreise (e.g. Hamburg 02000)
  if (is.na((Kreise %>% filter(Meldedatum==rkitimeframe$maxdate & IdLandkreis==idkreis) %>% pull(AnzahlFall))[1])) {
    n_missingkreise <- n_missingkreise+1
    sixdaysbefore <- Kreise %>% filter(Meldedatum>=rkitimeframe$maxdate-6 & IdLandkreis==idkreis)
    for (ag in rkiagegroups) {
      Kreise <- Kreise %>%
        add_row(IdBundesland=sixdaysbefore$IdBundesland[1],
                Bundesland=sixdaysbefore$Bundesland[1],
                Landkreis=sixdaysbefore$Landkreis[1],
                Altersgruppe=ag,
                Geschlecht="unbekannt",
                AnzahlFall=round(sum(sixdaysbefore$AnzahlFall[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6),
                AnzahlTodesfall=round(sum(sixdaysbefore$AnzahlTodesfall[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6),
                Meldedatum=rkitimeframe$maxdate,
                IdLandkreis=idkreis,
                AnzahlGenesen=round(sum(sixdaysbefore$AnzahlGenesen[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6))
    }
  }
}
cat("Kreise ohne aktuelle Meldung: ", n_missingkreise, "\n")
rki <- Kreise
## akut infizierte (X Tage infektiös nach Meldung)
akutinfiziert_data <- brd_timeseries %>% filter(id==0) %>%
  mutate(cases_rm=floor(zoo::rollmean(cases, 7, fill=NA)),
         cases=ifelse(is.na(cases_rm), cases, cases_rm),
         Infected=cases-lag(cases, infektperiode)) %>% #
  filter(!is.na(Infected)) %>%
  mutate(date=date(date)) %>%
  filter(date>=date("2020-03-02")) %>%
  select(date, Infected)
## its betten entwicklung
itsbetten_data <- divi_all %>%
  filter(id==0) %>%
  select(ICU_Betten, faelle_covid_aktuell, betten_belegt, daten_stand) %>%
  pivot_longer(cols = c("ICU_Betten", "faelle_covid_aktuell", "betten_belegt"),
               names_to = "Betten",
               values_to = "Anzahl")
## entwicklung alter, todesfälle, its
agefatality_data_pre <- rki %>% group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall, na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe == "80+", # %in% c("60-79","80+"),
                             Altersgruppe,"0-79")) %>% # 59")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols = Meldedatum,
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  left_join(divi_all %>% filter(id==0) %>% collect(), by=c('Meldedatum'='daten_stand')) %>%
  left_join(akutinfiziert_data %>% select(date, Infected), by=c('Meldedatum'='date'))
agefatality_data <- agefatality_data_pre %>%
  mutate("Fälle gesamt"= `Fälle_0-79`+ `Fälle_80+`, #"Fälle gesamt"= `Fälle_0-59`+ `Fälle_60-79`+ `Fälle_80+` , 
         "Todesfälle gesamt" = `Todesfälle_0-79`+ `Todesfälle_80+`,# "Todesfälle gesamt" = `Todesfälle_0-59`+ `Todesfälle_60-79`+ `Todesfälle_80+`,
         # "60+" = (`Fälle_80+` + `Fälle_60-79` )/ `Fälle gesamt`,
         "80+" = (`Fälle_80+`)/ `Fälle gesamt`,
         "itsfaelle"=`faelle_covid_aktuell`/lag(`Infected`, 14),
         'Todesfälle'= `Todesfälle gesamt`/ `Fälle gesamt`) %>%
  filter(Meldedatum>=as_date("2020-03-01")) %>%
  select(Meldedatum,
         # "Alter 60+ an Fällen"=`60+`,
         "Alter 80+ an Fällen"=`80+`,
         "ITS-Fälle an Fällen"=`itsfaelle`, 
         "Todesfälle an Fällen"= `Todesfälle`) %>% 
  gather(Merkmal, Anteil, 2:4) %>% mutate(Anteil=round(Anteil*100,digits=2))
age_its_death_data <- agefatality_data_pre %>%
  mutate(`Todesfälle`=`Todesfälle_0-79`+`Todesfälle_80+`) %>%
  select(Meldedatum, `Fälle_80+`, `Todesfälle`, faelle_covid_aktuell)
## faelle nach altersgruppe (mix rki und destatis altersgruppen)
rki_alter_destatis_kreise <- rki %>% lazy_dt() %>%
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% # this takes long unfortunately... but much faster with dtplyr!
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  collect() %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("00-04", "05-14"),
                                             "0-15", Altersgruppe)) %>%
  group_by(Meldedatum,Altersgruppe, id) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall, na.rm=T), .groups="drop") %>% 
  pivot_wider(id_cols = c(Meldedatum, id),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0, "Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum), blid=floor(id/1000000),
         `Fälle_60+`=`Fälle_60-79`+`Fälle_80+`) %>%
  right_join(.,
             expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days"),
                            id=unique(.$id)),
             by=c("Meldedatum", "id")) %>%
  replace(is.na(.), 0) %>%
  group_by(id) %>%
  arrange(Meldedatum) %>%
  mutate(cases059=cumsum(`Fälle_0-15`+`Fälle_15-34`+`Fälle_35-59`),
         cases6079=cumsum(`Fälle_60-79`),
         cases80=cumsum(`Fälle_80+`),
         cases60=cumsum(`Fälle_60+`)) %>%
  mutate(Infected059=cases059-lag(cases059, infektperiode),
         Infected6079=cases6079-lag(cases6079, infektperiode),
         Infected80=cases80-lag(cases80, infektperiode),
         Infected60=cases60-lag(cases60, infektperiode)) %>% 
  fill(Infected059, Infected6079, Infected80, Infected60, .direction = "up") %>%
  ungroup() %>%
  mutate(blid=floor(id/1000000)) %>%
  as_tibble()
rki_alter_destatis <- bind_rows(rki_alter_destatis_kreise %>%
                                  mutate(`Todesfälle_60+`=`Todesfälle_60-79`+`Todesfälle_80+`), # kreise
                                rki_alter_destatis_kreise %>% # bundeslaender
                                   group_by(Meldedatum, blid) %>%
                                   summarise(cases059=sum(cases059),
                                             cases6079=sum(cases6079),
                                             cases80=sum(cases80),
                                             cases60=sum(cases60),
                                             Infected059=sum(Infected059),
                                             Infected6079=sum(Infected6079),
                                             Infected80=sum(Infected80),
                                             Infected60=sum(Infected60),
                                             `Fälle_15-34`=sum(`Fälle_15-34`),
                                             `Fälle_35-59`=sum(`Fälle_35-59`),
                                             `Fälle_0-15`=sum(`Fälle_0-15`),
                                             `Fälle_60+`=sum(`Fälle_60+`),
                                             `Fälle_60-79`=sum(`Fälle_60-79`),
                                             `Fälle_80+`=sum(`Fälle_80+`),
                                             `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                             `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                             `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                             `Todesfälle_60-79`=sum(`Todesfälle_60-79`),
                                             `Todesfälle_80+`=sum(`Todesfälle_80+`),
                                             `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`, na.rm = TRUE), .groups="drop") %>%
                                   mutate(id=blid),
                                rki_alter_destatis_kreise %>% # bund gesamt
                                  group_by(Meldedatum) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            cases60=sum(cases60),
                                            Infected059=sum(Infected059),
                                            Infected6079=sum(Infected6079),
                                            Infected80=sum(Infected80),
                                            Infected60=sum(Infected60),
                                            `Fälle_15-34`=sum(`Fälle_15-34`),
                                            `Fälle_35-59`=sum(`Fälle_35-59`),
                                            `Fälle_0-15`=sum(`Fälle_0-15`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            `Fälle_60-79`=sum(`Fälle_60-79`),
                                            `Fälle_80+`=sum(`Fälle_80+`),
                                            `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                            `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                            `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                            `Todesfälle_60-79`=sum(`Todesfälle_60-79`),
                                            `Todesfälle_80+`=sum(`Todesfälle_80+`),
                                            `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`, na.rm = TRUE), .groups="drop") %>%
                                   mutate(id=0, blid=0))
## 7-tage-inzidenzen nach altersgruppe für den bund
letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                                  Altersgruppe,
                                  "0-59")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols =  c(Meldedatum),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdatum-6) %>%
  summarise(`Faelle_letzte_7_Tage_0-59`=sum(`Fälle_0-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`),
            `Faelle_letzte_7_Tage_60+`=sum(`Fälle_80+`+`Fälle_60-79`), .groups="drop") %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select(., `unter 20`:`40 bis 60`))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round((`Faelle_letzte_7_Tage_80+`+`Faelle_letzte_7_Tage_60-79`)/((`80+`+`60 bis 80`)/100000)))

##### icurates nach altersgruppen
## delay für fälle-->icu:
# rkidivi <- rki_alter_destatis %>%
#   filter(id==0) %>%
#   left_join(., divi_all %>% filter(id==0), by=c("Meldedatum"="daten_stand")) %>% drop_na()
# lengthrkidivi <- dim(rkidivi)[1]
# autocorhorizont <- 30
# autocors <- rep(0, lengthrkidivi-autocorhorizont+1)
# for (lag in 0:autocorhorizont) { autocors[lag+1] <- cor(rkidivi$Infected80[1:(lengthrkidivi-autocorhorizont)], rkidivi$faelle_covid_aktuell[(1+lag):(lengthrkidivi-autocorhorizont+lag)]) }
# iculag <- which.max(autocors)-1
# iculag <- 0
# iculag <- 14
# cases_altersgruppen <- rki %>% group_by(Meldedatum,Altersgruppe) %>% 
#   summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
#             AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
#   arrange(Meldedatum, Altersgruppe) %>% collect() %>%
#   mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
#          Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
#                              Altersgruppe,"0-59")) %>%
#   group_by(Meldedatum, Altersgruppe) %>% 
#   summarise("Fälle"=sum(AnzahlFall , na.rm = T),
#             "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
#   arrange(Meldedatum, Altersgruppe) %>% 
#   pivot_wider(id_cols = Meldedatum,
#               names_from = Altersgruppe,
#               values_from = c("Fälle","Todesfälle"),
#               values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
#   mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
#   arrange(Meldedatum) %>%
#   mutate(cases059=cumsum(`Fälle_0-59`),
#          cases6079=cumsum(`Fälle_60-79`),
#          cases80=cumsum(`Fälle_80+`)) %>% 
#   filter(Meldedatum==dividay-iculag) %>% 
#   select(cases059, cases6079, cases80) 
# icurate_altersgruppen_busse <- icu_altersgruppen/cases_altersgruppen

##### Vorwarnzeit: Daten aggregieren und VWZ berechnen
## aggregiere daten für vorwarnzeit
letzte_7_tage <-  brd_timeseries %>% 
  group_by(id) %>% arrange(id,-as.numeric(date)) %>%
  summarise(Faelle_letzte_7_Tage=lag(cases, 7)-cases, 
            date=date+7,
            .groups="keep") %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7)) %>%
  ungroup() %>%
  drop_na()
# letzte_7_tage_altersgruppen_destatis <- rki_alter_destatis %>%
#   mutate(date=date(Meldedatum)) %>%
#   group_by(id) %>% arrange(id, -as.numeric(date)) %>%
#   summarise(`Faelle_letzte_7_Tage_0-14`=zoo::rollsum(`Fälle_0-15`, 7),
#             `Faelle_letzte_7_Tage_15-34`=zoo::rollsum(`Fälle_15-34`, 7),
#             `Faelle_letzte_7_Tage_35-59`=zoo::rollsum(`Fälle_35-59`, 7),
#             `Faelle_letzte_7_Tage_0-59`=`Faelle_letzte_7_Tage_0-14`+`Faelle_letzte_7_Tage_15-34`+`Faelle_letzte_7_Tage_35-59`,
#             `Faelle_letzte_7_Tage_60-79`=zoo::rollsum(`Fälle_60-79`, 7),
#             `Faelle_letzte_7_Tage_80+`=zoo::rollsum(`Fälle_80+`, 7),
#             `Faelle_letzte_7_Tage_60+`=zoo::rollsum(`Fälle_60+`, 7),
#             date=zoo::rollmax(date, 7),
#             .groups="drop") %>%
#   left_join(., strukturdaten, by="id") %>%
#   mutate(Faelle_letzte_7_Tage_pro_Tag_059=round(`Faelle_letzte_7_Tage_0-59`/7),
#          Faelle_letzte_7_Tage_pro_Tag_6079=round(`Faelle_letzte_7_Tage_60-79`/7),
#          Faelle_letzte_7_Tage_pro_Tag_80=round(`Faelle_letzte_7_Tage_80+`/7),
#          `Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((`unter 3 Jahre`+`3 bis unter 6 Jahre`+`6 bis unter 10 Jahre`+`10 bis unter 15 Jahre`)/100000)),
#          `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((`15 bis unter 18 Jahre`+`18 bis unter 20 Jahre`+`20 bis unter 25 Jahre`+`25 bis unter 30 Jahre`+`30 bis unter 35 Jahre`)/100000)),
#          `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((`35 bis unter 40 Jahre`+`40 bis unter 45 Jahre`+`45 bis unter 50 Jahre`+`50 bis unter 55 Jahre`+`55 bis unter 60 Jahre`)/100000)),
#          `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+`75 Jahre und mehr`)/100000))) %>%
#   left_join(., rki_alter_destatis %>% select(cases059, cases6079, cases80, Infected059, Infected6079, Infected80, id, Meldedatum), by=c("id"="id", "date"="Meldedatum")) %>%
#   mutate(EW059=rowSums(select(., `unter 3 Jahre`:`55 bis unter 60 Jahre`)),
#          EW6079=`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+round(0.4*`75 Jahre und mehr`),
#          EW80=round(0.6*`75 Jahre und mehr`))
letzte_7_tage_altersgruppen_regstat <- rki_alter_destatis %>%
  mutate(date=date(Meldedatum)) %>%
  group_by(id) %>% arrange(id, -as.numeric(date)) %>%
  summarise(`Faelle_letzte_7_Tage_0-14`=zoo::rollsum(`Fälle_0-15`, 7),
            `Faelle_letzte_7_Tage_15-34`=zoo::rollsum(`Fälle_15-34`, 7),
            `Faelle_letzte_7_Tage_35-59`=zoo::rollsum(`Fälle_35-59`, 7),
            `Faelle_letzte_7_Tage_0-59`=`Faelle_letzte_7_Tage_0-14`+`Faelle_letzte_7_Tage_15-34`+`Faelle_letzte_7_Tage_35-59`,
            `Faelle_letzte_7_Tage_60-79`=zoo::rollsum(`Fälle_60-79`, 7),
            `Faelle_letzte_7_Tage_80+`=zoo::rollsum(`Fälle_80+`, 7),
            `Faelle_letzte_7_Tage_60+`=zoo::rollsum(`Fälle_60+`, 7),
            date=zoo::rollmax(date, 7),
            .groups="drop") %>%
  left_join(., kreise_regstat_alter, by=c("id")) %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag_059=round(`Faelle_letzte_7_Tage_0-59`/7),
         Faelle_letzte_7_Tage_pro_Tag_6079=round(`Faelle_letzte_7_Tage_60-79`/7),
         Faelle_letzte_7_Tage_pro_Tag_80=round(`Faelle_letzte_7_Tage_80+`/7),
         `Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((ag_1+ag_2)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((ag_3)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((ag_4)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((ag_5+ag_6)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/((ag_5)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/((ag_6)/100000))) %>%
  left_join(., rki_alter_destatis %>% select(cases059, cases6079, cases80, Infected059, Infected6079, Infected80, id, Meldedatum), by=c("id"="id", "date"="Meldedatum")) %>%
  mutate(EW059=ag_1+ag_2+ag_3+ag_4,
         EW6079=ag_5,
         EW80=ag_6,
         EW_insgesamt=EW059+EW6079+EW80)
ausgangsdaten <- letzte_7_tage %>%
  left_join(., divi_all %>%
              select(id, ICU_Betten, betten_frei, faelle_covid_aktuell, daten_stand) %>%
              mutate(id=ifelse(id>16, id*1000, id)),
            by=c("id"="id", "date"="daten_stand")) %>%
  left_join(., letzte_7_tage_altersgruppen_regstat %>% select(
    id,
    date,
    cases059, cases6079, cases80,
    Infected059, Infected6079, Infected80,
    EW059, EW6079, EW80,
    EW_insgesamt,
    `Faelle_letzte_7_Tage_pro_Tag_059`,
    `Faelle_letzte_7_Tage_pro_Tag_6079`,
    `Faelle_letzte_7_Tage_pro_Tag_80`,
    `Faelle_letzte_7_Tage_0-59`,
    `Faelle_letzte_7_Tage_60-79`,
    `Faelle_letzte_7_Tage_60+`,
    `Faelle_letzte_7_Tage_80+`,
    `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
    `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
    `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60-79`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60+`,
    `Faelle_letzte_7_Tage_je100TsdEinw_80+`), by=c("id", "date")) %>%
  left_join(., brd_timeseries %>% select(date, cases, id), by=c("id", "date")) %>%
  mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/((EW059+EW6079+EW80)/100000)),
         Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw),
         Kapazitaet_Betten=(faelle_covid_aktuell+betten_frei)) # /icu_days
ausgangsdaten_ror <- ausgangsdaten %>%
  inner_join(., kreise_ror %>%
               mutate(krs17=ifelse(krs17==11000, 11, 1000*krs17)),
             by=c("id"="krs17")) %>%
  filter(date%in%c(maxdatum, lastsunday, sundaybeforelastsunday)) %>%
  group_by(ROR11, date) %>%
  summarise(EW059=sum(EW059, na.rm = TRUE),
            EW6079=sum(EW6079, na.rm = TRUE),
            EW80=sum(EW80, na.rm = TRUE),
            Infected059=sum(Infected059, na.rm = TRUE),
            Infected6079=sum(Infected6079, na.rm = TRUE),
            Infected80=sum(Infected80, na.rm = TRUE),
            cases059=sum(cases059, na.rm = TRUE),
            cases6079=sum(cases6079, na.rm = TRUE),
            cases80=sum(cases80, na.rm = TRUE),
            faelle_covid_aktuell=sum(faelle_covid_aktuell, na.rm = TRUE),
            Kapazitaet_Betten=sum(Kapazitaet_Betten, na.rm = TRUE),
            .groups="drop")
## berechne vorwarnzeit
myTage <- ausgangsdaten %>% filter((date>=as_date("2020-03-13") & id<=16) |
                                     (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  mutate(periode=case_when(
    date>="2020-01-01" & date<="2020-03-31" ~ 1,
    date>="2020-04-01" & date<="2020-04-30" ~ 2,
    date>="2020-05-01" & date<="2020-05-31"  ~ 3,
    date>="2020-06-01" & date<="2020-09-30"  ~ 4,
    date>="2020-10-01" & date<="2020-11-30"  ~ 5,
    date>="2020-12-01" & date<="2021-12-31"  ~ 6
  )) %>%
  left_join(itsquoten %>% select(periode, `bis 60`, `60-80`, `ueber 80`),
            by="periode") %>% 
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round(
                                       (.$faelle_covid_aktuell*
                                          altersgruppen_beatmet/
                                          sum(altersgruppen_beatmet)) %>%
                                         as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = c(.$`bis 60`, 
                                                               .$`60-80`, 
                                                               .$`ueber 80`)
                                     )
     ) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
myTage_ror <- ausgangsdaten_ror %>% 
  mutate(periode=case_when(
    date>="2020-01-01" & date<="2020-03-31" ~ 1,
    date>="2020-04-01" & date<="2020-04-30" ~ 2,
    date>="2020-05-01" & date<="2020-05-31"  ~ 3,
    date>="2020-06-01" & date<="2020-09-30"  ~ 4,
    date>="2020-10-01" & date<="2020-11-30"  ~ 5,
    date>="2020-12-01" & date<="2021-12-31"  ~ 6
  )) %>%
  left_join(itsquoten %>% select(periode, `bis 60`, `60-80`, `ueber 80`),
            by="periode") %>% 
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, 
                                                       .$Infected6079, 
                                                       .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*
                                                         altersgruppen_beatmet/
                                                         sum(altersgruppen_beatmet)) %>%
                                       as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
     icurate_altersgruppen = c(.$`bis 60`, 
                               .$`60-80`, 
                               .$`ueber 80`)
  )) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- ausgangsdaten %>%
  filter((date>=as_date("2020-03-13") & id<=16) |
           (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis_ror <- ausgangsdaten_ror %>%
  mutate(Vorwarnzeit = myTage_ror$Tage, Vorwarnzeit_effektiv = pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis <- vorwarnzeitergebnis %>%
  left_join(., kreise_ror %>%
               mutate(krs17=ifelse(krs17==11000, 11, 1000*krs17)),
             by=c("id"="krs17")) %>%
  left_join(., vorwarnzeitergebnis_ror %>%
              select(ROR11, date, Vorwarnzeit, Vorwarnzeit_effektiv),
            by=c("date", "ROR11"),
            suffix=c("", "_ROR"))
R_aktuell_Bund <- aktuell$R0[aktuell$id==0]


##### weitere daten für dashboard
## siebentageinzidenzen verlauf
rki_7ti_kreise <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  filter(AnzahlFall>=0) %>%
  mutate(KW=isoweek(Meldedatum),
         Jahr=ifelse(KW==53, 2020, year(Meldedatum)),
         JahrKW=paste0(Jahr, "-", str_pad(KW, 2, pad="0")),
         # Altersgruppe=case_when(
         #   Altersgruppe=="A00-A04" ~ "ag_1",
         #   Altersgruppe=="A05-A14" ~ "ag_2",
         #   Altersgruppe=="A15-A34" ~ "ag_3",
         #   Altersgruppe=="A35-A59" ~ "ag_4",
         #   Altersgruppe=="A60-A79" ~ "ag_5",
         #   Altersgruppe=="A80+" ~ "ag_6"
         Altersgruppe=case_when(
           Altersgruppe=="A80+" ~ "80+",
           Altersgruppe=="A60-A79" ~ "60-79",
           TRUE ~ "0-59"
         )) %>%
  group_by(IdLandkreis, JahrKW, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = TRUE), 
            .groups="drop")
rki_7ti_kreise_full <- rki_7ti_kreise %>%
  expand(IdLandkreis, JahrKW, Altersgruppe) %>%
  left_join(., rki_7ti_kreise, by=c("IdLandkreis", "Altersgruppe", "JahrKW")) %>%
  mutate(AnzahlFall=ifelse(is.na(AnzahlFall), 0, AnzahlFall))
rki_7ti_laender <- rki_7ti_kreise_full %>%
  mutate(BLID=floor(as.integer(IdLandkreis)/1000)) %>%
  group_by(BLID, JahrKW, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = TRUE),
            .groups="drop")
rki_7ti_bund <- rki_7ti_laender %>%
  group_by(JahrKW, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = TRUE),
            .groups="drop")
rki_7ti_alle <- bind_rows(rki_7ti_bund %>%
                            mutate(id=0),
                          rki_7ti_laender %>%
                            mutate(id=BLID) %>%
                            select(-BLID),
                          rki_7ti_kreise_full %>%
                            mutate(id=as.integer(IdLandkreis)*1000) %>%
                            filter(id>=12000000 | id<11000000) %>%
                            select(-IdLandkreis)) %>%
  left_join(., kreise_regstat_alter %>%
              pivot_longer(cols=contains("ag_"),
                           names_to="Altersgruppe",
                           values_to="Einwohnende") %>%
              mutate(Altersgruppe=case_when(
                Altersgruppe=="ag_6" ~ "80+",
                Altersgruppe=="ag_5" ~ "60-79",
                TRUE ~ "0-59"
              )) %>%
              group_by(id, Altersgruppe) %>%
              summarise(Einwohnende=sum(Einwohnende, na.rm=TRUE),
                        .groups="drop"), by=c("id", "Altersgruppe")) %>%
  mutate(STI=round(AnzahlFall/Einwohnende*100000),
         datesunday=case_when(JahrKW=="2020-53" ~ base::as.Date("2021-01-03"),
                              JahrKW<"2020-53" ~ base::as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w"),
                              JahrKW>="2021-01" ~ base::as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w")+7)
                           ) %>%
  filter(datesunday<=lastsunday)
## rki-r-wert und vorwarnzeit
rki_reformat_r_ts <- RKI_R %>%
  dplyr::select(datum, r7tage)
colnames(rki_reformat_r_ts) <-c("date","RKI-R-Wert")
rki_reformat_r_ts <- rki_reformat_r_ts %>% mutate(date=base::as.Date(date)+5)
rki_r_und_zi_vwz_data <- full_join(vorwarnzeitergebnis %>%
                                     filter(id==0) %>%
                                     mutate(Vorwarnzeit=Vorwarnzeit), # _effektiv
                                   rki_reformat_r_ts,
                                   by=c("date")) %>%
  pivot_longer(c("Vorwarnzeit", "RKI-R-Wert"), names_to="Variable", values_to="Wert") %>%
  select(date, Wert, Variable)
## mitigation data generate
mitigation_data <- function(myid=0){
  df <- brd_timeseries %>% filter(id==myid) %>% collect()
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
  blmitidata = bind_rows(blmitidata, mitigation_data(theid) %>% mutate(name=thename, id=theid, date=date+5) %>%
                           left_join(., vorwarnzeitergebnis %>% filter(id==theid) %>% select(date, Vorwarnzeit), by="date")) # _effektiv
}
myblmitidata <- blmitidata %>%
  filter(Merkmal=="Fälle"  & R_Mean<10 & date>=date("2020-03-13"))
## r-wert bund verlauf
rwert_bund_data <- myblmitidata %>%
  rename(R=R_Mean) %>%
  mutate(R=round(R,digits = 2)) %>%
  select(date, R, name, I_cases)
## r-wert- und vwz-verlauf bundesländer
range_r <- range(myblmitidata$R_Mean)
range_vwz <- range(myblmitidata$Vorwarnzeit, na.rm = TRUE)
bundeslaender_r_und_vwz_data <- myblmitidata %>%
  rename(Datum=date, R=R_Mean) %>%
  mutate(R=round(R,digits = 2)) %>%
  pivot_longer(c("Vorwarnzeit", "R"), names_to="Variable", values_to="Wert") %>%
  mutate(y_min=ifelse(Variable=="R", 0, 0),
         y_max=ifelse(Variable=="R", range_r[2], range_vwz[2])) %>%
  select(id, name, Datum, Wert, Variable, I_cases, y_min, y_max)
## vaccination gesamt bundesländer
# vacc_bl <- vaccinations %>%
#   filter(datum==max(vaccinations$datum) &
#            metric=="impfungen_kumulativ") %>%
#   left_join(., pflegeheimbewohnende_2019_bundeslaender %>%
#               select(Kuerzel, Bundesland_ID),
#             by=c("region"="Kuerzel")) %>%
#   mutate(impfungen_kumulativ=value)
vacc_bl <- rki_vacc %>%
  filter(date==max(rki_vacc$date) &
           metric=="personen_erst_kumulativ") %>%# key=="sum_initial") %>% # mit mindestens erster impfung
  mutate(impfungen_initial=value)
## vaccination effects plot data
brd_sti <- rki %>%
  filter(Meldedatum>=as_date("2020-12-01")-7) %>%
  group_by(Meldedatum) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            einwohnende=kreise_regstat_alter %>%
              filter(id==0) %>%
              mutate(einwohnende=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6) %>%
              pull(einwohnende),
            .groups="drop") %>%
  mutate(STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                      lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000))
vaccination_sti_its_death_data <- rki_vacc %>%
  filter(geo=="Germany" & metric=="dosen_kumulativ") %>%
  select(datum=date, impfungen_kumulativ=value) %>%
  right_join(., brd_sti %>%
               select(Meldedatum, STI),
             by=c("datum"="Meldedatum")) %>%
  left_join(., divi_all %>%
              filter(id==0) %>%
              select(faelle_covid_aktuell, daten_stand),
            by=c("datum"="daten_stand")) %>%
  left_join(., jhu_germany %>% select(date, incident_mean7_deaths), by=c("datum"="date"))
## data table for subpage Impfungen
rki_vacc_complete <- rki_vacc %>%
  arrange(geotype) %>%
  bind_rows(rki_vacc %>% 
              filter(date<="2021-01-15" & metric=="dosen_kumulativ") %>%
              mutate(metric="personen_erst_kumulativ")) %>%
  mutate(geo=ifelse(geo=="Germany", "Deutschland", geo))
vacc_gesamt <- rki_vacc_complete %>%
  filter(metric=="personen_erst_kumulativ" | metric=="personen_voll_kumulativ" |
           metric=="dosen_kumulativ") %>%
  filter(date==max(date)) %>%
  left_join(aktuell %>% 
              select(geo=name, population=EW_insgesamt) %>% 
              mutate(geo=ifelse(geo=="Gesamt", "Deutschland", geo)),
            by="geo") %>% 
  select(geo, metric, value, population, date)
# vacc_prof <- rki_vacc_complete %>%
#   filter(key=="ind_prof_initial" | key=="ind_prof_booster") %>%
#   filter(date==max(date)) %>%
#   mutate(population=round(3600000/83166711*population)) %>%
#   select(geo, key, value, population, date)
# vacc_age <- rki_vacc_complete %>%
#   filter(key=="ind_alter_initial" | key=="ind_alter_booster") %>%
#   filter(date==max(date)) %>%
#   left_join(pflegeheimbewohnende_2019_bundeslaender %>% select(Bundesland, Bundesland_ID),
#             by=c("geo"="Bundesland")) %>%
#   left_join(kreise_regstat_alter %>% select(id, ag_6),
#             by=c("Bundesland_ID"="id")) %>%
#   mutate(population=ag_6) %>%
#   select(geo, key, value, population, date)
vacc_age_60p <- rki_vacc_complete %>%
  filter(metric=="personen_erst_kumulativ_impfstelle_zentral_alter_60plus" | metric=="personen_voll_kumulativ_impfstelle_zentral_alter_60plus" |
           metric=="personen_erst_kumulativ_impfstelle_aerzte_alter_60plus" | metric=="personen_voll_kumulativ_impfstelle_aerzte_alter_60plus") %>%
  filter(date==max(date)) %>%
  left_join(pflegeheimbewohnende_2019_bundeslaender %>% 
              select(Bundesland, Bundesland_ID),
            by=c("geo"="Bundesland")) %>%
  left_join(kreise_regstat_alter %>% select(id, ag_5, ag_6),
            by=c("Bundesland_ID"="id")) %>%
  mutate(population=ag_6+ag_5) %>%
  select(geo, metric, value, population, date)
vacc_age_60m <- rki_vacc_complete %>%
  filter(metric=="personen_erst_kumulativ_impfstelle_zentral_alter_unter60" | metric=="personen_voll_kumulativ_impfstelle_zentral_alter_unter60" |
           metric=="personen_erst_kumulativ_impfstelle_aerzte_alter_unter60" | metric=="personen_voll_kumulativ_impfstelle_aerzte_alter_unter60") %>%
  filter(date==max(date)) %>%
  left_join(pflegeheimbewohnende_2019_bundeslaender %>% 
              select(Bundesland, Bundesland_ID),
            by=c("geo"="Bundesland")) %>%
  left_join(kreise_regstat_alter %>% select(id, ag_1, ag_2, ag_3, ag_4),
            by=c("Bundesland_ID"="id")) %>%
  mutate(population=ag_1+ag_2+ag_3+ag_4) %>%
  select(geo, metric, value, population, date)
# vacc_pflege <- rki_vacc_complete %>%
#   filter(key=="ind_pflege_initial" | key=="ind_pflege_booster") %>%
#   filter(date==max(date)) %>%
#   left_join(pflegeheimbewohnende_2019_bundeslaender %>% select(Bundesland, Bundesland_ID, vollstationaere_dauerpflege),
#             by=c("geo"="Bundesland")) %>%
#   mutate(population=vollstationaere_dauerpflege) %>%
#   select(geo, key, value, population, date)
vacc_alle <- vacc_gesamt %>%
  # bind_rows(vacc_age, vacc_pflege, vacc_prof) %>%
  bind_rows(vacc_age_60m, vacc_age_60p) %>%
  mutate(population=ifelse(population<value, value, population))
vacc_alle_faktenblatt <- vacc_alle %>%
  pivot_wider(id_cols=c("geo"), 
              names_from="metric", 
              values_from=c("value", "population")) %>%
  mutate(quote_initial_gesamt=(value_personen_erst_kumulativ)/population_personen_erst_kumulativ,
         quote_booster_gesamt=(value_personen_voll_kumulativ)/population_personen_voll_kumulativ,
         quote_initial_alter_60m=(`value_personen_erst_kumulativ_impfstelle_zentral_alter_unter60`+`value_personen_erst_kumulativ_impfstelle_aerzte_alter_unter60`)/`population_personen_erst_kumulativ_impfstelle_aerzte_alter_unter60`,
         quote_booster_alter_60m=(`value_personen_voll_kumulativ_impfstelle_zentral_alter_unter60`+`value_personen_voll_kumulativ_impfstelle_aerzte_alter_unter60`)/`population_personen_erst_kumulativ_impfstelle_aerzte_alter_unter60`,
         quote_initial_alter_60p=(`value_personen_erst_kumulativ_impfstelle_zentral_alter_60plus`+`value_personen_erst_kumulativ_impfstelle_aerzte_alter_60plus`)/`population_personen_erst_kumulativ_impfstelle_zentral_alter_60plus`,
         quote_booster_alter_60p=(`value_personen_voll_kumulativ_impfstelle_zentral_alter_60plus`+`value_personen_voll_kumulativ_impfstelle_aerzte_alter_60plus`)/`population_personen_erst_kumulativ_impfstelle_zentral_alter_60plus`,
         # quote_initial_pflege=(value_ind_pflege_initial-value_ind_pflege_booster)/population_ind_pflege_initial,
         # quote_booster_pflege=value_ind_pflege_booster/population_ind_pflege_initial,
         # quote_initial_alter=(value_ind_alter_initial-value_ind_alter_booster)/population_ind_alter_initial,
         # quote_booster_alter=value_ind_alter_booster/population_ind_alter_initial,
         # quote_initial_prof=(value_ind_prof_initial-value_ind_prof_booster)/population_ind_prof_initial,
         # quote_booster_prof=value_ind_prof_booster/population_ind_prof_initial,         
         impfungen_gesamt=value_dosen_kumulativ)
vacc_table <- vacc_alle_faktenblatt %>%
  mutate(Bundesland=ifelse(geo=="Deutschland", "Gesamt", geo),
         # "PHB nur 1x"=format(round(100*quote_initial_pflege, 1), decimal.mark=","),
         # "PHB 2x"=format(round(100*quote_booster_pflege, 1), decimal.mark=","),
         "Alter 60+ min. 1x"=ifelse(is.na(quote_initial_alter_60p), "k.A.", format(round(100*quote_initial_alter_60p, 1), decimal.mark=",")),
         "Alter 60+ vollst."=ifelse(is.na(quote_booster_alter_60p), "k.A.", format(round(100*quote_booster_alter_60p, 1), decimal.mark=",")),
         "Alter <60 min. 1x"=ifelse(is.na(quote_initial_alter_60m), "k.A.", format(round(100*quote_initial_alter_60m, 1), decimal.mark=",")),
         "Alter <60 vollst."=ifelse(is.na(quote_booster_alter_60m), "k.A.", format(round(100*quote_booster_alter_60m, 1), decimal.mark=",")),
         "Gesamt min. 1x"=ifelse(is.na(quote_initial_gesamt), "k.A.", format(round(100*quote_initial_gesamt, 1), decimal.mark=",")),
         "Gesamt vollst."=ifelse(is.na(quote_booster_gesamt), "k.A.", format(round(100*quote_booster_gesamt, 1), decimal.mark=",")),
         # "Impfquote"=format(round(100*(quote_initial_gesamt), 1), decimal.mark=","),
         "Impfungen pro 100k EW"=format(round(impfungen_gesamt/population_personen_erst_kumulativ*100000), decimal.mark=",")
  ) %>%
  select(Bundesland, "Impfungen pro 100k EW", "Alter 60+ min. 1x", "Alter 60+ vollst.",
         "Alter <60 min. 1x", "Alter <60 vollst.",
         "Gesamt min. 1x", "Gesamt vollst.")
## data for table on subpage Bundeslaender
bundeslaender_table <- vorwarnzeitergebnis %>%
  filter(id<17 & date==maxdatum) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
         R0=format(round(R0,digits = 2), decimal.mark = ",")) %>%
  left_join(., vacc_bl %>% select(geo, impfungen_initial) %>%
              mutate(geo=ifelse(geo=="Germany", "Gesamt", geo)),
            by=c("name"="geo")) %>%
  mutate(impfungen_prozent=format(round(impfungen_initial/EW_insgesamt*100, 1), decimal.mark = ",")) %>%
  select(Bundesland=name,
         "min. 1x geimpfte Bevölkerung %"=impfungen_prozent,
         "R(t)"=R0,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "Vorwarnzeit aktuell"=Vorwarnzeit,
         "7-Tage-Inzidenz 80+"=`Faelle_letzte_7_Tage_je100TsdEinw_80+`,
         "7-Tage-Inzidenz 60-79"=`Faelle_letzte_7_Tage_je100TsdEinw_60-79`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
         "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
         "Fälle insgesamt"=cases
  )
## data for table on subpage Kreise
kreise_table <- vorwarnzeitergebnis %>%
  filter(((id>17 | id==11) & !(id>=11000000&id<12000000)) & date==maxdatum) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
         R0=format(round(R0,digits = 2), decimal.mark = ",")) %>%
  mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>%
  left_join(., aktuell %>%
              filter(id>0 & id<17) %>%
              select(blid=id, Bundesland=name)) %>%
  arrange(blid,id) %>%
  mutate(STI80=`Faelle_letzte_7_Tage_je100TsdEinw_80+`,
    EM_Vorwarnzeit=case_when(
    Vorwarnzeit_ROR > 21 ~ ">21",
    Vorwarnzeit_ROR <= 21 & Vorwarnzeit_ROR >= 7 ~ "7-21",
    Vorwarnzeit_ROR < 7 ~ "<7"
  ), 
  EM_Inzidenz=case_when(
    STI80 < 50 ~ "<50",
    STI80 >= 50 & STI80 <= 150 ~ "50-150",
    STI80 > 150 ~ ">150"
  ), 
  EM=case_when(
    STI80 < 50 & Vorwarnzeit_ROR > 21 ~ "grün",
    STI80 < 50 & Vorwarnzeit_ROR <= 21 & Vorwarnzeit_ROR >= 7 ~ "gelb",
    STI80 >= 50 & STI80 <= 150 & Vorwarnzeit_ROR > 21 ~ "gelb",
    STI80 < 50 & Vorwarnzeit_ROR < 7 ~ "orange",
    STI80 >= 50 & STI80 <= 150 & Vorwarnzeit_ROR <= 21 & Vorwarnzeit_ROR >= 7 ~ "orange",
    STI80 > 150 & Vorwarnzeit_ROR > 21 ~ "orange",
    STI80 >= 50 & STI80 <= 150 & Vorwarnzeit_ROR < 7 ~ "orange",
    STI80 > 150 & Vorwarnzeit_ROR <= 21 & Vorwarnzeit_ROR >= 7 ~ "orange",
    STI80 > 150 & Vorwarnzeit_ROR < 7 ~ "rot"
  )) %>%
  select(Kreis=name,
         Bundesland,
         "Vorwarnzeit ROR"=Vorwarnzeit_ROR,
         "7-Tage-Inzidenz 80+"=`Faelle_letzte_7_Tage_je100TsdEinw_80+`,
         "Risikostufe"=EM,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "R(t)"=R0,
         "7-Tage-Inzidenz 60-79"=`Faelle_letzte_7_Tage_je100TsdEinw_60-79`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
         "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
         "Fälle insgesamt"=cases,
         # "ROR"=ROR11,
         "Vorwarnzeit lokal"=Vorwarnzeit #, # needs communication
  )
# vacc table für datenblatt
vacc_table_faktenblatt <- vacc_table %>%
  select(-`Impfungen pro 100k EW`) %>%
  left_join(vacc_gesamt %>%
              filter(metric=="dosen_kumulativ") %>%
              mutate("Zahl der Impfungen gesamt"=value) %>%
              mutate(geo=ifelse(geo=="Deutschland", "Gesamt", geo)),
            by=c("Bundesland"="geo")) %>%
  left_join(bundeslaender_table %>%
              select(Bundesland, `7-Tage-Inzidenz`, `7-Tage-Inzidenz 80+`),
            by="Bundesland")
vacc_table_faktenblatt_new <- vacc_table %>%
  select(-`Impfungen pro 100k EW`) %>%
  left_join(vacc_gesamt %>%
              filter(metric=="dosen_kumulativ") %>%
              mutate("Zahl der Impfungen gesamt"=value) %>%
              mutate(geo=ifelse(geo=="Deutschland", "Gesamt", geo)),
            by=c("Bundesland"="geo")) %>%
  left_join(bundeslaender_table %>%
              select(Bundesland, `7-Tage-Inzidenz`),
            by="Bundesland") %>% 
  left_join(., aktuell %>% select(id, name), by=c("Bundesland"="name")) %>% 
  left_join(vorwarnzeitergebnis %>% 
              filter(id<17 & date==maxdatum) %>% 
              select(id, `7-Tage-Inzidenz 60+`=`Faelle_letzte_7_Tage_je100TsdEinw_60+`),
            by="id") %>% 
  select(Bundesland, `Gesamt min. 1x`, "Gesamt vollst.", `7-Tage-Inzidenz`, `7-Tage-Inzidenz 60+`)

rki_vacc_vortag <- max(rki_vacc %>% filter(date<max(date)) %>% pull(date)) 
vacc_table_vaccsim <- bind_rows(
  rki_vacc %>%
    filter(date==max(date) & (metric=="dosen_kumulativ" | metric=="personen_voll_kumulativ")),
  rki_vacc %>%
    filter(date==rki_vacc_vortag & (metric=="dosen_kumulativ" | metric=="personen_voll_kumulativ")),
  rki_vacc %>%
    filter(date==max(date)-7 & (metric=="dosen_kumulativ" | metric=="personen_voll_kumulativ")) # -6?
) %>%
  pivot_wider(id_cols=c("geo", "date"),
              names_from=metric,
              values_from=value) %>%
  group_by(geo) %>%
  arrange(date) %>%
  summarise(impfungen_letzter_tag=dosen_kumulativ[3]-dosen_kumulativ[2],
         impfungen_letzte_woche=dosen_kumulativ[3]-dosen_kumulativ[1],
         zweitimpfungen=personen_voll_kumulativ[3],
         .groups="drop") %>%
  mutate(geo=ifelse(geo=="Germany", "Gesamt", geo))
vacc_table_vaccsim <- vacc_table_faktenblatt %>%
  left_join(vacc_table_vaccsim, by=c("Bundesland"="geo")) %>%
  mutate(Stand_letzteImpfung=max(rki_vacc$date))
## data for Bundeslaender faktenblatt
bundeslaender_table_faktenblatt <- vorwarnzeitergebnis %>%
  filter(id<17 & date%in%c(lastsunday, sundaybeforelastsunday)) %>%
  left_join(., myblmitidata %>% select(id, name, date, R_Mean), by=c("id", "date")) %>%
  mutate(R_Mean=round(R_Mean, 2),
         inf_bev=paste0(format(round(100*cases/EW_insgesamt, 1), decimal.mark=","), " %")) %>%
  select(Bundesland=name,
         Datum=date,
         "R(t)"=R_Mean,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "Vorwarnzeit"=Vorwarnzeit,
         "Bereits infizierte Bevölkerung"=inf_bev,
         "7-Tage-Inzidenz 80+"=`Faelle_letzte_7_Tage_je100TsdEinw_80+`,
         "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "7-Tage-Inzidenz 60-79"=`Faelle_letzte_7_Tage_je100TsdEinw_60-79`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`
  )
## data for Kreise Faktenblatt
kreise_table_faktenblatt <- vorwarnzeitergebnis %>%
  filter((id>17 | id==11) & date%in%c(lastsunday, sundaybeforelastsunday)) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>%
  left_join(., aktuell %>%
              filter(id>0 & id<17) %>%
              select(blid=id, Bundesland=name)) %>%
  arrange(blid,id) %>%
  select(Datum=date,
         Kreis=name,
         Bundesland,
         "R(t)"=R0,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "7-Tage-Inzidenz 80+"=`Faelle_letzte_7_Tage_je100TsdEinw_80+`,
         "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "7-Tage-Inzidenz 60-79"=`Faelle_letzte_7_Tage_je100TsdEinw_60-79`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
         "Vorwarnzeit ROR"=Vorwarnzeit_ROR, 
         "Vorwarnzeit"=Vorwarnzeit #, # needs communication
  )

#projektions tables
bl_projektionen <- ausgangsdaten %>%
  filter(id<17 & date==maxdatum) %>%
  select(id, STI_aktuell=Faelle_letzte_7_Tage_je100TsdEinw) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate("R(t)"=format(round(R0, digits = 2), decimal.mark = ",")) %>%
  rowwise() %>%
  mutate(RaktuellSTI165=projektion_datum(STI_aktuell = STI_aktuell,
                                        STI_Ziel = 165,
                                        Rt = R0),
         RaktuellSTI100=projektion_datum(STI_aktuell, 100, R0),
         RaktuellSTI50=projektion_datum(STI_aktuell, 50, R0),
         RaktuellSTI35=projektion_datum(STI_aktuell, 35, R0),
         R07STI165=projektion_datum(STI_aktuell, 165),
         R07STI100=projektion_datum(STI_aktuell, 100),
         R07STI50=projektion_datum(STI_aktuell, 50),
         R07STI35=projektion_datum(STI_aktuell, 35),
         invisibleRaktuell165sort=as_date(RaktuellSTI165, format="%d.%m.%Y"),
         invisibleR07165sort=as_date(R07STI165, format="%d.%m.%Y"),
         invisibleRaktuell100sort=as_date(RaktuellSTI100, format="%d.%m.%Y"),
         invisibleR07100sort=as_date(R07STI100, format="%d.%m.%Y"),
         invisibleRaktuell50sort=as_date(RaktuellSTI50, format="%d.%m.%Y"),
         invisibleR0750sort=as_date(R07STI50, format="%d.%m.%Y"),
         invisibleRaktuell35sort=as_date(RaktuellSTI35, format="%d.%m.%Y"),
         invisibleR0735sort=as_date(R07STI35, format="%d.%m.%Y")
         ) %>%
  arrange(id) %>%
  select(-id, -R0,
         "7-Tage-Inzidenz"=STI_aktuell,
         `R(t)`,
         Bundesland=name,
         "Inzidenz<165 bei R(t) aktuell"=RaktuellSTI165,
         "Inzidenz<100 bei R(t) aktuell"=RaktuellSTI100,
         "Inzidenz<50 bei R(t) aktuell"=RaktuellSTI50,
         "Inzidenz<35 bei R(t) aktuell"=RaktuellSTI35,
         "Inzidenz<165 bei R(t)=0,7"=R07STI165,
         "Inzidenz<100 bei R(t)=0,7"=R07STI100,
         "Inzidenz<50 bei R(t)=0,7"=R07STI50,
         "Inzidenz<35 bei R(t)=0,7"=R07STI35,
         invisibleRaktuell165sort,
         invisibleR07165sort,
         invisibleRaktuell100sort,
         invisibleR07100sort,
         invisibleRaktuell50sort,
         invisibleR0750sort,
         invisibleRaktuell35sort,
         invisibleR0735sort
         )

kreise_projektionen <- ausgangsdaten %>%
  filter(((id>17 | id==11) & !(id>=11000000&id<12000000)) & date==maxdatum) %>%
  left_join(vorwarnzeitergebnis %>% filter(date==maxdatum) %>% select(id, Vorwarnzeit_ROR), by="id") %>% 
  select(id, STI_aktuell=Faelle_letzte_7_Tage_je100TsdEinw, Vorwarnzeit_ROR) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate("AGS (num)"=ifelse(id==11, 11000, id/1000)) %>%
  mutate("R(t)"=format(round(R0, digits = 2), decimal.mark = ",")) %>%
  mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>%
  left_join(., aktuell %>%
              filter(id>0 & id<17) %>%
              select(blid=id, Bundesland=name)) %>%
  rowwise() %>%
  mutate(RaktuellSTI165=projektion_datum(STI_aktuell = STI_aktuell,
                                         STI_Ziel = 165,
                                         Rt = R0),
         RaktuellSTI100=projektion_datum(STI_aktuell, 100, R0),
         RaktuellSTI50=projektion_datum(STI_aktuell, 50, R0),
         RaktuellSTI35=projektion_datum(STI_aktuell, 35, R0),
         R07STI165=projektion_datum(STI_aktuell, 165),
         R07STI100=projektion_datum(STI_aktuell, 100),
         R07STI50=projektion_datum(STI_aktuell, 50),
         R07STI35=projektion_datum(STI_aktuell, 35),
         invisibleRaktuell165sort=as_date(RaktuellSTI165, format="%d.%m.%Y"),
         invisibleR07165sort=as_date(R07STI165, format="%d.%m.%Y"),
         invisibleRaktuell100sort=as_date(RaktuellSTI100, format="%d.%m.%Y"),
         invisibleR07100sort=as_date(R07STI100, format="%d.%m.%Y"),
         invisibleRaktuell50sort=as_date(RaktuellSTI50, format="%d.%m.%Y"),
         invisibleR0750sort=as_date(R07STI50, format="%d.%m.%Y"),
         invisibleRaktuell35sort=as_date(RaktuellSTI35, format="%d.%m.%Y"),
         invisibleR0735sort=as_date(R07STI35, format="%d.%m.%Y")
  ) %>%
  arrange(name) %>%
  select(-id, -R0, -blid,
         "7-Tage-Inzidenz"=STI_aktuell,
         `R(t)`,
         Kreis=name,
         Bundesland=Bundesland,
         "Inzidenz<165 bei R(t) aktuell"=RaktuellSTI165,
         "Inzidenz<100 bei R(t) aktuell"=RaktuellSTI100,
         "Inzidenz<50 bei R(t) aktuell"=RaktuellSTI50,
         "Inzidenz<35 bei R(t) aktuell"=RaktuellSTI35,
         "Inzidenz<165 bei R(t)=0,7"=R07STI165,
         "Inzidenz<100 bei R(t)=0,7"=R07STI100,
         "Inzidenz<50 bei R(t)=0,7"=R07STI50,
         "Inzidenz<35 bei R(t)=0,7"=R07STI35,
         invisibleRaktuell165sort,
         invisibleR07165sort,
         invisibleRaktuell100sort,
         invisibleR07100sort,
         invisibleRaktuell50sort,
         invisibleR0750sort,
         invisibleRaktuell35sort,
         invisibleR0735sort,
         "AGS (num)",
         Vorwarnzeit_ROR)

# # impfdosen historisch
# rki_verabreicht_hersteller <- rki_vacc %>%
#   filter(geo=="Germany" & date==max(rki_vacc$date) & key%in%c("sum_booster", "sum_initial_moderna", "sum_initial_biontech"))
# dosen <- read_csv("R/adhoc_analyses/impfdosen_planung.csv")
# dosen_verabreicht <- dosen %>%
#   filter(jahr==2020 | quartal<=quarter(max(rki_vacc$date))) %>%
#   group_by(hersteller) %>%
#   summarise(dosen=sum(dosen), .groups = "drop") %>%
#   mutate(dosen_verabreicht_erst=case_when(
#     hersteller=="AZ" ~ 0,
#     hersteller=="BNT/Pfizer" ~ rki_verabreicht_hersteller %>%
#       filter(key=="sum_initial_biontech") %>% pull(value),
#     hersteller=="Curevac" ~ 0,
#     hersteller=="Sanofi/GSK" ~ 0,
#     hersteller=="J&J" ~ 0,
#     hersteller=="Moderna" ~ rki_verabreicht_hersteller %>%
#       filter(key=="sum_initial_moderna") %>% pull(value)
#   ),
#          dosen_verabreicht_zweit=case_when(
#            hersteller=="AZ" ~ 0,
#            hersteller=="BNT/Pfizer" ~ rki_verabreicht_hersteller %>%
#              filter(key=="sum_booster") %>% pull(value),
#            hersteller=="Curevac" ~ 0,
#            hersteller=="Sanofi/GSK" ~ 0,
#            hersteller=="J&J" ~ 0,
#            hersteller=="Moderna" ~ 0
#          )) %>%
#   mutate(dosen_geliefert=case_when(
#     hersteller=="AZ" ~ 600000,
#     hersteller=="BNT/Pfizer" ~ 1345500+4*672650+747630,
#     hersteller=="Curevac" ~ 0,
#     hersteller=="Sanofi/GSK" ~ 0,
#     hersteller=="J&J" ~ 0,
#     hersteller=="Moderna" ~ 2*(rki_verabreicht_hersteller %>%
#       filter(key=="sum_initial_moderna") %>% pull(value)) + 91200
#   )) %>%
#   select(-dosen)
# # nach bundeslaendern
# map_bl <- tibble(
#   BL_ID=1:16,
#   geo=c("Schleswig-Holstein",
#         "Hamburg",
#         "Niedersachsen",
#         "Bremen",
#         "Nordrhein-Westfalen",
#         "Hessen",
#         "Rheinland-Pfalz",
#         "Baden-Württemberg",
#         "Bayern",
#         "Saarland",
#         "Berlin",
#         "Brandenburg",
#         "Mecklenburg-Vorpommern",
#         "Sachsen",
#         "Sachsen-Anhalt",
#         "Thüringen")
# )
# rki_verabreicht_hersteller_bl <- rki_vacc %>%
#   filter(geo!="Germany" & date==max(rki_vacc$date) & key%in%c("sum_booster", "sum_initial_moderna", "sum_initial_biontech")) %>%
#   left_join(map_bl, by="geo")
# dosen_bl <- read_csv("R/adhoc_analyses/impfdosen_planung_bl.csv")
# dosen_verabreicht_bl <- dosen_bl %>%
#   filter(jahr==2020 | quartal<=quarter(max(rki_vacc$date))) %>%
#   group_by(BL_ID, hersteller) %>%
#   summarise(dosen=sum(dosen), .groups = "drop")
# dosen_verabreicht_bl_gesamt <- tibble()
# for (bl_id in 1:16) {
#   this_bl_dosen_verabreicht <- dosen_verabreicht_bl %>%
#     filter(BL_ID==bl_id) %>%
#     mutate(dosen_verabreicht_erst=case_when(
#       hersteller=="AZ" ~ 0,
#       hersteller=="BNT/Pfizer" ~ rki_verabreicht_hersteller_bl %>%
#         filter(BL_ID==bl_id) %>%
#         filter(key=="sum_initial_biontech") %>% pull(value),
#       hersteller=="Curevac" ~ 0,
#       hersteller=="Sanofi/GSK" ~ 0,
#       hersteller=="J&J" ~ 0,
#       hersteller=="Moderna" ~ rki_verabreicht_hersteller_bl %>%
#         filter(BL_ID==bl_id) %>%
#         filter(key=="sum_initial_moderna") %>% pull(value)
#     ),
#     dosen_verabreicht_zweit=case_when(
#       hersteller=="AZ" ~ 0,
#       hersteller=="BNT/Pfizer" ~ rki_verabreicht_hersteller_bl %>%
#         filter(BL_ID==bl_id) %>%
#         filter(key=="sum_booster") %>% pull(value),
#       hersteller=="Curevac" ~ 0,
#       hersteller=="Sanofi/GSK" ~ 0,
#       hersteller=="J&J" ~ 0,
#       hersteller=="Moderna" ~ 0
#     )) %>%
#     mutate(dosen_geliefert=case_when(
#       hersteller=="AZ" ~ round(600000*(rki_verabreicht_hersteller_bl %>%
#                                          filter(BL_ID==bl_id) %>%
#                                          filter(key=="sum_booster") %>% pull(population))/83166711),
#       hersteller=="BNT/Pfizer" ~ round((1345500+4*672650+747630)*(rki_verabreicht_hersteller_bl %>%
#         filter(BL_ID==bl_id) %>%
#         filter(key=="sum_booster") %>% pull(population))/83166711),
#       hersteller=="Curevac" ~ 0,
#       hersteller=="Sanofi/GSK" ~ 0,
#       hersteller=="J&J" ~ 0,
#       hersteller=="Moderna" ~ round((2*(rki_verabreicht_hersteller %>%
#                                    filter(key=="sum_initial_moderna") %>% pull(value)) + 91200)*(rki_verabreicht_hersteller_bl %>%
#                                                                                                   filter(BL_ID==bl_id) %>%
#                                                                                                   filter(key=="sum_booster") %>% pull(population))/83166711)
#     )) %>%
#     select(-dosen)
#   dosen_verabreicht_bl_gesamt <- bind_rows(dosen_verabreicht_bl_gesamt,
#                                            this_bl_dosen_verabreicht)
# }
  
  
##### write data for displayed tables/plots to jsons
write_json(bundeslaender_table, "./data/tabledata/bundeslaender_table.json")
write_json(bundeslaender_table_faktenblatt, "./data/tabledata/bundeslaender_table_faktenblatt.json")
write_json(kreise_table, "./data/tabledata/kreise_table.json")
write_json(kreise_table_faktenblatt, "./data/tabledata/kreise_table_faktenblatt.json")
write_json(vacc_table_faktenblatt_new, "./data/tabledata/vacc_table_faktenblatt.json")
write_json(vacc_table_vaccsim, "./data/tabledata/vacc_table_vaccsim.json")
write_json(vacc_alle_faktenblatt, "./data/tabledata/vacc_alle_faktenblatt.json")
write_json(rwert_bund_data, "./data/plotdata/rwert_bund.json")
write_json(rki_r_und_zi_vwz_data, "./data/plotdata/rki_r_und_zi_vwz_data.json")
write_json(akutinfiziert_data, "./data/plotdata/akutinfiziert.json")
write_json(agefatality_data, "./data/plotdata/agefatality.json")
write_json(itsbetten_data, "./data/plotdata/itsbetten.json")
write_json(bundeslaender_r_und_vwz_data, "./data/plotdata/bundeslaender_r_und_vwz.json")

#### write csv for aerzteblatt
kreise_projektionen_aeb <- ausgangsdaten %>%
  filter(((id>17 | id==11) & !(id>=11000000&id<12000000)) & date==maxdatum) %>%
  left_join(vorwarnzeitergebnis %>% filter(date==maxdatum) %>% select(id, Vorwarnzeit_ROR), by="id") %>% 
  select(id, STI_aktuell=Faelle_letzte_7_Tage_je100TsdEinw, Vorwarnzeit_ROR) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate("AGS (num)"=ifelse(id==11, 11000, id/1000)) %>%
  mutate("R(t)"=format(round(R0, digits = 2), decimal.mark = ",")) %>%
  mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>%
  left_join(., aktuell %>%
              filter(id>0 & id<17) %>%
              select(blid=id, Bundesland=name)) %>%
  rowwise() %>%
  mutate(RaktuellSTI50=projektion_datum(STI_aktuell = STI_aktuell,
                                        STI_Ziel = 50,
                                        Rt = R0),
         RaktuellSTI35=projektion_datum(STI_aktuell, 35, R0),
         RaktuellSTI10=projektion_datum(STI_aktuell, 10, R0),
         R07STI50=projektion_datum(STI_aktuell, 50),
         R07STI35=projektion_datum(STI_aktuell, 35),
         R07STI10=projektion_datum(STI_aktuell, 10),
         invisibleRaktuell50sort=as_date(RaktuellSTI50, format="%d.%m.%Y"),
         invisibleR0750sort=as_date(R07STI50, format="%d.%m.%Y"),
         invisibleRaktuell35sort=as_date(RaktuellSTI35, format="%d.%m.%Y"),
         invisibleR0735sort=as_date(R07STI35, format="%d.%m.%Y"),
         invisibleRaktuell10sort=as_date(RaktuellSTI10, format="%d.%m.%Y"),
         invisibleR0710sort=as_date(R07STI10, format="%d.%m.%Y")) %>%
  arrange(name) %>%
  select(-id, -R0, -blid,
         "7-Tage-Inzidenz"=STI_aktuell,
         `R(t)`,
         Kreis=name,
         Bundesland=Bundesland,
         "Inzidenz<50 bei R(t) aktuell"=RaktuellSTI50,
         "Inzidenz<35 bei R(t) aktuell"=RaktuellSTI35,
         "Inzidenz<10 bei R(t) aktuell"=RaktuellSTI10,
         "Inzidenz<50 bei R(t)=0,7"=R07STI50,
         "Inzidenz<35 bei R(t)=0,7"=R07STI35,
         "Inzidenz<10 bei R(t)=0,7"=R07STI10,
         invisibleRaktuell50sort,
         invisibleR0750sort,
         invisibleRaktuell35sort,
         invisibleR0735sort,
         invisibleRaktuell10sort,
         invisibleR0710sort,
         "AGS (num)",
         Vorwarnzeit_ROR)
write_csv(kreise_projektionen_aeb %>% mutate(datenstand=today()) %>%
            select(-contains("invisible")), "./data/tabledata/kreise_projektionen.csv")
####write csv for impfmodellierung
# write_csv(dosen_verabreicht, "./R/adhoc_analyses/impfdosen_bisher.csv")
# write_csv(dosen_verabreicht_bl_gesamt, "./R/adhoc_analyses/impfdosen_bisher_bl.csv")

##### Plots
akutinfiziert_plot <- ggplot(akutinfiziert_data,
                             aes(x=date, y=Infected,group=1)) +
  geom_area(fill="#0086C530") +
  geom_hline(aes(yintercept=0), color="black", linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_minimal() +
  scale_x_date(breaks = "2 months",date_labels = "%d.%m.") +
  labs(y="Anzahl akut infiziert",x = "Datum") +
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x=element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

agefatality_plot <- ggplot(agefatality_data,
                           aes(x=Meldedatum, y=Anteil, color=Merkmal)) +
  geom_line() +
  theme_minimal() + 
  scale_color_zi() +
  labs(y="Verhältnis in %", x="Datum", color="") + 
  scale_x_date(breaks="2 months", date_labels = "%d.%m.") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

rwert_bund_plot <- ggplot(rwert_bund_data,
                          aes(x=date, y=R, group=name, color=name=="Gesamt",
                              text=paste("Region: ", name, "<br>", "Neue Fälle:", I_cases))) +
  geom_hline(yintercept = 1) +
  geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
  scale_color_zi()  +
  theme_minimal() + scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
  labs(x="Datum",y="Reproduktionszahl R(t)",caption="Zeitlicher Verlauf des R-Wertes in Deutschland") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

rki_r_und_zi_vwz_plot <- ggplot(rki_r_und_zi_vwz_data,
                                  aes(x=date, y=Wert, color=Variable)) +
  facet_wrap(~Variable, scales = "free_y") +
  geom_line(size=2) +
  ylim(0, NA) +
  scale_color_zi() +
  labs(subtitle="Zi-Vorwarnzeit und RKI-R-Wert im Zeitverlauf",x="",y="") +
  theme_minimal() +
  theme(legend.position='none')

sti_ag_bund_plot <- ggplot(rki_7ti_alle %>%
                             filter(id==0),
                           aes(x=datesunday, y=STI, col=Altersgruppe)) +
  geom_line(size=1.5) +
  scale_x_date(breaks = "2 months",date_labels = "%d.%m.") +
  ylim(0, NA) +
  labs(subtitle="Sieben-Tage-Inzidenz nach Altersgruppen im Zeitverlauf",x="Datum",y="Sieben-Tage-Inzidenz") +
  scale_color_zi() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

zi_vwz_plot <- ggplot(rki_r_und_zi_vwz_data %>% filter(Variable=="Vorwarnzeit"),
                                aes(x=date, y=Wert)) +
  geom_line(size=2, col=zi_cols("zigreen")) +
  ylim(0, NA) +
  labs(subtitle="Zi-Vorwarnzeit im Zeitverlauf",x="Datum",y="Vorwarnzeit") +
  theme_minimal() +
  scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
  theme(legend.position='none') +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

itsbetten_plot <- ggplot(itsbetten_data %>%
                        filter(Betten=="faelle_covid_aktuell"),
                      aes(x=daten_stand, y=Anzahl, color=Betten)) +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=2, show.legend = FALSE, color=zi_cols("ziblue")) +
  theme_minimal() +
  scale_x_date(breaks = "2 months",date_labels = "%d.%m.") +
  labs(y="COVID-19-Fälle ITS", x = "Datum") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

age_its_death_plot <- ggplot(age_its_death_data %>%
                           pivot_longer(cols=c(`Fälle_80+`, "faelle_covid_aktuell", `Todesfälle`),
                                        names_to="Faelle",
                                        values_to="Anzahl"),
                         aes(x=Meldedatum, y=Anzahl, color=Faelle)) +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=1, show.legend = FALSE) +
  scale_color_zi() +
  theme_minimal() +
  labs(y="Anzahl", x="Datum", color="") + 
  scale_x_date(breaks="2 months", date_labels = "%d.%m.") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

vaccination_plot <- ggplot(rki_vacc %>%
                             filter(metric=="dosen_kumulativ" & geo=="Germany"),
                           aes(x=date, y=value)) +
  geom_line(size=2, col=zi_cols("ziblue")) +
  ylim(0, NA) +
  labs(subtitle="Verabreichte Impfdosen im Zeitverlauf",x="Datum",y="Impfungen") +
  theme_minimal() +
  scale_x_date(date_labels = "%d.%m.") + # , breaks="3 days"
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  theme(legend.position='none') +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

## two axis plot for vacc against its
# Value used to transform the data for two-axis plot (don't do this at home)
coeff <- max(vaccination_sti_its_death_data$impfungen_kumulativ, na.rm=TRUE)/max(vaccination_sti_its_death_data$faelle_covid_aktuell, na.rm=TRUE)
twoaxis_vacc_vs_its <- ggplot(vaccination_sti_its_death_data, aes(x=datum)) +
  geom_line(aes(y=faelle_covid_aktuell)) + 
  geom_line(aes(y=impfungen_kumulativ/coeff)) + # Divide by coeff to get the same range like the covid its cases
  scale_y_continuous(
    # Features of the first axis
    name = "ITS-Fälle COVID-19",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Impfungen")
  )

vaccination_sti_its_death_plot <- ggplot(vaccination_sti_its_death_data %>%
                                           pivot_longer(cols=!datum),
                           aes(x=datum, y=value)) +
  geom_line(size=2, col=zi_cols("ziblue")) +
  facet_wrap(.~name, nrow=2, ncol=2, scales="free_y") +
  ylim(0, NA) +
  labs(subtitle="bla",x="Datum",y="blay") +
  theme_minimal() +
  scale_x_date(date_labels = "%d.%m.") + # , breaks="3 days"
  # scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  theme(legend.position='none') +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


bundeslaender_r_und_vwz_plot <- function(myid) {
  myname <- bundeslaender_r_und_vwz_data %>% filter(id==myid) %>% head(1) %>% pull(name)
  my_r_vwz_data <- bundeslaender_r_und_vwz_data %>% filter(id==myid)
  myplot <- ggplot(my_r_vwz_data,
                   aes(x=Datum, y=Wert, group=name, color=Variable,
                       text=paste("Region: ", name, "<br>Neue Fälle:", I_cases))) +
    geom_hline(aes(yintercept=ifelse(Variable=="R",1, 0))) +
    geom_line(size=2, show.legend = F) +
    scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
    facet_grid(Variable~., scales = "free") +
    # facet_wrap(~Variable, scales = "free") +
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

bundeslaender_vwz_plot <- function(myid) {
  myname <- bundeslaender_r_und_vwz_data %>% filter(id==myid & Variable=="Vorwarnzeit") %>% head(1) %>% pull(name)
  my_vwz_data <- bundeslaender_r_und_vwz_data %>% filter(id==myid & Variable=="Vorwarnzeit")
  myplot <- ggplot(my_vwz_data,
                   aes(x=Datum, y=Wert, group=name,
                       text=paste("Region: ", name, "<br>Neue Fälle:", I_cases))) +
    geom_hline(aes(yintercept=0)) +
    geom_line(size=2, show.legend = F, col=zi_cols("zigreen")) +
    scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
    # facet_grid(Variable~., scales = "free") +
    # facet_wrap(~Variable, scales = "free") +
    geom_blank(aes(y = y_min)) +
    geom_blank(aes(y = y_max)) +
    scale_color_zi()  +
    theme_minimal() +
    labs(x="Datum", y="Vorwarnzeit") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position='none',
          panel.spacing = unit(2, "lines")) +
    ggtitle(myname)
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}

bundeslaender_stiag_plot <- function(myid) {
  myname <- bundeslaender_r_und_vwz_data %>% filter(id==myid & Variable=="Vorwarnzeit") %>% head(1) %>% pull(name)
  my_stiag_data <- rki_7ti_alle %>% filter(id==myid)
  myplot <- ggplot(my_stiag_data %>%
                     mutate(Datum=datesunday, `Sieben-Tage-Inzidenz`=STI),
                   aes(x=Datum, y=`Sieben-Tage-Inzidenz`, col=Altersgruppe)) +
    geom_hline(aes(yintercept=0)) +
    geom_hline(aes(yintercept=50), linetype="dashed") +
    geom_line(size=1.5) +
    scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
    scale_color_zi()  +
    theme_minimal() +
    labs(x="Datum", y="Sieben-Tage-Inzidenz") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(2, "lines")) +
    ggtitle(myname)
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}

bundeslaender_stiag_und_vwz_plot <- function(myid) {
  myname <- bundeslaender_r_und_vwz_data %>% filter(id==myid) %>% head(1) %>% pull(name)
  my_vwz_data <- bundeslaender_r_und_vwz_data %>% filter(id==myid & Variable=="Vorwarnzeit")
  my_stiag_data <- rki_7ti_alle %>% filter(id==myid)
  my_stiag_vwz_data <- bind_rows(my_vwz_data %>%
                                   mutate(Altersgruppe="80+") %>%
                                   select(Datum, Wert, Variable, Altersgruppe),
                                 my_stiag_data %>%
                                   mutate(Wert=STI, Variable="Inzidenz", Datum=datesunday) %>%
                                   select(Datum, Wert, Variable, Altersgruppe)) %>%
    filter(Datum>="2020-03-13")
  myplot <- ggplot(my_stiag_vwz_data,
                   aes(x=Datum, y=Wert, color=Altersgruppe)) +
    geom_hline(aes(yintercept=ifelse(Variable=="R",1, 0))) +
    geom_line(size=1.5) +
    scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
    facet_grid(Variable~., scales = "free") +
    # facet_wrap(~Variable, scales = "free") +
    scale_color_zi() +
    theme_minimal() +
    labs(x="", y="") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(2, "lines")) +
    ggtitle(myname)
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}
