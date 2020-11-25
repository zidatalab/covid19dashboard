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

##### Source files
source("R/aux_functions.R")

##### parameters from literature
## AOK/DIVI/Busse Paper Lancet (Case characteristics...)
icu_days <- 10.1
busselancet_altersgruppen_hospital <- tibble("Hosp059"=2896,
                                             "Hosp6079"=1621+2158,
                                             "Hosp80"=3346)
## Destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html
altersgruppen_bund <- tibble("unter 20"=18.4,
                             "20 bis 40"=24.6,
                             "40 bis 60"=28.4,
                             "60 bis 80"=21.7,
                             "80+"=6.8)/100
## icu-quoten nach altersgruppe
dividay <- as_date("2020-11-16")
divi_behandlungen_aktuell <- 26372/1.27+3436 # divi intensivregister on dividay
icu_altersgruppen <- divi_behandlungen_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital)
## infectious period
infektperiode <- 14

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
## get data from db
brd_timeseries <- tbl(conn,"brd_timeseries") %>% collect() %>% mutate(date=as.Date(date))
rki <- tbl(conn,"rki") %>% collect()
divi <- tbl(conn,"divi") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
aktuell <- tbl(conn,"params") %>% collect()
## read/update RKI-R-estimates
RKI_R <- tryCatch(
  {
    mytemp <- tempfile()
    rki_r_data <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile"
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

##### make data for downstream analysis/plots
## if last divi report missing
maxdatum <- max(as_date(rki$Meldedatum))
lastsunday <- floor_date(maxdatum, "week")
sundaybeforelastsunday <- lastsunday-7
if (max(divi$daten_stand!=maxdatum)) {
  divi_all <- bind_rows(divi_all, divi %>% mutate(daten_stand=as_date(maxdatum)))
}
## rki imputation because of delayed gesundheitsamt-meldungen
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
agefatality_data <- rki %>% group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall, na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,"0-59")) %>%
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
  left_join(akutinfiziert_data %>% select(date, Infected), by=c('Meldedatum'='date')) %>%
  mutate("Fälle gesamt"= `Fälle_0-59`+ `Fälle_60-79`+ `Fälle_80+` , 
         "Todesfälle gesamt" = `Todesfälle_0-59`+ `Todesfälle_60-79`+ `Todesfälle_80+`,
         "60+" = (`Fälle_80+` + `Fälle_60-79` )/ `Fälle gesamt`, 
         "itsfaelle"=`faelle_covid_aktuell`/lag(`Infected`, 14),
         'Todesfälle'= `Todesfälle gesamt`/ `Fälle gesamt`) %>%
  filter(Meldedatum>=as_date("2020-03-01")) %>%
  select(Meldedatum,
         "Alter 60+ an Fällen"=`60+`,
         "ITS-Fälle an Fällen"=`itsfaelle`, 
         "Todesfälle an Fällen"= `Todesfälle`) %>% 
  gather(Merkmal, Anteil, 2:4) %>% mutate(Anteil=round(Anteil*100,digits=2))
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
         cases80=cumsum(`Fälle_80+`)) %>%
  mutate(Infected059=cases059-lag(cases059, infektperiode),
         Infected6079=cases6079-lag(cases6079, infektperiode),
         Infected80=cases80-lag(cases80, infektperiode)) %>% 
  fill(Infected059, Infected6079, Infected80, .direction = "up") %>%
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
                                             Infected059=sum(Infected059),
                                             Infected6079=sum(Infected6079),
                                             Infected80=sum(Infected80),
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
                                            Infected059=sum(Infected059),
                                            Infected6079=sum(Infected6079),
                                            Infected80=sum(Infected80),
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
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), .groups="drop") %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select(., `unter 20`:`40 bis 60`))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)))

##### icurates nach altersgruppen
## delay für fälle-->icu:
rkidivi <- rki_alter_destatis %>%
  filter(id==0) %>%
  left_join(., divi_all %>% filter(id==0), by=c("Meldedatum"="daten_stand")) %>% drop_na()
lengthrkidivi <- dim(rkidivi)[1]
autocorhorizont <- 30
autocors <- rep(0, lengthrkidivi-autocorhorizont+1)
for (lag in 0:autocorhorizont) { autocors[lag+1] <- cor(rkidivi$Infected80[1:(lengthrkidivi-autocorhorizont)], rkidivi$faelle_covid_aktuell[(1+lag):(lengthrkidivi-autocorhorizont+lag)]) }
iculag <- which.max(autocors)-1
# iculag <- 0
# iculag <- 14
cases_altersgruppen <- rki %>% group_by(Meldedatum,Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,"0-59")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols = Meldedatum,
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  arrange(Meldedatum) %>%
  mutate(cases059=cumsum(`Fälle_0-59`),
         cases6079=cumsum(`Fälle_60-79`),
         cases80=cumsum(`Fälle_80+`)) %>% 
  filter(Meldedatum==dividay-iculag) %>% 
  select(cases059, cases6079, cases80) 
icurate_altersgruppen_busse <- icu_altersgruppen/cases_altersgruppen

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
letzte_7_tage_altersgruppen_destatis <- rki_alter_destatis %>%
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
  left_join(., strukturdaten, by="id") %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag_059=round(`Faelle_letzte_7_Tage_0-59`/7),
         Faelle_letzte_7_Tage_pro_Tag_6079=round(`Faelle_letzte_7_Tage_60-79`/7),
         Faelle_letzte_7_Tage_pro_Tag_80=round(`Faelle_letzte_7_Tage_80+`/7),
         `Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((`unter 3 Jahre`+`3 bis unter 6 Jahre`+`6 bis unter 10 Jahre`+`10 bis unter 15 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((`15 bis unter 18 Jahre`+`18 bis unter 20 Jahre`+`20 bis unter 25 Jahre`+`25 bis unter 30 Jahre`+`30 bis unter 35 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((`35 bis unter 40 Jahre`+`40 bis unter 45 Jahre`+`45 bis unter 50 Jahre`+`50 bis unter 55 Jahre`+`55 bis unter 60 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+`75 Jahre und mehr`)/100000))) %>%
  left_join(., rki_alter_destatis %>% select(cases059, cases6079, cases80, Infected059, Infected6079, Infected80, id, Meldedatum), by=c("id"="id", "date"="Meldedatum")) %>%
  mutate(EW059=rowSums(select(., `unter 3 Jahre`:`55 bis unter 60 Jahre`)),
         EW6079=`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+round(0.4*`75 Jahre und mehr`),
         EW80=round(0.6*`75 Jahre und mehr`))
ausgangsdaten <- letzte_7_tage %>%
  left_join(., divi_all %>%
              select(id, ICU_Betten, betten_frei, faelle_covid_aktuell, daten_stand) %>%
              mutate(id=ifelse(id>16, id*1000, id)),
            by=c("id"="id", "date"="daten_stand")) %>%
  left_join(., letzte_7_tage_altersgruppen_destatis %>% select(
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
    `Faelle_letzte_7_Tage_80+`,
    `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
    `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
    `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60+`), by=c("id", "date")) %>%
  left_join(., brd_timeseries %>% select(date, cases, id), by=c("id", "date")) %>%
  mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/((EW059+EW6079+EW80)/100000)),
         Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw),
         Kapazitaet_Betten=(faelle_covid_aktuell+betten_frei)) # /icu_days
## berechne vorwarnzeit
myTage <- ausgangsdaten %>% filter((date>=as_date("2020-03-13") & id<=16) |
                                     (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital))%>%as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = icurate_altersgruppen_busse%>%slice(1)%>%as.numeric())) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- ausgangsdaten %>%
  filter((date>=as_date("2020-03-13") & id<=16) |
           (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0))

R_aktuell_Bund <- aktuell$R0[aktuell$id==0]

##### weitere daten für dashboard
## rki-r-wert und vorwarnzeit
rki_reformat_r_ts <- RKI_R %>%
  dplyr::select(contains("Datum"), contains("7-Tage-R Wertes")) %>% dplyr::select(contains("Datum"), contains("Punkt"))
colnames(rki_reformat_r_ts) <-c("date","RKI-R-Wert")
rki_reformat_r_ts <- rki_reformat_r_ts %>% mutate(date=as.Date(date)+5)
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
## data for table on subpage Bundeslaender
bundeslaender_table <- vorwarnzeitergebnis %>%
  filter(id<17 & date==maxdatum) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
         R0=round(R0,digits = 2)) %>%
  select(Bundesland=name,
         "R(t)"=R0,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "Vorwarnzeit aktuell"=Vorwarnzeit,
         "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
         "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
         "Fälle insgesamt"=cases
  )
## data for table on subpage Kreise
kreise_table <- vorwarnzeitergebnis %>%
  filter((id>17 | id==11) & date==maxdatum) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
         R0=round(R0,digits = 2)) %>%
  mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>%
  left_join(., aktuell %>%
              filter(id>0 & id<17) %>%
              select(blid=id, Bundesland=name)) %>%
  arrange(blid,id) %>%
  select(Kreis=name,
         Bundesland,
         "R(t)"=R0,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
         "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
         "Fälle insgesamt"=cases,
         "Vorwarnzeit lokal*"=Vorwarnzeit #, # needs communication
  )
## data for Bundeslaender faktenblatt
bundeslaender_table_faktenblatt <- vorwarnzeitergebnis %>%
  filter(id<17 & date%in%c(lastsunday, sundaybeforelastsunday)) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(R0=round(R0,digits = 2)) %>%
  select(Bundesland=name,
         Datum=date,
         "R(t)"=R0,
         "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
         "Vorwarnzeit"=Vorwarnzeit,
         "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`
  )
## data for Kreise Faktenblatt
kreise_table_faktenblatt <- vorwarnzeitergebnis %>%
  filter((id>17 | id==11) & date%in%c(lastsunday, sundaybeforelastsunday)) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(R0=round(R0,digits = 2)) %>%
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
         "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
         "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
         "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
         "Vorwarnzeit"=Vorwarnzeit #, # needs communication
  )
##### write data for displayed tables/plots to jsons
write_json(bundeslaender_table, "./data/tabledata/bundeslaender_table.json")
write_json(bundeslaender_table_faktenblatt, "./data/tabledata/bundeslaender_table_faktenblatt.json")
write_json(kreise_table, "./data/tabledata/kreise_table.json")
write_json(kreise_table_faktenblatt, "./data/tabledata/kreise_table_faktenblatt.json")
write_json(rwert_bund_data, "./data/plotdata/rwert_bund.json")
write_json(rki_r_und_zi_vwz_data, "./data/plotdata/rki_r_und_zi_vwz_data.json")
write_json(akutinfiziert_data, "./data/plotdata/akutinfiziert.json")
write_json(agefatality_data, "./data/plotdata/agefatality.json")
write_json(itsbetten_data, "./data/plotdata/itsbetten.json")
write_json(bundeslaender_r_und_vwz_data, "./data/plotdata/bundeslaender_r_und_vwz.json")

##### Plots
akutinfiziert_plot <- ggplot(akutinfiziert_data,
                             aes(x=date, y=Infected,group=1)) +
  geom_area(fill="#0086C530") +
  geom_hline(aes(yintercept=0), color="black", linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_minimal() +
  scale_x_date(breaks = "1 month",date_labels = "%d.%m.") +
  labs(y="Anzahl akut infiziert",x = "Datum") +
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x=element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

agefatality_plot <- ggplot(agefatality_data,
                           aes(x=Meldedatum, y=Anteil, color=Merkmal)) +
  geom_line() +
  theme_minimal() + 
  scale_color_zi() +
  labs(y="Verhältnis in %", x="Datum", color="") + 
  scale_x_date(breaks="1 month", date_labels = "%d.%m.") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

rwert_bund_plot <- ggplot(rwert_bund_data,
                          aes(x=date, y=R, group=name, color=name=="Gesamt",
                              text=paste("Region: ", name, "<br>", "Neue Fälle:", I_cases))) +
  geom_hline(yintercept = 1) +
  geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
  scale_color_zi()  +
  theme_minimal() + scale_x_date(date_labels = "%d.%m.", breaks="1 month") +
  labs(x="",y="Reproduktionszahl R(t)",caption="Zeitlicher Verlauf des R-Wertes in Deutschland") +
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

itsbetten_plot <- ggplot(itsbetten_data %>%
                        filter(Betten=="faelle_covid_aktuell"),
                      aes(x=daten_stand, y=Anzahl, color=Betten)) +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=2, show.legend = FALSE, color=zi_cols("ziblue")) +
  theme_minimal() +
  scale_x_date(breaks = "1 month",date_labels = "%d.%m.") +
  labs(y="COVID-19-Fälle ITS", x = "Datum") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

bundeslaender_r_und_vwz_plot <- function(myid) {
  myname <- bundeslaender_r_und_vwz_data %>% filter(id==myid) %>% head(1) %>% pull(name)
  my_r_vwz_data <- bundeslaender_r_und_vwz_data %>% filter(id==myid)
  myplot <- ggplot(my_r_vwz_data,
                   aes(x=Datum, y=Wert, group=name, color=Variable,
                       text=paste("Region: ", name, "<br>Neue Fälle:", I_cases))) +
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
