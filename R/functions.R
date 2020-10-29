# Packages
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

# parameters from literature
icu_days <- 10.1 # aok/divi paper lancet
altersgruppen_bund <- tibble("unter 20"=18.4, "20 bis 40"=24.6,	"40 bis 60"=28.4,
                             "60 bis 80"=21.7,	"80+"=6.8)/100 # destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html

## icu quoten nach busse-lancetpaper, divi-reports und rki-fallzahlen
share_icu <- (19632+1296)/429181  # divi intensivregister 25.10.2020 and rki daily report 25.10.2020 # (14 days delay?)
divi_behandlungen_aktuell <- (19632+1296)/1.27 # (16961+237)/1.27# divi intensivregister 25.10.2020 # SIEHE UNTEN rki_cases_infected
divi_abgeschlossen <- 19632/1.27 # 16961/1.27 # divi report 25.10.2020
deaths_divi <- 4500 # divi report 25.10.2020
busselancet_altersgruppen_hospital <- tibble("Hosp059"=2896, "Hosp6079"=1621+2158, "Hosp80"=3346)
busselancet_altersgruppen_deaths <- tibble("Mort059"=0.007*2474+0.277*422,
                                           "Mort6079"=0.054*1239+0.146*1623+0.455*382+0.626*535,
                                           "Mort80"=0.338*2958+0.722*388)
# mortalität icu nach divi ist vergleichbar zu mortalität KH nach busse:
deathrate_icu <- deaths_divi/divi_abgeschlossen
deathrate_busselancet <- sum(busselancet_altersgruppen_deaths)/sum(busselancet_altersgruppen_hospital)
# deshalb hochrechnung altersverteilung mortalität gesamt von divi auf icu möglich:
icu_altersgruppen <- divi_behandlungen_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital)

# divi_diff_behandlungen <- divi_behandlungen_aktuell-(9300+2189)/1.27 # stichtag 1. mai ende erste welle
# deaths_divi_diff <- deaths_divi-3512 # stichtag
# # deshalb hochrechnung altersverteilung mortalität gesamt von divi auf icu möglich:
# icu_altersgruppen_diff <- divi_diff_behandlungen*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital)


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
         Vorwarnzeit= log(16000/Neue_faelle)/log(Neue_faelle_Anstieg), # obsolete, we calculate it differently now
         Situation = case_when(Vorwarnzeit<0 ~ "grün",
                               (Vorwarnzeit>18 )  ~ "orange",
                               (Vorwarnzeit>=0 & Vorwarnzeit<18)   ~ "rot"),
         Situation=factor(Situation,levels=c("grün","orange","rot"),ordered=T),
         show_val=wday(date)==3) %>% filter(date>=date("2020-03-02"))
prognosen <- tbl(conn,"prognosen") %>% filter((Tage<=28) | Tage %in% c(30,60,90,120,150,180))
brdprognosen <- tbl(conn,"prognosen") %>% filter((Tage<=90) & ((ebene=="Kreis") | (name=="Berlin")) )
rki <- tbl(conn,"rki") %>% collect()

# rki imputation because of delayed gesundheitsamt-meldungen
rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)),maxdate=max(date(Meldedatum)))
rkidays <- date(rkitimeframe$maxdate)-date(rkitimeframe$mindate)
rkidates <- date(rkitimeframe$maxdate)-seq(0,rkidays-1)
rkiidkreise <- unique(rki$IdLandkreis)
rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+") # unique(rki$Altersgruppe)
rkigender <- c("M", "W") # unique(rki$Geschlecht)
Kreise_allvalues <- expand_grid(Meldedatum=rkitimeframe$maxdate, IdLandkreis=rkiidkreise) #, Geschlecht=rkigender, Altersgruppe=rkiagegroups)
Kreise <- rki %>%  mutate(Meldedatum=date(Meldedatum)) %>%
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
divi <- tbl(conn,"divi") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
rki_divi_n_alter <- rki %>% group_by(Meldedatum,Altersgruppe) %>% 
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
  left_join(divi_all %>% filter(id==0) %>% collect(), by=c('Meldedatum'='daten_stand')) %>%
  left_join(vorwarndata %>% select(date, Infected), by=c('Meldedatum'='date')) %>%
  mutate("Fälle gesamt"= `Fälle_0-59`+ `Fälle_60-79`+ `Fälle_80+` , 
         "Todesfälle gesamt" = `Todesfälle_0-59`+ `Todesfälle_60-79`+ `Todesfälle_80+`,
         "60+" = (`Fälle_80+` + `Fälle_60-79` )/ `Fälle gesamt`, 
         "itsfaelle"=`faelle_covid_aktuell`/lag(`Infected`, 14),
         'Todesfälle'= `Todesfälle gesamt`/ `Fälle gesamt`) 

rki_alter_destatis <- rki %>% lazy_dt() %>%
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% # this takes long unfortunately... but much faster with dtplyr!
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  # arrange(Meldedatum,Altersgruppe) %>%
  collect() %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("00-04", "05-14"),
                                             "0-15", Altersgruppe)) %>%
  group_by(Meldedatum,Altersgruppe, id) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  # arrange(Meldedatum,Altersgruppe, id) %>% 
  pivot_wider(id_cols = c(Meldedatum, id),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum), blid=floor(id/1000000),
         `Fälle_60+`=`Fälle_60-79`+`Fälle_80+`) %>%
  right_join(., expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days"), id=unique(.$id)), by=c("Meldedatum", "id")) %>%
  replace(is.na(.), 0) %>%
  group_by(id) %>%
  arrange(Meldedatum) %>%
  mutate(cases059=cumsum(`Fälle_0-15`+`Fälle_15-34`+`Fälle_35-59`),
         cases6079=cumsum(`Fälle_60-79`),
         cases80=cumsum(`Fälle_80+`)) %>% ungroup() %>%
  mutate(blid=floor(id/1000000)) %>%
  as_tibble()

rki_alter_destatis <- bind_rows(rki_alter_destatis,
                                 rki_alter_destatis %>%
                                   group_by(Meldedatum, blid) %>%
                                   summarise(cases059=sum(cases059),
                                             cases6079=sum(cases6079),
                                             cases80=sum(cases80),
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
                                             `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`), .groups="drop") %>%
                                   mutate(id=blid),
                                 rki_alter_destatis %>% group_by(Meldedatum) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
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
                                            `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`), .groups="drop") %>%
                                   mutate(id=0, blid=0))

rki_alter_bund <- rki %>% 
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
  mutate(Meldedatum=lubridate::as_date(Meldedatum))
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
aktuell <- tbl(conn,"params") %>% collect()
trends <- tbl(conn,"trends")
brd_testungen <- tbl(conn,"brd_testungen") %>% collect()
Datenstand <- tbl(conn,"Stand") %>% collect()
# bundprognose <- prognosen %>% filter(id==0) %>% collect() %>%
#   filter(Szenario!="Trend D") %>%
#   mutate(Szenario=ifelse(Szenario=="Trend lokal","aktueller Trend",Szenario),
#          Szenario=ifelse(Szenario=="Worst Case","Worst Case (R=1,3)",Szenario),
#          Datum=as.Date(Datum,format="%d.%m.%Y")) %>%
#   filter(Datum<date(max(Datum)+weeks(12)))
# labordaten <- tbl(conn, "Labordaten")
## 
rki_cases_infected <- rki %>% group_by(Meldedatum,Altersgruppe) %>% 
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
         cases80=cumsum(`Fälle_80+`),
         deaths059=cumsum(`Todesfälle_0-59`),
         deaths6079=cumsum(`Todesfälle_60-79`),
         deaths80=cumsum(`Todesfälle_80+`)) %>%
  mutate(Infected059=cases059-lag(cases059,15),
         Infected6079=cases6079-lag(cases6079,15),
         Infected80=cases80-lag(cases80,15),
         Infected2=Infected059+Infected6079+Infected80)
## delay für fälle-->icu:
rkidivi <- rki_cases_infected %>% left_join(., divi_all %>% filter(id==0), by=c("Meldedatum"="daten_stand")) %>% drop_na()
lengthrkidivi <- dim(rkidivi)[1]
autocorhorizont <- 30
autocors <- rep(0, lengthrkidivi-autocorhorizont+1)
for (lag in 0:autocorhorizont) { autocors[lag+1] <- cor(rkidivi$Infected80[1:(lengthrkidivi-autocorhorizont)], rkidivi$faelle_covid_aktuell[(1+lag):(lengthrkidivi-autocorhorizont+lag)]) }
iculag <- which.max(autocors)-1
# icurates nach erster welle
iculag <- 0
cases_ag <- rki_cases_infected %>% filter(Meldedatum==max(Meldedatum)-iculag) %>% # filter(Meldedatum==as_date("2020-09-14")-iculag) %>% # hier je nach divi datum? 2020-10-25
  select(cases059, cases6079, cases80) # mit 14 tage verzug abgeschlossene behandlungen
cases_ag_stichtag <- rki_cases_infected %>% filter(Meldedatum==as_date("2020-05-01")-iculag) %>% # hier je nach divi datum? 2020-10-25
  select(cases059, cases6079, cases80)
icurate_altersgruppen <- icu_altersgruppen/cases_ag
# icurate_altersgruppen_diff <- icu_altersgruppen_diff/(cases_ag-cases_ag_stichtag)
# icurate_altersgruppen <- tibble("Hosp059"=0.0191, "Hosp6079"=0.091, "Hosp80"=0.145)


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
  group_by(Meldedatum,NeuerFall) %>% summarise(AnzahlFall=sum(AnzahlFall,na.rm=T), .groups="drop") %>%
  collect() %>%
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
                    horizont = 180) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  myresult <- NA
  myresult <- mysir %>% mutate(Tage=row_number()-1) %>% filter(Neue_Faelle>=Kapazitaet) %>% head(1) %>% pull(Tage)
  return(myresult)
}

vorwarnzeit_berechnen_AG <- function(ngesamt,cases,faelle,Kapazitaet_Betten,Rt=1.3, icurate_altersgruppen){
  # achtung, hier sind ngesamt, cases und faelle jeweils vektoren der dim 3 (AG 0-59, 60-79, 80+)
  gamma <- 1/10
  infected <- faelle/gamma
  recovered <- cases-infected
  mysir_AG <- vector("list", 3)
  for (i in 1:3) {
    mysir <- sirmodel(ngesamt = ngesamt[i],
                      S = ngesamt[i] - infected[i] - recovered[i],
                      I = infected[i],
                      R = recovered[i],
                      R0 = Rt,
                      gamma = gamma,
                      horizont = 180) %>% mutate(Neue_Faelle_hq=icurate_altersgruppen[i]*(I-lag(I)+R-lag(R)))
    mysir_AG[[i]] <- mysir
  }
  myresult <- (mysir_AG[[1]]+mysir_AG[[2]]+mysir_AG[[3]]) %>% mutate(Tage=row_number()-1) %>% filter(Neue_Faelle_hq>=Kapazitaet_Betten) %>% head(1) %>% pull(Tage)
  return(myresult)
}

### Vorwarnzeit aktuell
maxdatum <- max(as_date(rki_alter_destatis$Meldedatum))
letzte_7_tage <-  brd_timeseries %>% collect() %>% mutate(date=date(date)) %>%
  group_by(id) %>% arrange(id,-as.numeric(date)) %>%
  filter(row_number()<=8) %>% # fuer die anzahl der neuen faelle der letzten 7 tage muessen wir die letzten 8 tage ziehen
  summarise(Faelle_letzte_7_Tage=first(cases)-last(cases), .groups="drop") %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7))
letzte_7_tage_altersgruppen_bund <-  rki_alter_bund %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdatum-6) %>%
  summarise(`Faelle_letzte_7_Tage_0-59`=sum(`Fälle_0-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), .groups="drop") %>%
  # mutate(`Faelle_letzte_7_Tage_pro_Tag_0-59`=round(`Faelle_letzte_7_Tage_0-59`/7),
  #        `Faelle_letzte_7_Tage_pro_Tag_60-79`=round(`Faelle_letzte_7_Tage_60-79`/7),
  #        `Faelle_letzte_7_Tage_pro_Tag_80+`=round(`Faelle_letzte_7_Tage_80+`/7)) %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select(., `unter 20`:`40 bis 60`))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)))

  # bind_cols(., strukturdaten %>% filter(id==0)) %>%
  # mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select_(., "`unter 3 Jahre`:`55 bis unter 60 Jahre`"))/100000)),
  #        `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(sum(select_(., "`unter 3 Jahre`:`55 bis unter 60 Jahre`"))/100000)),
  #        `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(sum(select_(., "`unter 3 Jahre`:`55 bis unter 60 Jahre`"))/100000)))
letzte_7_tage_altersgruppen_destatis <- rki_alter_destatis %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdatum-6) %>%
  group_by(id) %>% # arrange(id,-as.numeric(date)) %>%
  summarise(`Faelle_letzte_7_Tage_0-14`=sum(`Fälle_0-15`),
            `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
            `Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
            `Faelle_letzte_7_Tage_0-59`=`Faelle_letzte_7_Tage_0-14`+`Faelle_letzte_7_Tage_15-34`+`Faelle_letzte_7_Tage_35-59`,
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`),
            `Faelle_letzte_7_Tage_60+`=sum(`Fälle_60+`), .groups="drop") %>%
  # ungroup() %>%
  left_join(., strukturdaten, by="id") %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag_059=round(`Faelle_letzte_7_Tage_0-59`/7),
         Faelle_letzte_7_Tage_pro_Tag_6079=round(`Faelle_letzte_7_Tage_60-79`/7),
         Faelle_letzte_7_Tage_pro_Tag_80=round(`Faelle_letzte_7_Tage_80+`/7),
         `Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((`unter 3 Jahre`+`3 bis unter 6 Jahre`+`6 bis unter 10 Jahre`+`10 bis unter 15 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((`15 bis unter 18 Jahre`+`18 bis unter 20 Jahre`+`20 bis unter 25 Jahre`+`25 bis unter 30 Jahre`+`30 bis unter 35 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((`35 bis unter 40 Jahre`+`40 bis unter 45 Jahre`+`45 bis unter 50 Jahre`+`50 bis unter 55 Jahre`+`55 bis unter 60 Jahre`)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+`75 Jahre und mehr`)/100000))) %>%
  left_join(., rki_alter_destatis %>% filter(Meldedatum==maxdatum) %>% select(cases059, cases6079, cases80, id), by="id") %>%
  mutate(EW059=rowSums(select(., `unter 3 Jahre`:`55 bis unter 60 Jahre`)),
         EW6079=`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+round(0.4*`75 Jahre und mehr`),
         EW80=round(0.6*`75 Jahre und mehr`))

ausgangsdaten <- aktuell  %>%
  select(id,name,ICU_Betten,Einwohner,ebene, # EW_insgesamt
         cases,R0) %>% filter(ebene!="Staaten" & !is.na(ebene)) %>% select(-ebene) %>% collect() %>%
  left_join(., letzte_7_tage,by="id") %>%
  left_join(., divi %>% select(id, betten_frei, faelle_covid_aktuell) %>% mutate(id=ifelse(id>16, id*1000, id)), by="id") %>%
  left_join(., letzte_7_tage_altersgruppen_destatis %>% select(
    id,
    cases059, cases6079, cases80,
    EW059, EW6079, EW80,
    `Faelle_letzte_7_Tage_pro_Tag_059`,
    `Faelle_letzte_7_Tage_pro_Tag_6079`,
    `Faelle_letzte_7_Tage_pro_Tag_80`,
    `Faelle_letzte_7_Tage_0-59`,
    `Faelle_letzte_7_Tage_60-79`,
    `Faelle_letzte_7_Tage_80+`,
    `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
    `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
    `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60+`), by="id") %>%
  mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/(Einwohner/100000)),
         Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw))


# vorwarnzeit aktueller tag daten
R_aktuell_Bund <- ausgangsdaten$R0[ausgangsdaten$id==0]
vorwarnzeitergebnis <- ausgangsdaten %>%
  mutate(Handlungsgrenze_7_tage=50*(Einwohner/100000),
         Handlungsgrenze_pro_Tag=round(Handlungsgrenze_7_tage/7),
         R0 = ifelse((R0>1) & (Faelle_letzte_7_Tage_pro_Tag==0),NA,R0),
         Kapazitaet_Betten=(betten_frei + faelle_covid_aktuell)/icu_days,
         Kapazitaet=(betten_frei + faelle_covid_aktuell)/share_icu/icu_days,
         Auslastung_durch_Grenze=round(100*(Handlungsgrenze_pro_Tag/Kapazitaet)))

myTage <- vorwarnzeitergebnis %>% rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(c(.$EW059, .$EW6079, .$EW80),
                                  c(.$cases059, .$cases6079, .$cases80),
                                  c(.$Faelle_letzte_7_Tage_pro_Tag_059, .$Faelle_letzte_7_Tage_pro_Tag_6079, .$Faelle_letzte_7_Tage_pro_Tag_80),
                                  .$Kapazitaet_Betten, 1.3, icurate_altersgruppen%>%slice(1)%>%as.numeric())) %>% # max(1.3, R_aktuell_Bund)
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- vorwarnzeitergebnis %>%
  mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=pmax(0, Vorwarnzeit-21))

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
    summarise(Faelle_letzte_7_Tage=first(cases)-last(cases), .groups="drop") %>%
    mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7))
  letzte_7_tage_altersgruppen_destatis_h <- rki_alter_destatis %>%
    mutate(date=date(Meldedatum)) %>%
    filter(date<=stichtag) %>%
    group_by(id) %>% arrange(id,-as.numeric(date)) %>%
    filter(date>=stichtag-6) %>%
    group_by(id) %>% # arrange(id,-as.numeric(date)) %>%
    summarise(`Faelle_letzte_7_Tage_0-14`=sum(`Fälle_0-15`),
              `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
              `Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
              `Faelle_letzte_7_Tage_0-59`=`Faelle_letzte_7_Tage_0-14`+`Faelle_letzte_7_Tage_15-34`+`Faelle_letzte_7_Tage_35-59`,
              `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
              `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`),
              `Faelle_letzte_7_Tage_60+`=sum(`Fälle_60+`), .groups="drop") %>%
    # ungroup() %>%
    left_join(., strukturdaten, by="id") %>%
    mutate(Faelle_letzte_7_Tage_pro_Tag_059=round(`Faelle_letzte_7_Tage_0-59`/7),
           Faelle_letzte_7_Tage_pro_Tag_6079=round(`Faelle_letzte_7_Tage_60-79`/7),
           Faelle_letzte_7_Tage_pro_Tag_80=round(`Faelle_letzte_7_Tage_80+`/7),
           `Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((`unter 3 Jahre`+`3 bis unter 6 Jahre`+`6 bis unter 10 Jahre`+`10 bis unter 15 Jahre`)/100000)),
           `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((`15 bis unter 18 Jahre`+`18 bis unter 20 Jahre`+`20 bis unter 25 Jahre`+`25 bis unter 30 Jahre`+`30 bis unter 35 Jahre`)/100000)),
           `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((`35 bis unter 40 Jahre`+`40 bis unter 45 Jahre`+`45 bis unter 50 Jahre`+`50 bis unter 55 Jahre`+`55 bis unter 60 Jahre`)/100000)),
           `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+`75 Jahre und mehr`)/100000))) %>%
    left_join(., rki_alter_destatis %>% filter(Meldedatum==maxdatum) %>% select(cases059, cases6079, cases80, id), by="id") %>%
    mutate(EW059=rowSums(select(., `unter 3 Jahre`:`55 bis unter 60 Jahre`)),
           EW6079=`60 bis unter 65 Jahre`+`65 bis unter 75 Jahre`+round(0.4*`75 Jahre und mehr`),
           EW80=round(0.6*`75 Jahre und mehr`))
  
  ausgangsdaten_h <- aktuell  %>%
    select(id,name,ICU_Betten,Einwohner,ebene, # EW_insgesamt
           cases,R0) %>% filter(ebene!="Staaten" & !is.na(ebene)) %>% select(-ebene) %>% collect() %>%
    left_join(.,letzte_7_tage_h,by="id") %>%
    left_join(., divi %>% select(id, betten_frei, faelle_covid_aktuell) %>% mutate(id=ifelse(id>16, id*1000, id)), by="id") %>%
    left_join(., letzte_7_tage_altersgruppen_destatis_h %>% select(
      id,
      cases059, cases6079, cases80,
      EW059, EW6079, EW80,
      `Faelle_letzte_7_Tage_pro_Tag_059`,
      `Faelle_letzte_7_Tage_pro_Tag_6079`,
      `Faelle_letzte_7_Tage_pro_Tag_80`,
      `Faelle_letzte_7_Tage_0-59`,
      `Faelle_letzte_7_Tage_60-79`,
      `Faelle_letzte_7_Tage_80+`,
      `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
      `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
      `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
      `Faelle_letzte_7_Tage_je100TsdEinw_60+`), by="id") %>%
    mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/(Einwohner/100000)),
           Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw))
  
  vorwarnzeitergebnis_h <- ausgangsdaten_h %>%
    mutate(Handlungsgrenze_7_tage=50*(Einwohner/100000),
           Handlungsgrenze_pro_Tag=round(Handlungsgrenze_7_tage/7),
           R0 = ifelse((R0>1) & (Faelle_letzte_7_Tage_pro_Tag==0),NA,R0),
           Kapazitaet_Betten=(betten_frei + faelle_covid_aktuell)/icu_days,
           Kapazitaet=(betten_frei + faelle_covid_aktuell)/share_icu/icu_days,
           Auslastung_durch_Grenze=round(100*(Handlungsgrenze_pro_Tag/Kapazitaet))) %>%
  filter(id<=16)

  myTage <- vorwarnzeitergebnis_h %>% rowwise() %>%
    do(Tage = vorwarnzeit_berechnen_AG(c(.$EW059, .$EW6079, .$EW80),
                                       c(.$cases059, .$cases6079, .$cases80),
                                       c(.$Faelle_letzte_7_Tage_pro_Tag_059, .$Faelle_letzte_7_Tage_pro_Tag_6079, .$Faelle_letzte_7_Tage_pro_Tag_80),
                                       .$Kapazitaet_Betten, 1.3, icurate_altersgruppen%>%slice(1)%>%as.numeric())) %>% 
    unnest(cols = c(Tage), keep_empty=TRUE)
  vorwarnzeitergebnis_h <- vorwarnzeitergebnis_h %>%
    mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0), date=stichtag)
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
                           left_join(., vorwarnzeitverlauf %>% filter(id==theid) %>% select(date, Vorwarnzeit), by="date")) # _effektiv
}

myblmitidata <- blmitidata %>%
  filter(Merkmal=="Fälle"  & R_Mean<10 & date>=date("2020-03-13"))

### ALLE Bundesländer
mitigationsplot_blvergleich <- ggplot(myblmitidata %>% rename(R=R_Mean) %>% mutate(R=round(R,digits = 1)),
                   aes(x=date,y=R,group=name,color=name=="Gesamt",
                       text=paste("Region: ",name,"<br>","Neue Fälle:",I_cases))) +
    geom_hline(yintercept = 1) +
    # geom_line(data = . %>% filter(name!="Gesamt"),size=1,show.legend = F,color="lightgrey")+
    geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
    scale_color_zi()  +
    theme_minimal() + scale_x_date(date_labels = "%d.%m.", breaks="1 month") +
    labs(x="",y="Reproduktionszahl R(t)",caption="Zeitlicher Verlauf des R-Wertes in Deutschland") +
    # geom_vline(aes(xintercept=date("2020-03-16")),color="grey") +
    # geom_vline(aes(xintercept=date("2020-03-22")),color="grey") +
    # geom_vline(aes(xintercept=date("2020-04-17")),color="grey") +
    # annotate("text", x = date("2020-03-16"), y = 3.3, label = "Schulschließungen\n16.3.",color="black",size=3) +
    # annotate("text", x = date("2020-03-22"), y = 2.5, label = "Kontakteinschränkungen\n22.3.",color="black",size=3) +
    # annotate("text", x = date("2020-04-17"), y = 2.0, label = "Lockerung der \nMaßnahmen\n17.4.",color="black",size=3) +
    theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())

### Plot on Age of cases and case fatality 
age_plot_fatality <- ggplot(rki_divi_n_alter %>%
                              filter(Meldedatum>=as_date("2020-03-01")) %>%
                              select(Meldedatum,
                                     "Alter 60+ an Fällen"=`60+`,
                                     "ITS-Fälle an Fällen"=`itsfaelle`, 
                                     "Todesfälle an Fällen"= `Todesfälle`) %>% 
                              gather(Merkmal,Anteil,2:4) %>% mutate(Anteil=round(Anteil*100,digits=2)) ,
                            aes(x=Meldedatum,y=Anteil,color=Merkmal)) +
  # geom_smooth(span=0.1, se=FALSE, n=tally(rki_divi_n_alter)/3) +
  geom_line() +
  theme_minimal() + 
  scale_color_zi() +
  labs(y="Verhältnis in %",x="Datum",color="") + 
  scale_x_date(breaks="1 month", date_labels = "%d.%m.") + 
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())

#### Einzelne Länder # Hier neue Datenreihe Vorwarnzeit!
range_r <- range(myblmitidata$R_Mean)
range_vwz <- range(myblmitidata$Vorwarnzeit_effektiv, na.rm = TRUE)
mitigationsplot_bl <- function(myid){
  myname <- myblmitidata %>% filter(id==myid) %>% head(1) %>% pull(name)
  my_r_vwz_data <- myblmitidata %>% filter(id==myid) %>% rename(Datum=date, R=R_Mean) %>% # , Vorwarnzeit=Vorwarnzeit_effektiv
    mutate(R=round(R,digits = 1)) %>%
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
  myplot %>% ggplotly(tooltip = c("x", "y", "text")) # , width=800, height=400
}

### Akute infizierte Fälle
akutinfiziert <- ggplot(vorwarndata,aes(x=date,y=Infected,group=1)) +
  geom_area(fill="#0086C530") +
  # geom_vline(aes(xintercept=date("2020-03-16")),color="black",linetype ="dotted") +
  # geom_vline(aes(xintercept=date("2020-03-22")),color="black",linetype ="dotted") +
  # geom_vline(aes(xintercept=date("2020-04-17")),color="black",linetype ="dotted") +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_minimal() +
  scale_x_date(breaks = "1 month",date_labels = "%d.%m.") +
  # annotate("text", x = date("2020-03-16"), y = 22000, label = "Schulschließungen",color="black",size=3) +
  # annotate("text", x = date("2020-03-22"), y = 42000, label = "Kontakteinschränkungen",color="black",size=3) +
  # annotate("text", x = date("2020-04-17"), y = 43500, label = "Lockerungsbeschluss",color="black",size=3) +
  labs(y="Anzahl akut infiziert",x = "Datum") +
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

## plotfunction for vorwarnzeitverlauf brd
# change to function to avoid full breaks
rki_reformat_r_ts <- RKI_R %>%
  dplyr::select(contains("Datum"), contains("7-Tage-R Wertes")) %>% dplyr::select(contains("Datum"), contains("Punkt"))
colnames(rki_reformat_r_ts) <-c("date","RKI-R-Wert")
rki_reformat_r_ts <- rki_reformat_r_ts %>% mutate(date=as.Date(date)+5)
zivwz_vs_rkir_verlauf <- inner_join(vorwarnzeitverlauf %>%
                                      filter(id==0) %>%
                                      mutate(Vorwarnzeit=Vorwarnzeit), # _effektiv
                                    rki_reformat_r_ts,
                                    by=c("date")) %>%
  pivot_longer(c("Vorwarnzeit", "RKI-R-Wert"), names_to="Variable", values_to="Wert")
vorwarnzeitverlauf_plot <- ggplot()
# # handle errors
# tryCatch(
  vorwarnzeitverlauf_plot <- ggplot(zivwz_vs_rkir_verlauf,
                                   aes(x=date, y=Wert, color=Variable)) +
    facet_wrap(~Variable, scales = "free_y") +
    geom_line(size=2) +
    ylim(0, NA) +
    scale_color_zi() +
    labs(subtitle="Zi-Vorwarnzeit und RKI-R-Wert im Zeitverlauf",x="",y="") +
    theme_minimal() +
    theme(legend.position='none') #)

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
  mysir <- sirmodel(ngesamt = fall$Einwohner,
                    S = fall$Einwohner - fall$cases,
                    I = (fall$Faelle_letzte_7_Tage_pro_Tag)/gamma,
                    R = fall$cases - (fall$Faelle_letzte_7_Tage_pro_Tag)/gamma,
                    R0 = Rt[i],
                    gamma =gamma,
                    horizont = 365) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  Vorwarnzeit[i] <- which.max(mysir$Neue_Faelle>Belastungsgrenze)
}
as_tibble(cbind(Rt,Vorwarnzeit)) %>%
  mutate(id=myid,Effektive_Vorwarnzeit=pmax(0, Vorwarnzeit-Reaktionszeit)) %>%
  select(id,Rt,Vorwarnzeit,Effektive_Vorwarnzeit)
}

rki_fallzahl_bl <- function(){
  df <- vorwarnzeitergebnis %>% filter(id<17) %>% mutate(cases_je_100Tsd=round(cases/(Einwohner/100000)),
                                                         R0=round(R0,digits = 2))
  df %>% select(Bundesland=name,
                "R(t)"=R0,
                "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
                # "Effektive Vorwarnzeit aktuell"=Vorwarnzeit_effektiv,
                "Vorwarnzeit aktuell"=Vorwarnzeit,
                "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
                "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
                "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
                "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
                "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
                "Fälle insgesamt"=cases #,
                # "Fälle je 100 Tsd. Einw."=cases_je_100Tsd,
  )
}

rki_fallzahl_kreis <- function(){
  df <- vorwarnzeitergebnis %>% filter(id>17 | name=="Berlin") %>% mutate(cases_je_100Tsd=round(cases/(Einwohner/100000)),
                                                         R0=round(R0,digits = 2))
  bundeslaender <- aktuell %>% filter(id>0 & id<17) %>% select(blid=id,Bundesland=name) %>% collect()
  df <- df %>% mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>% left_join(.,bundeslaender) %>% arrange(blid,id)
  df %>% select(Kreis=name,
                Bundesland,
                "R(t)"=R0,
                "7-Tage-Inzidenz"=Faelle_letzte_7_Tage_je100TsdEinw,
                # "Effektive Vorwarnzeit lokal*"=Vorwarnzeit_effektiv, # needs communication
                "7-Tage-Inzidenz 60+"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
                "7-Tage-Inzidenz 35-59"=`Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
                "7-Tage-Inzidenz 15-34"=`Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
                "7-Tage-Inzidenz 0-14"=`Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
                "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
                "Fälle insgesamt"=cases, # "Fälle je 100 Tsd. Einw."=cases_je_100Tsd
                "Vorwarnzeit lokal*"=Vorwarnzeit #, # needs communication
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
plotdata_Anstieg <- make_theoretischedaten(myid=0) %>% select(Rt,Vorwarnzeit,"Effektive Vorwarnzeit"=Effektive_Vorwarnzeit) %>% gather(Merkmal,Wert,2:3) # %>% filter(`Effektive Vorwarnzeit`>=0) 
plotdata_Anstieg <- plotdata_Anstieg %>% mutate(Wert = replace(Wert, Wert < 0, 0))
plot_Anstiegtheor <- ggplot(plotdata_Anstieg, aes(x=Rt, y=Wert,color=Merkmal)) +
  geom_line(size=1.5, show.legend = F)+
  geom_point(size=3, show.legend = F)+
  geom_hline(yintercept = 0) +
  theme_minimal() + scale_color_zi() +
  scale_x_continuous(labels =  function(x) paste0(format(x,decimal.mark = ",")),breaks=seq(1.1, 2, 0.1))  +
  labs(y=paste0("Vorwarnzeit in Tagen"))+
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())

