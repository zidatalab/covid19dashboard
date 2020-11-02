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

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

rki <- tbl(conn,"rki") %>% collect()
divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
DBI::dbDisconnect(conn)

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
rki <- Kreise %>%
  mutate(IdLandkreis=as.integer(IdLandkreis))

# special case Berlin
rki_berlin <- rki %>%
  filter(IdLandkreis>=11000 & IdLandkreis<12000) %>%
  group_by(Meldedatum, Altersgruppe, Geschlecht) %>%
  summarise_at(vars("AnzahlFall", "AnzahlTodesfall"), sum) %>%
  ungroup() %>%
  mutate(IdLandkreis=11000, IdBundesland=11)

rkib <- bind_rows(rki %>% filter(IdLandkreis<11000 | IdLandkreis>=12000),
                  rki_berlin) 

rki_cases_infected_bund <- rkib %>% group_by(Meldedatum,Altersgruppe) %>% 
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
  pivot_wider(id_cols = c("Meldedatum"),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  right_join(., expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days")), by=c("Meldedatum")) %>%
  replace(is.na(.), 0) %>%
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

rki_cases_infected_laender <- rkib %>%
  group_by(Meldedatum,Altersgruppe, IdBundesland) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe, IdBundesland) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,"0-59")) %>%
  group_by(Meldedatum, Altersgruppe, IdBundesland) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe, IdBundesland) %>% 
  pivot_wider(id_cols = c("Meldedatum", "IdBundesland"),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  right_join(., expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days"), IdBundesland=unique(.$IdBundesland)), by=c("Meldedatum", "IdBundesland")) %>%
  replace(is.na(.), 0) %>%
  group_by(IdBundesland) %>%
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
         Infected2=Infected059+Infected6079+Infected80) %>%
  ungroup() %>%
  filter(IdBundesland!=0)

rki_cases_infected <- rkib %>% group_by(Meldedatum,Altersgruppe, IdLandkreis) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe, IdLandkreis) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,"0-59")) %>%
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe, IdLandkreis) %>% 
  pivot_wider(id_cols = c("Meldedatum", "IdLandkreis"),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  right_join(., expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days"), IdLandkreis=unique(.$IdLandkreis)), by=c("Meldedatum", "IdLandkreis")) %>%
  replace(is.na(.), 0) %>%
  group_by(IdLandkreis) %>%
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
         Infected2=Infected059+Infected6079+Infected80) %>%
  ungroup()

rkidivi_bund <- rki_cases_infected_bund %>%
  left_join(., divi_all %>%
              filter(id==0),
            by=c("Meldedatum"="daten_stand")) %>%
  drop_na()
rkidivi <- rki_cases_infected %>% 
  left_join(., divi_all,
            by=c("Meldedatum"="daten_stand", "IdLandkreis"="id")) %>% 
  drop_na()
rkidivi_laender <- rki_cases_infected_laender %>% 
  left_join(., divi_all,
            by=c("Meldedatum"="daten_stand", "IdBundesland"="id")) %>% 
  drop_na()

# iculag
## lag bund
lengthrkidivi <- dim(rkidivi_bund)[1]
autocorhorizont <- 50
autocors <- rep(0, autocorhorizont+1)
for (lag in 0:autocorhorizont) {
  autocors[lag+1] <- cor(rkidivi_bund$Infected80[1:(lengthrkidivi-autocorhorizont)],
                         rkidivi_bund$faelle_covid_aktuell[(1+lag):(lengthrkidivi-autocorhorizont+lag)])
  }
iculag <- which.max(autocors)-1
print(iculag)
## lag kreise
autocorhorizont <- 50
kreise_ids <- unique(rkidivi$IdLandkreis)
nkreise <- length(kreise_ids) # nur 397? --> divi_all: nicht überall ITS?
autocors <- matrix(0, nrow=autocorhorizont+1, ncol=nkreise)
iculag <- rep(0, nkreise)
for (nk in seq(nkreise)) {
  thisrkidivikreis <- rkidivi %>% filter(IdLandkreis==kreise_ids[nk])
  lengthrkidivi <- dim(thisrkidivikreis)[1]
  for (lag in 0:autocorhorizont) {
    autocors[lag+1, nk] <- cor(thisrkidivikreis$Infected80[1:(lengthrkidivi-autocorhorizont)],
                           thisrkidivikreis$faelle_covid_aktuell[(1+lag):(lengthrkidivi-autocorhorizont+lag)])
  }
  thisiculag <- which.max(autocors[, nk])
  iculag[nk] <- ifelse(is_empty(thisiculag), NA, thisiculag-1)
}
sum(iculag==0, na.rm = TRUE)
sum(is.na(iculag))
hist(iculag[!is.na(iculag) & iculag!=0])
mean(iculag[!is.na(iculag) & iculag!=0])
median(iculag[!is.na(iculag) & iculag!=0])
hist(iculag[!is.na(iculag)])
mean(iculag[!is.na(iculag)])
median(iculag[!is.na(iculag)])
autocors_median <- apply(autocors, 1, median, na.rm=TRUE)

# infection tipping point
## lag bund
lengthrkidivi <- dim(rkidivi_bund)[1]
autocorhorizont <- 60
autocors <- rep(0, autocorhorizont+1)
lmr2s <- autocors
for (lag in 0:autocorhorizont) {
  thislm <- lm(rkidivi_bund$Infected80[(1+lag):(lengthrkidivi-autocorhorizont+lag)] ~ 0 + rkidivi_bund$Infected059[1:(lengthrkidivi-autocorhorizont)])
  lmr2s[lag+1] <- summary(thislm)$adj.r.squared
  autocors[lag+1] <- cor(rkidivi_bund$Infected059[1:(lengthrkidivi-autocorhorizont)],
                         rkidivi_bund$Infected80[(1+lag):(lengthrkidivi-autocorhorizont+lag)])
}
inflag <- which.max(autocors)-1
print(inflag)

## lag laender
autocorhorizont <- 60
laender_ids <- unique(rkidivi_laender$IdBundesland)
nlaender <- length(laender_ids) # nur 397? --> divi_all: nicht überall ITS?
autocors <- matrix(0, nrow=autocorhorizont+1, ncol=nlaender)
lmr2s <- autocors
lmr2lag <- rep(0, nlaender)
autocorlag <- rep(0, nlaender)
for (nl in seq(nlaender)) {
  thisrkidiviland <- rkidivi_laender %>% filter(IdBundesland==laender_ids[nl])
  lengthrkidivi <- dim(thisrkidiviland)[1]
  for (thislag in 0:autocorhorizont) {
    thislm <- lm(thisrkidiviland$Infected80[(1+thislag):(lengthrkidivi-autocorhorizont+thislag)] ~ 0 + thisrkidiviland$Infected059[1:(lengthrkidivi-autocorhorizont)])
    lmr2s[thislag+1, nl] <- summary(thislm)$adj.r.squared
    autocors[thislag+1, nl] <- cor(thisrkidiviland$Infected059[1:(lengthrkidivi-autocorhorizont)],
                                   thisrkidiviland$Infected80[(1+thislag):(lengthrkidivi-autocorhorizont+thislag)])
  }
  thislmr2lag <- which.max(lmr2s[, nl])
  lmr2lag[nl] <- ifelse(is_empty(thislmr2lag), NA, thislmr2lag-1)
  thisautocorlag <- which.max(autocors[, nl])
  autocorlag[nl] <- ifelse(is_empty(thisautocorlag), NA, thisautocorlag-1)
}
hist(lmr2lag)
mean(lmr2lag, na.rm = TRUE)
median(lmr2lag, na.rm = TRUE)
hist(autocorlag)
mean(autocorlag, na.rm = TRUE)
median(autocorlag, na.rm = TRUE)

## lag kreise
autocorhorizont <- 60
kreise_ids <- unique(rkidivi$IdLandkreis)
nkreise <- length(kreise_ids) # nur 397? --> divi_all: nicht überall ITS?
autocors <- matrix(0, nrow=autocorhorizont+1, ncol=nkreise)
lmr2s <- autocors
lmr2lag <- rep(0, nkreise)
autocorlag <- rep(0, nkreise)
for (nk in seq(nkreise)) {
  thisrkidivikreis <- rkidivi %>% filter(IdLandkreis==kreise_ids[nk])
  lengthrkidivi <- dim(thisrkidivikreis)[1]
  for (lag in 0:autocorhorizont) {
    thislm <- lm(thisrkidivikreis$Infected80[(1+lag):(lengthrkidivi-autocorhorizont+lag)] ~ 0 + thisrkidivikreis$Infected059[1:(lengthrkidivi-autocorhorizont)])
    lmr2s[lag+1, nk] <- summary(thislm)$adj.r.squared
    autocors[lag+1, nk] <- cor(thisrkidivikreis$Infected059[1:(lengthrkidivi-autocorhorizont)],
                               thisrkidivikreis$Infected80[(1+lag):(lengthrkidivi-autocorhorizont+lag)])
  }
  thislmr2lag <- which.max(lmr2s[, nk])
  lmr2lag[nk] <- ifelse(is_empty(thislmr2lag), NA, thislmr2lag-1)
  thisautocorlag <- which.max(autocors[, nk])
  autocorlag[nk] <- ifelse(is_empty(thisautocorlag), NA, thisautocorlag-1)
}
sum(lmr2lag==0, na.rm = TRUE)
sum(is.na(lmr2lag))
hist(lmr2lag[!is.na(lmr2lag)])
mean(lmr2lag[!is.na(lmr2lag) & lmr2lag!=0])
median(lmr2lag[!is.na(lmr2lag) & lmr2lag!=0])
mean(lmr2lag[!is.na(lmr2lag)])
median(lmr2lag[!is.na(lmr2lag)])
hist(autocorlag[!is.na(autocorlag)])

## one lag kreise
rkidivi_dichte <- rkidivi %>%
  left_join(., strukturdaten %>%
              mutate(ewdichte=EW_insgesamt/flaeche,
                     id=floor(id/1000)) %>%
              select(id, ewdichte),
            by=c("IdLandkreis"="id"))
lengthrkidivi <- dim(rkidivi_bund)[1]
autocorhorizont <- 50
autocors <- rep(0, autocorhorizont+1)
lmr2s <- autocors
for (thislag in 0:autocorhorizont) {
  thislagrkidivi <- rkidivi_dichte %>%
    group_by(IdLandkreis) %>%
    mutate(laggedInfected059=dplyr::lag(Infected059, n=thislag, order_by = Meldedatum)) %>%
    ungroup() %>%
    select(laggedInfected059, Infected80, ewdichte) %>%
    drop_na()
  thislm <- lm(Infected80 ~ 0 + laggedInfected059 + ewdichte, data=thislagrkidivi)
  lmr2s[thislag+1] <- summary(thislm)$adj.r.squared
  autocors[thislag+1] <- cor(thislagrkidivi$laggedInfected059,
                         thislagrkidivi$Infected80)
}
inflag <- which.max(autocors)-1
print(inflag)
