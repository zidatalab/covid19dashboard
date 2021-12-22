library(lubridate)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(zoo)
library(openxlsx)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_rki_impfstoff <- tbl(conn, "kbv_rki_impfstoffe_laender") %>% 
  collect()
kbv_rki_age <- tbl(conn,"kbv_rki_altersgruppen_kreise") %>%
  collect()

kbv_rki_impfstoff_gesamt <- kbv_rki_impfstoff %>% 
  group_by(vacc_date, vacc_series, Impfstoff) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
            anzahl_alleorte=sum(anzahl_alleorte, na.rm=TRUE),
            .groups = "drop")
kbv_rki_age_gesamt <- kbv_rki_age %>% 
  group_by(vacc_date, vacc_series, Altersgruppe) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
            anzahl_alleorte=sum(anzahl_alleorte, na.rm=TRUE),
            .groups = "drop")

letztewoche <- kbv_rki_age_gesamt %>% 
  filter(vacc_date>="2021-12-13" & vacc_date<="2021-12-19") %>% 
  group_by(vacc_series) %>% 
  summarise(`Anzahl Praxen`=sum(anzahl_praxen, na.rm=TRUE),
            `Anzahl Gesamt`=sum(anzahl_alleorte, na.rm=TRUE),
            `Anteil Praxen`=paste0(round(`Anzahl Praxen`/`Anzahl Gesamt`*100, 1), "%"))

write.xlsx(letztewoche, "data/letztewoche_impfen_brd.xlsx")

stschnitt <- kbv_rki_age_gesamt %>% 
  group_by(vacc_date) %>% 
  summarise(impfungen_praxen=sum(anzahl_praxen, na.rm=TRUE),
            impfungen_gesamt=sum(anzahl_alleorte, na.rm=TRUE)) %>% 
  mutate(wtag=wday(vacc_date, week_start = 1)) %>% 
  arrange(rev(vacc_date)) %>% 
  mutate(stschnitt_praxen=rollmean(impfungen_praxen, 7, fill = 0, align = "left"),
         stschnitt_gesamt=rollmean(impfungen_gesamt, 7, fill = 0, align = "left")) %>% 
  filter(wtag==1 & vacc_date>="2021-10-25") %>% 
  select(vacc_date, stschnitt_praxen, stschnitt_gesamt)

write.xlsx(stschnitt, "data/stschnitt_montags_gesamt.xlsx")


stschnitt_123 <- kbv_rki_age_gesamt %>% 
  group_by(vacc_date, vacc_series) %>% 
  summarise(impfungen_praxen=sum(anzahl_praxen, na.rm=TRUE),
            impfungen_gesamt=sum(anzahl_alleorte, na.rm=TRUE),
            .groups = "drop") %>% 
  mutate(wtag=wday(vacc_date, week_start = 1)) %>% 
  arrange(rev(vacc_date)) %>% 
  group_by(vacc_series) %>% 
  mutate(stschnitt_praxen=rollmean(impfungen_praxen, 7, fill = 0, align = "left"),
         stschnitt_gesamt=rollmean(impfungen_gesamt, 7, fill = 0, align = "left")) %>% 
  filter(wtag==1 & vacc_date>="2021-10-25") %>% 
  mutate(anteil_praxen=stschnitt_praxen/stschnitt_gesamt) %>% 
  select(vacc_date, stschnitt_praxen, stschnitt_gesamt, anteil_praxen) %>% 
  pivot_wider(id_cols = vacc_date, names_from = vacc_series,
              values_from = stschnitt_praxen:anteil_praxen)

stschnitt_1 <- stschnitt_123 %>% 
  select(vacc_date, contains("_1"))

write.xlsx(stschnitt_1, "data/stschnitt_erst_montags_gesamt.xlsx")

kw_age_erst <- kbv_rki_age_gesamt %>% filter(vacc_series==1) %>% 
  mutate(KW=isoweek(vacc_date)) %>% 
           group_by(Altersgruppe, KW) %>% 
           summarise(across(contains("anzahl"), sum))
