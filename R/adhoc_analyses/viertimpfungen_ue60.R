library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(openxlsx)
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

DBI::dbListTables(conn)

kbv_rki_age <- tbl(conn, "kbv_rki_altersgruppen_kreise") %>% 
  collect()

rki_faelle <- tbl(conn, "rki") %>% collect()

faelle_sommer2022 <- rki_faelle %>% 
  mutate(Meldedatum=as_date(Meldedatum)) %>% 
  filter(Meldedatum>="2022-03-01") %>% 
  mutate(myage=case_when(
    Altersgruppe %in% c("A60-A79", "A80+") ~ "60+",
    TRUE ~ "unter 60"
    ),
    mydate=case_when(
      Meldedatum<"2022-06-01" ~ "ab März",
      Meldedatum>="2022-06-1" ~ "ab Juni"
    )) %>% 
  group_by(myage, mydate) %>% 
  summarise(Faelle=sum(AnzahlFall[NeuerFall>=0]), 
            .groups="drop")



# new (für herbstwelle 2022)
booster_nachalter_bydategesamt <- kbv_rki_age %>% 
  filter(vacc_series>=2) %>% 
  group_by(Altersgruppe, vacc_series) %>% 
  summarise(anzahl=sum(anzahl_alleorte),
            .groups = "drop")

library(openxlsx)
write.xlsx(booster_nachalter_bydategesamt, "data/impfungenbooster_standsommer2022.xlsx")






# old:

booster_ue60_bydateandkv <- kbv_rki_age %>% 
  filter(vacc_series==3 & Altersgruppe=="60+") %>% 
  group_by(kv, vacc_date) %>% 
  summarise(anzahlbooster=sum(anzahl_alleorte, na.rm = TRUE),
            .groups = "drop") %>% 
  filter(kv!="unbekannt")

booster_ue60_bydategesamt <- booster_ue60_bydateandkv %>% 
  group_by(vacc_date) %>% 
  summarise(anzahlbooster=sum(anzahlbooster),
            kv="Bund",
            .groups = "drop")

booster_ue60_bydate <- bind_rows(booster_ue60_bydateandkv,
                               booster_ue60_bydategesamt)

booster4_ue60_bydate_projection <- booster_ue60_bydate %>% 
  mutate(vacc_date=as_date(ifelse(vacc_date<today()-days(91),
                         today()-days(92),
                         vacc_date))) %>% 
  group_by(vacc_date, kv) %>% 
  summarise(anzahlbooster=sum(anzahlbooster, na.rm=TRUE),
            .groups = "drop") %>% 
  mutate(date4booster2=vacc_date+days(91),
         JahrKW4booster2=100*isoyear(date4booster2)+isoweek(date4booster2),
         anzahlbooster_est_ue70=round(0.555*anzahlbooster)) %>% 
  group_by(JahrKW4booster2, kv) %>% 
  summarise(anzahlbooster=sum(anzahlbooster, an.rm=TRUE),
            anzahlbooster_est_ue70=sum(anzahlbooster_est_ue70, na.rm=TRUE),
            .groups = "drop") %>% 
  mutate(JahrKW4booster2=JahrKW4booster2-202200)

booster_ue60_wodate <- booster4_ue60_bydate_projection %>% 
  group_by(kv) %>% 
  summarise(anzahlbooster=sum(anzahlbooster, na.rm=TRUE),
            anzahlbooster_est_ue70=sum(anzahlbooster_est_ue70, na.rm=TRUE),
            JahrKW4booster2="gesamt",
            .groups = "drop")

excellist <- vector("list", length(unique(booster4_ue60_bydate_projection$kv)))
names(excellist) <- c("Bund", unique(booster_ue60_bydateandkv$kv))

for (kv_idx in seq_along(excellist)) {
  excellist[[kv_idx]] <- bind_rows(
    booster_ue60_wodate %>% 
      filter(kv==names(excellist)[kv_idx]),
    booster4_ue60_bydate_projection %>% 
      filter(kv==names(excellist)[kv_idx]) %>% 
      mutate(JahrKW4booster2=as.character(JahrKW4booster2))
  ) %>% 
    select(`KW (2022)`=JahrKW4booster2,
           `Viertimpfung ü70`=anzahlbooster_est_ue70,
           `Viertimpfung ü60`=anzahlbooster)
}

write.xlsx(excellist,
           "data/viertimpfung_projektion.xlsx",
           overwrite = TRUE)

# View(booster4_ue60_bykw_projection %>% filter(kv=="Gesamt") %>% 
#        select(-weekdate, -JahrKW, -date4booster2))

