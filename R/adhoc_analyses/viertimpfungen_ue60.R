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


kbv_rki_age <- tbl(conn, "kbv_rki_altersgruppen_kreise") %>% 
  collect()

booster_ue60_bykwandkv <- kbv_rki_age %>% 
  filter(vacc_series==3 & Altersgruppe=="60+") %>% 
  group_by(kv, JahrKW) %>% 
  summarise(anzahlbooster=sum(anzahl_alleorte, na.rm = TRUE),
            weekdate=max(vacc_date),
            .groups = "drop")

booster_ue60_bykwgesamt <- booster_ue60_bykwandkv %>% 
  group_by(JahrKW) %>% 
  summarise(anzahlbooster=sum(anzahlbooster),
            weekdate=max(weekdate),
            kv="Gesamt",
            .groups = "drop")

booster_ue60_bykw <- bind_rows(booster_ue60_bykwandkv,
                               booster_ue60_bykwgesamt)

booster4_ue60_bykw_projection <- booster_ue60_bykw %>% 
  mutate(weekdate=as_date(ifelse(weekdate<today()-months(3),
                         today()-months(3)-days(1),
                         weekdate))) %>% 
  group_by(weekdate, kv) %>% 
  summarise(anzahlbooster=sum(anzahlbooster, na.rm=TRUE),
            JahrKW=max(JahrKW),
            .groups = "drop") %>% 
  mutate(date4booster2=weekdate+months(3),
         JahrKW4booster2=100*isoyear(date4booster2)+isoweek(date4booster2),
         anzahlbooster_est_ue70=round(0.555*anzahlbooster))

write.xlsx(booster4_ue60_bykw_projection %>% 
             select(KV=kv,
                    JahrKW=JahrKW4booster2,
                    `Viertimpfung ü70`=anzahlbooster_est_ue70,
                    `Viertimpfung ü60`=anzahlbooster),
           "data/viertimpfung_projektion.xlsx",
           overwrite = TRUE)

# View(booster4_ue60_bykw_projection %>% filter(kv=="Gesamt") %>% 
#        select(-weekdate, -JahrKW, -date4booster2))

