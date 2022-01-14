library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_rki_impfstoff <- tbl(conn,"kbv_rki_impfstoffe_laender") %>% 
  collect()

lieferungen_bmg <- read_tsv(
  "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region")# %>% 
  # mutate(dosen=case_when(
  #   impfstoff=="moderna" & date>="2021-10-26" ~ dosen*2, # booster moderna sind doppelt ab KW43 laut bmg
  #   TRUE ~ dosen
  # ))

moderna_table <- kbv_rki_impfstoff %>% 
  filter(Impfstoff=="Moderna") %>% 
  group_by(vacc_date, vacc_series) %>% 
  summarise(verimpft_praxen=sum(anzahl_praxen, na.rm = TRUE),
            verimpft_andere=sum(anzahl_alleorte, na.rm = TRUE) - verimpft_praxen,
            .groups="drop") %>% 
  pivot_wider(id_cols = vacc_date,
              names_from = vacc_series,
              values_from = c(verimpft_praxen, verimpft_andere)) %>% 
  mutate(verimpft_praxen_12=verimpft_praxen_1 + verimpft_praxen_2,
         verimpft_andere_12=verimpft_andere_1 + verimpft_andere_2) %>% 
  select(date=vacc_date,
         verimpft_praxen_12, verimpft_praxen_3,
         verimpft_andere_12, verimpft_andere_3) %>% 
  mutate(across(-"date", ~ ifelse(is.na(.) | .<0, 0, .))) %>% 
  left_join(lieferungen_bmg %>% 
              filter(impfstoff=="moderna") %>% 
              mutate(einrichtung=case_when(
                einrichtung=="arztpraxen" ~ "praxen",
                TRUE ~ "andere"
              )) %>% 
              group_by(date, einrichtung) %>% 
              summarise(geliefert=sum(dosen, na.rm=TRUE),
                        .groups="drop") %>% 
              pivot_wider(id_cols=date,
                          names_from=einrichtung,
                          values_from=geliefert,
                          names_prefix="geliefert_"),
            by="date") %>% 
  mutate(across(contains("geliefert"), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(across(-"date", cumsum)) %>% 
  mutate(lager_praxen=geliefert_praxen-verimpft_praxen_12-round(0.5*verimpft_praxen_3),
         lager_andere=geliefert_andere-verimpft_andere_12-round(0.5*verimpft_andere_3))

bnt_table <- kbv_rki_impfstoff %>% 
  filter(Impfstoff=="Comirnaty") %>% 
  group_by(vacc_date, vacc_series) %>% 
  summarise(verimpft_praxen=sum(anzahl_praxen, na.rm = TRUE),
            verimpft_andere=sum(anzahl_alleorte, na.rm = TRUE) - verimpft_praxen,
            .groups="drop") %>% 
  pivot_wider(id_cols = vacc_date,
              names_from = vacc_series,
              values_from = c(verimpft_praxen, verimpft_andere)) %>% 
  mutate(verimpft_praxen_12=verimpft_praxen_1 + verimpft_praxen_2,
         verimpft_andere_12=verimpft_andere_1 + verimpft_andere_2) %>% 
  select(date=vacc_date,
         verimpft_praxen_12, verimpft_praxen_3,
         verimpft_andere_12, verimpft_andere_3) %>% 
  mutate(across(-"date", ~ ifelse(is.na(.) | .<0, 0, .))) %>% 
  left_join(lieferungen_bmg %>% 
              filter(impfstoff=="comirnaty") %>% 
              mutate(einrichtung=case_when(
                einrichtung=="arztpraxen" ~ "praxen",
                TRUE ~ "andere"
              )) %>% 
              group_by(date, einrichtung) %>% 
              summarise(geliefert=sum(dosen, na.rm=TRUE),
                        .groups="drop") %>% 
              pivot_wider(id_cols=date,
                          names_from=einrichtung,
                          values_from=geliefert,
                          names_prefix="geliefert_"),
            by="date") %>% 
  mutate(across(contains("geliefert"), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(across(-"date", cumsum)) %>% 
  mutate(lager_praxen=geliefert_praxen-verimpft_praxen_12-verimpft_praxen_3,
         lager_andere=geliefert_andere-verimpft_andere_12-verimpft_andere_3)
