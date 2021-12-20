library(lubridate)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(zoo)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_rki_impfstoff <- tbl(conn, "kbv_rki_impfstoffe_laender") %>% 
  collect()

lieferungen <- read_tsv(
  "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(dosen=case_when(
    impfstoff=="moderna" & date>="2021-10-05" ~ dosen*2, 
    TRUE ~ dosen
  )) %>%
  mutate(
    geo=ifelse(region=="DE-BUND", "Bund", geo),
    geo=ifelse(region=="DE-Betriebe", "Betriebe", geo),
    Hersteller=case_when(
      impfstoff=="comirnaty" ~ "BNT/Pfizer",
      impfstoff=="moderna" ~ "Moderna",
      impfstoff=="astra" ~ "AZ",
      impfstoff=="johnson" ~ "J&J",
      TRUE ~ "error"),
    KW=isoweek(date),
    wochentag=lubridate::wday(date, week_start = 1),
    KW=ifelse(wochentag>=5, KW+1, KW),
    Jahr=year(date),
    Monat=month(date),
    Jahr=case_when(
      KW>=52 & Monat==1 ~ Jahr-1L,
      TRUE ~ Jahr
    ),
    JahrKW=Jahr*100+KW
  )

# bestellungen <- read_csv2("../../../impfdatakbv/otherdata/kbv_bestelldaten.csv")

lieferungen_moderna_praxen <- lieferungen %>% 
  filter(date>="2021-10-05") %>%
  filter(impfstoff=="moderna" & einrichtung=="arztpraxen") %>%
  summarise(modernageliefert=sum(dosen, na.rm = TRUE)) %>% 
  pull(modernageliefert)

impfungen_moderna_praxen <- kbv_rki_impfstoff %>% 
  filter(Impfstoff=="Moderna") %>%
  filter(vacc_date>="2021-10-05") %>%
  mutate(anzahl_praxen=ifelse(vacc_series==3, anzahl_praxen, 2*anzahl_praxen)) %>% 
  summarise(modernageimpft=sum(anzahl_praxen, na.rm = TRUE)) %>%
  pull(modernageimpft)

# bestellungen_moderna_praxen <- bestellungen %>% 
#   filter(Lieferwoche>=40 & Lieferwoche<=50) %>% 
#   summarise(Moderna_Bestellung=sum(Moderna_Bestellung, na.rm=TRUE)) %>% 
#   pull(Moderna_Bestellung)

impfungen_moderna_praxen/lieferungen_moderna_praxen
