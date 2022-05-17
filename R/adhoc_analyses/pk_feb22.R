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

impfungen_tc <- read.xlsx("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_dritt_bl.xlsx")%>% 
  filter(Bundesland=="Gesamt") %>% 
  # select(Datum, `Erst-Gesamt`) %>% 
  mutate(Datum=as_date(Datum, origin = as_date("1899-12-30")),
         `Viert-Gesamt`=`Viert-Praxen`,
         "Gesamt-Praxen"=
           `Erst-Praxen`+`Zweit-Praxen`+`Dritt-Praxen`+`Viert-Praxen`,
         "Gesamt-Gesamt"=
           `Erst-Gesamt`+`Zweit-Gesamt`+`Dritt-Gesamt`+`Viert-Gesamt`)

kbv_rki_altersgruppen <- tbl(conn,"kbv_rki_altersgruppen_kreise") %>% 
  collect()

impfungen_tc <- kbv_rki_altersgruppen %>% 
  mutate(vacc_series=case_when(
    vacc_series==1 ~ "Erst",
    vacc_series==2 ~ "Zweit",
    vacc_series==3 ~ "Dritt",
    vacc_series==4 ~ "Viert",
    TRUE ~ "error"
  )) %>% 
  group_by(vacc_date, vacc_series) %>% 
  summarise(Praxen=sum(anzahl_praxen, na.rm=TRUE),
            Gesamt=sum(anzahl_alleorte, na.rm=TRUE),
            .groups="drop") %>% 
  pivot_wider(id_cols = "vacc_date",
              names_from = "vacc_series",
              values_from = c("Praxen", "Gesamt"),
              names_sep="-")

table_pk <- bind_rows(
  impfungen_tc %>% 
    summarise(across(`Praxen-Erst`:`Gesamt-Viert`, sum, na.rm=TRUE)) %>% 
    pivot_longer(cols = everything(),
                 names_to = c(".value", "Impfserie"),
                 names_pattern="(.*)-(.*)"),
  impfungen_tc %>% 
    summarise(across(`Praxen-Erst`:`Gesamt-Viert`, sum, na.rm=TRUE)) %>% 
    pivot_longer(cols = everything(),
                 names_to = c(".value", "Impfserie"),
                 names_pattern="(.*)-(.*)") %>% 
    summarise(Impfserie="Gesamt",
              Praxen=sum(Praxen),
              Gesamt=sum(Gesamt))) %>% 
  mutate(`Anteil Praxen`=paste0(round(100*`Praxen`/`Gesamt`, 1), "%")) %>% 
  mutate(Praxen=format(Praxen, big.mark="."),
         Gesamt=format(Gesamt, big.mark="."))

kbv_impfstoff <- tbl(conn,"kbv_impfstoff_kreise") %>% 
  collect()

kinderdosen <- kbv_impfstoff %>% 
  filter(vacc_product=="BNT162b2-Kinder") %>% 
  summarise(kinderdosen=sum(anzahl_praxen))
