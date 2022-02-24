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

table_pk <- impfungen_tc %>% 
  summarise(across(`Erst-Praxen`:`Gesamt-Gesamt`, sum)) %>% 
  pivot_longer(cols = everything(),
               names_to = c("Impfserie", ".value"),
               names_pattern="(.*)-(.*)") %>% 
  mutate(`Anteil Praxen`=paste0(round(100*`Praxen`/`Gesamt`), "%")) %>% 
  mutate(Praxen=format(Praxen, big.mark="."),
         Gesamt=format(Gesamt, big.mark=".")) %>% 
  mutate(Gesamt=ifelse(Impfserie=="Viert", "unbekannt", Gesamt)) %>% 
  mutate(`Anteil Praxen`=ifelse(Impfserie=="Viert", "unbekannt", `Anteil Praxen`))

anteil_viert <- impfungen_tc %>% 
  filter(Datum>="2022-02-10") %>% 
  summarise(across(contains("Praxen"), sum))

for (i in 1:4) {
  cat(anteil_viert[1, i]/anteil_viert[1, 5], "\n")
}

kbv_impfstoff <- tbl(conn,"kbv_impfstoff_kreise") %>% 
  collect()

kinderdosen <- kbv_impfstoff %>% 
  filter(vacc_product=="BNT162b2-Kinder") %>% 
  summarise(kinderdosen=sum(anzahl_praxen))
