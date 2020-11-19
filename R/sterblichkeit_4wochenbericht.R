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
rki <- tbl(conn,"rki") %>% collect()

##### here filter for date of wochenbericht
stichtag <- as_date("2020-08-02")
rki2 <- rki %>% mutate(Meldedatum=as_date(Meldedatum)) %>% filter(Meldedatum<=stichtag)

sterb <- rki2 %>%
  filter(Meldedatum>=stichtag-6) %>%
  group_by(Altersgruppe) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop")

sterb_gesamt <- rki2 %>%
  filter(Meldedatum>=stichtag-6) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop")
