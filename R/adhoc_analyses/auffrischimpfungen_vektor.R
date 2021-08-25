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
library(zoo)
require(ISOcodes)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
rki <- tbl(conn,"rki") %>% 
  collect()
rki_original <- rki
rki <- rki %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)), 
                                  maxdate=max(date(Meldedatum)))
rkiidkreise <- unique(rki$IdLandkreis)
rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")


## read/update rki impf data from github
rki_vacc <- tryCatch(
  {
    mytemp <- tempfile()
    rki_vacc_data <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v3/all.csv"
    download.file(rki_vacc_data, mytemp, method = "curl")
    vacc_zahlen <- read_csv(mytemp)
    if (dim(vacc_zahlen)[2] != 5){
      stop("they changed the vacc table")
    } else {
      write_csv(vacc_zahlen, "./data/vacc_zahlen_ard.csv")
      vacc_zahlen
    }
  },
  error=function(e) {
    # read old data
    vacc_zahlen <- read_csv("./data/vacc_zahlen_ard.csv")
    return(vacc_zahlen)
  }
)
rki_vacc <- rki_vacc %>% 
  mutate(region=ifelse(region=="DE", "DE", paste0("DE-", region))) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(geo=ifelse(region=="DE", "Germany", geo),
         geotype=ifelse(region=="DE", "nation", "state"))

####
rki_neuinfekte <- rki %>% 
  mutate(Meldedatum=as_date(Meldedatum),
         Meldedatum=ifelse(Meldedatum<="2021-03-01",
                           as_date("2021-03-01"),
                           Meldedatum)) %>% 
  mutate(Meldedatum=as_date(Meldedatum)) %>% 
  filter(Meldedatum<="2021-08-01") %>% 
  mutate(date_plus_6m=as_date(Meldedatum)+days(183),
         jahrmonat=year(date_plus_6m)*100+month(date_plus_6m)) %>% 
  group_by(Bundesland, jahrmonat) %>% 
  summarise(AnzahlFall=sum(AnzahlFall))


auffrischen <- rki_vacc %>% 
  filter(metric %in% c("personen_voll_astrazeneca_kumulativ",
                       "personen_voll_janssen_kumulativ") &
           date>="2021-03-01" &
           date<="2021-08-01") %>% 
  pivot_wider(id_cols=c("date", "region"),
              names_from="metric",
              values_from="value") %>% 
  group_by(region) %>%
  arrange(date) %>% 
  mutate(new_jj=c(personen_voll_janssen_kumulativ[1], 
                  diff(personen_voll_janssen_kumulativ)),
         new_az_voll=c(personen_voll_astrazeneca_kumulativ[1], 
                  diff(personen_voll_astrazeneca_kumulativ)),
         date_plus_6m=date + days(183), # +months(6) ... 2021-03-31 and 2021-05-31 machen probleme?
         jahrmonat=year(date_plus_6m)*100+month(date_plus_6m)) %>% 
  group_by(region, jahrmonat) %>% 
  summarise(jj_auffrischen=sum(new_jj),
            az_auffrischen=sum(new_az_voll)) %>% 
  mutate(region=case_when(
    region=="DE" ~ "Gesamt",
    region=="BE" ~ "Berlin",
    region=="BB" ~ "Brandenburg",
    region=="BY" ~ "Bayern",
    region=="BW" ~ "Baden-Württemberg",
    region=="HB" ~ "Bremen",
    region=="HH" ~ "Hamburg",
    region=="NI" ~ "Niedersachsen",
    region=="HE" ~ "Hessen",
    region=="MV" ~ "Mecklenburg-Vorpommern",
    region=="SH" ~ "Schleswig-Holstein",
    region=="SN" ~ "Sachsen",
    region=="ST" ~ "Sachsen-Anhalt",
    region=="RP" ~ "Rheinland-Pfalz",
    region=="SL" ~ "Saarland",
    region=="NW" ~ "Nordrhein-Westfalen",
    region=="TH" ~ "Thüringen",
    TRUE ~ region
  ))

neuinfektionen_plus_vektor <- rki_neuinfekte %>% 
  left_join(auffrischen,
            by=c("jahrmonat", "Bundesland"="region"))

write_csv(neuinfektionen_plus_vektor, "data/auffrischen.csv")
