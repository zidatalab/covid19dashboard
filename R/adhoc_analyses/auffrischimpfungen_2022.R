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

alter_pop <- c(unter5=3961376,
               "5bis12"=5211009,
               "12bis18"=4505517,
               ueber18=69488809)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

rki_quoten <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Impfquoten_COVID-19.csv")

rki_vacc <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv") %>% 
  mutate(Impfdatum=as_date(Impfdatum)) %>% 
  select(vacc_date=Impfdatum, vacc_series=Impfserie,
         anzahl_alleorte=Anzahl) %>% 
  mutate(vacc_date=as_date(ifelse(vacc_date<as_date("2021-01-08"),
                                  as_date("2021-01-08"),
                                  vacc_date))) %>% 
  group_by(vacc_date, vacc_series) %>% 
  summarise(anzahl_alleorte=sum(anzahl_alleorte))

# ausblick_monate <- tibble(Jahr=c(rep(2021, 12), rep(2022, 3)), Monat=c(1:12,1,2,3))

vacc_monat <- rki_vacc %>% 
  mutate(Jahr=year(vacc_date),
         Monat=month(vacc_date)) %>% 
  group_by(Jahr, Monat, vacc_series) %>% 
  summarise(anzahl=sum(anzahl_alleorte)) %>% 
  # right_join(ausblick_monate) %>% 
  pivot_wider(id_cols=c(Jahr, Monat), 
              names_from = vacc_series, names_prefix = "Impfung_",
              values_from=anzahl) %>% 
  # select(-Impfung_NA) %>% 
  mutate()

abstand_booster <- 6

bedarf_ohneimpfquote <- tibble(
  Jahr=2022,
  Monat=1:3,
  Impfung_2=0,
  Impfung_3=0
)

bedarf_ohneimpfquote[1, "Impfung_2"] <- sum(vacc_monat$Impfung_1)-sum(vacc_monat$Impfung_2) # im dezember 21 noch erfolgende erst/zweitimpfungen gleichen sich etwa aus?
bedarf_ohneimpfquote[1, "Impfung_3"] <- sum(vacc_monat$Impfung_2[1:(7+6-abstand_booster)])-sum(vacc_monat$Impfung_3)
bedarf_ohneimpfquote[2, "Impfung_3"] <- vacc_monat$Impfung_2[7+6-abstand_booster+1]
bedarf_ohneimpfquote[3, "Impfung_3"] <- vacc_monat$Impfung_2[7+6-abstand_booster+2]

sum(bedarf_ohneimpfquote)

bedarf_mitimpfquote <- bedarf_ohneimpfquote %>% 
  mutate(Impfung_1=0)

final_quoten <- rki_quoten %>% 
  filter(Datum==max(Datum) & Bundesland=="Deutschland") %>% 
  select(contains("12"), contains("18plus"))

bedarf_mitimpfquote[1:3, "Impfung_1"] <- bedarf_mitimpfquote[1:3, "Impfung_1"] + 
  ((0.95-final_quoten$Impfquote_18plus_min1/100)*alter_pop[4] +
  (0.75-final_quoten$Impfquote_12bis17_min1/100)*alter_pop[3] +
  (0.5-0/100)*alter_pop[2])/3

bedarf_mitimpfquote[1, "Impfung_2"] <- sum(vacc_monat$Impfung_1)-sum(vacc_monat$Impfung_2) # im dezember 21 noch erfolgende erst/zweitimpfungen gleichen sich etwa aus?
bedarf_mitimpfquote[2:3, "Impfung_2"] <- bedarf_mitimpfquote[1:2, "Impfung_1"]

bedarf_mitimpfquote[1, "Impfung_3"] <- sum(vacc_monat$Impfung_2[1:(7+6-abstand_booster)])-sum(vacc_monat$Impfung_3)
bedarf_mitimpfquote[2, "Impfung_3"] <- vacc_monat$Impfung_2[7+6-abstand_booster+1]
bedarf_mitimpfquote[3, "Impfung_3"] <- vacc_monat$Impfung_2[7+6-abstand_booster+2]

sum(bedarf_mitimpfquote)

### lager
impfdashboardde <- read_tsv(
  "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(dosen=case_when(
    impfstoff=="moderna" & date>="2021-10-26" ~ dosen*2, # booster moderna sind doppelt ab KW43 laut bmg
    TRUE ~ dosen
  ))

dosen_gesamt_lager_bund <- (12.4+50.3+41.5+4.4+5.9+2.9+
                                   1.8+6.7+
                                   2*(5.7+4.1+6+7.2+4.9)+
                                   5.7+13.9+13.4+
                                   4+2.8)*1e6 +
  - (8.1 + 1.2 + 0.8)*1e6 +
  - (10.6 + 10 + 10)*1e6 +
  - sum(impfdashboardde$dosen)

dosen_gesamt_lager_le <- sum(impfdashboardde$dosen) - sum(rki_vacc$Anzahl)
