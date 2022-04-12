library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)
library(zoo)

rki_mappings <- read_csv("data/rki_mappings_landkreise.csv") %>% 
  mutate(IdLandkreis=as.integer(IdLandkreis)) %>% 
  bind_rows(tibble(IdLandkreis=11000, IdBundesland=11,
                   Bundesland="Berlin", Landkreis="Berlin"))

kreise_plz <- read_csv2("data/plz_kreis_bl.csv") %>% 
#  mutate(PLZ=as.integer(PLZ)) %>% 
  select(PLZ, Kreis2016, Kreis2016name, Bundesland=BLname, BundeslandId=BL) %>% 
  bind_rows(tibble(PLZ="0", Kreis2016=0, Kreis2016name="unbekannt",
                   Bundesland="unbekannt", BundeslandId=99))

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_impfstoff <- tbl(conn,"kbvcovidvacczi") %>% 
  collect()
kbv_age <- tbl(conn,"kbvcovidagegroup") %>%
  collect()

# check impfstofftabelle SH 4.
kbv_impfstoff_sh4 <- kbv_impfstoff %>% 
  filter(vacc_series==4) %>% 
  left_join(kreise_plz, by=c("arzt_plz"="PLZ")) %>% 
  # left_join(rki_mappings, by=c("Kreis2016"="IdLandkreis")) %>% 
  group_by(Bundesland) %>% 
  summarise(anzahlpraxen4is=sum(anzahl))

kbv_alter_sh4 <- kbv_age %>% 
  filter(vacc_series==4) %>% 
  left_join(kreise_plz, by=c("arzt_plz"="PLZ")) %>% 
  # left_join(rki_mappings, by=c("Kreis2016"="IdLandkreis")) %>% 
  group_by(Bundesland) %>% 
  summarise(anzahlpraxen4age=sum(anzahl))

kbv_ageimpfstoff <- kbv_impfstoff %>% 
  left_join(kreise_plz, by=c("arzt_plz"="PLZ")) %>% 
  # left_join(rki_mappings, by=c("Kreis2016"="IdLandkreis")) %>% 
  group_by(Bundesland, vacc_series) %>% 
  summarise(anzahlpraxen_is=sum(anzahl)) %>% 
  left_join(kbv_age %>% 
              left_join(kreise_plz, by=c("arzt_plz"="PLZ")) %>% 
              # left_join(rki_mappings, by=c("Kreis2016"="IdLandkreis")) %>% 
              group_by(Bundesland, vacc_series) %>% 
              summarise(anzahlpraxen_age=sum(anzahl)),
            by=c("Bundesland", "vacc_series")) %>% 
  mutate(differenz=anzahlpraxen_age-anzahlpraxen_is,
         betrag_differenz=abs(differenz),
         relativ=round(differenz/anzahlpraxen_is*100, 1))

kbv_rki_impfstoff <- tbl(conn,"kbv_rki_impfstoffe_laender") %>% 
  collect()
kbv_rki_age <- tbl(conn,"kbv_rki_altersgruppen_kreise") %>%
  collect()

series_bl <- kbv_rki_impfstoff %>%
  # filter(vacc_date<="2022-03-15") %>% 
  group_by(vacc_series, Bundesland) %>% 
  summarise(praxen_is=sum(anzahl_praxen, na.rm=TRUE)) %>% 
  full_join(kbv_rki_age %>% 
              # filter(vacc_date<="2022-03-15") %>% 
              group_by(Bundesland, vacc_series) %>% 
              summarise(praxen_ag=sum(anzahl_praxen, na.rm=TRUE))) %>% 
  replace_na(list(Bundesland = "unbekannt",
                  praxen_is = 0,
                  praxen_ag = 0)) %>% 
  group_by(vacc_series, Bundesland) %>% 
  summarise(praxen_is=sum(praxen_is),
            praxen_ag=sum(praxen_ag)) %>% 
  mutate(differenz=praxen_ag-praxen_is,
         betrag_differenz=abs(differenz),
         relativ=round(differenz/praxen_is*100, 1)) %>% 
  # filter(Bundesland!="unbekannt") %>% 
  filter(Bundesland!="Bundesressorts")

series_bl %>% group_by(vacc_series) %>% 
  summarise(praxen_is=sum(praxen_is),
            praxen_ag=sum(praxen_ag))

series <- kbv_rki_impfstoff %>%
  group_by(vacc_series) %>% 
  summarise(praxen_is=sum(anzahl_praxen, na.rm=TRUE)) %>% 
  left_join(kbv_rki_age %>% 
              group_by(vacc_series) %>% 
              summarise(praxen_ag=sum(anzahl_praxen, na.rm=TRUE))) %>% 
  mutate(differenz=praxen_ag-praxen_is)

bl <- kbv_rki_impfstoff %>% 
  group_by(Bundesland) %>% 
  summarise(praxen_is=sum(anzahl_praxen, na.rm=TRUE)) %>% 
  left_join(kbv_rki_age %>% 
              group_by(Bundesland) %>% 
              summarise(praxen_ag=sum(anzahl_praxen, na.rm=TRUE))) %>% 
  mutate(differenz=praxen_ag-praxen_is,
         relativ=round(differenz/praxen_is*100, 1))

plzs <- kbv_impfstoff %>% 
  group_by(arzt_plz) %>% 
  summarise(praxen_is=sum(anzahl)) %>% 
  left_join(kbv_age %>% 
              group_by(arzt_plz) %>% 
              summarise(praxen_ag=sum(anzahl))) %>% 
  mutate(differenz=praxen_ag-praxen_is,
         relativ=round(differenz/praxen_is*100, 1))

plzs_datum <- kbv_impfstoff %>% 
  group_by(arzt_plz, vacc_date) %>% 
  summarise(praxen_is=sum(anzahl)) %>% 
  full_join(kbv_age %>% 
              group_by(arzt_plz, vacc_date) %>% 
              summarise(praxen_ag=sum(anzahl))) %>% 
  mutate(praxen_is=ifelse(is.na(praxen_is), 0, praxen_is),
         praxen_ag=ifelse(is.na(praxen_ag), 0, praxen_ag)) %>% 
  mutate(differenz=praxen_ag-praxen_is,
         relativ=round(differenz/praxen_is*100, 1),
         isdiff=differenz!=0) 

plzs_datum_any <- plzs_datum %>% 
  group_by(arzt_plz) %>% 
  summarise(isanydiff=any(isdiff))

anteil_probleme <- sum(plzs$differenz!=0)/dim(plzs)[1]
anteil_probleme <- sum(plzs_datum_any$isanydiff)/dim(plzs_datum_any)[1]

ggplot(plzs_datum %>% 
         filter(isdiff) %>% 
         group_by(vacc_date) %>% 
         summarise(anzahl_plz_diff=n()),
       aes(x=vacc_date, y=anzahl_plz_diff)) +
  geom_line()

problem_plzs <- plzs$arzt_plz[plzs$differenz!=0]

# randomplz <- sample(unique(kbv_age$arzt_plz), 1)
# thisplz <- kbv_impfstoff %>% 
#   filter(arzt_plz==randomplz) %>% 
#   group_by(vacc_date) %>% 
#   summarise(praxen_is=sum(anzahl)) %>% 
#   left_join(
#     kbv_age %>% 
#       filter(arzt_plz==randomplz) %>% 
#       group_by(vacc_date) %>% 
#       summarise(praxen_ag=sum(anzahl)),
#     by="vacc_date"
#   ) %>% 
#   mutate(differenz=praxen_ag-praxen_is,
#          relativ=round(differenz/praxen_is*100, 1))

# rki landkreise

rki_vacc_kreise <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv") %>% 
  mutate(LandkreisId=ifelse(LandkreisId_Impfort=="u" | 
                              LandkreisId_Impfort=="17000", 
                            "0",
                            LandkreisId_Impfort),
         LandkreisId=as.integer(LandkreisId),
         Impfdatum=as_date(Impfdatum)) %>% 
  select(vacc_date=Impfdatum, LandkreisId, vacc_series=Impfschutz,
         Altersgruppe,
         anzahl_alleorte=Anzahl) %>% 
  group_by(vacc_date, LandkreisId, vacc_series, Altersgruppe) %>% 
  summarise(anzahl_alleorte=sum(anzahl_alleorte))

rki_vacc_laender <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv") %>% 
  mutate(BundeslandId_Impfort=as.integer(BundeslandId_Impfort),
         Bundesland=case_when(
           BundeslandId_Impfort == 1 ~ "Schleswig-Holstein",
           BundeslandId_Impfort == 2 ~ "Hamburg",
           BundeslandId_Impfort == 3 ~ "Niedersachsen",
           BundeslandId_Impfort == 4 ~ "Bremen",
           BundeslandId_Impfort == 6 ~ "Hessen",
           BundeslandId_Impfort == 7 ~ "Rheinland-Pfalz",
           BundeslandId_Impfort == 8 ~ "Baden-Württemberg",
           BundeslandId_Impfort == 9 ~ "Bayern",
           BundeslandId_Impfort == 10 ~ "Saarland",
           BundeslandId_Impfort == 11 ~ "Berlin",
           BundeslandId_Impfort == 12 ~ "Brandenburg",
           BundeslandId_Impfort == 13 ~ "Mecklenburg-Vorpommern",
           BundeslandId_Impfort == 14 ~ "Sachsen",
           BundeslandId_Impfort == 15 ~ "Sachsen-Anhalt",
           BundeslandId_Impfort == 16 ~ "Thüringen",
           BundeslandId_Impfort == 5 ~ "Nordrhein-Westfalen",
           BundeslandId_Impfort == 17 ~ "Bundesressorts",
           TRUE ~ "ERROR"
         ),
         Impfdatum=as_date(Impfdatum)) %>% 
  select(vacc_date=Impfdatum, BundeslandId_Impfort,
         Bundesland,
         vacc_series=Impfserie,
         Impfstoff,
         anzahl_alleorte=Anzahl)

rki_is_ag <- rki_vacc_laender %>% 
  mutate(vacc_series=ifelse(vacc_series==1&Impfstoff=="Janssen",
                            2,
                            vacc_series)) %>%
  group_by(Bundesland, vacc_series) %>% 
  summarise(rki_gesamt_is=sum(anzahl_alleorte, na.rm=TRUE)) %>% 
  left_join(rki_vacc_kreise %>% 
              # left_join(rki_mappings, by=c("LandkreisId"="IdLandkreis")) %>% 
              left_join(kreise_plz %>% 
                          select(Kreis2016, Bundesland) %>% 
                          distinct(),
                        by=c("LandkreisId"="Kreis2016")) %>% 
              group_by(Bundesland, vacc_series) %>% 
              summarise(rki_gesamt_ag=sum(anzahl_alleorte, na.rm=TRUE)),
            by=c("Bundesland", "vacc_series")) %>% 
  mutate(differenz=rki_gesamt_ag-rki_gesamt_is,
         relativ=round(differenz/rki_gesamt_is*100, 1))

# lek 7tage
kbv_rki_impfstoff_7t <- kbv_rki_impfstoff %>% 
  group_by(vacc_date) %>% 
  summarise(praxen=sum(anzahl_praxen, na.rm = TRUE),
            alle=sum(anzahl_alleorte, na.rm = TRUE),
            .groups="drop") %>% 
  arrange(rev(vacc_date)) %>% 
  mutate(alle_7t = rollmean(alle, 7, fill=0, align="left"))

kbv_impfstoff_7t <- kbv_impfstoff %>% 
  group_by(vacc_date) %>% 
  summarise(praxen=sum(anzahl, na.rm = TRUE),
            .groups="drop") %>% 
  arrange(rev(vacc_date)) %>% 
  mutate(praxen_7t = rollmean(praxen, 7, fill=0, align="left"))
