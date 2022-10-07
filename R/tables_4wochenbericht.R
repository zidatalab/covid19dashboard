# manual updates:
# almev.csv tuesdays from https://www.alm-ev.de/aktuell/corona-themenseite/datenerhebung-alm-ev/
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
library(readr)
library(openxlsx)
library(zoo)
library(curl)
library(ISOcodes)
#library(dotenv)
#load_dot_env()

source("aux_functions.R")

# daten impfdax
impfdax_data <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/impfdax.csv")
impfdax_data <- impfdax_data %>% 
  mutate(JahrKW=Jahr*100+kw)
# impfen_praxen_letztekw <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/impfdax_praxen_wirkstoffe_allerletzte_kw.csv")
gelieferte_dosen <- read_json("../data/tabledata/impfsim_start.json",
                              simplifyVector = TRUE) %>% 
  filter(geo=="Gesamt")
impfen_praxen_bl <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_bl_date_wirkstoff.csv") %>% 
  select(-1) %>%
  mutate(KW=isoweek(date),
         Jahr=year(date),
         Monat=month(date),
         Jahr=case_when(
           KW>=52 & Monat==1 ~ Jahr-1L,
           TRUE ~ Jahr
         ),
         JahrKW=100*Jahr+KW) %>% 
  group_by(Jahr, KW, Bundesland) %>% 
  summarise(Impfungen=sum(`Ad26.COV2.S`+`AZD1222`+`BNT162b2`+`mRNA-1273`))
impfen_praxen_bl <- bind_rows(impfen_praxen_bl,
                              impfen_praxen_bl %>% 
                                group_by(Jahr, KW) %>% 
                                summarise(Bundesland="Gesamt",
                                          Impfungen=sum(Impfungen)))

# impfdashboard.de/daten für lieferungen -> bund
impfdashboardde <- read_tsv(
  "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(Hersteller=case_when(
    impfstoff %in% c("comirnaty", "comirnaty BA.4/5") &
      impfstofftyp %in% c("bivalent Wildtyp/BA.1",
                          "comirnaty BA.4/5") ~ "BNT/Pfizer-Omikron",
    impfstoff=="comirnaty" & impfstofftyp=="Wildtyp" ~ "BNT/Pfizer",
    impfstoff=="moderna" & impfstofftyp=="Wildtyp" ~ "Moderna",
    impfstoff=="moderna" & impfstofftyp=="bivalent Wildtyp/BA.1" ~ "Moderna-Omikron",
    impfstoff=="astra" ~ "AZ",
    impfstoff=="johnson" ~ "J&J",
    impfstoff=="novavax" ~ "Novavax",
    impfstoff=="valneva" ~ "Valneva",
    TRUE ~ "error")) %>% 
  mutate(dosen=case_when(
    Hersteller=="Moderna" & date>="2021-10-26" & date<="2022-09-12" ~ dosen*2, # booster sind doppelt für moderna ab kw43 laut bmg
    TRUE ~ dosen
  ))

# daten übersterblichkeit
## download mit curl funktioniert evtl nicht (11. maerz 2022)
## dann muss von hand downgeloadet und dort gespeichert werden:
# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.html
url_sterblk <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"
destfile_sterblk <- "../data/sonderauswertung-sterbefaelle.xlsx"
curl::curl_download(url_sterblk, destfile_sterblk)

# daten rki symptomanteil, hospitalisierungsrate und sterberate
url_rkihosp <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Klinische_Aspekte.xlsx?__blob=publicationFile"
destfile_rkihosp <- "../data/klinische_aspekte.xlsx"
curl::curl_download(url_rkihosp, destfile_rkihosp)
curl::curl_download(url_rkihosp,
                    paste0("../data/klinische_aspekte_old/klinische_aspekte_",
                           str_remove_all(today(), "-"),
                           ".xlsx"))

# daten rki hospitalisierungsinzidenzen
rki_hospinz <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")

## Destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html
altersgruppen_bund <- tibble("unter 20"=18.4,
                             "20 bis 40"=24.6,
                             "40 bis 60"=28.4,
                             "60 bis 80"=21.7,
                             "80+"=6.8)/100

# rki rwert
rki_r_data <- "https://raw.githubusercontent.com/robert-koch-institut/SARS-CoV-2-Nowcasting_und_-R-Schaetzung/main/Nowcast_R_aktuell.csv"
Nowcasting_Zahlen <- read_csv(rki_r_data)

## regstat alter
kreise_regstat_alter <- read_delim("../data/Bev2019_Kreis_AG_rki_geschlecht.txt", 
                                   ";",
                                   escape_double = FALSE,
                                   col_types = cols(`...1` = col_skip()), 
                                   trim_ws = TRUE) %>%
  mutate(id=ifelse(Kreis==11000, 11, Kreis*1000)) %>%
  select(-Kreis, -m, -w) %>%
  pivot_wider(id_cols="id",
              names_from="AG_rki",
              names_prefix="ag_",
              values_from="ges")
kreise_regstat_alter <- bind_rows(tibble(id=0,
                                         ag_1=sum(kreise_regstat_alter$ag_1),
                                         ag_2=sum(kreise_regstat_alter$ag_2),
                                         ag_3=sum(kreise_regstat_alter$ag_3),
                                         ag_4=sum(kreise_regstat_alter$ag_4),
                                         ag_5=sum(kreise_regstat_alter$ag_5),
                                         ag_6=sum(kreise_regstat_alter$ag_6)),
                                  kreise_regstat_alter %>%
                                    mutate(blid=ifelse(id==11, 11, floor(id/1000000))) %>%
                                    group_by(blid) %>%
                                    summarise(ag_1=sum(ag_1, na.rm = TRUE),
                                              ag_2=sum(ag_2, na.rm = TRUE),
                                              ag_3=sum(ag_3, na.rm = TRUE),
                                              ag_4=sum(ag_4, na.rm = TRUE),
                                              ag_5=sum(ag_5, na.rm = TRUE),
                                              ag_6=sum(ag_6, na.rm = TRUE),
                                              .groups="drop") %>%
                                    mutate(id=blid) %>%
                                    filter(id!=11) %>%
                                    select(-blid),
                                  kreise_regstat_alter)
bund_regstat_alter <- kreise_regstat_alter %>% filter(id==0)

# impflieferungen


# mapping eurpean countries german
eumapping <- tibble(english=c(
  "France",
  "Spain",
  "United Kingdom",
  "Italy",
  "Germany",
  "Poland",
  "Belgium",
  "Czechia", 
  "Netherlands",
  "Romania",
  "Portugal",
  "Austria",
  "Sweden",
  "Hungary",
  "Bulgaria",
  "Croatia",
  "Slovakia",
  "Greece",
  "Denmark",
  "Ireland",
  "Slovenia",
  "Lithuania",
  "Norway",
  "Luxembourg",
  "Finland",
  "Latvia",
  "Estonia",
  "Cyprus",
  "Malta",
  "Iceland",
  "Liechtenstein"
), german=c(
  "Frankreich",
  "Spanien",
  "Vereinigtes Königreich",
  "Italien",
  "Deutschland",
  "Polen",
  "Belgien",
  "Tschechien", 
  "Niederlande",
  "Rumänien",
  "Portugal",
  "Österreich",
  "Schweden",
  "Ungarn",
  "Bulgarien",
  "Kroatien",
  "Slowakei",
  "Griechenland",
  "Dänemark",
  "Irland",
  "Slowenien",
  "Litauen",
  "Norwegen",
  "Luxemburg",
  "Finnland",
  "Lettland",
  "Estland",
  "Zypern",
  "Malta",
  "Island",
  "Liechtenstein"
)
)

rki_mappings <- read_csv("../data/rki_mappings_landkreise.csv") %>% 
  mutate(IdLandkreis=as.integer(IdLandkreis))

almev <- read_csv("../data/almev.csv")

# # rki_ifsg <- read_csv("../data/rki_ifsg.csv")
# 
rki_hosp <- read_excel(destfile_rkihosp,
                       # sheet = "Daten",
                       skip = 2)
rki_hosp_age <- read_excel(destfile_rkihosp,
                                  sheet = 3)
skip = which(rki_hosp_age[1] == "Meldejahr" & rki_hosp_age[2] == "Meldewoche") # often changes
rki_hosp_age <- read_excel(destfile_rkihosp,
                           sheet = 3,
                           skip = skip)

rki_hosp <- rki_hosp %>% mutate(YearKW=Meldejahr*100+MW)

rki_hosp_age <- rki_hosp_age %>% mutate(YearKW=as.integer(Meldejahr)*100+as.integer(Meldewoche))

# ard impfdaten
# check
# vacc_zahlen <- read_csv("../data/vacc_zahlen_ard.csv")

# bundeslaender_table_faktenblatt <- read_json("../data/tabledata/bundeslaender_table_faktenblatt.json",
#                                 simplifyVector = TRUE) %>%
#   mutate(Datum=as_date(Datum))
# kreise_table_faktenblatt <- read_json("../data/tabledata/kreise_table_faktenblatt.json",
#                                 simplifyVector = TRUE) %>%
#   mutate(Datum=as_date(Datum))
# vacc_table_faktenblatt <- read_json("../data/tabledata/vacc_table_faktenblatt.json",
#                                              simplifyVector = TRUE)
# vacc_alle_faktenblatt <- read_json("../data/tabledata/vacc_alle_faktenblatt.json",
#                                     simplifyVector = TRUE)

sterbefaelle_kw <- bind_rows(read_excel(destfile_sterblk, 
                                        sheet = "D_2016_2022_KW_AG_Männlich", 
                                        skip = 8,
                                        na="X") %>% mutate(sex="maennlich"),
                             read_excel(destfile_sterblk, 
                                        sheet = "D_2016_2022_KW_AG_Weiblich", 
                                        skip = 8,
                                        na="X") %>% mutate(sex="weiblich")) %>%
  select(-"Nr.") %>% 
  rename("Jahr"="...2", "Alter"= "unter … Jahren" ) %>%
  relocate(Jahr,Alter,sex) %>% 
  pivot_longer(cols=-c("Jahr", "Alter", "sex"), names_to="KW", values_to="Tote")
sterbefaelle_kw.rec <- 
  left_join(sterbefaelle_kw %>% filter(Jahr>=2020 & !is.na(Tote) & KW!="53"),
            sterbefaelle_kw %>% filter(Jahr<2020) %>%
              group_by(Alter,KW,sex) %>% 
              summarise(Tote_2016_2019=mean(Tote,na.rm=T), .groups="drop"),
            by=c("KW","Alter","sex")) %>% 
  bind_rows(left_join(sterbefaelle_kw %>% filter(Jahr>=2020 & !is.na(Tote) & KW=="53"),
                      sterbefaelle_kw %>% filter(Jahr<2020 & KW=="52") %>%
                        group_by(Alter,KW,sex) %>% 
                        summarise(Tote_2016_2019=mean(Tote,na.rm=T), .groups="drop"),
                      by=c("Alter","sex")) %>% select(-KW.y) %>% rename(KW=KW.x)) %>%
  mutate(
    KW=as.numeric(KW),
    YearKW=as.numeric(Jahr)*100+KW,
    Vergleich=(Tote/Tote_2016_2019),
    startage=as.numeric(ifelse(grepl("-", Alter), stringr::str_split_fixed(Alter,"-",2)[,1], NA)),
    stopage=as.numeric(ifelse(grepl("-", Alter), stringr::str_split_fixed(Alter,"-",2)[,2], NA)),
    agegrp = case_when(
      stopage<=60 ~ 1,
      startage>=60 & startage<=75 ~ 2,
      startage>=80 ~ 3,
      Alter=="95 u. mehr" ~ 3,
      Alter=="Insgesamt" ~ 4
    ),
    agegrp=factor(agegrp,
                  ordered = T,
                  levels=c(1,2,3,4),
                  labels=c("0-59","60-79","80+","Gesamt"))
  )  %>%
  group_by(agegrp, YearKW) %>% 
  summarise(
    Tote_diff=round(sum(Tote)-sum(Tote_2016_2019)),
    Vergleich=(sum(Tote)/sum(Tote_2016_2019))-1,
    .groups="drop") # %>% 
  # filter(!is.na(agegrp))

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

divi_all <- tbl(conn,"divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
# takes a while ...
rki <- tbl(conn,"rki") %>% collect()

rki <- read_csv("~/Dokumente/Aktuell_Deutschland_SarsCov2_Infektionen.csv")

# rki <- tbl(conn,"rki_archive") %>% filter(Datenstand=="2021-07-29") %>% collect()
params <- tbl(conn,"params") %>% select(name, id, cases, R0, EW_insgesamt) %>% collect()
brd_timeseries <- tbl(conn,"brd_timeseries") %>% collect()
rki <- rki %>%
  left_join(rki_mappings, by="IdLandkreis") %>% 
  mutate(Meldedatum=as_date(Meldedatum)) %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
international <- tbl(conn,"trends") %>%
  filter(Country %in% c(
    "France",
    "Spain",
    "United Kingdom",
    "Italy",
    "Germany",
    "Poland",
    "Belgium",
    "Czechia", 
    "Netherlands",
    "Romania",
    "Portugal",
    "Austria",
    "Sweden",
    "Hungary",
    "Bulgaria",
    "Croatia",
    "Slovakia",
    "Greece",
    "Denmark",
    "Ireland",
    "Slovenia",
    "Lithuania",
    "Norway",
    "Luxembourg",
    "Finland",
    "Latvia",
    "Estonia",
    "Cyprus",
    "Malta",
    "Iceland",
    "Liechtenstein"
  )) %>%
  collect() %>%
  mutate(date=as_date(date)) %>%
  left_join(., params %>% select(-cases), by=c("Country"="name"))
kbv_rki_impfstoffe <- tbl(conn,"kbv_rki_impfstoffe_laender") %>% 
  collect()
rki_impfstoffe <- tbl(conn,"rki_impfstoffe_laender") %>% 
  collect()
rki_altersgruppen <- tbl(conn,"rki_altersgruppen_kreise") %>% 
  collect()

# tables for excel
maxdate <- floor_date(today(), "week") # immer sonntag
# Regionale Daten
regional_hospinzidenz <- rki_hospinz %>% 
  filter(Datum==maxdate & Altersgruppe=="00+") %>% 
  mutate(Bundesland=ifelse(Bundesland=="Bundesgebiet", "Gesamt", Bundesland)) %>% 
  select(Bundesland, `7T_Hospitalisierung_Inzidenz`)

eumaxdate <- max(international$date)
eumaxdate <- maxdate
eutabelle <- international %>%
  filter(date >= eumaxdate-14 & date <= eumaxdate) %>%
  group_by(Country) %>%
  summarise(`COVID-19-Fälle`=max(cases),
            AnteilBev=100*max(cases)/EW_insgesamt,
            `COVID-19-Fälle Anteil Bev.`=paste0(format(round(AnteilBev, 1), decimal.mark = ","), " %"),
            `Todesfälle`=max(deaths),
            Fallsterb=100*max(deaths)/max(cases),
            `Fallsterblichkeit`=paste0(format(round(Fallsterb, 1), decimal.mark = ","), " %"),
            `Neue Fälle je 100.000 EW in 14 Tagen`=round((max(cases)-min(cases))/EW_insgesamt*100000),
            `Todesfälle je 100.000 EW in 14 Tagen`=round((max(deaths)-min(deaths))/EW_insgesamt*100000, 1),
            .groups="drop") %>%
  distinct() %>%
  left_join(., eumapping, by=c("Country"="english"))
top10eu <- eutabelle %>% arrange(-`COVID-19-Fälle`) %>% filter(row_number()<=10) %>% pull(german)
# Internationaler Vergleich
EUmal4tabelle <- tibble(
  `Fälle gesamt`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-AnteilBev) %>% pull(german),
  `Anteil Bevölk.`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-AnteilBev) %>% filter(row_number()<=10) %>% pull(`COVID-19-Fälle Anteil Bev.`),
  `Anzahl Fälle`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-AnteilBev) %>% filter(row_number()<=10) %>% pull(`COVID-19-Fälle`),
  `Todesfälle`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-Fallsterb) %>% pull(german),
  `Fallsterblichkeit`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-Fallsterb) %>% pull(Fallsterblichkeit),
  `Anzahl Todesfälle`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-Fallsterb) %>% pull(`Todesfälle`),
  `Länder nach neuen Fällen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Neue Fälle je 100.000 EW in 14 Tagen`) %>% pull(german),
  `Neue Fälle je 100.000 EW in 14 Tagen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Neue Fälle je 100.000 EW in 14 Tagen`) %>% pull(`Neue Fälle je 100.000 EW in 14 Tagen`),
  `Länder nach neuen Todesfällen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Todesfälle je 100.000 EW in 14 Tagen`) %>% pull(german),
  `Todesfälle je 100.000 EW in 14 Tagen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Todesfälle je 100.000 EW in 14 Tagen`) %>% pull(`Todesfälle je 100.000 EW in 14 Tagen`)
) %>%
  mutate(`Todesfälle je 100.000 EW in 14 Tagen`=format(`Todesfälle je 100.000 EW in 14 Tagen`, decimal.mark = ","))

# # Vorwarnzeit
# vwztabelle <- tibble(
#   Vorwarnzeit=c(
#     "Bundesdurchschnitt",
#     "kürzeste",
#     "längste"
#   ),
#   Vorwoche=c(
#     paste0(bundeslaender_table_faktenblatt %>%
#                  filter(Datum==max(Datum)-7 & Bundesland=="Gesamt") %>%
#                  pull(`Vorwarnzeit`),
#            " Tage"), # Bundesdurchschnitt
#     paste0(min(bundeslaender_table_faktenblatt %>%
#                  filter(Datum==max(Datum)-7) %>%
#                  pull(Vorwarnzeit)),
#            " Tage\n",
#            glue_collapse(bundeslaender_table_faktenblatt %>%
#              filter(Datum==max(Datum)-7 &
#                       Vorwarnzeit==min(bundeslaender_table_faktenblatt %>%
#                                   filter(Datum==max(Datum)-7) %>%
#                                   pull(Vorwarnzeit))) %>%
#              pull(Bundesland), ", ")), # kürzeste
#     paste0(max(bundeslaender_table_faktenblatt %>%
#                  filter(Datum==max(Datum)-7) %>%
#                  pull(Vorwarnzeit)),
#            " Tage\n",
#            glue_collapse(bundeslaender_table_faktenblatt %>%
#                            filter(Datum==max(Datum)-7 &
#                                     Vorwarnzeit==max(bundeslaender_table_faktenblatt %>%
#                                                 filter(Datum==max(Datum)-7) %>%
#                                                 pull(Vorwarnzeit))) %>%
#                            pull(Bundesland), ", ")) # längste
#   ),
#   dieseWoche=c(
#     paste0(bundeslaender_table_faktenblatt %>%
#              filter(Datum==max(Datum) & Bundesland=="Gesamt") %>%
#              pull(`Vorwarnzeit`),
#            " Tage"), # Bundesdurchschnitt
#     paste0(min(bundeslaender_table_faktenblatt %>%
#                  filter(Datum==max(Datum)) %>%
#                  pull(Vorwarnzeit)),
#            " Tage\n",
#            glue_collapse(bundeslaender_table_faktenblatt %>%
#                            filter(Datum==max(Datum) &
#                                     Vorwarnzeit==min(bundeslaender_table_faktenblatt %>%
#                                                        filter(Datum==max(Datum)) %>%
#                                                        pull(Vorwarnzeit))) %>%
#                            pull(Bundesland), ", ")), # kürzeste
#     paste0(max(bundeslaender_table_faktenblatt %>%
#                  filter(Datum==max(Datum)) %>%
#                  pull(Vorwarnzeit)),
#            " Tage\n",
#            glue_collapse(bundeslaender_table_faktenblatt %>%
#                            filter(Datum==max(Datum) &
#                                     Vorwarnzeit==max(bundeslaender_table_faktenblatt %>%
#                                                        filter(Datum==max(Datum)) %>%
#                                                        pull(Vorwarnzeit))) %>%
#                            pull(Bundesland), ", ")) # längste
#   ),
#   Veraenderung=c(
#     bundeslaender_table_faktenblatt %>%
#       filter(Datum==max(Datum) & Bundesland=="Gesamt") %>%
#       pull(Vorwarnzeit) - bundeslaender_table_faktenblatt %>%
#       filter(Datum==max(Datum)-7 & Bundesland=="Gesamt") %>%
#       pull(Vorwarnzeit), # Bundesdurchschnitt
#     min(bundeslaender_table_faktenblatt %>%
#           filter(Datum==max(Datum)) %>%
#           pull(Vorwarnzeit)) - min(bundeslaender_table_faktenblatt %>%
#                               filter(Datum==max(Datum)-7) %>%
#                               pull(Vorwarnzeit)), # kürzeste
#     max(bundeslaender_table_faktenblatt %>%
#           filter(Datum==max(Datum)) %>%
#           pull(Vorwarnzeit)) - max(bundeslaender_table_faktenblatt %>%
#                               filter(Datum==max(Datum)-7) %>%
#                               pull(Vorwarnzeit)) # längste
#   )
# )  %>%
#   select(Vorwarnzeit, Vorwoche, !!paste0("KW ", isoweek(max(bundeslaender_table_faktenblatt$Datum))):=dieseWoche, Veraenderung)


rki <- rki %>% mutate(KW=isoweek(Meldedatum),
                      Jahr=year(Meldedatum),
                      Monat=month(Meldedatum),
                      Jahr=case_when(
                        KW>=52 & Monat==1 ~ Jahr-1L,
                        TRUE ~ Jahr
                      ),
                      YearKW=100*Jahr+KW)
thisKW <- max(rki$YearKW)
sterbeKW <- sort(unique(rki$YearKW), decreasing = TRUE)[6]
sterbeJahr <- floor(sterbeKW/100)
vorsterbeKW <- sort(unique(rki$YearKW), decreasing = TRUE)[7]
vorsterbeJahr <- floor(vorsterbeKW/100)
sterbestichtag <- as_date(max(rki %>%
                                filter(YearKW==sterbeKW) %>%
                                pull(Meldedatum)))
vorsterbestichtag <- as_date(max(rki %>%
                                   filter(YearKW==vorsterbeKW) %>%
                                   pull(Meldedatum)))
sterberki <- rki %>% 
  filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
  mutate(Altersgruppe3=case_when(
    Altersgruppe=="A80+" ~ "80+",
    Altersgruppe=="A60-A79" ~ "60-79",
    Altersgruppe=="unbekannt" ~ "unbekannt",
    TRUE ~ "0-59"
  )) %>%
  group_by(Altersgruppe3) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), 
            Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall),
            .groups="drop") %>%
  filter(Altersgruppe3!="unbekannt") %>%
  bind_rows(., rki %>%
              filter(Meldedatum<=sterbestichtag & 
                       Meldedatum>=sterbestichtag-6) %>%
              summarise(Altersgruppe3="Gesamt", 
                        Todesfaelle=sum(AnzahlTodesfall), 
                        Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall),
                        .groups="drop"))
vorsterberki <- rki %>% 
  filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
  mutate(Altersgruppe3=case_when(
    Altersgruppe=="A80+" ~ "80+",
    Altersgruppe=="A60-A79" ~ "60-79",
    Altersgruppe=="unbekannt" ~ "unbekannt",
    TRUE ~ "0-59"
  )) %>%
  group_by(Altersgruppe3) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), 
            Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), 
            .groups="drop") %>%
  filter(Altersgruppe3!="unbekannt") %>%
  bind_rows(., rki %>% 
              filter(Meldedatum<=vorsterbestichtag &
                       Meldedatum>=vorsterbestichtag-6) %>%
              summarise(Altersgruppe3="Gesamt", 
                        Todesfaelle=sum(AnzahlTodesfall), 
                        Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), 
                        .groups="drop"))
sterbetabelle <- tibble(
  `Todesfälle & Sterblichkeit`=c(
    "0 bis 59 Jahre",
    "60 bis 79 Jahre",
    "80 Jahre +",
    "Gesamt",
    "Übersterblichkeit",
    "0 bis 59 Jahre",
    "60 bis 79 Jahre",
    "80 Jahre +",
    "Gesamt"
  ),
  Vorwoche=c(
    vorsterberki %>% pull(Todesfaelle),
    NA,
    sterbefaelle_kw.rec %>% filter(YearKW==vorsterbeKW) %>% pull(Tote_diff)
  ),
  Vorwoche_sterblichkeit=c(
    vorsterberki %>% pull(Sterblichkeit),
    NA,
    sterbefaelle_kw.rec %>% filter(YearKW==vorsterbeKW) %>% pull(Vergleich)
  ),
  KWX=c(
    sterberki %>% pull(Todesfaelle),
    NA,
    sterbefaelle_kw.rec %>% filter(YearKW==sterbeKW) %>% pull(Tote_diff) 
  ),
  KWX_sterblichkeit=c(
    sterberki %>% pull(Sterblichkeit),
    NA,
    sterbefaelle_kw.rec %>% filter(YearKW==sterbeKW) %>% pull(Vergleich) 
  ),
  Veraenderung=ifelse(is.na(KWX), 
                      NA, 
                      paste0(format(round(100*(KWX-Vorwoche)/abs(Vorwoche), 1),
                                    decimal.mark = ","), "%"))
) %>%
  mutate(Vorwoche=ifelse(Vorwoche==0, 
                         0, 
                         paste0(Vorwoche, 
                                " (", 
                                format(round(100*Vorwoche_sterblichkeit, 1), 
                                       decimal.mark=","),
                                "%)")),
         !!paste0("KW ", sterbeKW-sterbeJahr*100) := 
           ifelse(KWX==0,
                  0, 
                  paste0(KWX, 
                         " (", 
                         format(round(100*KWX_sterblichkeit, 1), 
                                decimal.mark=","), 
                         "%)"))) %>%
  select(`Todesfälle & Sterblichkeit`, Vorwoche, 
         !!paste0("KW ", sterbeKW-sterbeJahr*100), Veraenderung)


divi <- read_csv("~/Dokumente/covid19dashboard/data/divi.csv", 
                 col_types = cols(date = col_character(), 
                                  bundesland = col_integer(), gemeindeschluessel = col_integer())) %>% 
  mutate(daten_stand=as_date(date)) %>% 
  bind_rows(read_csv("~/Dokumente/covid19dashboard/data/divi.csv", 
                     col_types = cols(date = col_character(), 
                                      bundesland = col_integer(), gemeindeschluessel = col_integer())) %>% 
              mutate(daten_stand=as_date(date)) %>% 
              group_by(date, daten_stand) %>% 
              summarise(bundesland=0, gemeindeschluessel=0,
                      across(anzahl_standorte:betten_frei_nur_erwachsen, ~ sum(., na.rm=TRUE),
                             ), .groups="drop"
                      )) %>% 
  mutate(id=gemeindeschluessel,
         ICU_Betten=betten_frei+betten_belegt)
maxdividate <- maxdate # max(divi_all$daten_stand)
divi0 <- divi %>%
  filter(id==0) %>%
  mutate(auslastungcovid=faelle_covid_aktuell/ICU_Betten,
         quotefrei=betten_frei/ICU_Betten,
         date)
# Intensivbaetten
itstabelle <- tibble(
  Intensivbetten=c(
    "Intensivbetten gesamt",
    "Belegung durch Patient*innen mit COVID-19",
    "Freie Intensivbetten"
  ),
  Vorwoche=c(
    divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(ICU_Betten),
           divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(faelle_covid_aktuell),
           divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(betten_frei)
  ),
  dieseWoche=c(
    divi0 %>% filter(daten_stand==maxdividate) %>% pull(ICU_Betten),
           divi0 %>% filter(daten_stand==maxdividate) %>% pull(faelle_covid_aktuell),
           divi0 %>% filter(daten_stand==maxdividate) %>% pull(betten_frei)
  ),
  Veraenderung=paste0(format(round(100*(dieseWoche-Vorwoche)/Vorwoche, 1), decimal.mark = ","), " %")
) %>% mutate(
  Vorwoche=c(
    divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(ICU_Betten),
    paste0(divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(faelle_covid_aktuell), 
           "\n", 
           round(((divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(auslastungcovid))*100)),
           " %"),
    paste0(divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(betten_frei), 
           "\n",
           round(((divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(quotefrei))*100)),
           " %")
  ),
  dieseWoche=c(
    divi0 %>% filter(daten_stand==maxdividate) %>% pull(ICU_Betten),
    paste0(divi0 %>% filter(daten_stand==maxdividate) %>% pull(faelle_covid_aktuell), 
           "\n",
           round(((divi0 %>% filter(daten_stand==maxdividate) %>% pull(auslastungcovid))*100)),
           " %"),
    paste0(divi0 %>% filter(daten_stand==maxdividate) %>% pull(betten_frei), 
           "\n",
           round(((divi0 %>% filter(daten_stand==maxdividate) %>% pull(quotefrei))*100)),
           " %")
  )
)  %>%
  select(Intensivbetten, Vorwoche, !!paste0("KW ", isoweek(maxdividate)):=dieseWoche, Veraenderung)

letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A")#,
         # Altersgruppe=case_when(Altersgruppe %in% c("00-04","05-14") ~ "0-14",
         #                        TRUE ~ Altersgruppe
         #                        )
         ) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols =  c(Meldedatum),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdate-6 & date<=maxdate) %>%
  summarise(`Faelle_letzte_7_Tage_0-4`=sum(`Fälle_00-04`),
            `Faelle_letzte_7_Tage_5-14`=sum(`Fälle_05-14`),
            `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
            `Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), 
            `Faelle_letzte_7_Tage`=sum(`Fälle_00-04`)+sum(`Fälle_05-14`)+
              sum(`Fälle_15-34`)+sum(`Fälle_35-59`)+sum(`Fälle_60-79`)+
              sum(`Fälle_80+`),
            .groups="drop") %>%
  mutate(#`Faelle_letzte_7_Tage_je100TsdEinw_0-14`=
         #  round(`Faelle_letzte_7_Tage_0-14`/((bund_regstat_alter$ag_1+bund_regstat_alter$ag_2)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_0-4`=
           round(`Faelle_letzte_7_Tage_0-4`/((bund_regstat_alter$ag_1)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_5-14`=
           round(`Faelle_letzte_7_Tage_5-14`/((bund_regstat_alter$ag_2)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=
           round(`Faelle_letzte_7_Tage_15-34`/(bund_regstat_alter$ag_3/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=
           round(`Faelle_letzte_7_Tage_35-59`/(bund_regstat_alter$ag_4/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=
           round(`Faelle_letzte_7_Tage_60-79`/(bund_regstat_alter$ag_5/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=
           round(`Faelle_letzte_7_Tage_80+`/(bund_regstat_alter$ag_6/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw`=
           round(`Faelle_letzte_7_Tage`/(sum(bund_regstat_alter)/100000)))
vorwoche_letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Meldedatum<=maxdate-7) %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A")#,
         #Altersgruppe=case_when(Altersgruppe %in% c("00-04","05-14") ~ "0-14",
         #                       TRUE ~ Altersgruppe
         #)
         ) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols =  c(Meldedatum),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdate-6-7 & date<=maxdate-7) %>%
  summarise(#`Faelle_letzte_7_Tage_0-14`=sum(`Fälle_0-14`),
    `Faelle_letzte_7_Tage_0-4`=sum(`Fälle_00-04`),
    `Faelle_letzte_7_Tage_5-14`=sum(`Fälle_05-14`),
            `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
            `Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), 
    `Faelle_letzte_7_Tage`=sum(`Fälle_00-04`)+sum(`Fälle_05-14`)+
      sum(`Fälle_15-34`)+sum(`Fälle_35-59`)+sum(`Fälle_60-79`)+
      sum(`Fälle_80+`),
    .groups="drop") %>%
  mutate(#`Faelle_letzte_7_Tage_je100TsdEinw_0-14`=
    #  round(`Faelle_letzte_7_Tage_0-14`/((bund_regstat_alter$ag_1+bund_regstat_alter$ag_2)/100000)),
    `Faelle_letzte_7_Tage_je100TsdEinw_0-4`=
      round(`Faelle_letzte_7_Tage_0-4`/((bund_regstat_alter$ag_1)/100000)),
    `Faelle_letzte_7_Tage_je100TsdEinw_5-14`=
      round(`Faelle_letzte_7_Tage_5-14`/((bund_regstat_alter$ag_2)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=
           round(`Faelle_letzte_7_Tage_15-34`/(bund_regstat_alter$ag_3/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=
           round(`Faelle_letzte_7_Tage_35-59`/(bund_regstat_alter$ag_4/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=
           round(`Faelle_letzte_7_Tage_60-79`/(bund_regstat_alter$ag_5/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=
           round(`Faelle_letzte_7_Tage_80+`/(bund_regstat_alter$ag_6/100000)),
    `Faelle_letzte_7_Tage_je100TsdEinw`=
      round(`Faelle_letzte_7_Tage`/(sum(bund_regstat_alter)/100000)))

testmaxkw <- max(almev$KW)
testmaxjahr <- floor(testmaxkw/100)
vortestmaxkw <- sort(unique(almev$KW), decreasing = TRUE)[2]
vortestmaxjahr <- floor(vortestmaxkw/100)

almev <- almev %>%
  mutate(positivrate=positivtests/pcrtests,
         auslastung=pcrtests/testkapazitaet)
# Testungen
testtabelle <- tibble(
  Testungen=c(
    "Zahl der PCR-Tests",
    "Positive Tests",
    "Positivrate",
    "Testkapazität",
    "Auslastung"
  ),
  Vorwoche=c(
    almev %>% filter(KW==vortestmaxkw) %>% pull(pcrtests),
    almev %>% filter(KW==vortestmaxkw) %>% pull(positivtests),
    paste0(format(round((almev %>% filter(KW==vortestmaxkw) %>% pull(positivrate))*100, 2), decimal.mark = ","), " %"),
    almev %>% filter(KW==vortestmaxkw) %>% pull(testkapazitaet),
    paste0(round((almev %>% filter(KW==vortestmaxkw) %>% pull(auslastung))*100, 0), " %")
  ),
  dieseWoche=c(
    almev %>% filter(KW==testmaxkw) %>% pull(pcrtests),
    almev %>% filter(KW==testmaxkw) %>% pull(positivtests),
    paste0(format(round((almev %>% filter(KW==testmaxkw) %>% pull(positivrate))*100, 2), decimal.mark = ","), " %"),
    almev %>% filter(KW==testmaxkw) %>% pull(testkapazitaet),
    paste0(round((almev %>% filter(KW==testmaxkw) %>% pull(auslastung))*100, 0), " %")
  ),
  Veraenderung=c(
    paste0(round(100*(almev %>% filter(KW==testmaxkw) %>% pull(pcrtests) - almev %>% filter(KW==vortestmaxkw) %>% pull(pcrtests))/almev %>% filter(KW==vortestmaxkw) %>% pull(pcrtests), 0), " %"),
    paste0(format(round(100*(almev %>% filter(KW==testmaxkw) %>% pull(positivtests) - almev %>% filter(KW==vortestmaxkw) %>% pull(positivtests))/almev %>% filter(KW==vortestmaxkw) %>% pull(positivtests), 1), decimal.mark = ","), " %"),
    paste0(format(round((almev %>% filter(KW==testmaxkw) %>% pull(positivrate))*100, 2)- round((almev %>% filter(KW==vortestmaxkw) %>% pull(positivrate))*100, 2), decimal.mark = ","), " PP"),
    paste0(round(100*(almev %>% filter(KW==testmaxkw) %>% pull(testkapazitaet) - almev %>% filter(KW==vortestmaxkw) %>% pull(testkapazitaet))/almev %>% filter(KW==vortestmaxkw) %>% pull(testkapazitaet), 0), " %"),
    paste0(round((almev %>% filter(KW==testmaxkw) %>% pull(auslastung))*100, 0)- round((almev %>% filter(KW==vortestmaxkw) %>% pull(auslastung))*100, 0), " PP")
  )
)  %>%
  select(Testungen, Vorwoche, !!paste0("KW ", testmaxkw-testmaxjahr*100):=dieseWoche, Veraenderung)

# ifsgmaxkw <- max(rki_ifsg$KW, rki_hosp$YearKW, na.rm = TRUE) # min(max(rki_ifsg$KW), max(rki_hosp$KW))
# ifsgmaxjahr <- floor(ifsgmaxkw/100)
# vorifsgmaxkw <- ifelse(ifsgmaxkw==202101, 202053, ifsgmaxkw-1)
# vorifsgmaxjahr <- floor(vorifsgmaxkw/100)
# c19erkranktetabelle <- tibble(
#   Erkrankte=c(
#     # "Ohne Symptomatik",
#     # "Nicht stationär behandelt",
#     # "Intensivmedizinisch behandelt (Schätzung)",
#     "Klinik- und Praxispersonal",
#     "Neuinfizierte",
#     "Neu stationär behandelt",
#     "Neu verstorben",
#     "Betreut nach IfSG §36 (u.a. Pflegewohnheim)",
#     "Neuinfizierte",
#     "Neuinfizierte über 60 J.",
#     "Neu stationär behandelt",
#     "Neu verstorben"
#   ),
#   Vorwoche=c(
#     # paste0(format(round(100*(rki_hosp %>% filter(YearKW==vorifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark = ","), " %"),
#     # paste0(format(round(100-100*(rki_hosp %>% 
#     #                                filter(YearKW==vorifsgmaxkw) %>% 
#     #                                pull(`Anteil der Hospitalisierten bei Fällen mit Angabe zur Hospitalisation`)), 1), 
#     #               decimal.mark = ","), 
#     #        " %"),
#     # paste0(format(agefatality_data %>% filter(Meldedatum==maxdate-7 & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark = ","), " %"),
#     NA,
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuinfiziert),
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neustationaer),
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuverstorben),
#     NA,
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert),
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert60),
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neustationaer),
#     rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuverstorben)
#   ),
#   dieseWoche=c(
#     # paste0(format(round(100*(rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark = ","), " %"),
#     # paste0(format(round(100-100*(rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil der Hospitalisierten bei Fällen mit Angabe zur Hospitalisation`)), 1), decimal.mark = ","), " %"),
#     # paste0(format(agefatality_data %>% filter(Meldedatum==maxdate & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark = ","), " %"),
#     NA,
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuinfiziert),
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neustationaer),
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuverstorben),
#     NA,
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert),
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert60),
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neustationaer),
#     rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuverstorben)
#   ),
#   Veraenderung=c(
#     # paste0(format(round(100*(rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)-rki_hosp %>% filter(YearKW==ifsgmaxkw-1) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark=","), " PP"),
#     # paste0(format(round(100*(rki_hosp %>% filter(YearKW==vorifsgmaxkw) %>% pull(`Anteil der Hospitalisierten bei Fällen mit Angabe zur Hospitalisation`)-rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil der Hospitalisierten bei Fällen mit Angabe zur Hospitalisation`)), 1), decimal.mark=","), " PP"),
#     # paste0(format(agefatality_data %>% filter(Meldedatum==maxdate & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil)-agefatality_data %>% filter(Meldedatum==maxdate-7 & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark=","), " PP"),
#     NA,
#     paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuinfiziert) - rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuinfiziert))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuinfiziert)), " %"),
#     paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neustationaer)- rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neustationaer))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neustationaer)), " %"),
#     NA,
#     NA,
#     paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert) - rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert)), " %"),
#     paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert60) - rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert60))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert60)), " %"),
#     paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neustationaer)- rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neustationaer))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neustationaer)), " %"),
#     paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuverstorben)- rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuverstorben))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuverstorben)), " %")
#   )
# ) %>%
#   select(Erkrankte, Vorwoche, !!paste0("KW ", ifsgmaxkw-ifsgmaxjahr*100):=dieseWoche, Veraenderung)
# 
## hospitalisierungen nach altersgruppen
maxweek_rki_hosp_age <- max(rki_hosp_age$YearKW, na.rm=TRUE)
hosp_age_thisweek <- sort(unique(rki_hosp_age$YearKW), decreasing = TRUE)[3]
hosp_age_beforeweek <- sort(unique(rki_hosp_age$YearKW), decreasing = TRUE)[4]

rki_agg_kw_ag <- rki %>%
  mutate(KW=isoweek(Meldedatum),
         Jahr=year(Meldedatum),
         Monat=month(Meldedatum),
         Jahr=case_when(
           KW>=52 & Monat==1 ~ Jahr-1L,
           TRUE ~ Jahr
         ),
         YearKW=100*Jahr+KW) %>%
  group_by(YearKW, Altersgruppe) %>%
  summarise(AnzahlFaelle=sum(AnzahlFall[NeuerFall>=0]),
            .groups="drop")

agg_anteil_ag <- rki_hosp_age %>%
  pivot_longer(cols=`Fälle A00..04`:`Fälle A80+`, names_to = "Altersgruppe",
               values_to="AnzahlHosp") %>%
  select(Meldejahr, Meldewoche, YearKW,
         Altersgruppe, AnzahlHosp) %>%
  mutate(Altersgruppe=str_replace(Altersgruppe, fixed(".."), "-A"),
         Altersgruppe=str_replace(Altersgruppe, fixed("Fälle "), "")) %>%
  left_join(rki_agg_kw_ag,
            by=c("YearKW", "Altersgruppe")) %>%
  mutate(Anteil_an_FaelleAG=AnzahlHosp/AnzahlFaelle) %>%
  mutate(Altersgruppe=str_replace_all(Altersgruppe, fixed("A"), ""))

hosp_ag_tabelle <- tibble(
  Hospitalisierungen = c(
    "Gesamt",
    "Davon unter 4-Jährige",
    "Davon 5- bis 14-Jährige",
    "Davon 15- bis 34-Jährige",
    "Davon 35- bis 59-Jährige",
    "Davon 60- bis 79-Jährige",
    "Davon über 80-Jährige"
  ),
  Vorwoche = c(
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      summarise(Faellegesamt=sum(across(`Fälle A00..04`:`Fälle A80+`))) %>%
      pull(Faellegesamt),
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      pull(`Fälle A00..04`),
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      pull(`Fälle A05..14`),
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      pull(`Fälle A15..34`),
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      pull(`Fälle A35..59`),
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      pull(`Fälle A60..79`),
    rki_hosp_age %>% filter(YearKW==hosp_age_beforeweek) %>%
      pull(`Fälle A80+`)
  ),
  VorwocheAnteil = c(
    agg_anteil_ag %>% filter(YearKW==hosp_age_beforeweek) %>%
      summarise(Anteil_an_FaelleAG=sum(AnzahlHosp)/sum(AnzahlFaelle)) %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_beforeweek & Altersgruppe=="00-04") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_beforeweek & Altersgruppe=="05-14") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_beforeweek & Altersgruppe=="15-34") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_beforeweek & Altersgruppe=="35-59") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_beforeweek & Altersgruppe=="60-79") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_beforeweek & Altersgruppe=="80+") %>%
      pull(Anteil_an_FaelleAG)
  ),
  dieseWoche = c(
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      summarise(Faellegesamt=sum(across(`Fälle A00..04`:`Fälle A80+`))) %>%
      pull(Faellegesamt),
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      pull(`Fälle A00..04`),
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      pull(`Fälle A05..14`),
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      pull(`Fälle A15..34`),
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      pull(`Fälle A35..59`),
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      pull(`Fälle A60..79`),
    rki_hosp_age %>% filter(YearKW==hosp_age_thisweek) %>%
      pull(`Fälle A80+`)
  ),
  dieseWocheAnteil = c(
    agg_anteil_ag %>% filter(YearKW==hosp_age_thisweek) %>%
      summarise(Anteil_an_FaelleAG=sum(AnzahlHosp)/sum(AnzahlFaelle)) %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_thisweek & Altersgruppe=="00-04") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_thisweek & Altersgruppe=="05-14") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_thisweek & Altersgruppe=="15-34") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_thisweek & Altersgruppe=="35-59") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_thisweek & Altersgruppe=="60-79") %>%
      pull(Anteil_an_FaelleAG),
    agg_anteil_ag %>%
      filter(YearKW==hosp_age_thisweek & Altersgruppe=="80+") %>%
      pull(Anteil_an_FaelleAG)
  ),
  Veraenderung=paste0(format(round(100*(dieseWoche-Vorwoche)/Vorwoche, 1),
                             decimal.mark = ","),
                      " %")
) %>%
  mutate(Vorwoche=ifelse(Vorwoche==0,
                         0,
                         paste0(Vorwoche,
                                " (",
                                format(round(100*VorwocheAnteil, 1),
                                       decimal.mark=","),
                                "%)")),
         !!paste0("KW ", hosp_age_thisweek%%100) :=
           ifelse(dieseWoche==0,
                  0,
                  paste0(dieseWoche,
                         " (",
                         format(round(100*dieseWocheAnteil, 1),
                                decimal.mark=","),
                         "%)"))) %>%
  select(Hospitalisierungen, Vorwoche,
         !!paste0("KW ", hosp_age_thisweek%%100), Veraenderung)


vaccmaxdate <- max(kbv_rki_impfstoffe$vacc_date) # max(vacc_zahlen$date)
vorvaccmaxdate <- vaccmaxdate-7
# vacc_brd <- vacc_alle_faktenblatt %>% filter(geo=="Deutschland")
vacc_brd <- rki_impfstoffe %>% filter(vacc_date<=vaccmaxdate)
# vacc_brd <- vacc_zahlen %>% filter(region=="DE" & date==vaccmaxdate) %>% 
#   pivot_wider(names_from = metric, values_from = value)
vacc_brd_vorwoche <- rki_impfstoffe %>% filter(vacc_date<=vorvaccmaxdate)
# vacc_brd_vorwoche <- vacc_zahlen %>% 
#   filter(region=="DE" & date==vorvaccmaxdate) %>% # -days(1)
#   pivot_wider(names_from = metric, values_from = value)
# Geimpfte Personen
geimpfte_gesamt <- tibble(
  "Geimpfte Personen"=c(
    "Gesamtbevölkerung",
    "Gesamt",
    "Nicht vollst. geimpft",
    "Vollst. geimpft",
    "Zusätzl. Booster-Impfung",
    "Zusätzl. 2. Booster-Impfung"
  ),
  Vorwoche=c(
    NA,
    sum(vacc_brd_vorwoche %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)) - 
      sum(vacc_brd_vorwoche %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==3) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==4) %>% pull(anzahl_alleorte))
  ),
  VorwocheAnteil=c(
    NA,
    sum(vacc_brd_vorwoche %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)) - 
      sum(vacc_brd_vorwoche %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==3) %>% pull(anzahl_alleorte)),
    sum(vacc_brd_vorwoche %>% filter(vacc_series==4) %>% pull(anzahl_alleorte))
  )/83166711*100,
  dieseWoche=c(
    NA,
    sum(vacc_brd %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)) - 
      sum(vacc_brd %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==3) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==4) %>% pull(anzahl_alleorte))
  ),
  dieseWocheAnteil=c(
    NA,
    sum(vacc_brd %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==1) %>% pull(anzahl_alleorte)) - 
      sum(vacc_brd %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==2) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==3) %>% pull(anzahl_alleorte)),
    sum(vacc_brd %>% filter(vacc_series==4) %>% pull(anzahl_alleorte))
  )/83166711*100
) %>%
  mutate(Vorwoche=ifelse(is.na(Vorwoche), 
                         NA,
                         paste0(Vorwoche,
                                " (",
                                format(round(VorwocheAnteil, 1), 
                                       decimal.mark=","),
                                " %)")),
         dieseWoche=ifelse(is.na(dieseWoche), 
                           NA,
                           paste0(dieseWoche,
                                  " (",
                                  format(round(dieseWocheAnteil, 1), 
                                         decimal.mark=","),
                                  " %)")),
         Anteil_Veraenderung=if_else(is.na(dieseWoche),
                                     "",
                                     paste0(format(round(dieseWocheAnteil-
                                                   VorwocheAnteil, 1),
                                           decimal.mark=","),
                                           " PP"))
         ) %>%
  select("Geimpfte Personen",
         Vorwoche,
         !!paste0("Stand ", 
                  day(vaccmaxdate)+1, 
                  ".", month(vaccmaxdate), 
                  ".") := 
           dieseWoche,
         Anteil_Veraenderung)

impfkw <- sort(unique(rki$YearKW), decreasing = TRUE)[2]
vorimpfkw <- sort(unique(rki$YearKW), decreasing = TRUE)[3]
# Impffortschritt
fortschritt_table <- tibble(
  "Impffortschritt"=c(
    "Impfungen pro Woche",
    "Gesamt",
    "davon in Impfzentren und Betrieben",
    "davon in ärztl. Praxen"
  ),
  "Vorwoche"=c(
    NA,
    impfdax_data %>% filter(JahrKW==vorimpfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`) +
      impfdax_data %>% filter(JahrKW==vorimpfkw) %>%
      pull(`Anzahl_Impfungen Impfzentren_und_Betriebe`),
    impfdax_data %>% filter(JahrKW==vorimpfkw) %>% 
      pull(`Anzahl_Impfungen Impfzentren_und_Betriebe`),
    impfdax_data %>% filter(JahrKW==vorimpfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`)
  ),
  "dieseWoche"=c(
    NA,
    impfdax_data %>% filter(JahrKW==impfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`) +
      impfdax_data %>% filter(JahrKW==impfkw) %>%
      pull(`Anzahl_Impfungen Impfzentren_und_Betriebe`),
    impfdax_data %>% filter(JahrKW==impfkw) %>% 
      pull(`Anzahl_Impfungen Impfzentren_und_Betriebe`),
    impfdax_data %>% filter(JahrKW==impfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`)
  )
) %>%
  mutate(Veraenderung=paste0(format(round((dieseWoche-Vorwoche)/Vorwoche*100, 1),
                             decimal.mark=","),
                             " %"),
         Vorwoche=case_when(
    is.na(Vorwoche) ~ " ",
    Impffortschritt=="Gesamt" ~ as.character(Vorwoche),
    TRUE ~ paste0(Vorwoche,
                  " (",
                  format(round(100*Vorwoche/Vorwoche[Impffortschritt=="Gesamt"], 1), 
                         decimal.mark=","),
                  " %)")),
         dieseWoche=case_when(
    is.na(dieseWoche) ~ " ",
    Impffortschritt=="Gesamt" ~ as.character(dieseWoche),
    TRUE ~ paste0(dieseWoche,
                  " (",
                  format(round(100*dieseWoche/dieseWoche[Impffortschritt=="Gesamt"], 1), 
                         decimal.mark=","),
                  " %)"))) %>% 
  rename("letzteKW"=dieseWoche) %>% 
  select(Impffortschritt,
         Vorwoche,
         letzteKW,
         Veraenderung)

# Regionale Daten
bl_impfungen_ohneinzidenz <- bind_rows(
  kbv_rki_impfstoffe %>% 
    summarise(Bundesland="Gesamt",
              id=0,
              `Impfungen Praxen`=sum(anzahl_praxen[vacc_date>maxdate-days(7) &
                                                     vacc_date<=maxdate], 
                                     na.rm=TRUE),
              `Gesamt min. 1x`=sum(anzahl_alleorte[vacc_series==1], na.rm=TRUE),
              `Gesamt vollst.`=sum(anzahl_alleorte[vacc_series==2], na.rm=TRUE),
              `Gesamt Auffr.`=sum(anzahl_alleorte[vacc_series==3], na.rm=TRUE)),
  kbv_rki_impfstoffe %>% 
    filter(!Bundesland%in%c("Bundesressorts", "unbekannt")) %>% 
    group_by(Bundesland) %>% 
    summarise(id=BundeslandId_Impfort[1],
              `Impfungen Praxen`=sum(anzahl_praxen[vacc_date>maxdate-days(7) &
                                                     vacc_date<=maxdate], 
                                     na.rm=TRUE),
              `Gesamt min. 1x`=sum(anzahl_alleorte[vacc_series==1], na.rm=TRUE),
              `Gesamt vollst.`=sum(anzahl_alleorte[vacc_series==2], na.rm=TRUE),
              `Gesamt Auffr.`=sum(anzahl_alleorte[vacc_series==3], na.rm=TRUE)) %>% 
    arrange(Bundesland)
) %>% 
  left_join(params %>% select(-id), by=c("Bundesland"="name")) %>% 
  mutate(across(contains("Gesamt "), ~ format(round(100*.x/EW_insgesamt, 
                                                    1),
                                              decimal.mark=","))) %>% 
  select(-EW_insgesamt)

rki_alter_destatis_kreise <- rki %>% lazy_dt() %>%
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% # this takes long unfortunately... but much faster with dtplyr!
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  collect() %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A")) %>%
  group_by(Meldedatum,Altersgruppe, id) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall, na.rm=T), .groups="drop") %>% 
  pivot_wider(id_cols = c(Meldedatum, id),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0, "Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum), blid=floor(id/1000000),
         `Fälle_60+`=`Fälle_60-79`+`Fälle_80+`) %>%
  right_join(.,
             expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days"),
                         id=unique(.$id)),
             by=c("Meldedatum", "id")) %>%
  replace(is.na(.), 0) %>%
  group_by(id) %>%
  arrange(Meldedatum) %>%
  mutate(cases059=cumsum(`Fälle_00-04`+`Fälle_05-14`+`Fälle_15-34`+`Fälle_35-59`),
         cases6079=cumsum(`Fälle_60-79`),
         cases80=cumsum(`Fälle_80+`),
         cases60=cumsum(`Fälle_60+`)) %>%
  ungroup() %>%
  mutate(blid=floor(id/1000000),
         `Fälle`=`Fälle_00-04`+`Fälle_05-14`+`Fälle_15-34`+`Fälle_35-59`+`Fälle_60+`) %>%
  as_tibble()
rki_alter_destatis <- bind_rows(rki_alter_destatis_kreise %>% # bundeslaender
                                  group_by(Meldedatum, blid) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            cases60=sum(cases60),
                                            `Fälle`=sum(`Fälle`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            .groups="drop") %>%
                                  mutate(id=blid),
                                rki_alter_destatis_kreise %>% # bund gesamt
                                  group_by(Meldedatum) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            cases60=sum(cases60),
                                            `Fälle`=sum(`Fälle`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            .groups="drop") %>%
                                  mutate(id=0, blid=0))
bl_inzidenz <- rki_alter_destatis %>%
  mutate(date=date(Meldedatum)) %>%
  group_by(id) %>% arrange(id, -as.numeric(date)) %>%
  summarise(`Faelle_letzte_7_Tage`=zoo::rollsum(`Fälle`, 7),
            `Faelle_letzte_7_Tage_60+`=zoo::rollsum(`Fälle_60+`, 7),
            date=zoo::rollmax(date, 7),
            .groups="drop") %>%
  left_join(., kreise_regstat_alter, by=c("id")) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw`=round(`Faelle_letzte_7_Tage`/((ag_1+ag_2+ag_3+ag_4+ag_5+ag_6)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((ag_5+ag_6)/100000))) %>%
  left_join(., rki_alter_destatis %>% select(cases059, cases6079, cases80, 
                                             id, Meldedatum), 
            by=c("id"="id", "date"="Meldedatum")) %>%
  mutate(EW059=ag_1+ag_2+ag_3+ag_4,
         EW6079=ag_5,
         EW80=ag_6,
         EW_insgesamt=EW059+EW6079+EW80) %>% 
  filter(id<=16 & date==maxdate) %>% 
  select(id,
         `7-Tage-Inzidenz`=Faelle_letzte_7_Tage_je100TsdEinw,
         `7-Tage-Inzidenz 60+`=`Faelle_letzte_7_Tage_je100TsdEinw_60+`)

bl_impfungen <- bl_impfungen_ohneinzidenz %>% 
  left_join(bl_inzidenz, by="id") %>% 
  select(-id, -cases, -R0)

hersteller_brd <- vacc_brd
hersteller_brd_vorwoche <- vacc_brd_vorwoche
geliefert <- impfdashboardde %>% 
  group_by(Hersteller) %>% 
  summarise(dosen_geliefert=sum(dosen))
geliefert_vorwoche <- impfdashboardde %>% 
  filter(date<=today()-14) %>% 
  group_by(Hersteller) %>% 
  summarise(dosen_geliefert=sum(dosen))

# Impfstoffdosen
hersteller_table <- tibble(
  "Impfstoffdosen"=c(
    "Biontech/Pfizer",
    "Impfungen Original",
    "Impfungen Omikron",
    "geliefert Original",
    "geliefert Omikron",
    "Moderna",
    "Impfungen Original",
    "Impfungen Omikron",
    "geliefert Original",
    "geliefert Omikron",
    "AstraZeneca",
    "geliefert",
    "Johnson&Johnson",
    "geliefert",
    "Novavax",
    "geliefert",
    "Valneva",
    "geliefert"
  ),
  "Vorwoche"=c(
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff %in% c("Comirnaty", "Comirnaty-Omikron")) %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Comirnaty") %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Comirnaty-Omikron") %>%
          pull(anzahl_alleorte)),
    geliefert_vorwoche %>% filter(Hersteller=="BNT/Pfizer") %>% pull(dosen_geliefert),
    geliefert_vorwoche %>% filter(Hersteller=="BNT/Pfizer-Omikron") %>% pull(dosen_geliefert),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff %in% c("Moderna", "Moderna-Omikron")) %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Moderna") %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Moderna-Omikron") %>%
          pull(anzahl_alleorte)),
    geliefert_vorwoche %>% filter(Hersteller=="Moderna") %>% pull(dosen_geliefert),
    geliefert_vorwoche %>% filter(Hersteller=="Moderna-Omikron") %>% pull(dosen_geliefert),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="AstraZeneca") %>%
          pull(anzahl_alleorte)),
    geliefert_vorwoche %>% filter(Hersteller=="AZ") %>% pull(dosen_geliefert),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Janssen") %>%
          pull(anzahl_alleorte)),
    geliefert_vorwoche %>% filter(Hersteller=="J&J") %>% pull(dosen_geliefert),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Novavax") %>%
          pull(anzahl_alleorte)),
    geliefert_vorwoche %>% filter(Hersteller=="Novavax") %>% pull(dosen_geliefert),
    sum(hersteller_brd_vorwoche %>% 
          filter(Impfstoff=="Valneva") %>%
          pull(anzahl_alleorte)),
    geliefert_vorwoche %>% filter(Hersteller=="Valneva") %>% pull(dosen_geliefert)
  ),
  "dieseWoche"=c(
    sum(hersteller_brd %>% 
          filter(Impfstoff %in% c("Comirnaty", "Comirnaty-Omikron")) %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Comirnaty") %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Comirnaty-Omikron") %>%
          pull(anzahl_alleorte)),
    geliefert %>% filter(Hersteller=="BNT/Pfizer") %>% pull(dosen_geliefert),
    geliefert %>% filter(Hersteller=="BNT/Pfizer-Omikron") %>% pull(dosen_geliefert),
    sum(hersteller_brd %>% 
          filter(Impfstoff %in% c("Moderna", "Moderna-Omikron")) %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Moderna") %>%
          pull(anzahl_alleorte)),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Moderna-Omikron") %>%
          pull(anzahl_alleorte)),
    geliefert %>% filter(Hersteller=="Moderna") %>% pull(dosen_geliefert),
    geliefert %>% filter(Hersteller=="Moderna-Omikron") %>% pull(dosen_geliefert),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="AstraZeneca") %>%
          pull(anzahl_alleorte)),
    geliefert %>% filter(Hersteller=="AZ") %>% pull(dosen_geliefert),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Janssen") %>%
          pull(anzahl_alleorte)),
    geliefert %>% filter(Hersteller=="J&J") %>% pull(dosen_geliefert),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Novavax") %>%
          pull(anzahl_alleorte)),
    geliefert %>% filter(Hersteller=="Novavax") %>% pull(dosen_geliefert),
    sum(hersteller_brd %>% 
          filter(Impfstoff=="Valneva") %>%
          pull(anzahl_alleorte)),
    geliefert %>% filter(Hersteller=="Valneva") %>% pull(dosen_geliefert)
  )
) %>%
  mutate(anteil_vorwoche=paste0(" (", 
                       format(round(100*Vorwoche/(Vorwoche[1]+Vorwoche[6]+Vorwoche[11]+Vorwoche[13]+Vorwoche[15]+Vorwoche[17]), 
                                    1), 
                              decimal.mark=","), 
                       " %)"),
         anteil=paste0(" (", 
                       format(round(100*dieseWoche/(dieseWoche[1]+dieseWoche[6]+dieseWoche[11]+dieseWoche[13]+dieseWoche[15]+dieseWoche[17]), 
                                    1), 
                              decimal.mark=","), 
                       " %)")) %>%
  mutate(Vorwoche=ifelse(
    Impfstoffdosen%in%c("Biontech/Pfizer", "Moderna", "AstraZeneca", "Johnson&Johnson", "Novavax", "Valneva"), 
    paste0(Vorwoche, anteil_vorwoche),
    Vorwoche),
         dieseWoche=ifelse(
    Impfstoffdosen%in%c("Biontech/Pfizer", "Moderna", "AstraZeneca", "Johnson&Johnson", "Novavax", "Valneva"), 
    paste0(dieseWoche, anteil),
    dieseWoche)) %>%
  select(-anteil, -anteil_vorwoche)

bltabelle <- bind_rows(
  params %>%
    filter(name=="Gesamt"),
  params %>%
    filter(name!="Gesamt" & id<17) %>%
    arrange(name)
) %>%
  left_join(bl_inzidenz, by="id") %>% 
  rowwise() %>%
  mutate(`Inzidenzprojektion`=projektion_datum(STI_aktuell = `7-Tage-Inzidenz`,
                                               STI_Ziel = 100,
                                               Rt = `R0`,
                                               tage_infektioes = 5)) %>%
  ungroup() %>%
  left_join(regional_hospinzidenz, by=c("name"="Bundesland")) %>% 
  mutate(`R(t)`=format(round(`R0`, 2), decimal.mark = ","),
         `Hospitalisierungen`=format(round(`7T_Hospitalisierung_Inzidenz`,1), 
                                     decimal.mark = ","),
         `Bereits infizierte Bevölkerung`=paste0(format(round(100*cases/EW_insgesamt, 
                                                1),
                                                decimal.mark=","),
                                                " %"),
         Inzidenzprojektion=ifelse(Inzidenzprojektion=="nie",
                                   "wird nicht erreicht", 
                                   Inzidenzprojektion)) %>%
  select(Bundesland=name, 
         `Bereits infizierte Bevölkerung`, 
         # Vorwarnzeit=`Vorwarnzeit`, 
         `R(t)`,
         `7-Tage-Inzidenz 60+`, Hospitalisierungen, `7-Tage-Inzidenz`, 
         Inzidenzprojektion)

rwert7ti <- tibble(
  `R-Wert & 7-Tage-Inzidenz`=c(
    "Reproduktionszahl R",
    "Neue Fälle je 100.000 EW in 7 Tagen bezogen auf die jeweilige Gruppe:",
    "Gesamtbevölkerung",
    # "- Davon Unter-14-Jährige",
    "- Davon Unter-4-Jährige",
    "- Davon 5-bis-14-Jährige",
    "- Davon 15-bis-34-Jährige",
    "- Davon 35-bis-59-Jährige",
    "- Davon 60-bis-79-Jährige",
    "- Davon Über-80-Jährige",
    "Neue hospitalisierte Fälle je 100.000 EW in 7 Tagen:",
    "Gesamtbevölkerung",
    "- Davon über 80-Jährige"#,
    # "Regionen mit 7-TI bei Über-60-Jährigen:",
    # "> 35",
    # "> 50"#,
    # "Regionen mit 7-TI bei Über-80-Jährigen:",
    # "> 35",
    # "> 50"
  ),
  Vorwoche=c(
    round(Nowcasting_Zahlen %>% 
            filter(Datum==maxdate-7) %>% pull(PS_7_Tage_R_Wert), 2),
    NA,
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw`),
    # vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-14`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-4`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_5-14`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_15-34`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_35-59`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_60-79`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_80+`),
    NA,
    rki_hospinz %>% filter(Datum==maxdate-days(7) & Altersgruppe=="00+" & Bundesland_Id=="00") %>% pull(`7T_Hospitalisierung_Inzidenz`),
    rki_hospinz %>% filter(Datum==maxdate-days(7) & Altersgruppe=="80+" & Bundesland_Id=="00") %>% pull(`7T_Hospitalisierung_Inzidenz`)#,
    # NA,
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 60+`))>35, na.rm=TRUE)),
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE))#,
    # NA,
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 80+`))>35, na.rm=TRUE)),
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 80+`))>50, na.rm=TRUE))
  ),
  dieseWoche=c(
    round(Nowcasting_Zahlen %>% 
            filter(Datum==maxdate-1) %>% pull(PS_7_Tage_R_Wert), 2),
    NA,
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw`)),
    # round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-14`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-4`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_5-14`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_15-34`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_35-59`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_60-79`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_80+`)),
    NA,
    rki_hospinz %>% filter(Datum==maxdate & Altersgruppe=="00+" & Bundesland_Id=="00") %>% pull(`7T_Hospitalisierung_Inzidenz`),
    rki_hospinz %>% filter(Datum==maxdate & Altersgruppe=="80+" & Bundesland_Id=="00") %>% pull(`7T_Hospitalisierung_Inzidenz`)#,
    # NA,
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 60+`))>35, na.rm=TRUE)),
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE))#,
    # NA,
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 80+`))>35, na.rm=TRUE)),
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 80+`))>50, na.rm=TRUE))
  ),
  Veraenderung=ifelse(is.na(Vorwoche), NA, paste0(format(round(100*(dieseWoche-Vorwoche)/Vorwoche, 1), decimal.mark = ","), " %"))
)
rwert7ti <- rwert7ti %>%
  mutate(Vorwoche = replace(Vorwoche, c(1,11,12), format(as.numeric(unlist(rwert7ti[c(1,11,12), 2])), decimal.mark=",")),
         dieseWoche = replace(dieseWoche, c(1,11,12), format(as.numeric(unlist(rwert7ti[c(1,11,12), 3])), decimal.mark=",")))  %>%
  select(`R-Wert & 7-Tage-Inzidenz`, Vorwoche, !!paste0("KW ", isoweek(maxdate)):=dieseWoche, Veraenderung)


library(openxlsx)
list_of_datasets <- list(
  "Geimpfte Personen"=geimpfte_gesamt,
  "Impffortschritt"=fortschritt_table,
  "Regional Geimpfte"=bl_impfungen,
  "Impfstoffdosen"=hersteller_table,
  "Testungen"=testtabelle,
  "R-Wert und 7-Tage-Inzidenz" = rwert7ti,
  # "COVID-19-Erkrankte"=c19erkranktetabelle,
  "Hospitalisierungen"=hosp_ag_tabelle,
  "Intensivbetten"=itstabelle,
  "Todesfälle"=sterbetabelle,
  # "Vorwarnzeit"=vwztabelle,
  "Regionale Daten"=bltabelle,
  "Internationaler Vergleich"=EUmal4tabelle)
write.xlsx(list_of_datasets, 
           file = paste0("../data/kbvreport_export/faktenblatttabellen_", 
                         maxdate, 
                         ".xlsx"),
           overwrite=TRUE)

