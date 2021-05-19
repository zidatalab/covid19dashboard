# manual updates:
# almev.csv tuesdays from https://www.alm-ev.de/aktuell/corona-themenseite/datenerhebung-alm-ev/
# rki_ifsg.csv ifsg 23 and 36 data from rki situationsbericht last sunday and sunday before
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
impfen_praxen_letztekw <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/impfdax_praxen_wirkstoffe_letzte_kw.csv")
gelieferte_dosen <- read_json("../data/tabledata/impfsim_start.json",
                              simplifyVector = TRUE) %>% 
  filter(geo=="Gesamt")

# impfdashboard.de/daten für lieferungen -> bund
impfdashboardde <- read_tsv(
  "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region")

# daten übersterblichkeit
url_sterblk <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"
destfile_sterblk <- "../data/sonderauswertung_sterbefaelle.xlsx"
curl::curl_download(url_sterblk, destfile_sterblk)

# daten rki symptomanteil, hospitalisierungsrate und sterberate
url_rkihosp <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Klinische_Aspekte.xlsx?__blob=publicationFile"
destfile_rkihosp <- "../data/klinische_aspekte.xlsx"
curl::curl_download(url_rkihosp, destfile_rkihosp)

## Destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html
altersgruppen_bund <- tibble("unter 20"=18.4,
                             "20 bis 40"=24.6,
                             "40 bis 60"=28.4,
                             "60 bis 80"=21.7,
                             "80+"=6.8)/100

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

almev <- read_csv("../data/almev.csv")

rki_ifsg <- read_csv("../data/rki_ifsg.csv")

rki_hosp <- read_excel(destfile_rkihosp, 
                       # sheet = "Daten", 
                       skip = 1) %>%
  mutate(YearKW=Meldejahr*100+MW)

# ard impfdaten
vacc_zahlen <- read_csv("../data/vacc_zahlen_ard.csv")

bundeslaender_table_faktenblatt <- read_json("../data/tabledata/bundeslaender_table_faktenblatt.json",
                                simplifyVector = TRUE) %>%
  mutate(Datum=as_date(Datum))
kreise_table_faktenblatt <- read_json("../data/tabledata/kreise_table_faktenblatt.json",
                                simplifyVector = TRUE) %>%
  mutate(Datum=as_date(Datum))
vacc_table_faktenblatt <- read_json("../data/tabledata/vacc_table_faktenblatt.json",
                                             simplifyVector = TRUE)
vacc_alle_faktenblatt <- read_json("../data/tabledata/vacc_alle_faktenblatt.json",
                                    simplifyVector = TRUE)

agefatality_data <- read_json("../data/plotdata/agefatality.json",
                                             simplifyVector = TRUE) %>%
  mutate(Meldedatum=as_date(Meldedatum))

sterbefaelle_kw <- bind_rows(read_excel(destfile_sterblk, 
                                        sheet = "D_2016_2021_KW_AG_Männlich", 
                                        skip = 8,
                                        na="X") %>% mutate(sex="maennlich"),
                             read_excel(destfile_sterblk, 
                                        sheet = "D_2016_2021_KW_AG_Weiblich", 
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
params <- tbl(conn,"params") %>% select(name, EW_insgesamt) %>% collect()
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
rki <- rki %>% mutate(Meldedatum=as_date(Meldedatum)) %>%
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
  left_join(., params, by=c("Country"="name"))

# tables for excel
maxdate <- max(bundeslaender_table_faktenblatt$Datum)
# Regionale Daten
bltabelle <- bind_rows(
  bundeslaender_table_faktenblatt %>%
    filter(Bundesland=="Gesamt" & Datum==maxdate),
  bundeslaender_table_faktenblatt %>%
    filter(Bundesland!="Gesamt" & Datum==maxdate) %>%
    arrange(Bundesland)
) %>%
  rowwise() %>%
  mutate(`Inzidenzprojektion`=projektion_datum(STI_aktuell = `7-Tage-Inzidenz`,
                                               STI_Ziel = 100,
                                               Rt = `R(t)`,
                                               tage_infektioes = 5)) %>%
  ungroup() %>%
  mutate(`R(t)`=format(`R(t)`, decimal.mark = ","),
         Inzidenzprojektion=ifelse(Inzidenzprojektion=="nie", "wird nicht erreicht", Inzidenzprojektion)) %>%
  select(Bundesland, 
         `Bereits infizierte Bevölkerung`, 
         Vorwarnzeit=`Vorwarnzeit`, 
         `R(t)`,
         `7-Tage-Inzidenz 60+`, `7-Tage-Inzidenz`, 
         Inzidenzprojektion)

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

# Vorwarnzeit
vwztabelle <- tibble(
  Vorwarnzeit=c(
    "Bundesdurchschnitt",
    "kürzeste",
    "längste"
  ),
  Vorwoche=c(
    paste0(bundeslaender_table_faktenblatt %>%
                 filter(Datum==max(Datum)-7 & Bundesland=="Gesamt") %>%
                 pull(`Vorwarnzeit`),
           " Tage"), # Bundesdurchschnitt
    paste0(min(bundeslaender_table_faktenblatt %>%
                 filter(Datum==max(Datum)-7) %>%
                 pull(Vorwarnzeit)),
           " Tage\n",
           glue_collapse(bundeslaender_table_faktenblatt %>%
             filter(Datum==max(Datum)-7 &
                      Vorwarnzeit==min(bundeslaender_table_faktenblatt %>%
                                  filter(Datum==max(Datum)-7) %>%
                                  pull(Vorwarnzeit))) %>%
             pull(Bundesland), ", ")), # kürzeste
    paste0(max(bundeslaender_table_faktenblatt %>%
                 filter(Datum==max(Datum)-7) %>%
                 pull(Vorwarnzeit)),
           " Tage\n",
           glue_collapse(bundeslaender_table_faktenblatt %>%
                           filter(Datum==max(Datum)-7 &
                                    Vorwarnzeit==max(bundeslaender_table_faktenblatt %>%
                                                filter(Datum==max(Datum)-7) %>%
                                                pull(Vorwarnzeit))) %>%
                           pull(Bundesland), ", ")) # längste
  ),
  dieseWoche=c(
    paste0(bundeslaender_table_faktenblatt %>%
             filter(Datum==max(Datum) & Bundesland=="Gesamt") %>%
             pull(`Vorwarnzeit`),
           " Tage"), # Bundesdurchschnitt
    paste0(min(bundeslaender_table_faktenblatt %>%
                 filter(Datum==max(Datum)) %>%
                 pull(Vorwarnzeit)),
           " Tage\n",
           glue_collapse(bundeslaender_table_faktenblatt %>%
                           filter(Datum==max(Datum) &
                                    Vorwarnzeit==min(bundeslaender_table_faktenblatt %>%
                                                       filter(Datum==max(Datum)) %>%
                                                       pull(Vorwarnzeit))) %>%
                           pull(Bundesland), ", ")), # kürzeste
    paste0(max(bundeslaender_table_faktenblatt %>%
                 filter(Datum==max(Datum)) %>%
                 pull(Vorwarnzeit)),
           " Tage\n",
           glue_collapse(bundeslaender_table_faktenblatt %>%
                           filter(Datum==max(Datum) &
                                    Vorwarnzeit==max(bundeslaender_table_faktenblatt %>%
                                                       filter(Datum==max(Datum)) %>%
                                                       pull(Vorwarnzeit))) %>%
                           pull(Bundesland), ", ")) # längste
  ),
  Veraenderung=c(
    bundeslaender_table_faktenblatt %>%
      filter(Datum==max(Datum) & Bundesland=="Gesamt") %>%
      pull(Vorwarnzeit) - bundeslaender_table_faktenblatt %>%
      filter(Datum==max(Datum)-7 & Bundesland=="Gesamt") %>%
      pull(Vorwarnzeit), # Bundesdurchschnitt
    min(bundeslaender_table_faktenblatt %>%
          filter(Datum==max(Datum)) %>%
          pull(Vorwarnzeit)) - min(bundeslaender_table_faktenblatt %>%
                              filter(Datum==max(Datum)-7) %>%
                              pull(Vorwarnzeit)), # kürzeste
    max(bundeslaender_table_faktenblatt %>%
          filter(Datum==max(Datum)) %>%
          pull(Vorwarnzeit)) - max(bundeslaender_table_faktenblatt %>%
                              filter(Datum==max(Datum)-7) %>%
                              pull(Vorwarnzeit)) # längste
  )
)  %>%
  select(Vorwarnzeit, Vorwoche, !!paste0("KW ", isoweek(max(bundeslaender_table_faktenblatt$Datum))):=dieseWoche, Veraenderung)


rki <- rki %>% mutate(KW=isoweek(Meldedatum),
                      YearKW=ifelse(KW==53, 202053, year(Meldedatum)*100+KW))
thisKW <- max(rki$YearKW)
sterbeKW <- thisKW-5
sterbeJahr <- floor(sterbeKW/100)
vorsterbeKW <- ifelse(sterbeKW==202101, 202053, sterbeKW-1)
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

maxdividate <- maxdate # max(divi_all$daten_stand)
divi0 <- divi_all %>%
  filter(id==0) %>%
  mutate(auslastungcovid=faelle_covid_aktuell/ICU_Betten,
         quotefrei=betten_frei/ICU_Betten)
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
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(auslastungcovid))*100)), 
           " %\n",
           divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(faelle_covid_aktuell)),
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(quotefrei))*100)), 
           " %\n",
           divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(betten_frei))
  ),
  dieseWoche=c(
    divi0 %>% filter(daten_stand==maxdividate) %>% pull(ICU_Betten),
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate) %>% pull(auslastungcovid))*100)), 
           " %\n",
           divi0 %>% filter(daten_stand==maxdividate) %>% pull(faelle_covid_aktuell)),
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate) %>% pull(quotefrei))*100)), 
           " %\n",
           divi0 %>% filter(daten_stand==maxdividate) %>% pull(betten_frei))
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
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,
                             "0-59")) %>%
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
  summarise(`Faelle_letzte_7_Tage_0-59`=sum(`Fälle_0-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), .groups="drop") %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select(., `unter 20`:`40 bis 60`))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)))
vorwoche_letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Meldedatum<=maxdate-7) %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("60-79","80+"),
                             Altersgruppe,
                             "0-59")) %>%
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
  summarise(`Faelle_letzte_7_Tage_0-59`=sum(`Fälle_0-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), .groups="drop") %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select(., `unter 20`:`40 bis 60`))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)))

rwert7ti <- tibble(
  `R-Wert & 7-Tage-Inzidenz`=c(
    "Reproduktionszahl R",
    "Neue Fälle je 100.000 EW in 7 Tagen bezogen auf die jeweilige Gruppe:",
    "Gesamtbevölkerung",
    "Unter-60-Jährige",
    "Über-60-Jährige",
    "- Davon 60-bis-79-Jährige",
    "- Davon Über-80-Jährige",
    "Regionen mit 7-TI bei Über-60-Jährigen:",
    "> 35",
    "> 50"#,
    # "Regionen mit 7-TI bei Über-80-Jährigen:",
    # "> 35",
    # "> 50"
  ),
  Vorwoche=c(
    round(bundeslaender_table_faktenblatt %>% filter(Bundesland=="Gesamt" & Datum==maxdate-7) %>% pull(`R(t)`), 2),
    NA,
    round(bundeslaender_table_faktenblatt %>% filter(Bundesland=="Gesamt" & Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz`)),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`),
    round(bundeslaender_table_faktenblatt %>% filter(Bundesland=="Gesamt" & Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 60+`)),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_60-79`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_80+`),
    NA,
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 60+`))>35, na.rm=TRUE)),
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE))#,
    # NA,
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 80+`))>35, na.rm=TRUE)),
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 80+`))>50, na.rm=TRUE))
  ),
  dieseWoche=c(
    round(bundeslaender_table_faktenblatt %>% filter(Bundesland=="Gesamt" & Datum==maxdate) %>% pull(`R(t)`), 2),
    NA,
    round(bundeslaender_table_faktenblatt %>% filter(Bundesland=="Gesamt" & Datum==maxdate) %>% pull(`7-Tage-Inzidenz`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`)),
    round(bundeslaender_table_faktenblatt %>% filter(Bundesland=="Gesamt" & Datum==maxdate) %>% pull(`7-Tage-Inzidenz 60+`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_60-79`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_80+`)),
    NA,
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 60+`))>35, na.rm=TRUE)),
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE))#,
    # NA,
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 80+`))>35, na.rm=TRUE)),
    # round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 80+`))>50, na.rm=TRUE))
  ),
  Veraenderung=ifelse(is.na(Vorwoche), NA, paste0(format(round(100*(dieseWoche-Vorwoche)/Vorwoche, 1), decimal.mark = ","), " %"))
)
rwert7ti <- rwert7ti %>%
  mutate(Vorwoche = replace(Vorwoche, 1, format(as.numeric(rwert7ti[1, 2]), decimal.mark=",")),
         dieseWoche = replace(dieseWoche, 1, format(as.numeric(rwert7ti[1, 3]), decimal.mark=",")))  %>%
  select(`R-Wert & 7-Tage-Inzidenz`, Vorwoche, !!paste0("KW ", isoweek(maxdate)):=dieseWoche, Veraenderung)


testmaxkw <- max(almev$KW)
testmaxjahr <- floor(testmaxkw/100)
vortestmaxkw <- ifelse(testmaxkw==202101, 202053, testmaxkw-1)
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

ifsgmaxkw <- max(rki_ifsg$KW, rki_hosp$YearKW, na.rm = TRUE) # min(max(rki_ifsg$KW), max(rki_hosp$KW))
ifsgmaxjahr <- floor(ifsgmaxkw/100)
vorifsgmaxkw <- ifelse(ifsgmaxkw==202101, 202053, ifsgmaxkw-1)
vorifsgmaxjahr <- floor(vorifsgmaxkw/100)
c19erkranktetabelle <- tibble(
  Erkrankte=c(
    "Ohne Symptomatik",
    "Nicht stationär behandelt",
    "Intensivmedizinisch behandelt (Schätzung)",
    "Klinik- und Praxispersonal",
    "Neuinfizierte",
    "Neu stationär behandelt",
    "Neu verstorben",
    "Betreut nach IfSG §36 (u.a. Pflegewohnheim)",
    "Neuinfizierte",
    "Neuinfizierte über 60 J.",
    "Neu stationär behandelt",
    "Neu verstorben"
  ),
  Vorwoche=c(
    paste0(format(round(100*(rki_hosp %>% filter(YearKW==vorifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark = ","), " %"),
    paste0(format(round(100-100*(rki_hosp %>% filter(YearKW==vorifsgmaxkw) %>% pull(`Anteil hospitalisiert`)), 1), decimal.mark = ","), " %"),
    paste0(format(agefatality_data %>% filter(Meldedatum==maxdate-7 & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark = ","), " %"),
    NA,
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuinfiziert),
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neustationaer),
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuverstorben),
    NA,
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert),
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert60),
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neustationaer),
    rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuverstorben)
  ),
  dieseWoche=c(
    paste0(format(round(100*(rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark = ","), " %"),
    paste0(format(round(100-100*(rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil hospitalisiert`)), 1), decimal.mark = ","), " %"),
    paste0(format(agefatality_data %>% filter(Meldedatum==maxdate & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark = ","), " %"),
    NA,
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuinfiziert),
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neustationaer),
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuverstorben),
    NA,
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert),
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert60),
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neustationaer),
    rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuverstorben)
  ),
  Veraenderung=c(
    paste0(format(round(100*(rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)-rki_hosp %>% filter(YearKW==ifsgmaxkw-1) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark=","), " PP"),
    paste0(format(round(100*(rki_hosp %>% filter(YearKW==vorifsgmaxkw) %>% pull(`Anteil hospitalisiert`)-rki_hosp %>% filter(YearKW==ifsgmaxkw) %>% pull(`Anteil hospitalisiert`)), 1), decimal.mark=","), " PP"),
    paste0(format(agefatality_data %>% filter(Meldedatum==maxdate & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil)-agefatality_data %>% filter(Meldedatum==maxdate-7 & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark=","), " PP"),
    NA,
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuinfiziert) - rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuinfiziert))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neuinfiziert)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neustationaer)- rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neustationaer))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg23neustationaer)), " %"),
    NA,
    NA,
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert) - rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert60) - rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert60))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuinfiziert60)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neustationaer)- rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neustationaer))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neustationaer)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuverstorben)- rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuverstorben))/rki_ifsg %>% filter(KW==vorifsgmaxkw) %>% pull(ifsg36neuverstorben)), " %")
  )
) %>%
  select(Erkrankte, Vorwoche, !!paste0("KW ", ifsgmaxkw-ifsgmaxjahr*100):=dieseWoche, Veraenderung)

vaccmaxdate <- max(vacc_zahlen$date)
vorvaccmaxdate <- vaccmaxdate-7
# vacc_brd <- vacc_alle_faktenblatt %>% filter(geo=="Deutschland")
vacc_brd <- vacc_zahlen %>% filter(region=="DE" & date==vaccmaxdate) %>% 
  pivot_wider(names_from = metric, values_from = value)
vacc_brd_vorwoche <- vacc_zahlen %>% 
  filter(region=="DE" & date==vorvaccmaxdate) %>% 
  pivot_wider(names_from = metric, values_from = value)
# Geimpfte Personen
geimpfte_gesamt <- tibble(
  "Geimpfte Personen"=c(
    "Gesamtbevölkerung",
    "Gesamt",
    "Nicht vollst. geimpft",
    "Vollst. geimpft"
  ),
  Vorwoche=c(
    NA,
    vacc_brd_vorwoche$personen_erst_kumulativ + 
      vacc_brd_vorwoche$dosen_janssen_kumulativ,
    vacc_brd_vorwoche$personen_erst_kumulativ - 
      vacc_brd_vorwoche$personen_voll_kumulativ + 
      vacc_brd_vorwoche$dosen_janssen_kumulativ,
    vacc_brd_vorwoche$personen_voll_kumulativ
  ),
  VorwocheAnteil=c(
    NA,
    (vacc_brd_vorwoche$personen_erst_kumulativ + 
       vacc_brd_vorwoche$dosen_janssen_kumulativ)/83166711*100,
    (vacc_brd_vorwoche$personen_erst_kumulativ - 
       vacc_brd_vorwoche$personen_voll_kumulativ + 
       vacc_brd_vorwoche$dosen_janssen_kumulativ)/83166711*100,
    (vacc_brd_vorwoche$personen_voll_kumulativ)/83166711*100
  ),
  dieseWoche=c(
    NA,
    vacc_brd$personen_erst_kumulativ + vacc_brd$dosen_janssen_kumulativ,
    vacc_brd$personen_erst_kumulativ - 
      vacc_brd$personen_voll_kumulativ + vacc_brd$dosen_janssen_kumulativ,
    vacc_brd$personen_voll_kumulativ
  ),
  dieseWocheAnteil=c(
    NA,
    (vacc_brd$personen_erst_kumulativ + 
       vacc_brd$dosen_janssen_kumulativ)/83166711*100,
    (vacc_brd$personen_erst_kumulativ - 
       vacc_brd$personen_voll_kumulativ + 
       vacc_brd$dosen_janssen_kumulativ)/83166711*100,
    (vacc_brd$personen_voll_kumulativ)/83166711*100
  )
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

impfkw <- thisKW-1
vorimpfkw <- impfkw-1
# Impffortschritt
fortschritt_table <- tibble(
  "Impffortschritt"=c(
    "Impfungen pro Woche",
    "Gesamt",
    "davon in Impfzentren",
    "davon in ärztl. Praxen"
  ),
  "Vorwoche"=c(
    NA,
    impfdax_data %>% filter(JahrKW==vorimpfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`) +
      impfdax_data %>% filter(JahrKW==vorimpfkw) %>%
      pull(`Anzahl_Impfungen Impfzentren`),
    impfdax_data %>% filter(JahrKW==vorimpfkw) %>% 
      pull(`Anzahl_Impfungen Impfzentren`),
    impfdax_data %>% filter(JahrKW==vorimpfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`)
  ),
  "dieseWoche"=c(
    NA,
    impfdax_data %>% filter(JahrKW==impfkw) %>% 
      pull(`Anzahl_Impfungen Arztpraxen`) +
      impfdax_data %>% filter(JahrKW==impfkw) %>%
      pull(`Anzahl_Impfungen Impfzentren`),
    impfdax_data %>% filter(JahrKW==impfkw) %>% 
      pull(`Anzahl_Impfungen Impfzentren`),
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
bl_impfungen <- vacc_table_faktenblatt %>%
  select(Bundesland, 
         `Gesamt min. 1x`,
         `Gesamt vollst.`,
         `7-Tage-Inzidenz`,
         `7-Tage-Inzidenz 60+`) %>% 
  left_join(impfen_praxen_letztekw %>% 
              select(Bundesland, Impfungen), by="Bundesland") %>% 
  select("Bundesland", "Impfungen Praxen"=Impfungen,
         `Gesamt min. 1x`,
         `Gesamt vollst.`,
         `7-Tage-Inzidenz`,
         `7-Tage-Inzidenz 60+`)

hersteller_brd <- vacc_zahlen %>%
  filter(date==vaccmaxdate & region=="DE")
hersteller_brd_vorwoche <- vacc_zahlen %>%
  filter(date==vaccmaxdate-7 & region=="DE")
geliefert <- impfdashboardde %>% 
  group_by(impfstoff) %>% 
  summarise(dosen_geliefert=sum(dosen))
geliefert_vorwoche <- impfdashboardde %>% 
  filter(date<=today()-7) %>% 
  group_by(impfstoff) %>% 
  summarise(dosen_geliefert=sum(dosen))

# Impfstoffdosen
hersteller_table <- tibble(
  "Impfstoffdosen"=c(
    "Biontech/Pfizer",
    "Erstimpfungen",
    "Zweitimpfungen",
    "geliefert",
    "Moderna",
    "Erstimpfungen",
    "Zweitimpfungen",
    "geliefert",
    "AstraZeneca",
    "Erstimpfungen",
    "Zweitimpfungen",
    "geliefert",
    "Johnson&Johnson",
    "geliefert"
  ),
  "Vorwoche"=c(
    hersteller_brd_vorwoche %>% filter(metric=="dosen_biontech_kumulativ") %>% pull(value),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_erst_biontech_kumulativ") %>% pull(value),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_voll_biontech_kumulativ") %>% pull(value),
    geliefert_vorwoche %>% filter(impfstoff=="comirnaty") %>% pull(dosen_geliefert),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_moderna_kumulativ") %>% pull(value),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_erst_moderna_kumulativ") %>% pull(value),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_voll_moderna_kumulativ") %>% pull(value),
    geliefert_vorwoche %>% filter(impfstoff=="moderna") %>% pull(dosen_geliefert),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_astrazeneca_kumulativ") %>% pull(value),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_erst_astrazeneca_kumulativ") %>% pull(value),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_voll_astrazeneca_kumulativ") %>% pull(value),
    geliefert_vorwoche %>% filter(impfstoff=="astra") %>% pull(dosen_geliefert),
    hersteller_brd_vorwoche %>% filter(metric=="dosen_voll_janssen_kumulativ") %>% pull(value),
    geliefert_vorwoche %>% filter(impfstoff=="johnson") %>% pull(dosen_geliefert)
  ),
  "dieseWoche"=c(
    hersteller_brd %>% filter(metric=="dosen_biontech_kumulativ") %>% pull(value),
    hersteller_brd %>% filter(metric=="dosen_erst_biontech_kumulativ") %>% pull(value),
    hersteller_brd %>% filter(metric=="dosen_voll_biontech_kumulativ") %>% pull(value),
    geliefert %>% filter(impfstoff=="comirnaty") %>% pull(dosen_geliefert),
    hersteller_brd %>% filter(metric=="dosen_moderna_kumulativ") %>% pull(value),
    hersteller_brd %>% filter(metric=="dosen_erst_moderna_kumulativ") %>% pull(value),
    hersteller_brd %>% filter(metric=="dosen_voll_moderna_kumulativ") %>% pull(value),
    geliefert %>% filter(impfstoff=="moderna") %>% pull(dosen_geliefert),
    hersteller_brd %>% filter(metric=="dosen_astrazeneca_kumulativ") %>% pull(value),
    hersteller_brd %>% filter(metric=="dosen_erst_astrazeneca_kumulativ") %>% pull(value),
    hersteller_brd %>% filter(metric=="dosen_voll_astrazeneca_kumulativ") %>% pull(value),
    geliefert %>% filter(impfstoff=="astra") %>% pull(dosen_geliefert),
    hersteller_brd %>% filter(metric=="dosen_voll_janssen_kumulativ") %>% pull(value),
    geliefert %>% filter(impfstoff=="johnson") %>% pull(dosen_geliefert)
  )
) %>%
  mutate(anteil_vorwoche=paste0(" (", 
                       format(round(100*Vorwoche/(Vorwoche[1]+Vorwoche[5]+Vorwoche[9]+Vorwoche[13]), 
                                    1), 
                              decimal.mark=","), 
                       " %)"),
         anteil=paste0(" (", 
                       format(round(100*dieseWoche/(dieseWoche[1]+dieseWoche[5]+dieseWoche[9]+dieseWoche[13]), 
                                    1), 
                              decimal.mark=","), 
                       " %)")) %>%
  mutate(Vorwoche=ifelse(
    Impfstoffdosen%in%c("Biontech/Pfizer", "Moderna", "AstraZeneca", "Johnson&Johnson"), 
    paste0(Vorwoche, anteil_vorwoche),
    Vorwoche),
         dieseWoche=ifelse(
    Impfstoffdosen%in%c("Biontech/Pfizer", "Moderna", "AstraZeneca", "Johnson&Johnson"), 
    paste0(dieseWoche, anteil),
    dieseWoche)) %>%
  select(-anteil, -anteil_vorwoche)



library(openxlsx)
list_of_datasets <- list("Testungen"=testtabelle,
                         "R-Wert und 7-Tage-Inzidenz" = rwert7ti,
                         "Intensivbetten"=itstabelle,
                         "COVID-19-Erkrankte"=c19erkranktetabelle,
                         "Todesfälle und Fallsterblichkeit"=sterbetabelle,
                         "Vorwarnzeit"=vwztabelle,
                         "Regionale Daten"=bltabelle,
                         "Internationaler Vergleich"=EUmal4tabelle,
                         "Geimpfte Personen"=geimpfte_gesamt,
                         "Impffortschritt"=fortschritt_table,
                         "Regional Geimpfte"=bl_impfungen,
                         "Impfstoffdosen"=hersteller_table)
write.xlsx(list_of_datasets, file = paste0("../data/kbvreport_export/faktenblatttabellen_", maxdate, ".xlsx"))

