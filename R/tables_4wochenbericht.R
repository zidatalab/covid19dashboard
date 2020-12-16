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
                       sheet = "Daten", 
                       skip = 2)

bundeslaender_table_faktenblatt <- read_json("../data/tabledata/bundeslaender_table_faktenblatt.json",
                                simplifyVector = TRUE) %>%
  mutate(Datum=as_date(Datum))
kreise_table_faktenblatt <- read_json("../data/tabledata/kreise_table_faktenblatt.json",
                                simplifyVector = TRUE) %>%
  mutate(Datum=as_date(Datum))

agefatality_data <- read_json("../data/plotdata/agefatality.json",
                                             simplifyVector = TRUE) %>%
  mutate(Meldedatum=as_date(Meldedatum))

sterbefaelle_kw <- bind_rows(read_excel(destfile_sterblk, 
                                        sheet = "D_2016_2020_KW_AG_Männlich", 
                                        skip = 8) %>% mutate(sex="maennlich"),
                             read_excel(destfile_sterblk, 
                                        sheet = "D_2016_2020_KW_AG_Weiblich", 
                                        skip = 8) %>% mutate(sex="weiblich")) %>%
  select(-"Nr.") %>% 
  rename("Jahr"="...2", "Alter"= "unter … Jahren" ) %>%
  relocate(Jahr,Alter,sex) %>% 
  pivot_longer(cols=-c("Jahr", "Alter", "sex"), names_to="KW", values_to="Tote")
sterbefaelle_kw.rec <- 
  left_join(sterbefaelle_kw %>% filter(Jahr==2020 & !is.na(Tote)),
            sterbefaelle_kw %>% filter(Jahr<2020) %>%
              group_by(Alter,KW,sex) %>% 
              summarise(Tote_2016_2019=mean(Tote,na.rm=T), .groups="drop"),
            by=c("KW","Alter","sex")) %>% 
  mutate(
    KW=as.numeric(KW),
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
  group_by(agegrp, KW) %>% 
  summarise(
    Tote_diff=sum(Tote)-sum(Tote_2016_2019),
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
rki <- tbl(conn,"rki") %>% collect()
params <- tbl(conn,"params") %>% select(name, EW_insgesamt) %>% collect()
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
rki <- rki %>% mutate(Meldedatum=as_date(Meldedatum))
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

maxdate <- max(bundeslaender_table_faktenblatt$Datum)
bltabelle <- bind_rows(
  bundeslaender_table_faktenblatt %>%
    filter(Bundesland=="Gesamt" & Datum==maxdate),
  bundeslaender_table_faktenblatt %>%
    filter(Bundesland!="Gesamt" & Datum==maxdate) %>%
    arrange(Bundesland)
) %>%
  select(Bundesland, `R(t)`, `7-Tage-Inzidenz`, `7-Tage-Inzidenz 60+`, Vorwarnzeit=`Vorwarnzeit`)

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
EUmal4tabelle <- tibble(
  `Fälle gesamt`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-AnteilBev) %>% pull(german),
  `Anteil Bevölk.`=eutabelle %>% arrange(-AnteilBev) %>% filter(row_number()<=10) %>% pull(`COVID-19-Fälle Anteil Bev.`),
  `Anzahl Fälle`=eutabelle %>% arrange(-AnteilBev) %>% filter(row_number()<=10) %>% pull(`COVID-19-Fälle`),
  `Todesfälle`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-Fallsterb) %>% pull(german),
  `Fallsterblichkeit`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-Fallsterb) %>% pull(Fallsterblichkeit),
  `Anzahl Todesfälle`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-Fallsterb) %>% pull(`Todesfälle`),
  `Länder nach neuen Fällen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Neue Fälle je 100.000 EW in 14 Tagen`) %>% pull(german),
  `Neue Fälle je 100.000 EW in 14 Tagen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Neue Fälle je 100.000 EW in 14 Tagen`) %>% pull(`Neue Fälle je 100.000 EW in 14 Tagen`),
  `Länder nach neuen Todesfällen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Todesfälle je 100.000 EW in 14 Tagen`) %>% pull(german),
  `Todesfälle je 100.000 EW in 14 Tagen`=eutabelle %>% filter(german%in%top10eu) %>% arrange(-`Todesfälle je 100.000 EW in 14 Tagen`) %>% pull(`Todesfälle je 100.000 EW in 14 Tagen`)
) %>%
  mutate(`Todesfälle je 100.000 EW in 14 Tagen`=format(`Todesfälle je 100.000 EW in 14 Tagen`, decimal.mark = ","))

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


rki <- rki %>% mutate(KW=isoweek(Meldedatum))
thisKW <- max(rki$KW)
sterbeKW <- thisKW-5
vorsterbeKW <- sterbeKW-1
sterbestichtag <- max(rki%>%filter(KW==sterbeKW)%>%pull(Meldedatum))
vorsterbestichtag <- max(rki%>%filter(KW==vorsterbeKW)%>%pull(Meldedatum))
sterberki <- rki %>% filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
  mutate(Altersgruppe3=case_when(
    Altersgruppe=="A80+" ~ "80+",
    Altersgruppe=="A60-A79" ~ "60-79",
    Altersgruppe=="unbekannt" ~ "unbekannt",
    TRUE ~ "0-59"
  )) %>%
  group_by(Altersgruppe3) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop") %>%
  filter(Altersgruppe3!="unbekannt") %>%
  bind_rows(., rki %>% filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
              summarise(Altersgruppe3="Gesamt", Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop"))
vorsterberki <- rki %>% filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
  mutate(Altersgruppe3=case_when(
    Altersgruppe=="A80+" ~ "80+",
    Altersgruppe=="A60-A79" ~ "60-79",
    Altersgruppe=="unbekannt" ~ "unbekannt",
    TRUE ~ "0-59"
  )) %>%
  group_by(Altersgruppe3) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop") %>%
  filter(Altersgruppe3!="unbekannt") %>%
  bind_rows(., rki %>% filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
              summarise(Altersgruppe3="Gesamt", Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop"))
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
    sterbefaelle_kw.rec %>% filter(KW==vorsterbeKW) %>% pull(Tote_diff)
  ),
  Vorwoche_sterblichkeit=c(
    vorsterberki %>% pull(Sterblichkeit),
    NA,
    sterbefaelle_kw.rec %>% filter(KW==vorsterbeKW) %>% pull(Vergleich)
  ),
  KWX=c(
    sterberki %>% pull(Todesfaelle),
    NA,
    sterbefaelle_kw.rec %>% filter(KW==sterbeKW) %>% pull(Tote_diff)
  ),
  KWX_sterblichkeit=c(
    sterberki %>% pull(Sterblichkeit),
    NA,
    sterbefaelle_kw.rec %>% filter(KW==sterbeKW) %>% pull(Vergleich)
  ),
  Veraenderung=ifelse(is.na(KWX), NA, paste0(format(round(100*(KWX-Vorwoche)/Vorwoche, 1), decimal.mark = ","), "%"))
) %>%
  mutate(Vorwoche=ifelse(Vorwoche==0, 0, paste0(Vorwoche, " (", format(round(100*Vorwoche_sterblichkeit, 1), decimal.mark=","), "%)")),
         !!paste0("KW ", sterbeKW):=ifelse(KWX==0, 0, paste0(KWX, " (", format(round(100*KWX_sterblichkeit, 1), decimal.mark=","), "%)"))) %>%
  select(`Todesfälle & Sterblichkeit`, Vorwoche, !!paste0("KW ", sterbeKW), Veraenderung)

maxdividate <- maxdate # max(divi_all$daten_stand)
divi0 <- divi_all %>%
  filter(id==0) %>%
  mutate(auslastungcovid=faelle_covid_aktuell/ICU_Betten,
         quotefrei=betten_frei/ICU_Betten)
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
           " %, \n",
           divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(faelle_covid_aktuell)),
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(quotefrei))*100)), 
           " %, \n",
           divi0 %>% filter(daten_stand==maxdividate-7) %>% pull(betten_frei))
  ),
  dieseWoche=c(
    divi0 %>% filter(daten_stand==maxdividate) %>% pull(ICU_Betten),
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate) %>% pull(auslastungcovid))*100)), 
           " %, \n",
           divi0 %>% filter(daten_stand==maxdividate) %>% pull(faelle_covid_aktuell)),
    paste0(round(((divi0 %>% filter(daten_stand==maxdividate) %>% pull(quotefrei))*100)), 
           " %, \n",
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
    "> 50",
    "Regionen mit 7-TI bei Über-80-Jährigen:",
    "> 35",
    "> 50"
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
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE)),
    NA,
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 80+`))>35, na.rm=TRUE)),
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate-7) %>% pull(`7-Tage-Inzidenz 80+`))>50, na.rm=TRUE))
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
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE)),
    NA,
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 80+`))>35, na.rm=TRUE)),
    round(sum((kreise_table_faktenblatt %>% filter(Datum==maxdate) %>% pull(`7-Tage-Inzidenz 80+`))>50, na.rm=TRUE))
  ),
  Veraenderung=ifelse(is.na(Vorwoche), NA, paste0(format(round(100*(dieseWoche-Vorwoche)/Vorwoche, 1), decimal.mark = ","), " %"))
)
rwert7ti <- rwert7ti %>%
  mutate(Vorwoche = replace(Vorwoche, 1, format(as.numeric(rwert7ti[1, 2]), decimal.mark=",")),
         dieseWoche = replace(dieseWoche, 1, format(as.numeric(rwert7ti[1, 3]), decimal.mark=",")))  %>%
  select(`R-Wert & 7-Tage-Inzidenz`, Vorwoche, !!paste0("KW ", isoweek(maxdate)):=dieseWoche, Veraenderung)


testmaxkw <- max(almev$KW)
almev <- almev %>%
  mutate(positivrate=positivtests/pcrtests,
         auslastung=pcrtests/testkapazitaet)
testtabelle <- tibble(
  Testungen=c(
    "Zahl der PCR-Tests",
    "Positive Tests",
    "Positivrate",
    "Testkapazität",
    "Auslastung"
  ),
  Vorwoche=c(
    almev %>% filter(KW==testmaxkw-1) %>% pull(pcrtests),
    almev %>% filter(KW==testmaxkw-1) %>% pull(positivtests),
    paste0(format(round((almev %>% filter(KW==testmaxkw-1) %>% pull(positivrate))*100, 2), decimal.mark = ","), " %"),
    almev %>% filter(KW==testmaxkw-1) %>% pull(testkapazitaet),
    paste0(round((almev %>% filter(KW==testmaxkw-1) %>% pull(auslastung))*100, 0), " %")
  ),
  dieseWoche=c(
    almev %>% filter(KW==testmaxkw) %>% pull(pcrtests),
    almev %>% filter(KW==testmaxkw) %>% pull(positivtests),
    paste0(format(round((almev %>% filter(KW==testmaxkw) %>% pull(positivrate))*100, 2), decimal.mark = ","), " %"),
    almev %>% filter(KW==testmaxkw) %>% pull(testkapazitaet),
    paste0(round((almev %>% filter(KW==testmaxkw) %>% pull(auslastung))*100, 0), " %")
  ),
  Veraenderung=c(
    paste0(round(100*(almev %>% filter(KW==testmaxkw) %>% pull(pcrtests) - almev %>% filter(KW==testmaxkw-1) %>% pull(pcrtests))/almev %>% filter(KW==testmaxkw-1) %>% pull(pcrtests), 0), " %"),
    paste0(format(round(100*(almev %>% filter(KW==testmaxkw) %>% pull(positivtests) - almev %>% filter(KW==testmaxkw-1) %>% pull(positivtests))/almev %>% filter(KW==testmaxkw-1) %>% pull(positivtests), 1), decimal.mark = ","), " %"),
    paste0(format(round((almev %>% filter(KW==testmaxkw) %>% pull(positivrate))*100, 2)- round((almev %>% filter(KW==testmaxkw-1) %>% pull(positivrate))*100, 2), decimal.mark = ","), " PP"),
    paste0(round(100*(almev %>% filter(KW==testmaxkw) %>% pull(testkapazitaet) - almev %>% filter(KW==testmaxkw-1) %>% pull(testkapazitaet))/almev %>% filter(KW==testmaxkw-1) %>% pull(testkapazitaet), 0), " %"),
    paste0(round((almev %>% filter(KW==testmaxkw) %>% pull(auslastung))*100, 0)- round((almev %>% filter(KW==testmaxkw-1) %>% pull(auslastung))*100, 0), " PP")
  )
)  %>%
  select(Testungen, Vorwoche, !!paste0("KW ", testmaxkw):=dieseWoche, Veraenderung)

ifsgmaxkw <- max(rki_ifsg$KW, rki_hosp$KW, na.rm = TRUE) # min(max(rki_ifsg$KW), max(rki_hosp$KW))
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
    paste0(format(round(100*(rki_hosp %>% filter(KW==ifsgmaxkw-1) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark = ","), " %"),
    paste0(format(round(100-100*(rki_hosp %>% filter(KW==ifsgmaxkw-1) %>% pull(`Anteil hospitalisiert`)), 1), decimal.mark = ","), " %"),
    paste0(format(agefatality_data %>% filter(Meldedatum==maxdate-7 & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark = ","), " %"),
    NA,
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neuinfiziert),
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neustationaer),
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neuverstorben),
    NA,
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuinfiziert),
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuinfiziert60),
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neustationaer),
    rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuverstorben)
  ),
  dieseWoche=c(
    paste0(format(round(100*(rki_hosp %>% filter(KW==ifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark = ","), " %"),
    paste0(format(round(100-100*(rki_hosp %>% filter(KW==ifsgmaxkw) %>% pull(`Anteil hospitalisiert`)), 1), decimal.mark = ","), " %"),
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
    paste0(format(round(100*(rki_hosp %>% filter(KW==ifsgmaxkw) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)-rki_hosp %>% filter(KW==ifsgmaxkw-1) %>% pull(`Anteil keine, bzw. keine für COVID-19 bedeutsamen Symptome`)), 1), decimal.mark=","), " PP"),
    paste0(format(round(100*(rki_hosp %>% filter(KW==ifsgmaxkw-1) %>% pull(`Anteil hospitalisiert`)-rki_hosp %>% filter(KW==ifsgmaxkw) %>% pull(`Anteil hospitalisiert`)), 1), decimal.mark=","), " PP"),
    paste0(format(agefatality_data %>% filter(Meldedatum==maxdate & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil)-agefatality_data %>% filter(Meldedatum==maxdate-7 & Merkmal=="ITS-Fälle an Fällen") %>% pull(Anteil), decimal.mark=","), " PP"),
    NA,
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neuinfiziert) - rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neuinfiziert))/rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neuinfiziert)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg23neustationaer)- rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neustationaer))/rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg23neustationaer)), " %"),
    NA,
    NA,
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert) - rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuinfiziert))/rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuinfiziert)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuinfiziert60) - rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuinfiziert60))/rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuinfiziert60)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neustationaer)- rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neustationaer))/rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neustationaer)), " %"),
    paste0(round(100*(rki_ifsg %>% filter(KW==ifsgmaxkw) %>% pull(ifsg36neuverstorben)- rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuverstorben))/rki_ifsg %>% filter(KW==ifsgmaxkw-1) %>% pull(ifsg36neuverstorben)), " %")
  )
) %>%
  select(Erkrankte, Vorwoche, !!paste0("KW ", ifsgmaxkw):=dieseWoche, Veraenderung)

library(openxlsx)
list_of_datasets <- list("Testungen"=testtabelle,
                         "R-Wert und 7-Tage-Inzidenz" = rwert7ti,
                         "Intensivbetten"=itstabelle,
                         "COVID-19-Erkrankte"=c19erkranktetabelle,
                         "Todesfälle und Fallsterblichkeit"=sterbetabelle,
                         "Vorwarnzeit"=vwztabelle,
                         "Regionale Daten"=bltabelle,
                         "Internationaler Vergleich"=EUmal4tabelle)
write.xlsx(list_of_datasets, file = paste0("../data/faktenblatttabellen_", maxdate, ".xlsx"))
