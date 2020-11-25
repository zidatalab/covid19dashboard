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

## Destatis 2019 https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/liste-altersgruppen.html
altersgruppen_bund <- tibble("unter 20"=18.4,
                             "20 bis 40"=24.6,
                             "40 bis 60"=28.4,
                             "60 bis 80"=21.7,
                             "80+"=6.8)/100

bundeslaender_table<- read_json("./data/tabledata/bundeslaender_table.json",
                                simplifyVector = TRUE)
kreise_table<- read_json("./data/tabledata/kreise_table.json",
                                simplifyVector = TRUE)
bundeslaender_r_und_vwz_data <- read_json("./data/plotdata/bundeslaender_r_und_vwz.json",
                                          simplifyVector = TRUE) %>%
  mutate(Datum=as_date(Datum))

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

## rki imputation because of delayed gesundheitsamt-meldungen
rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)), maxdate=max(date(Meldedatum)))
rkiidkreise <- unique(rki$IdLandkreis)
rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
Kreise_allvalues <- expand_grid(Meldedatum=rkitimeframe$maxdate, IdLandkreis=rkiidkreise)
Kreise <- rki %>%
  mutate(Meldedatum=date(Meldedatum)) %>%
  full_join(., Kreise_allvalues, by=c("Meldedatum", "IdLandkreis"))
n_missingkreise <- 0
for (idkreis in rkiidkreise) { # take care of delayed reporting Landkreise (e.g. Hamburg 02000)
  if (is.na((Kreise %>% filter(Meldedatum==rkitimeframe$maxdate & IdLandkreis==idkreis) %>% pull(AnzahlFall))[1])) {
    n_missingkreise <- n_missingkreise+1
    sixdaysbefore <- Kreise %>% filter(Meldedatum>=rkitimeframe$maxdate-6 & IdLandkreis==idkreis)
    for (ag in rkiagegroups) {
      Kreise <- Kreise %>%
        add_row(IdBundesland=sixdaysbefore$IdBundesland[1],
                Bundesland=sixdaysbefore$Bundesland[1],
                Landkreis=sixdaysbefore$Landkreis[1],
                Altersgruppe=ag,
                Geschlecht="unbekannt",
                AnzahlFall=round(sum(sixdaysbefore$AnzahlFall[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6),
                AnzahlTodesfall=round(sum(sixdaysbefore$AnzahlTodesfall[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6),
                Meldedatum=rkitimeframe$maxdate,
                IdLandkreis=idkreis,
                AnzahlGenesen=round(sum(sixdaysbefore$AnzahlGenesen[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6))
    }
  }
}
cat("Kreise ohne aktuelle Meldung: ", n_missingkreise, "\n")
rki <- Kreise

eumaxdate <- max(international$date)
eutabelle <- international %>%
  filter(date >= eumaxdate-14) %>%
  group_by(Country) %>%
  summarise(`COVID-19-Fälle`=max(cases),
            `Todesfälle`=max(deaths),
            `Neue Fälle je 100.000 EW in 14 Tagen`=round((max(cases)-min(cases))/EW_insgesamt*100000),
            `Todesfälle je 100.000 EW in 14 Tagen`=round((max(deaths)-min(deaths))/EW_insgesamt*100000, 1),
            .groups="drop") %>%
  distinct()

bltabelle <- bind_rows(
  bundeslaender_table %>%
    filter(Bundesland=="Gesamt"),
  bundeslaender_table %>%
    filter(Bundesland!="Gesamt") %>%
    arrange(Bundesland)
) %>%
  select(Bundesland, `R(t)`, `7-Tage-Inzidenz`, `7-Tage-Inzidenz 60+`, Vorwarnzeit=`Vorwarnzeit aktuell`)

vwztabelle <- tibble(
  Vorwarnzeit=c(
    "Bundesdurchschnitt",
    "kürzeste",
    "längste"
  ),
  Vorwoche=c(
    paste0(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
                 pull(Wert),
           " Tage"), # Bundesdurchschnitt
    paste0(min(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
             filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" &
                      Wert==min(bundeslaender_r_und_vwz_data %>%
                                  filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                                  pull(Wert))) %>%
             pull(name), ", ")), # kürzeste
    paste0(max(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
                           filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" &
                                    Wert==max(bundeslaender_r_und_vwz_data %>%
                                                filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                                                pull(Wert))) %>%
                           pull(name), ", ")) # längste
  ),
  dieseWoche=c(
    paste0(bundeslaender_r_und_vwz_data %>%
             filter(Datum==max(Datum) & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
             pull(Wert),
           " Tage"), # Bundesdurchschnitt
    paste0(min(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
             filter(Datum==max(Datum) & Variable=="Vorwarnzeit" &
                      Wert==min(bundeslaender_r_und_vwz_data %>%
                                  filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                                  pull(Wert))) %>%
             pull(name), ", ")), # kürzeste
    paste0(max(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
                           filter(Datum==max(Datum) & Variable=="Vorwarnzeit" &
                                    Wert==max(bundeslaender_r_und_vwz_data %>%
                                                filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                                                pull(Wert))) %>%
                           pull(name), ", ")) # längste
  ),
  Veraenderung=c(
    bundeslaender_r_und_vwz_data %>%
      filter(Datum==max(Datum) & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
      pull(Wert) - bundeslaender_r_und_vwz_data %>%
      filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
      pull(Wert), # Bundesdurchschnitt
    min(bundeslaender_r_und_vwz_data %>%
          filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
          pull(Wert)) - min(bundeslaender_r_und_vwz_data %>%
                              filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                              pull(Wert)), # kürzeste
    max(bundeslaender_r_und_vwz_data %>%
          filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
          pull(Wert)) - max(bundeslaender_r_und_vwz_data %>%
                              filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                              pull(Wert)) # längste
  )
)

rki <- rki %>% mutate(KW=isoweek(Meldedatum))
thisKW <- max(rki$KW)
sterbeKW <- thisKW-4
vorsterbeKW <- sterbeKW-1
sterbestichtag <- max(rki%>%filter(KW==sterbeKW)%>%pull(Meldedatum))
vorsterbestichtag <- max(rki%>%filter(KW==vorsterbeKW)%>%pull(Meldedatum))
sterberki <- rki %>% filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
  group_by(Altersgruppe) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop") %>%
  filter(Altersgruppe!="unbekannt") %>%
  bind_rows(., rki %>% filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
              summarise(Altersgruppe="Gesamt", Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop"))
vorsterberki <- rki %>% filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
  group_by(Altersgruppe) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop") %>%
  filter(Altersgruppe!="unbekannt") %>%
  bind_rows(., rki %>% filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
              summarise(Altersgruppe="Gesamt", Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop"))
sterbetabelle <- tibble(
  `Todesfälle & Sterblichkeit`=c(
    "0 bis 4 Jahre",
    "5 bis 14 Jahre",
    "15 bis 34 Jahre",
    "35 bis 59 Jahre",
    "60 bis 79 Jahre",
    "80 Jahre +",
    "Gesamt"
  ),
  Vorwoche=c(
    vorsterberki %>% pull(Todesfaelle)
  ),
  Vorwoche_sterblichkeit=c(
    vorsterberki %>% pull(Sterblichkeit)
  ),
  KWX=c(
    sterberki %>% pull(Todesfaelle)
  ),
  KWX_sterblichkeit=c(
    sterberki %>% pull(Sterblichkeit)
  ),
  Veraenderung=paste0(round(100*(KWX-Vorwoche)/Vorwoche, 1), "%")
) %>%
  mutate(Vorwoche=ifelse(Vorwoche==0, 0, paste0(Vorwoche, " (", format(round(100*Vorwoche_sterblichkeit, 1), decimal.mark=","), "%)")),
         KWX=ifelse(KWX==0, 0, paste0(KWX, " (", format(round(100*KWX_sterblichkeit, 1), decimal.mark=","), "%)"))) %>%
  select(`Todesfälle & Sterblichkeit`, Vorwoche, KWX, Veraenderung)

maxdividate <- max(divi_all$daten_stand)
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
  Veraenderung=format(round(100*(dieseWoche-Vorwoche)/Vorwoche, 1), decimal.mark = ",")
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
)

rvwzmaxdate <- max(bundeslaender_r_und_vwz_data$Datum)
maxdatum <- max(as_date(rki$Meldedatum))
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
  filter(date>=maxdatum-6) %>%
  summarise(`Faelle_letzte_7_Tage_0-59`=sum(`Fälle_0-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), .groups="drop") %>%
  bind_cols(., altersgruppen_bund*strukturdaten%>%filter(id==0)%>%pull(EW_insgesamt)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`=round(`Faelle_letzte_7_Tage_0-59`/(sum(select(., `unter 20`:`40 bis 60`))/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(`60 bis 80`/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(`80+`/100000)))
vorwoche_letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Meldedatum<=maxdatum-7) %>%
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
  filter(date>=maxdatum-6-7) %>%
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
    "> 50"
  ),
  Vorwoche=c(
    bundeslaender_r_und_vwz_data %>% filter(id==0 & Datum==rvwzmaxdate-7 & Variable=="R") %>% pull(Wert),
    NA,
    round((vorwoche_letzte_7_tage_altersgruppen_bund$`Faelle_letzte_7_Tage_0-59`+vorwoche_letzte_7_tage_altersgruppen_bund$`Faelle_letzte_7_Tage_60-79`+vorwoche_letzte_7_tage_altersgruppen_bund$`Faelle_letzte_7_Tage_80+`)/83166711*100000),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`),
    round((vorwoche_letzte_7_tage_altersgruppen_bund$`Faelle_letzte_7_Tage_60-79`+vorwoche_letzte_7_tage_altersgruppen_bund$`Faelle_letzte_7_Tage_80+`)/(letzte_7_tage_altersgruppen_bund$`60 bis 80`+letzte_7_tage_altersgruppen_bund$`80+`)*100000),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_60-79`),
    vorwoche_letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_80+`),
    NA,
    NA,
    NA
    ),
  dieseWoche=c(
    bundeslaender_r_und_vwz_data %>% filter(id==0 & Datum==rvwzmaxdate & Variable=="R") %>% pull(Wert),
    NA,
    round(bundeslaender_table %>% filter(Bundesland=="Gesamt") %>% pull(`7-Tage-Inzidenz`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_0-59`)),
    round(bundeslaender_table %>% filter(Bundesland=="Gesamt") %>% pull(`7-Tage-Inzidenz 60+`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_60-79`)),
    round(letzte_7_tage_altersgruppen_bund %>% pull(`Faelle_letzte_7_Tage_je100TsdEinw_80+`)),
    NA,
    round(sum((kreise_table %>% pull(`7-Tage-Inzidenz 60+`))>35, na.rm=TRUE)),
    round(sum((kreise_table %>% pull(`7-Tage-Inzidenz 60+`))>50, na.rm=TRUE))
  )
)

write_excel_csv(bltabelle, "./data/faktenblatt_bltabelle.csv")
write_excel_csv(eutabelle, "./data/faktenblatt_eutabelle.csv")
