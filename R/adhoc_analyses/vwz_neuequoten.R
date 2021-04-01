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

##### Source files
source("R/aux_functions.R")

infektperiode <- 14
icu_days <- 10.1

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

altersgruppen_beatmet <- as.vector(read_csv(file = "data/its_altersverteilung.csv") %>% 
  pivot_wider(names_from = Altersgruppe,
              values_from = anzahl_beatmet))[c(2,1,3)]

itsquoten <- read_csv("data/itsquoten_final.csv") %>% 
  mutate(quote_its=quote_its/100) %>% 
  pivot_wider(id_cols = c(periode, beginn, ende),
              names_from = Altersgruppe,
              values_from = quote_its)

kreise_regstat_alter <- read_delim("data/Bev2019_Kreis_AG_rki_geschlecht.txt", 
                                   ";",
                                   escape_double = FALSE,
                                   col_types = cols(X1 = col_skip()), 
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

brd_timeseries <- tbl(conn,"brd_timeseries") %>% collect() %>% mutate(date=base::as.Date(date))
rki <- tbl(conn,"rki") %>% collect()
divi <- tbl(conn,"divi") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))
divi_all <- tbl(conn, "divi_all") %>% collect() %>% mutate(daten_stand=as_date(daten_stand))

## if last divi report missing
maxdatum <- max(as_date(rki$Meldedatum))
lastsunday <- floor_date(maxdatum, "week")
sundaybeforelastsunday <- lastsunday-7
if (max(divi$daten_stand<maxdatum)) {
  divi_all <- bind_rows(divi_all, divi %>% mutate(daten_stand=as_date(maxdatum)))
}
## rki imputation because of delayed gesundheitsamt-meldungen
rki_original <- rki
rki <- rki %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
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

rki_alter_destatis_kreise <- rki %>% lazy_dt() %>%
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% # this takes long unfortunately... but much faster with dtplyr!
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  collect() %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("00-04", "05-14"),
                             "0-15", Altersgruppe)) %>%
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
  mutate(cases059=cumsum(`Fälle_0-15`+`Fälle_15-34`+`Fälle_35-59`),
         cases6079=cumsum(`Fälle_60-79`),
         cases80=cumsum(`Fälle_80+`)) %>%
  mutate(Infected059=cases059-lag(cases059, infektperiode),
         Infected6079=cases6079-lag(cases6079, infektperiode),
         Infected80=cases80-lag(cases80, infektperiode)) %>% 
  fill(Infected059, Infected6079, Infected80, .direction = "up") %>%
  ungroup() %>%
  mutate(blid=floor(id/1000000)) %>%
  as_tibble()
rki_alter_destatis <- bind_rows(rki_alter_destatis_kreise %>%
                                  mutate(`Todesfälle_60+`=`Todesfälle_60-79`+`Todesfälle_80+`), # kreise
                                rki_alter_destatis_kreise %>% # bundeslaender
                                  group_by(Meldedatum, blid) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            Infected059=sum(Infected059),
                                            Infected6079=sum(Infected6079),
                                            Infected80=sum(Infected80),
                                            `Fälle_15-34`=sum(`Fälle_15-34`),
                                            `Fälle_35-59`=sum(`Fälle_35-59`),
                                            `Fälle_0-15`=sum(`Fälle_0-15`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            `Fälle_60-79`=sum(`Fälle_60-79`),
                                            `Fälle_80+`=sum(`Fälle_80+`),
                                            `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                            `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                            `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                            `Todesfälle_60-79`=sum(`Todesfälle_60-79`),
                                            `Todesfälle_80+`=sum(`Todesfälle_80+`),
                                            `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`, na.rm = TRUE), .groups="drop") %>%
                                  mutate(id=blid),
                                rki_alter_destatis_kreise %>% # bund gesamt
                                  group_by(Meldedatum) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            Infected059=sum(Infected059),
                                            Infected6079=sum(Infected6079),
                                            Infected80=sum(Infected80),
                                            `Fälle_15-34`=sum(`Fälle_15-34`),
                                            `Fälle_35-59`=sum(`Fälle_35-59`),
                                            `Fälle_0-15`=sum(`Fälle_0-15`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            `Fälle_60-79`=sum(`Fälle_60-79`),
                                            `Fälle_80+`=sum(`Fälle_80+`),
                                            `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                            `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                            `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                            `Todesfälle_60-79`=sum(`Todesfälle_60-79`),
                                            `Todesfälle_80+`=sum(`Todesfälle_80+`),
                                            `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`, na.rm = TRUE), .groups="drop") %>%
                                  mutate(id=0, blid=0))

letzte_7_tage <-  brd_timeseries %>% 
  group_by(id) %>% arrange(id,-as.numeric(date)) %>%
  summarise(Faelle_letzte_7_Tage=lag(cases, 7)-cases, 
            date=date+7,
            .groups="keep") %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7)) %>%
  ungroup() %>%
  drop_na()

letzte_7_tage_altersgruppen_regstat <- rki_alter_destatis %>%
  mutate(date=date(Meldedatum)) %>%
  group_by(id) %>% arrange(id, -as.numeric(date)) %>%
  summarise(`Faelle_letzte_7_Tage_0-14`=zoo::rollsum(`Fälle_0-15`, 7),
            `Faelle_letzte_7_Tage_15-34`=zoo::rollsum(`Fälle_15-34`, 7),
            `Faelle_letzte_7_Tage_35-59`=zoo::rollsum(`Fälle_35-59`, 7),
            `Faelle_letzte_7_Tage_0-59`=`Faelle_letzte_7_Tage_0-14`+`Faelle_letzte_7_Tage_15-34`+`Faelle_letzte_7_Tage_35-59`,
            `Faelle_letzte_7_Tage_60-79`=zoo::rollsum(`Fälle_60-79`, 7),
            `Faelle_letzte_7_Tage_80+`=zoo::rollsum(`Fälle_80+`, 7),
            `Faelle_letzte_7_Tage_60+`=zoo::rollsum(`Fälle_60+`, 7),
            date=zoo::rollmax(date, 7),
            .groups="drop") %>%
  left_join(., kreise_regstat_alter, by=c("id")) %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag_059=round(`Faelle_letzte_7_Tage_0-59`/7),
         Faelle_letzte_7_Tage_pro_Tag_6079=round(`Faelle_letzte_7_Tage_60-79`/7),
         Faelle_letzte_7_Tage_pro_Tag_80=round(`Faelle_letzte_7_Tage_80+`/7),
         `Faelle_letzte_7_Tage_je100TsdEinw_0-14`=round(`Faelle_letzte_7_Tage_0-14`/((ag_1+ag_2)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/((ag_3)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/((ag_4)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60+`=round(`Faelle_letzte_7_Tage_60+`/((ag_5+ag_6)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/((ag_5)/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/((ag_6)/100000))) %>%
  left_join(., rki_alter_destatis %>% select(cases059, cases6079, cases80, Infected059, Infected6079, Infected80, id, Meldedatum), by=c("id"="id", "date"="Meldedatum")) %>%
  mutate(EW059=ag_1+ag_2+ag_3+ag_4,
         EW6079=ag_5,
         EW80=ag_6,
         EW_insgesamt=EW059+EW6079+EW80)

ausgangsdaten <- letzte_7_tage %>%
  left_join(., divi_all %>%
              select(id, ICU_Betten, betten_frei, faelle_covid_aktuell, daten_stand) %>%
              mutate(id=ifelse(id>16, id*1000, id)),
            by=c("id"="id", "date"="daten_stand")) %>%
  left_join(., letzte_7_tage_altersgruppen_regstat %>% select(
    id,
    date,
    cases059, cases6079, cases80,
    Infected059, Infected6079, Infected80,
    EW059, EW6079, EW80,
    EW_insgesamt,
    `Faelle_letzte_7_Tage_pro_Tag_059`,
    `Faelle_letzte_7_Tage_pro_Tag_6079`,
    `Faelle_letzte_7_Tage_pro_Tag_80`,
    `Faelle_letzte_7_Tage_0-59`,
    `Faelle_letzte_7_Tage_60-79`,
    `Faelle_letzte_7_Tage_60+`,
    `Faelle_letzte_7_Tage_80+`,
    `Faelle_letzte_7_Tage_je100TsdEinw_0-14`,
    `Faelle_letzte_7_Tage_je100TsdEinw_15-34`,
    `Faelle_letzte_7_Tage_je100TsdEinw_35-59`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60-79`,
    `Faelle_letzte_7_Tage_je100TsdEinw_60+`,
    `Faelle_letzte_7_Tage_je100TsdEinw_80+`), by=c("id", "date")) %>%
  left_join(., brd_timeseries %>% select(date, cases, id), by=c("id", "date")) %>%
  mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/((EW059+EW6079+EW80)/100000)),
         Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw),
         Kapazitaet_Betten=(faelle_covid_aktuell+betten_frei)) # /icu_days

## berechne vorwarnzeit
myTage <-
  ausgangsdaten %>% 
  filter((date>=as_date("2020-03-13") & id==0)) %>%
  mutate(periode=case_when(
    date>="2020-01-01" & date<="2020-03-31" ~ 1,
    date>="2020-04-01" & date<="2020-04-30" ~ 2,
    date>="2020-05-01" & date<="2020-05-31"  ~ 3,
    date>="2020-06-01" & date<="2020-09-30"  ~ 4,
    date>="2020-10-01" & date<="2020-11-30"  ~ 5,
    date>="2020-12-01" & date<="2021-12-31"  ~ 6
  )) %>%
  left_join(itsquoten %>% select(periode, `bis 60`, `60-80`, `ueber 80`),
            by="periode") %>% 
  rowwise() %>%
  do(Tage = 
       vorwarnzeit_berechnen_AG(
         ngesamt = c(.$EW059, .$EW6079, .$EW80),
         cases = c(.$cases059, .$cases6079, .$cases80),
         akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
         icubelegt = round(
           (.$faelle_covid_aktuell*
              altersgruppen_beatmet/
              sum(altersgruppen_beatmet)) %>%
             as.numeric()),
         Kapazitaet_Betten = .$Kapazitaet_Betten,
         Rt = 1.3,
         # icurate_altersgruppen = c(0.0079, 0.0769,0.0497)
         icurate_altersgruppen = c(.$`bis 60`, .$`60-80`, .$`ueber 80`)
         # icurate_altersgruppen = itsquoten %>%
         #   filter(periode==.$periode) %>%
         #   select(`bis 60`, `60-80`, `ueber 80`) %>%
         #   slice(1) %>%
         #   as.numeric()
         )
     ) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- ausgangsdaten %>%
  filter(
    (date>=as_date("2020-03-13") & id==0)) %>%
  mutate(Vorwarnzeit = myTage$Tage, 
         Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0))

zi_vwz_plot <- ggplot(vorwarnzeitergebnis,
                      aes(x=date, y=Vorwarnzeit)) +
  geom_line(size=2, col=zi_cols("zigreen")) +
  ylim(0, NA) +
  labs(subtitle="Zi-Vorwarnzeit im Zeitverlauf",x="Datum",y="Vorwarnzeit") +
  theme_minimal() +
  scale_x_date(date_labels = "%d.%m.", breaks="2 months") +
  theme(legend.position='none') +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
zi_vwz_plot
