library(lubridate)
library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(zoo)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_rki_impfstoff <- tbl(conn, "kbv_rki_impfstoffe_laender") %>% 
  collect()
kbv_rki_age <- tbl(conn,"kbv_rki_altersgruppen_kreise") %>%
  collect()

kbv_rki_impfstoff_bremen <- kbv_rki_impfstoff %>% 
  filter(Bundesland=="Bremen")
kbv_rki_age_bremen <- kbv_rki_age %>% 
  filter(kvname=="Bremen")

# bremen_gesamt <- kbv_rki_age_bremen %>% 
#   summarise(gesamt=sum(anzahl_alleorte, na.rm = TRUE)) %>%
#   pull(gesamt)
# 
# bremen_praxen_gesamt <- kbv_rki_age_bremen %>% 
#   summarise(gesamt_praxen=sum(anzahl_praxen, na.rm = TRUE)) %>% 
#   pull(gesamt_praxen)
# 
# anteil_praxen_bremen <- bremen_praxen_gesamt/bremen_gesamt

bremen_gesamtezeit <- kbv_rki_age_bremen %>% 
  mutate(anzahl_alleorte=ifelse(vacc_series==4, anzahl_praxen, anzahl_alleorte)) %>% 
  group_by(vacc_series) %>% 
  summarise(gesamtezeit_praxen=sum(anzahl_praxen, na.rm=TRUE),
            gesamtezeit_gesamt=sum(anzahl_alleorte, na.rm=TRUE),
            anteil_praxen=sum(gesamtezeit_praxen/gesamtezeit_gesamt)) %>% 
  pivot_wider(names_from = vacc_series, values_from = gesamtezeit_praxen:anteil_praxen) %>% 
  mutate('Land Bremen'="ingesamt") %>% 
  select('Land Bremen',
         'Erst-Praxen'=gesamtezeit_praxen_1,
         'Erst-Gesamt'=gesamtezeit_gesamt_1,
         'Erst-Anteil'=anteil_praxen_1,
         'Zweit-Praxen'=gesamtezeit_praxen_2,
         'Zweit-Gesamt'=gesamtezeit_gesamt_2,
         'Zweit-Anteil'=anteil_praxen_2,
         'Booster-Praxen'=gesamtezeit_praxen_3,
         'Booster-Gesamt'=gesamtezeit_gesamt_3,
         'Booster-Anteil'=anteil_praxen_3,
         'Viert-Praxen'=gesamtezeit_praxen_4,
         'Viert-Gesamt'=gesamtezeit_praxen_4, # !!!
         'Viert-Anteil'=anteil_praxen_4) %>% 
  mutate(`Viert-Anteil`=NA) %>% 
  mutate('Alle-Praxen'=`Erst-Praxen`+`Zweit-Praxen`+`Booster-Praxen`+`Viert-Praxen`,
         'Alle-Gesamt'=`Erst-Gesamt`+`Zweit-Gesamt`+`Booster-Gesamt`+`Viert-Gesamt`,
         "Alle-Anteil"=`Alle-Praxen`/`Alle-Gesamt`) %>% 
  pivot_longer(cols=`Erst-Praxen`:`Alle-Anteil`,
               names_to = c("Impfserie", "Impfort"),
               values_to = "value",
               names_pattern="(.*)-(.*)") %>% 
  pivot_wider(id_cols="Impfserie",
              names_from="Impfort") %>% 
  mutate(`Anteil Praxen`=paste0(round(100*`Praxen`/`Gesamt`), "%")) %>% 
  mutate(Praxen=format(Praxen, big.mark=".", decimal.mark=","),
         Gesamt=format(Gesamt, big.mark=".", decimal.mark=",")) %>% 
  mutate(Gesamt=ifelse(Impfserie=="Viert", "unbekannt", Gesamt)) %>% 
  mutate(`Anteil Praxen`=ifelse(Impfserie=="Viert", "unbekannt", `Anteil Praxen`)) %>% 
  select(-Anteil)

bremen_letzte4wochen <- kbv_rki_age_bremen %>%
  filter(vacc_date>=today()-days(28)) %>%
  mutate(anzahl_alleorte=ifelse(vacc_series==4, anzahl_praxen, anzahl_alleorte)) %>%
  group_by(vacc_series) %>%
  summarise(letzte4wochen_praxen=sum(anzahl_praxen, na.rm=TRUE),
            letzte4wochen_gesamt=sum(anzahl_alleorte, na.rm=TRUE),
            anteil_praxen=sum(letzte4wochen_praxen/letzte4wochen_gesamt),
            .groups="drop") %>%
  pivot_wider(names_from = vacc_series, values_from = letzte4wochen_praxen:anteil_praxen) %>%
  mutate('Land Bremen'='letzte 4 Wochen') %>%
  select('Land Bremen',
         'Erst-Praxen'=letzte4wochen_praxen_1,
         'Erst-Gesamt'=letzte4wochen_gesamt_1,
         'Erst-Anteil'=anteil_praxen_1,
         'Zweit-Praxen'=letzte4wochen_praxen_2,
         'Zweit-Gesamt'=letzte4wochen_gesamt_2,
         'Zweit-Anteil'=anteil_praxen_2,
         'Booster-Praxen'=letzte4wochen_praxen_3,
         'Booster-Gesamt'=letzte4wochen_gesamt_3,
         'Booster-Anteil'=anteil_praxen_3,
         'Viert-Praxen'=letzte4wochen_praxen_4,
         'Viert-Gesamt'=letzte4wochen_gesamt_4, # !!!
         'Viert-Anteil'=anteil_praxen_4)  %>%
  mutate(`Viert-Anteil`=NA) %>%
  mutate('Alle-Praxen'=`Erst-Praxen`+`Zweit-Praxen`+`Booster-Praxen`+`Viert-Praxen`,
         'Alle-Gesamt'=`Erst-Gesamt`+`Zweit-Gesamt`+`Booster-Gesamt`+`Viert-Gesamt`,
         "Alle-Anteil"=`Alle-Praxen`/`Alle-Gesamt`) %>% 
  pivot_longer(cols=`Erst-Praxen`:`Alle-Anteil`,
               names_to = c("Impfserie", "Impfort"),
               values_to = "value",
               names_pattern="(.*)-(.*)") %>% 
  pivot_wider(id_cols="Impfserie",
              names_from="Impfort") %>% 
  mutate(`Anteil Praxen`=paste0(round(100*`Praxen`/`Gesamt`), "%")) %>% 
  mutate(Praxen=format(Praxen, big.mark=".", decimal.mark=","),
         Gesamt=format(Gesamt, big.mark=".", decimal.mark=",")) %>% 
  mutate(Gesamt=ifelse(Impfserie=="Viert", "unbekannt", Gesamt)) %>% 
  mutate(`Anteil Praxen`=ifelse(Impfserie=="Viert", "unbekannt", `Anteil Praxen`)) %>% 
  select(-Anteil)

# tabelle_4wochen <- bind_rows(bremen_gesamtezeit,
#                              bremen_letzte4wochen) %>% 
#   mutate(across(contains("Anteil"), function(x) paste0(round(100*x), "%")))

stschnitt <- kbv_rki_age_bremen %>% 
  mutate(anzahl_alleorte=ifelse(vacc_series==4, anzahl_praxen, anzahl_alleorte)) %>%
  group_by(vacc_date) %>% 
  summarise(impfungen_praxen=sum(anzahl_praxen, na.rm=TRUE),
            impfungen_gesamt=sum(anzahl_alleorte, na.rm=TRUE)) %>% 
  mutate(wtag=wday(vacc_date, week_start = 1)) %>% 
  arrange(rev(vacc_date)) %>% 
  mutate(stschnitt_praxen=rollmean(impfungen_praxen, 7, fill = 0, align = "left"),
         stschnitt_gesamt=rollmean(impfungen_gesamt, 7, fill = 0, align = "left"))

