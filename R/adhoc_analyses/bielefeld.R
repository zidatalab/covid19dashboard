Kreise %>% filter(str_detect(Landkreis, "Bielefeld")) %>% 
  select(IdLandkreis, Landkreis) %>% 
  distinct()

myTage <- ausgangsdaten %>% 
  filter((date>=as_date("2020-03-13") & id==5711000)) %>%
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
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round(
                                       (.$faelle_covid_aktuell*
                                          altersgruppen_beatmet/
                                          sum(altersgruppen_beatmet)) %>%
                                         as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = c(.$`bis 60`, 
                                                               .$`60-80`, 
                                                               .$`ueber 80`)
  )
  ) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)

ausgangsdaten_ror <- ausgangsdaten %>%
  inner_join(., kreise_ror %>%
               mutate(krs17=ifelse(krs17==11000, 11, 1000*krs17)),
             by=c("id"="krs17")) %>%
  filter((date>=as_date("2020-03-13") & ROR11==503)) %>%
  group_by(ROR11, date) %>%
  summarise(EW059=sum(EW059, na.rm = TRUE),
            EW6079=sum(EW6079, na.rm = TRUE),
            EW80=sum(EW80, na.rm = TRUE),
            Infected059=sum(Infected059, na.rm = TRUE),
            Infected6079=sum(Infected6079, na.rm = TRUE),
            Infected80=sum(Infected80, na.rm = TRUE),
            cases059=sum(cases059, na.rm = TRUE),
            cases6079=sum(cases6079, na.rm = TRUE),
            cases80=sum(cases80, na.rm = TRUE),
            faelle_covid_aktuell=sum(faelle_covid_aktuell, na.rm = TRUE),
            Kapazitaet_Betten=sum(Kapazitaet_Betten, na.rm = TRUE),
            .groups="drop")

myTage_ror <- ausgangsdaten_ror %>% 
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
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, 
                                                       .$Infected6079, 
                                                       .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*
                                                          altersgruppen_beatmet/
                                                          sum(altersgruppen_beatmet)) %>%
                                                         as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = c(.$`bis 60`, 
                                                               .$`60-80`, 
                                                               .$`ueber 80`)
  )) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- ausgangsdaten %>%
  filter((date>=as_date("2020-03-13") & id==5711000)) %>%
  mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis_ror <- ausgangsdaten_ror %>%
  mutate(Vorwarnzeit = myTage_ror$Tage, Vorwarnzeit_effektiv = pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis <- vorwarnzeitergebnis %>%
  left_join(., kreise_ror %>%
              mutate(krs17=ifelse(krs17==11000, 11, 1000*krs17)),
            by=c("id"="krs17")) %>%
  left_join(., vorwarnzeitergebnis_ror %>%
              select(ROR11, date, Vorwarnzeit, Vorwarnzeit_effektiv),
            by=c("date", "ROR11"),
            suffix=c("", "_ROR"))

bielefeldmitidata <- tibble()
theid=5711000
thename<-strukturdaten %>% filter(id==theid) %>% collect() %>% head(1) %>% pull(name)

bielefeldmitidata <- bind_rows(bielefeldmitidata, 
                               mitigation_data(theid) %>% 
                                 mutate(name=thename, id=theid, date=date+5) %>%
                                 left_join(., 
                                           vorwarnzeitergebnis %>% 
                                             filter(id==theid) %>% 
                                             select(date, Vorwarnzeit, Vorwarnzeit_ROR), 
                                           by="date")) # _effektiv

mybielefeldmitidata <- bielefeldmitidata %>%
  filter(Merkmal=="Fälle"  & R_Mean<10 & date>=date("2020-03-13"))
write_csv(mybielefeldmitidata %>% 
            select(date, name, R_Mean, Vorwarnzeit, Vorwarnzeit_ROR, 
                   cases, deaths, I_cases, I_dead),
          "data/bielefeld.csv")
library(openxlsx)
write.xlsx(mybielefeldmitidata %>% 
             select(date, name, R_Mean, Vorwarnzeit, Vorwarnzeit_ROR, 
                    cases, deaths, I_cases, I_dead),
           "data/bielefeld.xlsx")
