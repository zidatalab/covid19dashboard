myTage <- ausgangsdaten %>% filter((date>=as_date("2020-03-13") & id<=16) |
                                     (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
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
  filter((date>=as_date("2020-03-13") & id<=16) |
           (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
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