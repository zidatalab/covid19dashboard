mysir <- sihrmodel(ngesamt = 83000000,
                   S = 83000000-1762200-276400,
                   I = 276400,
                   H=5000,
                   R = 1762200,
                   R0 = 1.3,
                   gamma = 1/10,
                   qa = 0.018,
                   delta = 1/14,
                   nu = 1/10.1,
                   horizont = 180) %>%
  mutate(IHlag7=(I+H)/(lag(I, 20)+lag(H, 20)))

myresult <- (mysir) %>% mutate(Tage=row_number()-1) %>% filter(H>=10000) %>% head(1) %>% pull(Tage)

####
infektperiode <- 10
myTage10 <- ausgangsdaten %>% filter((date>=as_date("2020-03-13") & id<=16) |
                                     (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital))%>%as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = icurate_altersgruppen_busse%>%slice(1)%>%as.numeric())) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
myTage10_ror <- ausgangsdaten_ror %>% 
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital))%>%as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = icurate_altersgruppen_busse%>%slice(1)%>%as.numeric())) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis10 <- ausgangsdaten %>%
  filter((date>=as_date("2020-03-13") & id<=16) |
           (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  mutate(Vorwarnzeit = myTage10$Tage, Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis10_ror <- ausgangsdaten_ror %>%
  mutate(Vorwarnzeit = myTage10_ror$Tage, Vorwarnzeit_effektiv = pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis10 <- vorwarnzeitergebnis10 %>%
  left_join(., kreise_ror %>%
              mutate(krs17=ifelse(krs17==11000, 11, 1000*krs17)),
            by=c("id"="krs17")) %>%
  left_join(., vorwarnzeitergebnis10_ror %>%
              select(ROR11, date, Vorwarnzeit, Vorwarnzeit_effektiv),
            by=c("date", "ROR11"),
            suffix=c("", "_ROR"))

####
infektperiode <- 4
myTage4 <- ausgangsdaten %>% filter((date>=as_date("2020-03-13") & id<=16) |
                                       (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital))%>%as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = icurate_altersgruppen_busse%>%slice(1)%>%as.numeric())) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
myTage4_ror <- ausgangsdaten_ror %>% 
  rowwise() %>%
  do(Tage = vorwarnzeit_berechnen_AG(ngesamt = c(.$EW059, .$EW6079, .$EW80),
                                     cases = c(.$cases059, .$cases6079, .$cases80),
                                     akutinfiziert = c(.$Infected059, .$Infected6079, .$Infected80),
                                     icubelegt = round((.$faelle_covid_aktuell*busselancet_altersgruppen_hospital/sum(busselancet_altersgruppen_hospital))%>%as.numeric()),
                                     Kapazitaet_Betten = .$Kapazitaet_Betten,
                                     Rt = 1.3,
                                     icurate_altersgruppen = icurate_altersgruppen_busse%>%slice(1)%>%as.numeric())) %>% 
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis4 <- ausgangsdaten %>%
  filter((date>=as_date("2020-03-13") & id<=16) |
           (date%in%c(maxdatum, lastsunday, sundaybeforelastsunday) & id>16) ) %>%
  mutate(Vorwarnzeit = myTage4$Tage, Vorwarnzeit_effektiv=pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis4_ror <- ausgangsdaten_ror %>%
  mutate(Vorwarnzeit = myTage4_ror$Tage, Vorwarnzeit_effektiv = pmax(Vorwarnzeit-21, 0))
vorwarnzeitergebnis4 <- vorwarnzeitergebnis4 %>%
  left_join(., kreise_ror %>%
              mutate(krs17=ifelse(krs17==11000, 11, 1000*krs17)),
            by=c("id"="krs17")) %>%
  left_join(., vorwarnzeitergebnis4_ror %>%
              select(ROR11, date, Vorwarnzeit, Vorwarnzeit_effektiv),
            by=c("date", "ROR11"),
            suffix=c("", "_ROR"))