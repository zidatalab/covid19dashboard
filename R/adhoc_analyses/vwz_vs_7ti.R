# modell für lars vwz - 7ti
vwz_modeldata <- vorwarnzeitergebnis %>% 
  filter(date>="2020-07-01" & date<="2020-12-31") %>%
  filter(id>16) %>%
  select(id, date, Vorwarnzeit, Vorwarnzeit_ROR, Faelle_letzte_7_Tage_je100TsdEinw)