tt_query23 <- system.time({rki23 <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand=="2020-12-23") %>% 
  collect() })

tt_query27 <- system.time({rki27 <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand=="2020-12-27") %>% 
  collect() })

tt_query30 <- system.time({rki30 <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand=="2020-12-30") %>% 
  collect() })

tod23 <- rki23 %>%
  mutate(KW=isoweek(Meldedatum)) %>%
  group_by(KW) %>%
  summarise(anzahltod=sum(AnzahlTodesfall),
            .groups="drop")

tod27 <- rki27 %>%
  mutate(KW=isoweek(Meldedatum)) %>%
  group_by(KW) %>%
  summarise(anzahltod=sum(AnzahlTodesfall),
            .groups="drop")

tod30 <- rki30 %>%
  mutate(KW=isoweek(Meldedatum)) %>%
  group_by(KW) %>%
  summarise(anzahltod=sum(AnzahlTodesfall),
            .groups="drop")

tod_alle <- tod30 %>%
  left_join(., tod27, by="KW", suffix=c(".30", ".27")) %>%
  left_join(., tod23, by="KW", suffix=c("", ".23"))
