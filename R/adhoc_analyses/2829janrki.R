thisdate <- as_date("2021-01-29")
thisrki29 <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand==thisdate) %>% collect()

thisdate <- as_date("2021-01-28")
thisrki28 <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand==thisdate) %>% collect()

thisdate <- as_date("2021-01-27")
thisrki27 <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand==thisdate) %>% collect()

thisrki27d <- thisrki27 %>% 
  select(-Altersgruppe2, -IstErkrankungsbeginn, -AnzahlGenesen, -NeuGenesen,
         -ObjectId) %>%
  distinct()

sum(thisrki27d$AnzahlFall[thisrki27d$NeuerFall>=0], na.rm=TRUE)

thisrki28d <- thisrki28 %>% 
  select(-Altersgruppe2, -IstErkrankungsbeginn, -AnzahlGenesen, -NeuGenesen,
         -ObjectId) %>%
  distinct()

sum(thisrki28d$AnzahlFall[thisrki28d$NeuerFall>=0], na.rm=TRUE)

thisrki29d <- thisrki29 %>% 
  select(-Altersgruppe2, -IstErkrankungsbeginn, -AnzahlGenesen, -NeuGenesen,
         -ObjectId) %>%
  distinct()

sum(thisrki29d$AnzahlFall[thisrki29d$NeuerFall>=0], na.rm=TRUE)
