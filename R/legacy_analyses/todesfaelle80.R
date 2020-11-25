todesfaelle <- rki %>%
  mutate(IdLandkreis=as.integer(IdLandkreis)) %>%
  mutate(IdLandkreis=ifelse(IdLandkreis>=11000 & IdLandkreis<12000, 11, IdLandkreis)) %>%
  group_by(IdLandkreis, Altersgruppe) %>%
  summarise(todesfaelle=sum(AnzahlTodesfall), .groups="drop")

todesfaelle80 <- todesfaelle %>%
  filter(Altersgruppe=="A80+") %>%
  select(IdLandkreis, `todesfaelle80`=todesfaelle)
todesfaellegesamt <- todesfaelle %>%
  group_by(IdLandkreis) %>%
  summarise(todesfaellegesamt=sum(todesfaelle, na.rm=TRUE), .groups="drop")

todesfaelle_export <- letzte_7_tage_altersgruppen_destatis %>%
  select(id, EW_insgesamt, EW80) %>%
  mutate(id=ifelse(id>16, id/1000, id)) %>%
  distinct() %>%
  right_join(., todesfaelle80, by=c("id"="IdLandkreis")) %>%
  left_join(., todesfaellegesamt, by=c("id"="IdLandkreis"))

write_csv(todesfaelle_export, "./data/todesfaelle80.csv")
