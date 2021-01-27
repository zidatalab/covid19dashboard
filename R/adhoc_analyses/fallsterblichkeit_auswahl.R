rki24 <- rki %>%
  filter(Meldedatum <= "2021-01-24")

rki_auswahl <- bind_rows(
  rki24 %>%
    filter(Bundesland=="Sachsen-Anhalt") %>%
    mutate(Level="Sachsen-Anhalt",
           id=15),
  rki24 %>%
    filter(Landkreis=="LK Stendal" | Landkreis=="LK Salzlandkreis") %>%
    mutate(Level=ifelse(Landkreis=="LK Stendal", "Stendal", "Salzlandkreis"),
           id=ifelse(Landkreis=="LK Stendal", 15090000, 15089000)),
  rki24 %>%
    mutate(Level="BRD", id=0)
  ) %>%
  group_by(Level, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall[NeuerFall>=0], na.rm=TRUE),
            id=id[1],
            AnzahlTodesfall=sum(AnzahlTodesfall[NeuerTodesfall>=0], na.rm=TRUE),
            .groups="drop") %>%
  left_join(kreise_regstat_alter %>%
              rename(`A00-A04`=ag_1,
                     `A05-A14`=ag_2,
                     `A15-A34`=ag_3,
                     `A35-A59`=ag_4,
                     `A60-A79`=ag_5,
                     `A80+`=ag_6) %>%
              pivot_longer(cols=contains("A"),
                           names_to="Altersgruppe",
                           values_to="Einwohnende"),
            by=c("id", "Altersgruppe")) %>%
  mutate(FallsterblichkeitProzent=100*AnzahlTodesfall/AnzahlFall,
         ProzentinfizierteEW=AnzahlFall/Einwohnende*100,
         Todesfaellepro100kEW=AnzahlTodesfall/Einwohnende*100000)

write.xlsx(rki_auswahl, "auswahl_salzlandkreis_ua.xlsx")
