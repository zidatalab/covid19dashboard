letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall , na.rm=T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols =  c(Meldedatum),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0,"Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdatum-6) %>%
  summarise(`Faelle_letzte_7_Tage_35-59`=sum(`Fälle_35-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`Fälle_60-79`),
            `Faelle_letzte_7_Tage_15-34`=sum(`Fälle_15-34`),
            `Faelle_letzte_7_Tage_5-14`=sum(`Fälle_05-14`),
            `Faelle_letzte_7_Tage_0-4`=sum(`Fälle_00-04`),
            `Faelle_letzte_7_Tage_80+`=sum(`Fälle_80+`), .groups="drop") %>%
  bind_cols(., kreise_regstat_alter%>%filter(id==0)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-4`=round(`Faelle_letzte_7_Tage_0-4`/(ag_1/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_5-14`=round(`Faelle_letzte_7_Tage_5-14`/(ag_2/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/(ag_3/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/(ag_4/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(ag_5/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(ag_6/100000)))

todesfaelle_gesamt <- rki %>%
  group_by(Altersgruppe) %>%
  summarise(sumFaelle=sum(AnzahlFall, na.rm = TRUE),
            sumTodesfaelle=sum(AnzahlTodesfall, na.rm=TRUE),
            AnteilTodesfaelle=sumTodesfaelle/sumFaelle,
            .groups="drop")

todesfaelle_novdez <- rki %>%
  filter(Meldedatum>="2020-11-16" & Meldedatum<="2020-12-13") %>%
  group_by(Altersgruppe) %>%
  summarise(sumFaelle=sum(AnzahlFall, na.rm = TRUE),
            sumTodesfaelle=sum(AnzahlTodesfall, na.rm=TRUE),
            AnteilTodesfaelle=sumTodesfaelle/sumFaelle,
            .groups="drop")

View(vaccinations %>%
  filter(region=="DE" & datum=="2021-01-09"))
