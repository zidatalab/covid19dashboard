maxdatum<-max(rki$Meldedatum)#as_date("2021-01-10")
letzte_7_tage_altersgruppen_bund <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id) & NeuerFall>=0) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T), .groups="drop") %>% 
  arrange(Meldedatum,Altersgruppe) %>% collect() %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A")) %>%
  group_by(Meldedatum, Altersgruppe) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T), .groups="drop") %>% 
  arrange(Meldedatum, Altersgruppe) %>% 
  pivot_wider(id_cols =  c(Meldedatum),
              names_from = Altersgruppe,
              values_from = c("Fälle"),
              values_fill = list("Fälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum)) %>%
  mutate(date=date(Meldedatum)) %>%
  filter(date>=maxdatum-6 & date<=maxdatum) %>%
  summarise(`Faelle_letzte_7_Tage_35-59`=sum(`35-59`),
            `Faelle_letzte_7_Tage_60-79`=sum(`60-79`),
            `Faelle_letzte_7_Tage_15-34`=sum(`15-34`),
            `Faelle_letzte_7_Tage_5-14`=sum(`05-14`),
            `Faelle_letzte_7_Tage_0-4`=sum(`00-04`),
            `Faelle_letzte_7_Tage_80+`=sum(`80+`), .groups="drop") %>%
  bind_cols(., kreise_regstat_alter%>%filter(id==0)) %>%
  mutate(`Faelle_letzte_7_Tage_je100TsdEinw_0-4`=round(`Faelle_letzte_7_Tage_0-4`/(ag_1/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_5-14`=round(`Faelle_letzte_7_Tage_5-14`/(ag_2/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_15-34`=round(`Faelle_letzte_7_Tage_15-34`/(ag_3/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_35-59`=round(`Faelle_letzte_7_Tage_35-59`/(ag_4/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_60-79`=round(`Faelle_letzte_7_Tage_60-79`/(ag_5/100000)),
         `Faelle_letzte_7_Tage_je100TsdEinw_80+`=round(`Faelle_letzte_7_Tage_80+`/(ag_6/100000)))

View(rki%>%filter(NeuerTodesfall>=0)%>%group_by(Altersgruppe)%>%summarise(ST=sum(AnzahlTodesfall, na.rm=TRUE)))

todesfaelle_gesamt <- rki %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0)) %>%
  group_by(Altersgruppe) %>%
  summarise(sumFaelle=sum(AnzahlFall, na.rm = TRUE),
            sumTodesfaelle=sum(AnzahlTodesfall, na.rm=TRUE),
            AnteilTodesfaelle=sumTodesfaelle/sumFaelle*100,
            .groups="drop")

todesfaelle_novdez <- rki %>%
  filter(Meldedatum>="2020-11-16" & Meldedatum<="2020-12-13") %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0)) %>%
  group_by(Altersgruppe) %>%
  summarise(sumFaelle=sum(AnzahlFall, na.rm = TRUE),
            sumTodesfaelle=sum(AnzahlTodesfall, na.rm=TRUE),
            AnteilTodesfaelle=sumTodesfaelle/sumFaelle*100,
            .groups="drop")

View(vaccinations %>%
  filter(region=="DE" & datum=="2021-01-11"))

plotdata_7ti_bund <- rki_7ti_alle %>%
  filter(id==0) %>%
  select(JahrKW, datesunday, Altersgruppe, STI)
library(openxlsx)
write.xlsx(plotdata_7ti_bund, "data/plotdata/sti_ag_bund_plot_data.xlsx")
