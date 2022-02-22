rki_kreise_sum_dritt <- rki_vacc_kreise %>% 
  filter(vacc_series==3) %>% 
  group_by(LandkreisId, Altersgruppe) %>% 
  summarise(sumdritt=sum(anzahl_alleorte),
            .groups="drop")
kbv_kreise_sum_dritt <- kbv_age_kreise_kv %>% 
  filter(vacc_series>=3) %>% 
  group_by(Kreis2016, Altersgruppe, vacc_series) %>% 
  summarise(sumpraxen=sum(anzahl_praxen),
            .groups="drop") %>% 
  pivot_wider(id_cols = c(Kreis2016, Altersgruppe),
              names_from = vacc_series,
              values_from = sumpraxen) %>% 
  mutate(sumpraxendrittviert=`3`+`4`)

beides_drittviert <- full_join(kbv_kreise_sum_dritt,
                               rki_kreise_sum_dritt %>% 
                                 mutate(Altersgruppe=ifelse(
                                   Altersgruppe=="05-11",
                                   "5-11",
                                   Altersgruppe
                                 )),
                               by=c("Kreis2016"="LandkreisId",
                                    "Altersgruppe"))

# tabelle 2 wochenbericht nach impfort
rki_laender_sum_dritt <- rki_vacc_laender %>% 
  filter(vacc_series==3 & vacc_date<="2022-02-15") %>% 
  group_by(Bundesland, BundeslandId_Impfort) %>% 
  summarise(sumdritt=sum(anzahl_alleorte),
            .groups="drop")
kbv_laender_sum_dritt <- kbv_impfstoff_laender %>% 
  filter(vacc_series>=3 & vacc_date<="2022-02-15") %>% 
  group_by(Bundesland, vacc_series) %>% 
  summarise(sumpraxen=sum(anzahl_praxen, na.rm=TRUE),
            .groups="drop") %>% 
  pivot_wider(id_cols = c(Bundesland),
              names_from = vacc_series,
              values_from = sumpraxen) %>% 
  mutate(sumpraxendrittviert=`3`+`4`)

beides_drittviert_laender <- full_join(kbv_laender_sum_dritt,
                                       rki_laender_sum_dritt,
                               by="Bundesland")
