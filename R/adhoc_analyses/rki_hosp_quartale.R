nisa <- rki_hosp_bl %>% 
  filter(Bundesland=="Niedersachsen") %>% 
  mutate(dow=wday(Datum, week_start = 1),
         quartal=quarter(Datum),
         Jahr=year(Datum)) %>% 
  filter(dow==1) %>% 
  group_by(quartal, Jahr) %>% 
  summarise(hosp=sum(`7T_Hospitalisierung_Faelle`),
            .groups="drop")

bund <- rki_hosp_bl %>% 
  mutate(dow=wday(Datum, week_start = 1),
         quartal=quarter(Datum),
         Jahr=year(Datum)) %>% 
  filter(dow==1) %>% 
  group_by(quartal, Jahr) %>% 
  summarise(hosp=sum(`7T_Hospitalisierung_Faelle`),
            .groups="drop")
