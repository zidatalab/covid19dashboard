thisdate <- today()-days(1)

age <- kbv_age %>% 
  filter(vacc_date==thisdate) %>% 
  summarise(impfungen_nachalter=sum(anzahl))

impfstoff <- kbv_impfstoff %>% 
  filter(vacc_date==thisdate) %>% 
  summarise(impfungen_nachimpfstoff=sum(anzahl))

age <- kbv_age %>% 
  # filter(vacc_date==thisdate) %>% 
  summarise(impfungen_nachalter=sum(anzahl))

impfstoff <- kbv_impfstoff %>% 
  # filter(vacc_date==thisdate) %>% 
  summarise(impfungen_nachimpfstoff=sum(anzahl))
