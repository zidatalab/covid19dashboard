bayern_iz <- rki_vacc %>% 
  filter(region=="DE-BY" & (date=="2021-03-30" | date==max(date))) %>% 
  filter(metric%in%c("dosen_kumulativ", "dosen_kumulativ_impfstelle_zentral",
                     "dosen_kumulativ_impfstelle_aerzte"))
