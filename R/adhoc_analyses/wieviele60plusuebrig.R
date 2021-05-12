ew_60plus <- 18057318+5681135

bl_vollstaendig <- vacc_alle_faktenblatt %>% 
  drop_na()

mean(bl_vollstaendig$quote_initial_alter_60p)*ew_60plus
