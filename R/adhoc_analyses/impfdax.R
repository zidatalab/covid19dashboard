library(tidyverse)
library(lubridate)
lieferungen <- read_tsv("https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv")
impfungen <- read_tsv('https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv')


plotdata <- lieferungen %>% 
  group_by(date) %>% summarise(dosen_geliefert=sum(dosen)) %>%
  bind_rows(.,impfungen %>% 
              select(date, dosen_kumulativ) %>% 
              arrange(date) %>% mutate(dosen_verimpft=ifelse(row_number()==1,dosen_kumulativ,dosen_kumulativ-lag(dosen_kumulativ))) %>%
              select(-dosen_kumulativ)) %>% group_by(date) %>%
  summarise(dosen_verimpft=sum(dosen_verimpft,na.rm=TRUE),dosen_geliefert=sum(dosen_geliefert,na.rm=TRUE)) %>%
  mutate(Jahr=year(date),kw=isoweek(date)) %>% 
  group_by(Jahr,kw) %>% 
  summarise(dosen_geliefert=sum(dosen_geliefert,na.rm = TRUE),
            dosen_verimpft=sum(dosen_verimpft,na.rm=TRUE),
            dosen_Impfungen_Arztpraxen=0,
            dosen_Impfungen_Impfzentren=dosen_verimpft-dosen_Impfungen_Arztpraxen,
            dosen_Dosen_unverimpft = dosen_geliefert-dosen_verimpft,
            "dosen_Dosen_geliefert"=dosen_geliefert,
            date=max(date)) %>% arrange(date) %>% 
  pivot_longer(contains("dosen"),
               names_to = "Dosen",names_prefix = "dosen_",values_to = "Anzahl") %>%
  #filter(Jahr<2020 | kw < isoweek(now())) %>%
  filter(Dosen %in% c("Dosen_unverimpft","Dosen_geliefert","Impfungen_Arztpraxen","Impfungen_Impfzentren")) %>%
  mutate(Dosen=str_replace(Dosen,"_"," ")) %>% 
  arrange(Dosen,date) %>% group_by(Dosen) %>% mutate(Anzahl_kum=cumsum(Anzahl))


plotdata_wide <- plotdata %>% pivot_wider(names_from=Dosen,values_from = contains('Anzahl'))

write_csv(plotdata_wide ,"data/tabledata/impfdax.csv")
