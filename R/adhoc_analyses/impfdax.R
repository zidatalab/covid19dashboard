library(tidyverse)
library(lubridate)
lieferungen <- read_tsv("https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv")
impfungen <- read_tsv('https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv') %>% 
  select(date, dosen_kumulativ) %>% 
  arrange(date) %>% mutate(dosen_verimpft=ifelse(row_number()==1,dosen_kumulativ,dosen_kumulativ-lag(dosen_kumulativ))) %>%
  select(-dosen_kumulativ)

impfungen_praxen <- tibble(date=alldates,dosen_Impfungen_Arztpraxen=0)

alldates <- seq(min(lieferungen$date),max(impfungen$date),1)

plotdata.full <- left_join(tibble(date=alldates), lieferungen %>% group_by(date) %>% summarise(dosen_geliefert=sum(dosen)) ,by="date") %>% 
  bind_rows(.,impfungen ) %>% group_by(date) %>%
  summarise(dosen_verimpft=sum(dosen_verimpft,na.rm=TRUE),dosen_geliefert=sum(dosen_geliefert,na.rm=TRUE)) %>%
  mutate(Jahr=year(date),kw=isoweek(date)) 
plotdata <- plotdata.full %>% left_join(.,impfungen_praxen,by="date") %>%
  group_by(Jahr,kw) %>% 
  summarise(dosen_geliefert=sum(dosen_geliefert,na.rm = TRUE),
            dosen_verimpft=sum(dosen_verimpft,na.rm=TRUE),
            dosen_Impfungen_Impfzentren=dosen_verimpft-dosen_Impfungen_Arztpraxen,
            dosen_Dosen_unverimpft = dosen_geliefert-dosen_verimpft,
            "dosen_Dosen_geliefert"=dosen_geliefert,
            date=max(date)) %>% arrange(date) %>% 
  pivot_longer(contains("dosen"),
               names_to = "Dosen",names_prefix = "dosen_",values_to = "Anzahl") %>%
  filter(Dosen %in% c("Dosen_unverimpft","Dosen_geliefert","Impfungen_Arztpraxen","Impfungen_Impfzentren")) %>%
  mutate(Dosen=str_replace(Dosen,"_"," ")) %>% 
  arrange(Dosen,date) %>% group_by(Dosen) %>% mutate(Anzahl_kum=cumsum(Anzahl))


plotdata_wide <- plotdata %>% pivot_wider(names_from=Dosen,values_from = contains('Anzahl'))
# > colnames(plotdata_wide)
# [1] "Jahr"                             "kw"                              
# [3] "date"                             "Anzahl_Dosen geliefert"          
# [5] "Anzahl_Dosen unverimpft"          "Anzahl_Impfungen Arztpraxen"     
# [7] "Anzahl_Impfungen Impfzentren"     "Anzahl_kum_Dosen geliefert"      
# [9] "Anzahl_kum_Dosen unverimpft"      "Anzahl_kum_Impfungen Arztpraxen" 
# [11] "Anzahl_kum_Impfungen Impfzentren"
write_csv(plotdata_wide ,"data/tabledata/impfdax.csv")

overview_data <- plotdata %>% group_by(Dosen) %>% summarise(Anzahl=sum(Anzahl,na.rm=T)) %>% ungroup() %>% mutate(Thema=str_split_fixed(Dosen," ",2)[,1],Was=str_split_fixed(Dosen," ",2)[,2])

# > colnames(overview_data)
# [1] "Dosen"  "Anzahl" "Thema"  "Was"   
write_csv(overview_data ,"data/tabledata/impfdax_overview.csv")


# Last 14 Impfungen
last14days <- plotdata.full %>% arrange(date) %>% tail(14) %>% select(date,Impfzentren=dosen_verimpft) %>%
  left_join(impfungen_praxen %>% select(date,"Arztpraxen"=dosen_Impfungen_Arztpraxen))

write_csv(last14days ,"data/tabledata/impfdax_last14days.csv")
