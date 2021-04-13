library(tidyverse)
library(lubridate)
library(ISOcodes)
lieferungen <- read_tsv("https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv")
impfungen.raw  <- read_tsv('https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv')
impfungen.raw.bl <- read_tsv('https://impfdashboard.de/static/data/germany_vaccinations_by_state.tsv') %>%
  left_join(ISOcodes::ISO_3166_2 %>% select(code=Code,Bundesland=Name),by="code")

pop.bl <- jsonlite::read_json("https://raw.githubusercontent.com/zidatalab/covid19dashboard/master/data/tabledata/impfsim_start.json",simplifyVector = TRUE) %>% 
  as_tibble() %>% select(Bundesland=geo,population,dosen_geliefert) %>% 
  group_by(Bundesland) %>% summarise(Bevölkerung=mean(population,na.rm=T),dosen_geliefert=sum(dosen_geliefert,na.rm=T)) %>%
  ungroup()


impfungen <- impfungen.raw %>% 
  select(date, dosen_kumulativ) %>% 
  arrange(date) %>% mutate(dosen_verimpft=ifelse(row_number()==1,dosen_kumulativ,dosen_kumulativ-lag(dosen_kumulativ))) %>%
  select(-dosen_kumulativ)

impfungen_praxen <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte.csv") %>%
  select(date,dosen_Impfungen_Arztpraxen,Anzahl_Praxen)

impfungen_praxen_bl.raw <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_bl_kw_wirkstoff.csv") 
impfungen_praxen_bl <-impfungen_praxen_bl.raw %>%
  group_by(Bundesland) %>% summarise("Impfungen Praxen"=sum(anzahl,na.rm=T))
  
  

alldates <- seq(min(lieferungen$date),max(impfungen$date,impfungen_praxen$date),1)

plotdata.full <- left_join(tibble(date=alldates), lieferungen %>% group_by(date) %>% summarise(dosen_geliefert=sum(dosen)) ,by="date") %>% 
  bind_rows(.,impfungen ) %>% group_by(date) %>%
  summarise(dosen_verimpft=sum(dosen_verimpft,na.rm=TRUE),dosen_geliefert=sum(dosen_geliefert,na.rm=TRUE)) %>%
  mutate(Jahr=year(date),kw=isoweek(date)) %>% 
  left_join(.,impfungen_praxen %>% select(-Anzahl_Praxen),by="date") %>%
  mutate(dosen_Impfungen_Arztpraxen=
           ifelse(is.na(dosen_Impfungen_Arztpraxen),0,
                  dosen_Impfungen_Arztpraxen),
         dosen_Impfungen_Impfzentren=dosen_verimpft-dosen_Impfungen_Arztpraxen,
         dosen_Impfungen_Impfzentren=ifelse(dosen_Impfungen_Impfzentren<0,0,dosen_Impfungen_Impfzentren)
         )


plotdata <- plotdata.full %>%   group_by(Jahr,kw) %>% 
  summarise(dosen_geliefert=sum(dosen_geliefert,na.rm = TRUE),
            dosen_verimpft=sum(dosen_verimpft,na.rm=TRUE),
            dosen_Impfungen_Impfzentren=sum(dosen_Impfungen_Impfzentren,na.rm=TRUE),
            dosen_Impfungen_Arztpraxen=sum(dosen_Impfungen_Arztpraxen,na.rm=TRUE),
            "dosen_Dosen_geliefert"=dosen_geliefert,
            date=max(date)) %>% arrange(date) %>% ungroup() %>%
  mutate(Dosen_unverimpft=cumsum(dosen_geliefert)-cumsum(dosen_Impfungen_Impfzentren)-cumsum(dosen_Impfungen_Arztpraxen)) %>%
  pivot_longer(contains("dosen"),
               names_to = "Dosen",names_prefix = "dosen_",values_to = "Anzahl") %>%
  filter(Dosen %in% c("Dosen_unverimpft","Dosen_geliefert","Impfungen_Arztpraxen","Impfungen_Impfzentren")) %>%
  mutate(Dosen=str_replace(Dosen,"_"," ")) %>% 
  arrange(Dosen,date) %>% group_by(Dosen) %>% mutate(Anzahl_kum=cumsum(Anzahl)) 


plotdata_wide <- plotdata %>% pivot_wider(names_from=Dosen,values_from = contains('Anzahl')) %>%
  mutate("Anzahl_kum_Dosen unverimpft"=`Anzahl_kum_Dosen geliefert`-`Anzahl_kum_Impfungen Arztpraxen`-`Anzahl_kum_Impfungen Impfzentren`)
# > colnames(plotdata_wide)
# [1] "Jahr"                             "kw"                              
# [3] "date"                             "Anzahl_Dosen geliefert"          
# [5] "Anzahl_Dosen unverimpft"          "Anzahl_Impfungen Arztpraxen"     
# [7] "Anzahl_Impfungen Impfzentren"     "Anzahl_kum_Dosen geliefert"      
# [9] "Anzahl_kum_Dosen unverimpft"      "Anzahl_kum_Impfungen Arztpraxen" 
# [11] "Anzahl_kum_Impfungen Impfzentren"
write_csv(plotdata_wide ,"data/tabledata/impfdax.csv")

overview_data <- plotdata_wide %>% arrange(date) %>% tail(1) %>% gather(Dosen,Anzahl,4:ncol(.) ) %>%
  filter(str_detect(Dosen,"kum")) %>% select(Dosen,Anzahl) %>% 
  ungroup() %>%
  mutate(
    Dosen=str_remove(Dosen,"Anzahl_kum_"),
    Thema=str_split_fixed(Dosen," ",2)[,1],Was=str_split_fixed(Dosen," ",2)[,2])
         

# > colnames(overview_data)
# [1] "Dosen"  "Anzahl" "Thema"  "Was"   
write_csv(overview_data ,"data/tabledata/impfdax_overview.csv")


# Last 14 Impfungen
# > colnames(last14days)
# [1] "date"        "Impfzentren" "Arztpraxen" 
last14days <- plotdata.full %>% arrange(date) %>% tail(14) %>% 
  select(date,contains("dosen_Impfungen")) %>% rename(Arztpraxen=dosen_Impfungen_Arztpraxen,Impfzentren= dosen_Impfungen_Impfzentren)

write_csv(last14days ,"data/tabledata/impfdax_last14days.csv")

# Export Impfungen nach Bundesland
export_bl_impfungen <- impfungen.raw.bl %>%  
  left_join(impfungen_praxen_bl,by="Bundesland") %>% ungroup() %>% 
  mutate(`Impfungen Praxen`=ifelse(Bundesland=="Gesamt",sum(`Impfungen Praxen`,na.rm=T),`Impfungen Praxen`)) %>%
  left_join(pop.bl,by="Bundesland") %>%
  mutate(
    "Verimpft" = vaccinationsTotal/dosen_geliefert,
    "Bevölkerung Erstimpfung"=peopleFirstTotal/`Bevölkerung`,
    "Bevölkerung Zweitimpfung"=peopleFullTotal/`Bevölkerung`)  %>% 
  select(Bundesland,"Gelieferte Dosen"=dosen_geliefert,Impfungen=vaccinationsTotal,Verimpft,contains("Praxen"),contains("Bevölkerung")) 

write_csv(export_bl_impfungen ,"data/tabledata/impfdax_bl_stand.csv")

# Datenstand
jsonlite::write_json(
  tibble("Impfdaschboard"=max(impfungen$date),
         "KBV Impfdokumentation"=max(impfungen_praxen$date),
         "Letzte Aktualisierung"=now(),
         ),"data/tabledata/impfdax_stand.json")
