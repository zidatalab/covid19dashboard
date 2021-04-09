library(tidyverse)
library(stringr)

all_global_files <-
  tibble(files=unlist(list.files("G:/30 Fachbereich_3/14 Daten/KBV/covidimpfungen"))) %>% 
  filter(str_detect(files,"kbv_covid_vacc_zi")) %>% pull(files) %>% 
  paste0("G:/30 Fachbereich_3/14 Daten/KBV/covidimpfungen/", .)

All_vac <- lapply(all_global_files,function(i){
  read.csv(i, colClasses = c("character","character", 
                             "character","character","character"))}) %>%
  bind_rows() %>% as_tibble() %>%
  mutate(anzahl=as.numeric(anzahl),vacc_series=as.numeric(vacc_series),
         arzt_plz=as.numeric(arzt_plz),date=as.Date(vacc_date, "%d.%m.%Y")) 

export_daten <- All_vac %>% mutate(anzahl=as.numeric(anzahl),vacc_series=as.numeric(vacc_series),arzt_plz=as.numeric(arzt_plz),date=as.Date(vacc_date, "%d.%m.%Y")) %>% group_by(date) %>% 
  summarise(dosen_Impfungen_Arztpraxen=sum(anzahl),Anzahl_Praxen=n()) %>%
  mutate(Dosen_pro_Praxis=round(dosen_Impfungen_Arztpraxen/Anzahl_Praxen))

old_export_daten <- read_csv("C:/Users/esteiger/Documents/covid19dashboard/data/impfdax/zeitreihe_impfungen_aerzte.csv")

if (max(old_export_daten$date)<max(export_daten$date)) {
  write.csv(export_daten, "C:/Users/esteiger/Documents/covid19dashboard/data/impfdax/zeitreihe_impfungen_aerzte.csv")
}

