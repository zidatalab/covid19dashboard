##### Packages
library(DT)
library(DBI)
library(forcats)
library(EpiEstim)
library(plotly)
library(zicolors)
library(deSolve)
library(jsonlite)
library(readxl)
library(data.table)
library(dplyr)
library(glue)
library(lubridate)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(dtplyr)
library(zoo)
require(ISOcodes)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
rki <- tbl(conn,"rki") %>% 
  collect()
rki_original <- rki
rki <- rki %>%
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)), 
                                  maxdate=max(date(Meldedatum)))
rkiidkreise <- unique(rki$IdLandkreis)
rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")


## read/update rki impf data from github
# rki_vacc <- tryCatch(
#   {
#     mytemp <- tempfile()
#     rki_vacc_data <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v3/all.csv"
#     download.file(rki_vacc_data, mytemp, method = "curl")
#     vacc_zahlen <- read_csv(mytemp)
#     if (dim(vacc_zahlen)[2] != 5){
#       stop("they changed the vacc table")
#     } else {
#       write_csv(vacc_zahlen, "./data/vacc_zahlen_ard.csv")
#       vacc_zahlen
#     }
#   },
#   error=function(e) {
#     # read old data
    vacc_zahlen <- read_csv("./data/vacc_zahlen_ard.csv")
#     return(vacc_zahlen)
#   }
# )
rki_vacc <- vacc_zahlen
rki_vacc <- rki_vacc %>% 
  mutate(region=ifelse(region=="DE", "DE", paste0("DE-", region))) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(geo=ifelse(region=="DE", "Germany", geo),
         geotype=ifelse(region=="DE", "nation", "state"))

### bevölkerung
destatis_pop_by_state <- read_csv("data/destatis_pop_by_state.csv")
pop_bev <- destatis_pop_by_state %>% 
  group_by(Bundesland) %>% 
  summarise(pop=sum(pop))

### auffr pro tag/woche
auffr_zeitreihe <- rki_vacc %>% 
  filter(metric=="personen_auffr_kumulativ" & isoyear(date)==2021) %>% 
  mutate(KW=isoweek(date),
         Monat=month(date)) %>% 
  group_by(KW, geo) %>% 
  summarise(auffr_kumulativ=max(value), .groups="drop") %>% 
  group_by(geo) %>% 
  arrange(-KW) %>% 
  mutate(auffr_kumulativ=ifelse(is.na(auffr_kumulativ), 0, auffr_kumulativ),
         new_auffr=auffr_kumulativ-lead(auffr_kumulativ)) %>% 
  ungroup() %>% 
  filter(new_auffr>0) %>% 
  select(geo, KW, new_auffr)

vollst_zeitreihe <- rki_vacc %>% 
  filter(metric=="personen_voll_kumulativ" & isoyear(date)==2021) %>% 
  mutate(KW=isoweek(date),
         Monat=month(date)) %>% 
  group_by(KW, geo) %>% 
  summarise(vollst_kumulativ=max(value), .groups="drop") %>% 
  group_by(geo) %>% 
  arrange(-KW) %>% 
  mutate(vollst_kumulativ=ifelse(is.na(vollst_kumulativ), 0, vollst_kumulativ),
         new_vollst=vollst_kumulativ-lead(vollst_kumulativ)) %>% 
  ungroup() %>% 
  filter(new_vollst>0) %>% 
  select(geo, KW, new_vollst)

istsoll_auffr <- expand_grid(KW=1:52, geo=unique(rki_vacc$geo)) %>% 
  left_join(vollst_zeitreihe %>% 
              mutate(KW=ifelse(KW<=13, 13, KW)) %>% 
              group_by(geo, KW) %>% 
              summarise(new_vollst=sum(new_vollst, na.rm=TRUE),
                        .groups="drop"),
            by=c("KW", "geo")) %>% 
  group_by(geo) %>% 
  arrange(-KW) %>% 
  mutate(soll=lead(new_vollst, 30)) %>% 
  ungroup() %>% 
  left_join(auffr_zeitreihe, by=c("geo", "KW"))

istsoll_stand <- istsoll_auffr %>% 
  filter(KW<=isoweek(max(vacc_zahlen$date))) %>% 
  group_by(geo) %>% 
  summarise(soll=sum(soll, na.rm=TRUE),
            ist=sum(new_auffr, na.rm=TRUE),
            .groups = "drop") %>% 
  mutate(luecke=ist-soll,
         istminussolldurchsoll=luecke/soll) %>% 
  rename(Bundesland=geo)

istsoll_ausblick <- istsoll_auffr %>% 
  filter(KW>isoweek(max(vacc_zahlen$date))) %>% 
  filter(KW<=isoweek(max(vacc_zahlen$date))+6) %>% 
  rowwise() %>% 
  mutate(luecke_6wochen=round(abs(istsoll_stand$luecke[istsoll_stand$Bundesland==geo[1]])/6),
         sollplusluecke=soll+luecke_6wochen) %>% 
  ungroup() %>% 
  select(KW, Bundesland=geo, soll, luecke_6wochen, sollplusluecke) %>% 
  arrange(Bundesland, KW)
  

library(openxlsx)
auffr_table <- list(stand_dritt=istsoll_stand,
                    ausblick_dritt=istsoll_ausblick)
write.xlsx(auffr_table, "data/auffrischen_kw.xlsx",
           overwrite=TRUE)

















####
rki_neuinfekte <- rki %>% 
  mutate(Meldedatum=as_date(Meldedatum),
         Meldedatum=ifelse(Meldedatum<"2021-03-02",
                           as_date("2021-03-01"),
                           Meldedatum)) %>% 
  mutate(Meldedatum=as_date(Meldedatum)) %>% 
  filter(Meldedatum<="2021-08-01") %>% 
  mutate(date_plus_6m=as_date(Meldedatum)+days(183),
         jahrmonat=year(date_plus_6m)*100+month(date_plus_6m)) %>% 
  group_by(Bundesland, jahrmonat) %>% 
  summarise(auffrischen=sum(AnzahlFall))
rki_neuinfekte <- bind_rows(rki_neuinfekte,
                            rki_neuinfekte %>% 
                              group_by(jahrmonat) %>% 
                              summarise(auffrischen=sum(auffrischen)) %>% 
                              mutate(Bundesland="Gesamt")) %>% 
  select(region=Bundesland, jahrmonat, auffrischen)

auffrischen_september <- rki_vacc %>% 
  filter(metric %in% c("personen_voll_kumulativ",
                       "indikation_beruf_voll",
                       "indikation_pflegeheim_voll",
                       "indikation_medizinisch_voll",
                       "indikation_alter_voll") &
           date>="2021-03-02" &
           date<="2021-08-01") %>% 
  pivot_wider(id_cols=c("date", "region"),
              names_from="metric",
              values_from="value") %>% 
  group_by(region) %>%
  arrange(date) %>% 
  mutate(new_alle_voll=c(personen_voll_kumulativ[1], 
                         diff(personen_voll_kumulativ)),
         new_beruf_voll=c(indikation_beruf_voll[1], 
                          diff(indikation_beruf_voll)),
         new_pflegeheim_voll=c(indikation_pflegeheim_voll[1], 
                               diff(indikation_pflegeheim_voll)),
         new_medizinisch_voll=c(indikation_medizinisch_voll[1], 
                                diff(indikation_medizinisch_voll)),
         new_alter_voll=c(indikation_alter_voll[1], 
                          diff(indikation_alter_voll)),
         date_plus_6m=date + days(183), # +months(6) ... 2021-03-31 and 2021-05-31 machen probleme?
         jahrmonat=year(date_plus_6m)*100+month(date_plus_6m)) %>% 
  group_by(region, jahrmonat) %>% 
  summarise(alle_auffrischen=sum(new_alle_voll),
            beruf_auffrischen=sum(new_beruf_voll),
            pflegeheim_auffrischen=sum(new_pflegeheim_voll),
            medizinisch_auffrischen=sum(new_medizinisch_voll),
            alter_auffrischen=sum(new_alter_voll),
            alleminusberuf_auffrischen=alle_auffrischen-beruf_auffrischen,
            summeohneberuf_auffrischen=pflegeheim_auffrischen +
              medizinisch_auffrischen +
              alter_auffrischen) %>% 
  mutate(region=case_when(
    region=="DE" ~ "Gesamt",
    region=="DE-BE" ~ "Berlin",
    region=="DE-BB" ~ "Brandenburg",
    region=="DE-BY" ~ "Bayern",
    region=="DE-BW" ~ "Baden-Württemberg",
    region=="DE-HB" ~ "Bremen",
    region=="DE-HH" ~ "Hamburg",
    region=="DE-NI" ~ "Niedersachsen",
    region=="DE-HE" ~ "Hessen",
    region=="DE-MV" ~ "Mecklenburg-Vorpommern",
    region=="DE-SH" ~ "Schleswig-Holstein",
    region=="DE-SN" ~ "Sachsen",
    region=="DE-ST" ~ "Sachsen-Anhalt",
    region=="DE-RP" ~ "Rheinland-Pfalz",
    region=="DE-SL" ~ "Saarland",
    region=="DE-NW" ~ "Nordrhein-Westfalen",
    region=="DE-TH" ~ "Thüringen",
    TRUE ~ region
  )) %>% 
  select(region, jahrmonat, auffrischen=alleminusberuf_auffrischen) %>% 
  filter(jahrmonat==202109)

auffrischen_oktober <- rki_vacc %>% 
  filter(metric %in% c("personen_voll_kumulativ_alter_60plus") &
           date>="2021-04-07" &
           date<="2021-08-01") %>% 
  pivot_wider(id_cols=c("date", "region"),
              names_from="metric",
              values_from="value") %>% 
  group_by(region) %>%
  arrange(date) %>% 
  drop_na() %>% 
  mutate(new_60plus_voll=c(personen_voll_kumulativ_alter_60plus[1], 
                               diff(personen_voll_kumulativ_alter_60plus)),
         date_plus_6m=date + days(183), # +months(6) ... 2021-03-31 and 2021-05-31 machen probleme?
         jahrmonat=year(date_plus_6m)*100+month(date_plus_6m)) %>% 
  group_by(region, jahrmonat) %>% 
  summarise(sechzigplus_auffrischen=sum(new_60plus_voll)) %>% 
  mutate(region=case_when(
    region=="DE" ~ "Gesamt",
    region=="DE-BE" ~ "Berlin",
    region=="DE-BB" ~ "Brandenburg",
    region=="DE-BY" ~ "Bayern",
    region=="DE-BW" ~ "Baden-Württemberg",
    region=="DE-HB" ~ "Bremen",
    region=="DE-HH" ~ "Hamburg",
    region=="DE-NI" ~ "Niedersachsen",
    region=="DE-HE" ~ "Hessen",
    region=="DE-MV" ~ "Mecklenburg-Vorpommern",
    region=="DE-SH" ~ "Schleswig-Holstein",
    region=="DE-SN" ~ "Sachsen",
    region=="DE-ST" ~ "Sachsen-Anhalt",
    region=="DE-RP" ~ "Rheinland-Pfalz",
    region=="DE-SL" ~ "Saarland",
    region=="DE-NW" ~ "Nordrhein-Westfalen",
    region=="DE-TH" ~ "Thüringen",
    TRUE ~ region
  )) %>% 
  select(region, jahrmonat, auffrischen=sechzigplus_auffrischen) %>% 
  filter(jahrmonat==202110)
# fehlende länder imputieren:
mean_oktober <- mean(auffrischen_oktober %>% 
  left_join(pop_bev, by=c("region"="Bundesland")) %>% 
  mutate(rate_region=auffrischen/pop) %>%
  pull(rate_region))
auffrischen_oktober <- pop_bev %>% 
  left_join(auffrischen_oktober, 
            by=c("Bundesland"="region")) %>% 
  mutate(auffrischen=case_when(
    is.na(auffrischen) ~ round(mean_oktober*pop),
    TRUE ~ auffrischen
  )) %>% 
  mutate(jahrmonat=202110) %>% 
  select(region=Bundesland, jahrmonat, auffrischen)
auffrischen_oktober <- bind_rows(auffrischen_oktober,
                                 tibble(
                                   region="Gesamt",
                                   jahrmonat=202110,
                                   auffrischen=sum(auffrischen_oktober$auffrischen)
                                 )) %>% 
  left_join(auffrischen_september %>% 
              select(region, september=auffrischen), by="region") %>% 
  mutate(auffrischen=auffrischen-september) %>% 
  select(region, jahrmonat, auffrischen)

auffrischen_november <- rki_vacc %>% 
  filter(metric %in% c("personen_voll_astrazeneca_kumulativ",
                       "personen_voll_janssen_kumulativ",
                       "personen_voll_biontech_kumulativ",
                       "personen_voll_moderna_kumulativ") &
           date>="2021-03-02" &
           date<="2021-08-01") %>% 
  pivot_wider(id_cols=c("date", "region"),
              names_from="metric",
              values_from="value") %>% 
  group_by(region) %>%
  arrange(date) %>% 
  mutate(new_jj=c(personen_voll_janssen_kumulativ[1], 
                  diff(personen_voll_janssen_kumulativ)),
         new_az_voll=c(personen_voll_astrazeneca_kumulativ[1], 
                  diff(personen_voll_astrazeneca_kumulativ)),
         new_bnt_voll=c(personen_voll_biontech_kumulativ[1], 
                       diff(personen_voll_biontech_kumulativ)),
         new_mod_voll=c(personen_voll_moderna_kumulativ[1], 
                       diff(personen_voll_moderna_kumulativ)),
         date_plus_6m=date + days(183), # +months(6) ... 2021-03-31 and 2021-05-31 machen probleme?
         jahrmonat=year(date_plus_6m)*100+month(date_plus_6m)) %>% 
  group_by(region, jahrmonat) %>% 
  summarise(jj_auffrischen=sum(new_jj),
            az_auffrischen=sum(new_az_voll),
            bnt_auffrischen=sum(new_bnt_voll),
            mod_auffrischen=sum(new_mod_voll)) %>% 
  mutate(region=case_when(
    region=="DE" ~ "Gesamt",
    region=="DE-BE" ~ "Berlin",
    region=="DE-BB" ~ "Brandenburg",
    region=="DE-BY" ~ "Bayern",
    region=="DE-BW" ~ "Baden-Württemberg",
    region=="DE-HB" ~ "Bremen",
    region=="DE-HH" ~ "Hamburg",
    region=="DE-NI" ~ "Niedersachsen",
    region=="DE-HE" ~ "Hessen",
    region=="DE-MV" ~ "Mecklenburg-Vorpommern",
    region=="DE-SH" ~ "Schleswig-Holstein",
    region=="DE-SN" ~ "Sachsen",
    region=="DE-ST" ~ "Sachsen-Anhalt",
    region=="DE-RP" ~ "Rheinland-Pfalz",
    region=="DE-SL" ~ "Saarland",
    region=="DE-NW" ~ "Nordrhein-Westfalen",
    region=="DE-TH" ~ "Thüringen",
    TRUE ~ region
  )) %>% 
  mutate(auffrischen=jj_auffrischen+az_auffrischen) %>% 
  filter(jahrmonat>=202111) %>% 
  select(region, jahrmonat, auffrischen)

ausblick_auffrischen <- bind_rows(auffrischen_september,
                                  auffrischen_oktober,
                                  auffrischen_november) %>% 
  left_join(rki_neuinfekte, by=c("region", "jahrmonat")) %>% 
  mutate(auffrischen=auffrischen.x+auffrischen.y) %>% 
  select(region, jahrmonat, auffrischen) %>% 
  pivot_wider(id_cols = region,
              names_from=jahrmonat,
              values_from=auffrischen)

write_csv(ausblick_auffrischen, "data/auffrischen.csv")
library(openxlsx)
write.xlsx(ausblick_auffrischen, "data/auffrischen.xlsx",
           overwrite=TRUE)
