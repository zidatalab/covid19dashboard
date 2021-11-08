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
    vacc_zahlen <- read_csv("data/vacc_zahlen_ard.csv")
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
  group_by(Monat, geo) %>% 
  summarise(auffr_kumulativ=max(value), .groups="drop") %>% 
  group_by(geo) %>% 
  arrange(-Monat) %>% 
  mutate(auffr_kumulativ=ifelse(is.na(auffr_kumulativ), 0, auffr_kumulativ),
         new_auffr=auffr_kumulativ-lead(auffr_kumulativ)) %>% 
  ungroup() %>% 
  filter(new_auffr>0) %>% 
  select(geo, Monat, new_auffr)

vollst_zeitreihe <- rki_vacc %>% 
  filter(metric=="personen_voll_kumulativ" & isoyear(date)==2021) %>% 
  mutate(KW=isoweek(date),
         Monat=month(date)) %>% 
  group_by(Monat, geo) %>% 
  summarise(vollst_kumulativ=max(value), .groups="drop") %>% 
  group_by(geo) %>% 
  arrange(-Monat) %>% 
  mutate(vollst_kumulativ=ifelse(is.na(vollst_kumulativ), 0, vollst_kumulativ),
         new_vollst=vollst_kumulativ-lead(vollst_kumulativ),
         new_vollst=ifelse(is.na(new_vollst), vollst_kumulativ, new_vollst)) %>% 
  ungroup() %>% 
  filter(new_vollst>0) %>% 
  select(geo, Monat, new_vollst)

istsoll_auffr <- expand_grid(Monat=c(1:13), geo=unique(rki_vacc$geo)) %>% 
  left_join(vollst_zeitreihe %>% 
              # mutate(KW=ifelse(KW<=13, 13, KW)) %>% 
              mutate(Monat=ifelse(Monat<=3, 3, Monat)) %>% 
              group_by(geo, Monat) %>% 
              summarise(new_vollst=sum(new_vollst, na.rm=TRUE),
                        .groups="drop"),
            by=c("Monat", "geo")) %>% 
  group_by(geo) %>% 
  arrange(-Monat) %>% 
  mutate(soll=lead(new_vollst, 7)) %>% 
  ungroup() %>% 
  left_join(auffr_zeitreihe, by=c("geo", "Monat"))

istsoll_stand <- istsoll_auffr %>% 
  filter(Monat<=month(max(vacc_zahlen$date))) %>% 
  group_by(geo) %>% 
  summarise(soll=sum(soll, na.rm=TRUE),
            ist=sum(new_auffr, na.rm=TRUE),
            .groups = "drop") %>% 
  mutate(luecke=ist-soll,
         istminussolldurchsoll=luecke/soll) %>% 
  mutate(luecke=-luecke, istminussolldurchsoll=-istminussolldurchsoll) %>% 
  rename(Bundesland=geo)

istsoll_ausblick <- istsoll_auffr %>% 
  filter(Monat>=month(max(vacc_zahlen$date))) %>% 
  filter(Monat<=month(max(vacc_zahlen$date))+3) %>% 
  rowwise() %>% 
  mutate(luecke_1monat=round(abs(istsoll_stand$luecke[istsoll_stand$Bundesland==geo[1]])),
         sollplusluecke=soll+ifelse(Monat==11, luecke_1monat, 0)) %>% 
  ungroup() %>% 
  select(Monat, Bundesland=geo, sollplusluecke) %>% 
  arrange(Bundesland, Monat) %>% 
  pivot_wider(id_cols=Bundesland, names_from = Monat, values_from = sollplusluecke)
  
final_auffrischen_bl <- istsoll_stand %>% 
  left_join(istsoll_ausblick, by="Bundesland")

library(openxlsx)
# auffr_table <- list(stand_dritt=istsoll_stand,
#                     ausblick_dritt=istsoll_ausblick)
write.xlsx(final_auffrischen_bl, "data/auffrischen_monat_bl.xlsx",
           overwrite=TRUE)

### boostern praxen tempo
kbv_boostern <- tbl(conn,"kbvcovidvacczi") %>% 
  collect()
auffr_dosen_praxen <- kbv_boostern %>% 
  mutate(anzahl=as.integer(anzahl)) %>% 
  filter(vacc_series==3) %>% 
  group_by(kw) %>% 
  summarise(dosen=sum(anzahl))

## niedersachsen
vollst_zeitreihe_ni <- rki_vacc %>% 
  filter(metric=="personen_voll_kumulativ" & isoyear(date)==2021 &
           geo=="Niedersachsen") %>% 
  mutate(KW=isoweek(date),
         Monat=month(date)) %>% 
  group_by(KW) %>% 
  summarise(vollst_kumulativ=max(value), .groups="drop") %>% 
  arrange(-KW) %>% 
  mutate(vollst_kumulativ=ifelse(is.na(vollst_kumulativ), 0, vollst_kumulativ),
         new_vollst=vollst_kumulativ-lead(vollst_kumulativ),
         new_vollst=ifelse(is.na(new_vollst), vollst_kumulativ, new_vollst)) %>% 
  ungroup() %>% 
  filter(new_vollst>0) %>% 
  select(KW, `neu vollst. geimpft`=new_vollst)
write.xlsx(vollst_zeitreihe_ni, "data/niedersachsen_vollstgeimpft.xlsx")
