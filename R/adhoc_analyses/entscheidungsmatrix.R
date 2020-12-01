library(tidyverse)
library(lubridate)
library(sf)
library(zicolors)
library(cowplot)
library(jsonlite)

source("./R/functions.R")

maxdatum <- max(as_date(rki$Meldedatum))

kreise_em <- vorwarnzeitergebnis %>%
  filter((id>17 | id==11) & date==maxdatum) %>%
  left_join(., aktuell %>% select(id, name, R0), by="id") %>%
  mutate(cases_je_100Tsd=round(cases/(EW_insgesamt/100000)),
         R0=round(R0,digits = 2)) %>%
  mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>%
  left_join(., aktuell %>%
              filter(id>0 & id<17) %>%
              select(blid=id, Bundesland=name)) %>%
  arrange(blid,id) %>%
  select(id=id,
    Kreis=name,
         Bundesland,
         "R"=R0,
         "STI"=Faelle_letzte_7_Tage_je100TsdEinw,
         "STI60"=`Faelle_letzte_7_Tage_je100TsdEinw_60+`,
         "Vorwarnzeit"=Vorwarnzeit
  ) %>%
  mutate(EM_Vorwarnzeit=case_when(
    Vorwarnzeit > 21 ~ ">21",
    Vorwarnzeit <= 21 & Vorwarnzeit >= 7 ~ "7-21",
    Vorwarnzeit < 7 ~ "<7"
  ), 
  EM_Inzidenz=case_when(
    STI60 < 50 ~ "<50",
    STI60 >= 50 & STI60 <= 150 ~ "50-150",
    STI60 > 150 ~ ">150"
  ), 
  EM=case_when(
    STI60 < 50 & Vorwarnzeit > 21 ~ 1,
    STI60 < 50 & Vorwarnzeit <= 21 & Vorwarnzeit >= 7 ~ 1,
    STI60 >= 50 & STI60 <= 150 & Vorwarnzeit > 21 ~ 1,
    STI60 < 50 & Vorwarnzeit < 7 ~ 2,
    STI60 >= 50 & STI60 <= 150 & Vorwarnzeit <= 21 & Vorwarnzeit >= 7 ~ 2,
    STI60 > 150 & Vorwarnzeit > 21 ~ 2,
    STI60 >= 50 & STI60 <= 150 & Vorwarnzeit < 7 ~ 2,
    STI60 > 150 & Vorwarnzeit <= 21 & Vorwarnzeit >= 7 ~ 2,
    STI60 > 150 & Vorwarnzeit < 7 ~ 3
  )) %>%
  select(IdLandkreis=id, EM, EM_Vorwarnzeit, EM_Inzidenz) %>%
  mutate(IdLandkreis=ifelse(IdLandkreis==11, 11000000, IdLandkreis))

# deutschlandkarte
KRS <- read_sf("./data/shp/kreise.shp")
REG <- KRS %>%
  group_by(RS) %>%
  count() %>%
  ungroup() %>%
  mutate(IdLandkreis=as.numeric(RS)*1000) %>%
  left_join(., kreise_em, by="IdLandkreis")
BL <- KRS %>%
  group_by(SN_L) %>%
  summarise(geometry = sf::st_union(geometry))

# final one
karte_em <-
  REG %>%
  ggplot() + 
  geom_sf(aes(fill=factor(EM)),
          lwd=0.2) +
  geom_sf(data=BL, col="black", lwd=0.4, alpha=0) +
  theme_void() +
  scale_fill_manual(values=c("#7EC850" ,"#FF8800","#FF0000")) +
  labs(fill=paste0("Risikostufe")) 
karte_em 

# table
table(kreise_em$EM_Vorwarnzeit, kreise_em$EM_Inzidenz, useNA = "always")
table(kreise_em$EM, useNA = "always")
