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
# rki <- tbl(conn,"rki") %>% 
#   collect()
# rki_original <- rki
# rki <- rki %>%
#   mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
#          AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
# rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)), 
#                                   maxdate=max(date(Meldedatum)))
# rkiidkreise <- unique(rki$IdLandkreis)
# rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")

### boostern praxen tempo
kbv_impfen <- tbl(conn,"kbvcovidvacczi") %>% 
  collect()
# kbv_age <- tbl(conn,"kbvcovidagegroup") %>% 
#   collect()

kreise_plz <- read_csv2("data/plz_krs_2016.csv") %>% 
  mutate(PLZ=as.integer(PLZ)) %>% 
  select(PLZ, Kreis2016, Kreis2016name) %>% 
  bind_rows(tibble(PLZ=0, Kreis2016=0, Kreis2016name="unbekannt"))

kbv_plz_kv_liste <- read_csv("data/kbv_plz_liste.csv") %>% 
  group_by(plz) %>% filter(row_number()==1) %>% 
  bind_rows(tibble(plz=0, kv="unbekannt"))

rki_vacc_kreise <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv") %>% 
  mutate(LandkreisId=ifelse(LandkreisId_Impfort=="u" | 
                              LandkreisId_Impfort=="17000", 
                            "0",
                            LandkreisId_Impfort),
         LandkreisId=as.integer(LandkreisId),
         Impfdatum=as_date(Impfdatum)) %>% 
  select(vacc_date=Impfdatum, LandkreisId, vacc_series=Impfschutz,
         anzahl_alleorte=Anzahl) %>% 
  mutate(vacc_date=as_date(ifelse(vacc_date<as_date("2021-01-08"),
                                  as_date("2021-01-08"),
                                  vacc_date))) %>% 
  group_by(vacc_date, LandkreisId, vacc_series) %>% 
  summarise(anzahl_alleorte=sum(anzahl_alleorte))

kbv_impfen_plz <- kbv_impfen %>% 
  mutate(anzahl=as.integer(anzahl),
         arzt_plz=as.integer(arzt_plz),
         vacc_series=as.integer(vacc_series)) %>% 
  select(vacc_date, PLZ=arzt_plz, 
         vacc_series, anzahl_praxen=anzahl) %>% 
  mutate(vacc_date=as_date(ifelse(vacc_date<as_date("2021-01-08"),
                                  as_date("2021-01-08"),
                                  vacc_date))) %>% 
  group_by(vacc_date, PLZ, vacc_series) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
            .groups="drop") %>% 
  mutate(PLZ=ifelse(PLZ%in%setdiff(unique(PLZ), unique(kreise_plz$PLZ)), 
                    0, 
                    PLZ))

kbv_impfen_kreise_kv <- kbv_impfen_plz %>% 
  left_join(kreise_plz, by="PLZ") %>% 
  left_join(kbv_plz_kv_liste %>% 
              select(plz, kv), by=c("PLZ"="plz")) %>% 
  group_by(vacc_date, Kreis2016, vacc_series) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
            kv=kv[1],
            Kreis2016name=Kreis2016name[1],
            .groups="drop")

impfen_kreise_kbv_rki <- full_join(kbv_impfen_kreise_kv,
                                   rki_vacc_kreise,
                                   by=c("vacc_date",
                                        "Kreis2016"="LandkreisId",
                                        "vacc_series")) %>% 
  mutate(across(c(anzahl_praxen, anzahl_alleorte), ~replace(., is.na(.), 0))) %>% 
  group_by(Kreis2016) %>% 
  mutate(kv=max(kv, na.rm=TRUE),
         Kreis2016name=max(Kreis2016name, na.rm=TRUE)) %>% 
  ungroup()

impfen_kreise_kbv_rki_kw <- impfen_kreise_kbv_rki %>% 
  mutate(KW=isoweek(vacc_date)) %>% 
  group_by(KW, Kreis2016, vacc_series) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen),
            anzahl_alleorte=sum(anzahl_alleorte),
            anteil_praxen=anzahl_praxen/max(1, anzahl_alleorte),
            anteil_oegd=(anzahl_alleorte-anzahl_praxen)/max(1, anzahl_alleorte),
            kv=kv[1],
            Kreis2016name=Kreis2016name[1],
            .groups="drop")

auffr_zeitreihe <- impfen_kreise_kbv_rki_kw %>% 
  filter(vacc_series==3) %>% 
  select(Kreis2016, Kreis2016name,
         kv, KW, 
         new_auffr_praxen=anzahl_praxen,
         new_auffr=anzahl_alleorte)

vollst_zeitreihe <- impfen_kreise_kbv_rki_kw %>% 
  filter(vacc_series==2) %>% 
  select(Kreis2016, Kreis2016name,
         kv, KW, 
         new_vollst_praxen=anzahl_praxen,
         new_vollst=anzahl_alleorte)

istsoll_auffr <- expand_grid(KW=c(13:56), 
                             Kreis2016=unique(impfen_kreise_kbv_rki$Kreis2016)) %>%  #expand_grid(Monat=c(1:13), geo=unique(rki_vacc$geo)) %>% 
  left_join(vollst_zeitreihe %>% 
              select(Kreis2016, Kreis2016name, kv) %>% 
              distinct(),
            by="Kreis2016") %>% 
  left_join(vollst_zeitreihe %>% 
              mutate(KW=ifelse(KW<=13, 13, KW)) %>% 
              group_by(Kreis2016, KW) %>% 
              summarise(new_vollst=sum(new_vollst, na.rm=TRUE),
                        new_vollst_praxen=sum(new_vollst_praxen, na.rm=TRUE),
                        .groups="drop"),
            by=c("KW", "Kreis2016")) %>% 
  group_by(Kreis2016) %>% 
  arrange(-KW) %>% 
  mutate(soll=lead(new_vollst, 27)) %>% # 7 für Monat 
  ungroup() %>% 
  left_join(auffr_zeitreihe %>% 
              select(Kreis2016, KW, new_auffr, new_auffr_praxen),
            by=c("Kreis2016", "KW"))

anteile_praxen_oegd_letztekw <- impfen_kreise_kbv_rki_kw %>% 
  filter(KW==isoweek(today())-1 & vacc_series==3) %>% 
  select(Kreis2016, 
         anzahl_praxen_letztekw=anzahl_praxen,
         anzahl_alleorte_letztekw=anzahl_alleorte,
         anteil_praxen_letztekw=anteil_praxen,
         anteil_oegd_letztekw=anteil_oegd)

anteile_praxen_oegd_kv_letztekw <- impfen_kreise_kbv_rki_kw %>% 
  filter(KW==isoweek(today())-1 & vacc_series==3) %>% 
  group_by(kv) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen),
            anzahl_alleorte=sum(anzahl_alleorte),
            anteil_praxen=anzahl_praxen/max(1, anzahl_alleorte),
            anteil_oegd=(anzahl_alleorte-anzahl_praxen)/max(1, anzahl_alleorte)) %>% 
  select(kv,
         anzahl_praxen_letztekw=anzahl_praxen,
         anzahl_alleorte_letztekw=anzahl_alleorte,
         anteil_praxen_letztekw=anteil_praxen,
         anteil_oegd_letztekw=anteil_oegd)

anzahl_praxen_oegd_diesekw <- impfen_kreise_kbv_rki_kw %>% 
  filter(KW==isoweek(today()) & vacc_series==3) %>% 
  select(Kreis2016, 
         anzahl_praxen_diesekw=anzahl_praxen,
         anzahl_alleorte_diesekw=anzahl_alleorte)

anzahl_praxen_oegd_kv_diesekw <- impfen_kreise_kbv_rki_kw %>% 
  filter(KW==isoweek(today()) & vacc_series==3) %>% 
  group_by(kv) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen),
            anzahl_alleorte=sum(anzahl_alleorte),
            anteil_praxen=anzahl_praxen/max(1, anzahl_alleorte),
            anteil_oegd=(anzahl_alleorte-anzahl_praxen)/max(1, anzahl_alleorte)) %>% 
  select(kv,
         anzahl_praxen_diesekw=anzahl_praxen,
         anzahl_alleorte_diesekw=anzahl_alleorte)

anteile_praxen_oegd_gesamt<- impfen_kreise_kbv_rki_kw %>% 
  filter(vacc_series==3) %>% 
  group_by(Kreis2016) %>%
  summarise(anzahl_praxen=sum(anzahl_praxen),
            anzahl_alleorte=sum(anzahl_alleorte),
            anteil_praxen=anzahl_praxen/max(1, anzahl_alleorte),
            anteil_oegd=(anzahl_alleorte-anzahl_praxen)/max(1, anzahl_alleorte)) %>% 
  select(Kreis2016,
         anteil_praxen_gesamtezeit=anteil_praxen,
         anteil_oegd_gesamtezeit=anteil_oegd)

anteile_praxen_oegd_kv_gesamt <- impfen_kreise_kbv_rki_kw %>% 
  filter(vacc_series==3) %>% 
  group_by(kv) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen),
            anzahl_alleorte=sum(anzahl_alleorte),
            anteil_praxen=anzahl_praxen/max(1, anzahl_alleorte),
            anteil_oegd=(anzahl_alleorte-anzahl_praxen)/max(1, anzahl_alleorte)) %>% 
  select(kv, anteil_praxen_gesamtezeit=anteil_praxen,
         anteil_oegd_gesamtezeit=anteil_oegd)

istsoll_stand <- istsoll_auffr %>% 
  filter(KW<=isoweek(max(rki_vacc_kreise$vacc_date))) %>% 
  group_by(Kreis2016) %>% 
  summarise(soll=sum(soll, na.rm=TRUE),
            ist=sum(new_auffr, na.rm=TRUE),
            Kreis2016name=Kreis2016name[1],
            kv=kv[1],
            .groups = "drop") %>% 
  mutate(luecke=ist-soll,
         istminussolldurchsoll=luecke/soll) %>% 
  mutate(luecke=-luecke, istminussolldurchsoll=-istminussolldurchsoll) %>% 
  left_join(anteile_praxen_oegd_letztekw, by="Kreis2016") %>% 
  left_join(anteile_praxen_oegd_gesamt, by="Kreis2016")

istsoll_stand_kv <- istsoll_stand %>% 
  group_by(kv) %>% 
  summarise(soll=sum(soll),
            ist=sum(ist),
            luecke=sum(luecke),
            istminussolldurchsoll=luecke/soll) %>% 
  left_join(anteile_praxen_oegd_kv_letztekw, by="kv") %>% 
  left_join(anteile_praxen_oegd_kv_gesamt, by="kv")

istsoll_ausblick <- istsoll_auffr %>% 
  filter(KW>=isoweek(today())) %>% 
  filter(KW<=max(istsoll_auffr$KW)) %>% 
  # rowwise() %>% 
  # mutate(luecke_verteilen=round(abs(
  #   istsoll_stand$luecke[istsoll_stand$Bundesland==geo[1]])),
  #        sollplusluecke=soll+
  #     round(luecke_verteilen/(max(istsoll_auffr$KW)-isoweek(max(vacc_zahlen$date))+1))
  #     # ifelse(KW<=isoweek(max(vacc_zahlen$date))+5, # auf nächste 6 wochen verteilt
  #     #        round(luecke_verteilen/6),
  #     #        0)
  #   ) %>% 
  # ungroup() %>% 
  select(KW, Kreis2016, Kreis2016name, kv, soll) %>% 
  arrange(Kreis2016name, KW) %>% 
  pivot_wider(id_cols=c(Kreis2016, Kreis2016name, kv), names_from = KW, values_from = soll)

istsoll_ausblick_kv <- istsoll_ausblick %>% 
  group_by(kv) %>% 
  summarise(across(-c(Kreis2016, Kreis2016name), sum),
            .groups="drop")

if (dim(anzahl_praxen_oegd_diesekw)[1]==0) {
  final_auffrischen_kreise <- istsoll_stand %>% 
    left_join(istsoll_ausblick %>% 
                select(-kv, -Kreis2016name), by="Kreis2016") %>% 
    select(-contains("oegd"), -anteil_praxen_gesamtezeit) %>% 
    mutate(aktuelleKW=0)
  
  final_auffrischen_kv <- istsoll_stand_kv %>% 
    left_join(istsoll_ausblick_kv, by="kv") %>% 
    select(-contains("oegd"), -anteil_praxen_gesamtezeit) %>% 
    mutate(aktuelleKW=0)
} else {
  final_auffrischen_kreise <- istsoll_stand %>% 
    left_join(istsoll_ausblick %>% 
                select(-kv, -Kreis2016name), by="Kreis2016") %>% 
    select(-contains("oegd"), -anteil_praxen_gesamtezeit) %>% 
    left_join(anzahl_praxen_oegd_diesekw %>% 
                select(Kreis2016, aktuelleKW=anzahl_alleorte_diesekw),
              by="Kreis2016")
  
  final_auffrischen_kv <- istsoll_stand_kv %>% 
    left_join(istsoll_ausblick_kv, by="kv") %>% 
    select(-contains("oegd"), -anteil_praxen_gesamtezeit) %>% 
    left_join(anzahl_praxen_oegd_kv_diesekw %>% 
                select(kv, aktuelleKW=anzahl_alleorte_diesekw),
              by="kv")
}
  
final_auffrischen_kv <- bind_rows(final_auffrischen_kv %>% 
              summarise(kv="Gesamt",
                        across(-c(kv, istminussolldurchsoll, anteil_praxen_letztekw),
                               sum),
                        istminussolldurchsoll=luecke/soll,
                        anteil_praxen_letztekw=anzahl_praxen_letztekw/max(1, anzahl_alleorte_letztekw)),
              final_auffrischen_kv %>% 
                filter(kv!="unbekannt")) %>% 
  relocate(KV=kv, "Soll Auffr."=soll,
           "Ist Auffr."=ist,
           "Soll nicht erfüllt"=luecke,
           "Anteil Soll nicht erfüllt"=istminussolldurchsoll,
           "Anzahl Auffr. in Praxen letzte KW"=anzahl_praxen_letztekw,
           "Anteil der Praxen an Auffr. letzte KW"=anteil_praxen_letztekw,
           "Gesamtzahl Auffr. letzte KW"=anzahl_alleorte_letztekw,
           "Ist aktuelle KW"=aktuelleKW)

final_auffrischen_kreise <- final_auffrischen_kreise %>% 
  relocate("Gemeindeschlüssel"=Kreis2016,
           "Kreisname"=Kreis2016name,
           "KV"=kv,
           "Soll Auffr."=soll,
           "Ist Auffr."=ist,
           "Soll nicht erfüllt"=luecke,
           "Anteil Soll nicht erfüllt"=istminussolldurchsoll,
           "Anzahl Auffr. in Praxen letzte KW"=anzahl_praxen_letztekw,
           "Anteil der Praxen an Auffr. letzte KW"=anteil_praxen_letztekw,
           "Gesamtzahl Auffr. letzte KW"=anzahl_alleorte_letztekw,
           "Ist aktuelle KW"=aktuelleKW)

library(openxlsx)
# auffr_table <- list(stand_dritt=istsoll_stand,
#                     ausblick_dritt=istsoll_ausblick)
write.xlsx(final_auffrischen_kv, "data/auffrischen_kw_kv.xlsx",
           overwrite=TRUE)
write.xlsx(final_auffrischen_kreise, "data/auffrischen_kw_kreise.xlsx",
           overwrite=TRUE)








##bayern
impfungen_gesamt_bayern <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  filter(Bundesland=="Bayern") %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_gesamt_bund <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  # filter(Bundesland=="Bayern") %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_auffr_bayern <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  filter(Bundesland=="Bayern" & vacc_series==3) %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_auffr_bund <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  filter(vacc_series==3) %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_gesamt_bayern_tag <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  filter(Bundesland=="Bayern") %>% 
  group_by(vacc_date) %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_gesamt_bund_tag <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  # filter(Bundesland=="Bayern") %>% 
  group_by(vacc_date) %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_auffr_bayern_tag <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  filter(Bundesland=="Bayern" & vacc_series==3) %>% 
  group_by(vacc_date) %>% 
  summarise(impfungen_praxen=sum(anzahl))
impfungen_auffr_bund_tag <- kbv_boostern %>% 
  mutate(arzt_plz=as.integer(arzt_plz),
         anzahl=as.integer(anzahl)) %>% 
  left_join(kbv_plz_liste, by=c("arzt_plz"="plz")) %>% 
  filter(vacc_series==3) %>% 
  group_by(vacc_date) %>% 
  summarise(impfungen_praxen=sum(anzahl))

impfungen_tag <- bind_rows(impfungen_gesamt_bayern_tag %>% mutate(geo="Bayern"),
                           impfungen_gesamt_bund_tag %>% mutate(geo="Bund")) %>% 
  left_join(bind_rows(impfungen_auffr_bayern_tag %>% mutate(geo="Bayern"),
                      impfungen_auffr_bund_tag %>% mutate(geo="Bund")) %>% 
              rename(auffrimpfungen_praxen=impfungen_praxen),
            by=c("geo", "vacc_date"))

ggplot(impfungen_tag %>% 
         filter(vacc_date>="2021-10-01") %>% 
         rename(`Auffr.-Impfungen`=auffrimpfungen_praxen,
                `alle Impfungen`=impfungen_praxen) %>% 
         pivot_longer(cols=c(`alle Impfungen`, `Auffr.-Impfungen`), 
                      values_to='Anzahl'), 
       aes(x=vacc_date, y=Anzahl)) +
  geom_line() +
  theme_zi() +
  geom_smooth(se=FALSE) +
  facet_wrap(geo~name, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA), 
                     labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ","))
# nur bund
ggplot(impfungen_tag %>% 
         filter(vacc_date>="2021-10-01" & geo=="Bund") %>% 
         rename(`Auffr.-Impfungen`=auffrimpfungen_praxen,
                `alle Impfungen`=impfungen_praxen) %>% 
         pivot_longer(cols=c(`alle Impfungen`, `Auffr.-Impfungen`), 
                      values_to='Anzahl'), 
       aes(x=vacc_date, y=Anzahl)) +
  geom_line() +
  theme_zi() +
  geom_smooth(se=FALSE) +
  facet_wrap(.~name, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA), 
                     labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ","))

## niedersachsen
# vollst_zeitreihe_ni <- rki_vacc %>% 
#   filter(metric=="personen_voll_kumulativ" & isoyear(date)==2021 &
#            geo=="Niedersachsen") %>% 
#   mutate(KW=isoweek(date),
#          Monat=month(date)) %>% 
#   group_by(KW) %>% 
#   summarise(vollst_kumulativ=max(value), .groups="drop") %>% 
#   arrange(-KW) %>% 
#   mutate(vollst_kumulativ=ifelse(is.na(vollst_kumulativ), 0, vollst_kumulativ),
#          new_vollst=vollst_kumulativ-lead(vollst_kumulativ),
#          new_vollst=ifelse(is.na(new_vollst), vollst_kumulativ, new_vollst)) %>% 
#   ungroup() %>% 
#   filter(new_vollst>0) %>% 
#   select(KW, `neu vollst. geimpft`=new_vollst)
# write.xlsx(vollst_zeitreihe_ni, "data/niedersachsen_vollstgeimpft.xlsx",
#            overwrite=TRUE)

# erst biontech
bnt_zeitreihe <- rki_vacc %>% 
  filter(metric=="personen_erst_biontech_kumulativ" & isoyear(date)==2021) %>% 
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
  select(KW, new_vollst)
