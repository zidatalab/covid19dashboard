library(lubridate)
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(openxlsx)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

rki_hosp_bl <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")
# hosp_erweiterung <- expand_grid(Datum=as_date("2020-02-22")+days(0:7),
#                                 Bundesland=unique(rki_hosp_bl$Bundesland),
#                                 Altersgruppe=unique(rki_hosp_bl$Altersgruppe),
#                                 Neue_Hospitalisierung_Faelle=0,
#                                 `7T_Hospitalisierung_Faelle`=0)
rki_hosp_bl <- rki_hosp_bl %>% 
  mutate(ST_neueFaelle_estimate=round(`7T_Hospitalisierung_Faelle`/7),
         Bundesland_Id=as.integer(Bundesland_Id))

# nisa <- rki_hosp_bl %>% 
#   filter(Bundesland=="Niedersachsen") %>% 
#   filter(Altersgruppe=="00+") %>% 
#   mutate(dow=wday(Datum, week_start = 1),
#          quartal=quarter(Datum),
#          Jahr=year(Datum)) %>% 
#   filter(dow==1) %>% 
#   group_by(quartal, Jahr) %>% 
#   summarise(hosp=sum(`7T_Hospitalisierung_Faelle`),
#             .groups="drop") %>% 
#   arrange(Jahr, quartal)
# 
# bund <- rki_hosp_bl %>% 
#   filter(Bundesland!="Bundesgebiet") %>% 
#   filter(Altersgruppe=="00+") %>% 
#   mutate(dow=wday(Datum, week_start = 1),
#          quartal=quarter(Datum),
#          Jahr=year(Datum)) %>% 
#   filter(dow==1) %>% 
#   group_by(quartal, Jahr) %>% 
#   summarise(hosp=sum(`7T_Hospitalisierung_Faelle`),
#             .groups="drop") %>% 
#   arrange(Jahr, quartal)
# 
# mappe <- list(niedersachsen=nisa,
#               bund=bund)
# 
# write.xlsx(mappe, "data/tabledata/hospfaelle_quartal.xlsx", overwrite = TRUE)

# check udn abgleich excel rki klinische aspekte
bund_gesamt <- rki_hosp_bl %>% 
  filter(Bundesland=="Bundesgebiet") %>% 
  filter(Altersgruppe=="00+") %>% 
  mutate(dow=lubridate::wday(Datum, week_start = 1),
         quartal=quarter(Datum),
         Jahr=year(Datum)) %>% 
  filter(dow==1) %>% 
  # group_by(quartal, Jahr) %>% 
  summarise(hosp=sum(`7T_Hospitalisierung_Faelle`),
            .groups="drop")

# nach bl und ag

rki <- tbl(conn,"rki") %>% 
  collect()

rki_faelle_bl_ag <- rki %>% 
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A")) %>% 
  mutate(Bundesland_Id=floor(IdLandkreis/1000),
         Meldedatum=as_date(Meldedatum),
         wtag=lubridate::wday(Meldedatum, week_start = 1)) %>% 
  filter(wtag==1) %>% 
  group_by(Bundesland_Id, Altersgruppe, Meldedatum) %>% 
  summarise(AnzahlFall=sum(AnzahlFall[NeuerFall>=0], na.rm=TRUE),
            .groups = "drop")

bl_regstat_alter <- read_delim("data/Bev2019_Kreis_AG_rki_geschlecht.txt", 
                                   ";",
                                   escape_double = FALSE,
                                   col_types = cols(`...1` = col_skip()), 
                                   trim_ws = TRUE) %>%
  mutate(Bundesland_Id=floor(Kreis/1000)) %>%
  select(-Kreis, -m, -w) %>%
  group_by(Bundesland_Id, AG_rki) %>% 
  summarise(Bev=sum(ges),
            .groups = "drop") %>% 
  mutate(Altersgruppe=case_when(
    AG_rki==1 ~ "00-04",
    AG_rki==2 ~ "05-14",
    AG_rki==3 ~ "15-34",
    AG_rki==4 ~ "35-59",
    AG_rki==5 ~ "60-79",
    AG_rki==6 ~ "80+",
    TRUE ~ "unbekannt"
  )) %>% 
  select(-AG_rki)

rki_hosp_bl_ag <- rki_hosp_bl %>% 
  filter(Bundesland!="Bundesgebiet" & Altersgruppe!="00+") %>% 
  mutate(wtag=lubridate::wday(Datum, week_start = 1),
         Bundesland_Id=as.integer(Bundesland_Id)) %>% 
  filter(wtag==1) %>% 
  rename(Meldedatum=Datum) %>% 
  group_by(Bundesland_Id, Altersgruppe, Meldedatum) %>% 
    summarise(Bundesland=Bundesland[1],
              Hospitalisierung_Faelle=round(sum(`7T_Hospitalisierung_Faelle`, 
                                          na.rm=TRUE)/7),
              .groups = "drop")

bl_jm_ag <- rki_hosp_bl_ag %>% 
  left_join(rki_faelle_bl_ag, 
            by=c("Bundesland_Id", "Meldedatum", "Altersgruppe")) %>% 
  left_join(bl_regstat_alter,
            by=c("Bundesland_Id", "Altersgruppe")) %>% 
  drop_na()

bund_jm_ag <- bl_jm_ag %>% 
  group_by(Altersgruppe, Meldedatum) %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            .groups="drop") %>% 
  left_join(bl_regstat_alter %>% 
              group_by(Altersgruppe) %>% 
              summarise(Bev=sum(Bev), .groups="drop"),
            by="Altersgruppe")

bl_jm <- bl_jm_ag %>% 
  group_by(Bundesland_Id, Meldedatum, Bundesland) %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            .groups="drop") %>% 
  left_join(bl_regstat_alter%>% 
              group_by(Bundesland_Id) %>% 
              summarise(Bev=sum(Bev), .groups="drop"), by="Bundesland_Id")

bund_jm <- bl_jm_ag %>% 
  group_by(Meldedatum) %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            .groups="drop") %>% 
  bind_cols(bl_regstat_alter %>% 
              summarise(Bev=sum(Bev), .groups="drop"))

bl_ag <- bl_jm_ag %>% 
  group_by(Bundesland_Id, Altersgruppe, Bundesland) %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            Bev=Bev[1],
            .groups="drop")

bund_ag <- bl_jm_ag %>% 
  group_by(Altersgruppe) %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            .groups="drop") %>% 
  left_join(bl_regstat_alter %>% 
              group_by(Altersgruppe) %>% 
              summarise(Bev=sum(Bev), .groups="drop"),
            by="Altersgruppe")

bl_ohnealles <- bl_jm_ag %>% 
  group_by(Bundesland_Id, Bundesland) %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            .groups="drop") %>% 
  left_join(bl_regstat_alter%>% 
              group_by(Bundesland_Id) %>% 
              summarise(Bev=sum(Bev), .groups="drop"), by="Bundesland_Id")

bund_ohnealles <- bl_jm_ag %>% 
  summarise(across(c(Hospitalisierung_Faelle, AnzahlFall), sum),
            .groups="drop") %>% 
  bind_cols(bl_regstat_alter%>% 
              summarise(Bev=sum(Bev), .groups="drop"))

full_data <- bind_rows(bund_ohnealles %>% 
                         mutate(Bundesland_Id=0,
                                Altersgruppe="alle",
                                Meldedatum=as_date(0),
                                Bundesland="Gesamt"),
                       bl_ohnealles %>% 
                         mutate(Altersgruppe="alle",
                                Meldedatum=as_date(0)),
                       bund_ag %>% 
                         mutate(Bundesland_Id=0,
                                Meldedatum=as_date(0),
                                Bundesland="Gesamt"),
                       bl_ag %>% 
                         mutate(Meldedatum=as_date(0)),
                       bund_jm %>% 
                         mutate(Bundesland_Id=0,
                                Altersgruppe="alle",
                                Bundesland="Gesamt"),
                       bl_jm %>% 
                         mutate(Altersgruppe="alle"),
                       bund_jm_ag %>% 
                         mutate(Bundesland_Id=0,
                                Bundesland="Gesamt"),
                       bl_jm_ag) %>% 
  mutate(Anteil_Hosp_an_Faelle=round(Hospitalisierung_Faelle/AnzahlFall, 3),
         Hosp_Inzidenz_100TEW=round(Hospitalisierung_Faelle/Bev*100000, 1))

ggplot(full_data %>% 
         filter(Bundesland_Id==0 & Meldedatum!=as_date(0)),
       aes(x=Meldedatum, y=Hosp_Inzidenz_100TEW)) +
  geom_line(aes(col=Altersgruppe))

ggplot(full_data %>% 
         filter(Meldedatum!=as_date(0) & Altersgruppe=="alle"),
       aes(x=Meldedatum, y=Hosp_Inzidenz_100TEW)) +
  geom_line() +
  facet_wrap(. ~ Bundesland, ncol=4)

ggplot(full_data %>% 
         filter(Bundesland_Id==0 & Meldedatum!=as_date(0) & Meldedatum<="2022-03-31"),
       aes(x=Meldedatum, y=Anteil_Hosp_an_Faelle, col=Altersgruppe)) +
  geom_smooth(se=FALSE, span=0.3) +
  geom_point()

### rpart for stepwise constant
library(rpart)
mytree <- rpart(Anteil_Hosp_an_Faelle ~ Meldedatum + Altersgruppe, 
                data=full_data %>% 
                  filter(  Altersgruppe!="alle" &
                             Meldedatum!=as_date(0) & 
                           Meldedatum<="2022-03-31"))
plotdata_ag_hospquote <- full_data %>% 
  filter(Bundesland_Id==0 & 
           Altersgruppe!="alle" &
           Meldedatum!=as_date(0) & 
           Meldedatum<="2022-03-31") %>% 
  mutate(treepred=predict(mytree, data.frame(Meldedatum=Meldedatum,
                                             Altersgruppe=Altersgruppe)))

ggplot(plotdata_ag_hospquote,
       aes(x=Meldedatum, y=Anteil_Hosp_an_Faelle, col=Altersgruppe)) +
  geom_line(alpha=0.5) +
  geom_line(aes(y=treepred))

savedata_anteil_hosp <- plotdata_ag_hospquote %>% 
  bind_rows(plotdata_ag_hospquote %>% 
              group_by(Meldedatum) %>% 
              summarise(Anteil_Hosp_an_Faelle=sum(Anteil_Hosp_an_Faelle*Bev)/sum(Bev),
                        treepred=sum(treepred*Bev)/sum(Bev),
                        across(c(Hospitalisierung_Faelle:Bev), sum),
                        Bundesland_Id=0,
                        Altersgruppe="alle",
                        Bundesland="Gesamt",
                        Hosp_Inzidenz_100TEW=round(Hospitalisierung_Faelle/Bev*100000, 1)
                        ))

ggplot(savedata_anteil_hosp,
       aes(x=Meldedatum, y=Anteil_Hosp_an_Faelle, col=Altersgruppe)) +
  geom_line(alpha=0.5) +
  geom_line(aes(y=treepred))

write_csv(savedata_anteil_hosp, "data/tree_prediction_hospquoten.csv")
