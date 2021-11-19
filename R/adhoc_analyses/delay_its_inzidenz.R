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

infektperiode <- 8

intensivregister_bl <- read_csv("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-bundeslaender.csv")

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
divi_all <- tbl(conn, "divi_all") %>% 
  collect() %>% 
  mutate(daten_stand=as_date(daten_stand))
map_bl <- tibble(
  BL_ID=0:16,
  geo=c("Gesamt",
        "Schleswig-Holstein",
        "Hamburg",
        "Niedersachsen",
        "Bremen",
        "Nordrhein-Westfalen",
        "Hessen",
        "Rheinland-Pfalz",
        "Baden-Württemberg",
        "Bayern",
        "Saarland",
        "Berlin",
        "Brandenburg",
        "Mecklenburg-Vorpommern",
        "Sachsen",
        "Sachsen-Anhalt",
        "Thüringen"),
  divi_geo=c(
    "GESAMT",
    "SCHLESWIG_HOLSTEIN",
    "HAMBURG",
    "NIEDERSACHSEN",
    "BREMEN",
    "NORDRHEIN_WESTFALEN",
    "HESSEN",
    "RHEINLAND_PFALZ",
    "BADEN_WUERTTEMBERG",
    "BAYERN",
    "SAARLAND",
    "BERLIN",
    "BRANDENBURG",
    "MECKLENBURG_VORPOMMERN",
    "SACHSEN",
    "SACHSEN_ANHALT",
    "THUERINGEN"
  )
)

rki <- tbl(conn,"rki") %>% 
  collect()

rki_mappings <- read_csv("data/rki_mappings_landkreise.csv") %>% 
  mutate(IdLandkreis=as.integer(IdLandkreis))

rki_original <- rki
rki <- rki %>%
  left_join(rki_mappings, by="IdLandkreis") %>% 
  mutate(AnzahlFall=ifelse(NeuerFall>=0, AnzahlFall, 0),
         AnzahlTodesfall=ifelse(NeuerTodesfall>=0, AnzahlTodesfall, 0))
rkitimeframe <- rki %>% summarise(mindate=min(date(Meldedatum)), 
                                  maxdate=max(date(Meldedatum)))
rkiidkreise <- unique(rki$IdLandkreis)
rkiagegroups <- c("A00-A04", "A05-A14", "A15-A34", "A35-A59", "A60-A79", "A80+")
Kreise_allvalues <- expand_grid(Meldedatum=rkitimeframe$maxdate, IdLandkreis=rkiidkreise)
Kreise <- rki %>%
  mutate(Meldedatum=date(Meldedatum)) %>%
  full_join(., Kreise_allvalues, by=c("Meldedatum", "IdLandkreis"))
n_missingkreise <- 0
for (idkreis in rkiidkreise) { # take care of delayed reporting Landkreise (e.g. Hamburg 02000)
  if (is.na((Kreise %>% filter(Meldedatum==rkitimeframe$maxdate & IdLandkreis==idkreis) %>% pull(AnzahlFall))[1])) {
    n_missingkreise <- n_missingkreise+1
    sixdaysbefore <- Kreise %>% filter(Meldedatum>=rkitimeframe$maxdate-6 & IdLandkreis==idkreis)
    for (ag in rkiagegroups) {
      Kreise <- Kreise %>%
        add_row(IdBundesland=sixdaysbefore$IdBundesland[1],
                Bundesland=sixdaysbefore$Bundesland[1],
                Landkreis=sixdaysbefore$Landkreis[1],
                Altersgruppe=ag,
                Geschlecht="unbekannt",
                AnzahlFall=round(sum(sixdaysbefore$AnzahlFall[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6),
                AnzahlTodesfall=round(sum(sixdaysbefore$AnzahlTodesfall[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6),
                Meldedatum=rkitimeframe$maxdate,
                IdLandkreis=idkreis,
                AnzahlGenesen=round(sum(sixdaysbefore$AnzahlGenesen[sixdaysbefore$Altersgruppe==ag], na.rm = TRUE)/6))
    }
  }
}
cat("Kreise ohne aktuelle Meldung: ", n_missingkreise, "\n")
rki <- Kreise

rki_alter_destatis_kreise <- rki %>% lazy_dt() %>%
  group_by(Meldedatum, Altersgruppe, IdLandkreis) %>% # this takes long unfortunately... but much faster with dtplyr!
  summarise(AnzahlFall=sum(AnzahlFall,na.rm = T),
            AnzahlTodesfall=sum(AnzahlTodesfall,na.rm=T), .groups="drop") %>% 
  collect() %>%
  filter(Altersgruppe!="unbekannt") %>%
  mutate(id=as.integer(IdLandkreis)*1000) %>%
  filter(!is.na(id)) %>%
  mutate(Altersgruppe=str_remove_all(Altersgruppe,"A"),
         Altersgruppe=ifelse(Altersgruppe %in% c("00-04", "05-14"),
                             "0-15", Altersgruppe)) %>%
  group_by(Meldedatum,Altersgruppe, id) %>% 
  summarise("Fälle"=sum(AnzahlFall , na.rm = T),
            "Todesfälle"=sum(AnzahlTodesfall, na.rm=T), .groups="drop") %>% 
  pivot_wider(id_cols = c(Meldedatum, id),
              names_from = Altersgruppe,
              values_from = c("Fälle","Todesfälle"),
              values_fill = list("Fälle"=0, "Todesfälle"=0)) %>% ungroup() %>%
  mutate(Meldedatum=lubridate::as_date(Meldedatum), blid=floor(id/1000000),
         `Fälle_60+`=`Fälle_60-79`+`Fälle_80+`) %>%
  right_join(.,
             expand_grid(Meldedatum=seq(min(.$Meldedatum), max(.$Meldedatum), by="days"),
                         id=unique(.$id)),
             by=c("Meldedatum", "id")) %>%
  replace(is.na(.), 0) %>%
  group_by(id) %>%
  arrange(Meldedatum) %>%
  mutate(cases059=cumsum(`Fälle_0-15`+`Fälle_15-34`+`Fälle_35-59`),
         cases6079=cumsum(`Fälle_60-79`),
         cases80=cumsum(`Fälle_80+`),
         cases60=cumsum(`Fälle_60+`)) %>%
  mutate(Infected059=cases059-lag(cases059, infektperiode),
         Infected6079=cases6079-lag(cases6079, infektperiode),
         Infected80=cases80-lag(cases80, infektperiode),
         Infected60=cases60-lag(cases60, infektperiode)) %>% 
  fill(Infected059, Infected6079, Infected80, Infected60, .direction = "up") %>%
  ungroup() %>%
  mutate(blid=floor(id/1000000)) %>%
  as_tibble()
rki_alter_destatis <- bind_rows(rki_alter_destatis_kreise %>%
                                  mutate(`Todesfälle_60+`=`Todesfälle_60-79`+`Todesfälle_80+`), # kreise
                                rki_alter_destatis_kreise %>% # bundeslaender
                                  group_by(Meldedatum, blid) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            cases60=sum(cases60),
                                            Infected059=sum(Infected059),
                                            Infected6079=sum(Infected6079),
                                            Infected80=sum(Infected80),
                                            Infected60=sum(Infected60),
                                            `Fälle_15-34`=sum(`Fälle_15-34`),
                                            `Fälle_35-59`=sum(`Fälle_35-59`),
                                            `Fälle_0-15`=sum(`Fälle_0-15`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            `Fälle_60-79`=sum(`Fälle_60-79`),
                                            `Fälle_80+`=sum(`Fälle_80+`),
                                            `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                            `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                            `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                            `Todesfälle_60-79`=sum(`Todesfälle_60-79`),
                                            `Todesfälle_80+`=sum(`Todesfälle_80+`),
                                            `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`, na.rm = TRUE), .groups="drop") %>%
                                  mutate(id=blid),
                                rki_alter_destatis_kreise %>% # bund gesamt
                                  group_by(Meldedatum) %>%
                                  summarise(cases059=sum(cases059),
                                            cases6079=sum(cases6079),
                                            cases80=sum(cases80),
                                            cases60=sum(cases60),
                                            Infected059=sum(Infected059),
                                            Infected6079=sum(Infected6079),
                                            Infected80=sum(Infected80),
                                            Infected60=sum(Infected60),
                                            `Fälle_15-34`=sum(`Fälle_15-34`),
                                            `Fälle_35-59`=sum(`Fälle_35-59`),
                                            `Fälle_0-15`=sum(`Fälle_0-15`),
                                            `Fälle_60+`=sum(`Fälle_60+`),
                                            `Fälle_60-79`=sum(`Fälle_60-79`),
                                            `Fälle_80+`=sum(`Fälle_80+`),
                                            `Todesfälle_15-34`=sum(`Todesfälle_15-34`),
                                            `Todesfälle_35-59`=sum(`Todesfälle_35-59`),
                                            `Todesfälle_0-15`=sum(`Todesfälle_0-15`),
                                            `Todesfälle_60-79`=sum(`Todesfälle_60-79`),
                                            `Todesfälle_80+`=sum(`Todesfälle_80+`),
                                            `Todesfälle_60+`=sum(`Todesfälle_60-79`+`Todesfälle_80+`, na.rm = TRUE), .groups="drop") %>%
                                  mutate(id=0, blid=0))

#### icurates nach altersgruppen
# delay für fälle-->icu:
rkidivi <- rki_alter_destatis %>%
  filter(id==0) %>%
  left_join(., divi_all %>% 
              filter(id==0), by=c("Meldedatum"="daten_stand")) %>% 
  drop_na() %>% 
  mutate(inf60steigerung=round(100*Infected60/lag(Infected60, 7)-100),
         itsteigerung=round(100*faelle_covid_aktuell/lag(faelle_covid_aktuell, 7)-100))
lengthrkidivi <- dim(rkidivi)[1]
autocorhorizont <- 30
obshorizont <- 100
autocors <- matrix(0, ncol=(autocorhorizont+1),
                   nrow=(lengthrkidivi-obshorizont+1-autocorhorizont))
for (lag in 0:autocorhorizont) {
  for (startidx in 1:(lengthrkidivi-obshorizont-autocorhorizont+1)) {
    autocors[startidx, lag+1] <- 
      cor(rkidivi$Infected60[startidx:(startidx+obshorizont-1)],
          rkidivi$faelle_covid_aktuell[(startidx+lag):(startidx+obshorizont-1+lag)])
    }
  }
# iculag <- which.max(autocors)-1
iculag_dyn <- apply(autocors, 1, which.max)
plot(iculag_dyn, ylim=c(0,autocorhorizont))
iculag_dyn_plot_data <- tibble(iculag_last100=iculag_dyn,
                               endtag=rkidivi$Meldedatum[obshorizont:(lengthrkidivi-autocorhorizont)])
ggplot(iculag_dyn_plot_data,
       aes(x=endtag, y=iculag_last100)) +
  geom_point()

ggplot(rkidivi %>% 
         pivot_longer(cols=c(Infected60, faelle_covid_aktuell)),
       aes(x=Meldedatum, y=value)) +
  geom_point() +
  facet_wrap(.~name, ncol=1, scales="free_y")

ggplot(rkidivi %>% 
         pivot_longer(cols=contains("steigerung")),
       aes(x=Meldedatum, y=value, col=name)) +
  geom_point() #+
  # facet_wrap(.~name, ncol=1, scales="free_y")


lmhorizont <- 30
itslm_data <- tibble(its=rkidivi %>%
                       filter(Meldedatum>=max(Meldedatum)-days(lmhorizont-1)) %>% 
                       pull(faelle_covid_aktuell),
                     infiziertue60=rkidivi %>% 
                       filter(Meldedatum>=max(Meldedatum)-days(lmhorizont+7) &
                                Meldedatum<max(Meldedatum)-days(7)) %>% 
                       pull(Infected60))

itslm <- lm(its ~ 1 + infiziertue60, itslm_data)
summary(itslm)
itslm$coefficients[1] + itslm$coefficients[2]*(rkidivi %>% 
  filter(Meldedatum>=max(Meldedatum)-days(7)) %>% 
  pull(Infected60))

itslm$coefficients[1] + itslm$coefficients[2]*c(60000, 70000)

# itsglm <- glm(its ~ 1 + infiziertue60, itslm_data, family=poisson)
# summary(itsglm)
# 
# exp(predict(itsglm, newdata = tibble(infiziertue60=rkidivi %>% 
#                                        filter(Meldedatum>=max(Meldedatum)-days(7)) %>% 
#                                        pull(Infected60))))

# its one-week steigerung
# intensivregister_bl_lastday <- intensivregister_bl %>% 
#   filter(Datum==max(Datum)) %>% 
#   select(Bundesland, Aktuelle_COVID_Faelle_ITS, Freie_Intensivbetten, 
#          Freie_IV_Kapazitaeten_Gesamt, Freie_IV_Kapazitaeten_Davon_COVID)
# intensivregister_bl_lastday_inklgesamt <- bind_rows(
#   intensivregister_bl_lastday %>% summarise(Bundesland="GESAMT",
#                                             across(-Bundesland, sum)),
#   intensivregister_bl_lastday
# )

intensivregister_bl_gesamt <- bind_rows(intensivregister_bl %>% 
                                          group_by(Datum) %>% 
                                          summarise(across(-c(Bundesland, Behandlungsgruppe), sum, na.rm=TRUE),
                                                    Bundesland="GESAMT"),
                                        intensivregister_bl)

itssteigerungen <- divi_all %>% 
  left_join(map_bl, by=c("id"="BL_ID")) %>% 
  left_join(intensivregister_bl_gesamt %>% 
              mutate(Datum=as_date(Datum)), by=c("divi_geo"="Bundesland", "daten_stand"="Datum")) %>% 
  mutate(aktuelle_kapazitaet_covid=Aktuelle_COVID_Faelle_ITS+Freie_IV_Kapazitaeten_Davon_COVID) %>% 
  filter(id<17) %>% 
  mutate(Meldedatum=as_date(daten_stand)) %>% 
  group_by(id) %>% 
  arrange(Meldedatum) %>% 
  mutate(itssteigerung_7tage=Aktuelle_COVID_Faelle_ITS/lag(Aktuelle_COVID_Faelle_ITS, 7)) %>% 
  ungroup() %>% 
  select(Meldedatum, id, itssteigerung_7tage, Aktuelle_COVID_Faelle_ITS, 
         aktuelle_kapazitaet_covid, geo,
         Freie_IV_Kapazitaeten_Davon_COVID) %>% 
  filter(Meldedatum>max(Meldedatum)-days(7)) %>% 
  group_by(id) %>% 
  arrange(rev(Meldedatum)) %>% 
  summarise(itssteigerung_7tage=mean(itssteigerung_7tage),
            faelle_covid_aktuell=Aktuelle_COVID_Faelle_ITS[1],
            aktuelle_kapazitaet_covid=aktuelle_kapazitaet_covid[1],
            # ICU_Betten=ICU_Betten[1],
            betten_frei=Freie_IV_Kapazitaeten_Davon_COVID[1],
            geo=geo[1],
            .groups="drop"
            ) %>% 
  mutate(its_projektion_naechstewoche=round(itssteigerung_7tage*faelle_covid_aktuell),
         auslastung_projektion_naechstewoche=round(its_projektion_naechstewoche/aktuelle_kapazitaet_covid, 3),
         itssteigerung_7tage_prozent=round(itssteigerung_7tage-1, 3)) %>% 
  select(Bundesland=geo, 
         `durchschnittl. ITS-Steigerung pro Woche`=itssteigerung_7tage_prozent,
         `Aktuelle Belegung ITS mit COVID-19`=faelle_covid_aktuell,
         `Projektion Belegung ITS mit COVID-19 in 7 Tagen`=its_projektion_naechstewoche,
         `Aktuelle Kapazität für COVID-19-Behandlungen auf ITS`=aktuelle_kapazitaet_covid,
         `Projizierte Auslastung Kapazität in 7 Tagen`=auslastung_projektion_naechstewoche)
library(openxlsx)
write.xlsx(itssteigerungen, "data/its_projektion_7tage.xlsx", overwrite=TRUE)

