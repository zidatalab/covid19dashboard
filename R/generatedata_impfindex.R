library("tidyverse")
library("lubridate")
# library("zicolors")
library("jsonlite")
library(ISOcodes)

## impfungen in praxen nach bundeslaendern von lars
impfungen_praxen_bl <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_bl_date_wirkstoff.csv") %>% 
  select(-1) %>% 
  rename(`BNT/Pfizer`=`BNT162b2`,
         `Moderna`=`mRNA-1273`,
         `AZ`=`AZD1222`,
         `J&J`=`Ad26.COV2.S`)

## static Bevoelkerung
bev_kreise <- read_delim(
  "../data/destatis_12411-0017_rein_2019.csv", 
  ";", 
  escape_double = FALSE,
  col_types = cols(Schlüssel = col_integer(), 
                   `unter 3 Jahre` = col_integer(), 
                   `3 bis unter 6 Jahre` = col_integer(), 
                   `6 bis unter 10 Jahre` = col_integer(), 
                   `10 bis unter 15 Jahre` = col_integer(), 
                   `15 bis unter 18 Jahre` = col_integer(), 
                   `18 bis unter 20 Jahre` = col_integer(), 
                   `20 bis unter 25 Jahre` = col_integer(), 
                   `25 bis unter 30 Jahre` = col_integer(), 
                   `30 bis unter 35 Jahre` = col_integer(), 
                   `35 bis unter 40 Jahre` = col_integer(), 
                   `40 bis unter 45 Jahre` = col_integer(), 
                   `45 bis unter 50 Jahre` = col_integer(), 
                   `50 bis unter 55 Jahre` = col_integer(), 
                   `55 bis unter 60 Jahre` = col_integer(), 
                   `60 bis unter 65 Jahre` = col_integer(), 
                   `65 bis unter 75 Jahre` = col_integer(), 
                   `75 Jahre und mehr` = col_integer(), 
                   Insgesamt = col_integer()), 
  locale = locale(encoding = "WINDOWS-1252"), 
  trim_ws = TRUE
)

bev_gesamt_laender <- bev_kreise %>%
  mutate(blid=floor(`Schlüssel`/1000)) %>%
  group_by(blid) %>%
  summarise(population=sum(Insgesamt, na.rm=TRUE), .groups="drop") %>%
  mutate(Name=c(
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
    "Thüringen"
  ))

bev_gesamt_laender <- bind_rows(
  bev_gesamt_laender,
  tibble(blid=0, 
         population=sum(bev_gesamt_laender$population), 
         Name="Gesamt")
)

## Impfungen laut RKI
rki_vacc <- # tryCatch(
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
    vacc_zahlen <- read_csv("../data/vacc_zahlen_ard.csv")
#     return(vacc_zahlen)
#   }
# )
rki_vacc <- rki_vacc %>% 
  mutate(region=ifelse(region=="DE", "DE", paste0("DE-", region))) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(geo=ifelse(region=="DE", "Germany", geo),
         geotype=ifelse(region=="DE", "nation", "state"))

rki_github_bl <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv")
dritt_tag_bl <- rki_github_bl %>% 
  mutate(blid=as.integer(BundeslandId_Impfort)) %>% 
  filter(Impfserie==3) %>% 
  group_by(Impfdatum, blid) %>% 
  summarise(dritte_alle=sum(Anzahl)) %>% 
  left_join(bev_gesamt_laender %>% select(blid, Name), by="blid") %>% 
  select(Bundesland=Name, date=Impfdatum, dritte_alle)
write_csv(dritt_tag_bl, "../data/tabledata/dritte_alle_rki.csv") # [1:finalrow, ]


## Impfdosen Bunddashboard
impfdashboardde <- read_tsv(
  "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region")
bunddashboard_daten <- impfdashboardde %>% 
  #filter(date <= "2021-05-11") %>%
  mutate(
    geo=ifelse(region=="DE-BUND", "Zentren_Bund", geo),
    Hersteller=case_when(
      impfstoff=="comirnaty" ~ "BNT/Pfizer",
      impfstoff=="moderna" ~ "Moderna",
      impfstoff=="astra" ~ "AZ",
      impfstoff=="johnson" ~ "J&J",
      TRUE ~ "error"),
    KW=isoweek(date),
    wochentag=wday(date, week_start = 1),
    KW=ifelse(wochentag>=5, KW+1, KW),
    Jahr=year(date)
  ) %>% 
  # filter(region!="Zentren_Bund") %>%
  group_by(Hersteller, KW, Jahr, geo) %>% 
  summarise(Lieferung_gesamt=sum(dosen),
            .groups="drop")

bunddashboard_daten <- 
  bind_rows(bunddashboard_daten,
            bunddashboard_daten %>%
              group_by(Hersteller, KW, Jahr) %>% 
              summarise(Lieferung_gesamt=sum(Lieferung_gesamt),
                        .groups="drop") %>% 
              mutate(geo="Gesamt"))

# last day
rki_vacc_lastday <- rki_vacc %>%
  filter(date==max(date)) %>% # deal with public holidays and/or delayed RKI upload
  mutate(geo=ifelse(geo=="Germany", "Gesamt", geo),
         metric=case_when(
           metric=="personen_voll_biontech_kumulativ" ~ "zweit_BNT/Pfizer",
           metric=="personen_erst_biontech_kumulativ" ~ "erst_BNT/Pfizer",
           metric=="personen_voll_moderna_kumulativ" ~ "zweit_Moderna",
           metric=="personen_erst_moderna_kumulativ" ~ "erst_Moderna",
           metric=="personen_voll_astrazeneca_kumulativ" ~ "zweit_AZ",
           metric=="personen_erst_astrazeneca_kumulativ" ~ "erst_AZ",
           metric=="personen_voll_janssen_kumulativ" ~ "zweit_J&J",
           metric=="personen_voll_janssen_kumulativ" ~ "erst_J&J",
           TRUE ~ "else"
         )) %>%
  filter(metric!="else")

## verabreichte dosen
hersteller_laender <- expand_grid(geo=unique(rki_vacc_lastday$geo),
                                  erstzweit=c("erst", "zweit"),
                                  hersteller=unique(
                                    bunddashboard_daten$Hersteller)
)

dosen_verabreicht <- rki_vacc_lastday %>%
  left_join(bev_gesamt_laender %>% 
              select(population, geo=Name) %>% 
              mutate(geo=ifelse(geo=="Gesamt", "Germany", geo)),
            by="geo") %>% 
  separate(metric, into=c("erstzweit", "hersteller"), sep="_") %>%
  right_join(hersteller_laender,
             by=c("hersteller", "geo", "erstzweit")) %>%
  group_by(geo) %>%
  fill(population) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("geo", "population", "hersteller"),
              names_from = "erstzweit",
              values_from=value) %>%
  rename(
    dosen_verabreicht_zweit=zweit,
    dosen_verabreicht_erst=erst
  ) %>%
  mutate(dosen_verabreicht_erst=ifelse(!is.na(dosen_verabreicht_erst), 
                                       dosen_verabreicht_erst, 
                                       0),
         dosen_verabreicht_zweit=ifelse(!is.na(dosen_verabreicht_zweit), 
                                        dosen_verabreicht_zweit,
                                        0)) %>%
  left_join(bunddashboard_daten %>%
              # filter(KW<=kwstart-1 | Jahr==2020) %>%
              group_by(Hersteller, geo) %>%
              summarise(dosen_geliefert=sum(Lieferung_gesamt), .groups="drop"),
            by=c("hersteller"="Hersteller", "geo"="geo")) %>%
  # left_join(dosen_planung_kw %>%
  #             filter(KW<=kwstart-1) %>%
  #             group_by(Hersteller, Bundesland) %>%
  #             summarise(dosen_geliefert=sum(Lieferung), .groups="drop"),
  #           by=c("hersteller"="Hersteller", "geo"="Bundesland")) %>%
  mutate(dosen_geliefert=ifelse(!is.na(dosen_geliefert), dosen_geliefert, 0),
         zugelassen=case_when(
           hersteller%in%c("AZ", "BNT/Pfizer", "Moderna", "J&J") ~ 1, # 
           TRUE ~ 0
         )) %>%
  mutate(#Stand_letzteKW=isoweek(prognosestart-days(1)),
         #Stand_BMG=bmgstand,
         lager=dosen_geliefert-dosen_verabreicht_erst-dosen_verabreicht_zweit)

## small check
sum(dosen_verabreicht %>% filter(geo=="Gesamt") %>% 
      pull(dosen_geliefert))

## rearrange laender lieferungen praxen
for (h in unique(dosen_verabreicht$hersteller)) {
  laender_neg <- dosen_verabreicht %>% 
    filter(lager<0 & hersteller==h) %>% 
    pull(geo)
  negsum <- sum(dosen_verabreicht %>% 
                  filter(lager<0 & hersteller==h) %>% 
                  pull(lager), na.rm = TRUE)
  if (negsum<0) {
    if (! "Gesamt" %in% laender_neg) {
      laender_neg_plusgesamt <- c(laender_neg, "Gesamt")
    } else{
      laender_neg_plusgesamt <- laender_neg
    }
    dosen_verabreicht[dosen_verabreicht$hersteller==h & !dosen_verabreicht$geo%in%laender_neg_plusgesamt, ] <- 
      dosen_verabreicht[dosen_verabreicht$hersteller==h & !dosen_verabreicht$geo%in%laender_neg_plusgesamt, ] %>% 
      mutate(dosen_geliefert=dosen_geliefert+round(negsum*population/sum(population[!geo%in%laender_neg_plusgesamt])))
    dosen_verabreicht[dosen_verabreicht$hersteller==h & dosen_verabreicht$geo%in%laender_neg, ] <- 
      dosen_verabreicht[dosen_verabreicht$hersteller==h & dosen_verabreicht$geo%in%laender_neg, ] %>% 
      mutate(dosen_geliefert=dosen_geliefert-lager)
  }
}

## small check
sum(dosen_verabreicht %>% filter(geo=="Gesamt") %>% 
      pull(dosen_geliefert))

dosen_verabreicht <- dosen_verabreicht %>% 
  select(-population, -lager)

write_json(dosen_verabreicht, "../data/tabledata/impfsim_start.json")

# für impfindex
fuerpraxen <- impfdashboardde %>% 
  filter(einrichtung=="arztpraxen") %>%
  mutate(
    geo=ifelse(region=="DE-BUND", "Zentren_Bund", geo),
    Hersteller=case_when(
      impfstoff=="comirnaty" ~ "BNT/Pfizer",
      impfstoff=="moderna" ~ "Moderna",
      impfstoff=="astra" ~ "AZ",
      impfstoff=="johnson" ~ "J&J",
      TRUE ~ "error"),
    KW=isoweek(date),
    wochentag=wday(date, week_start = 1),
    KW=ifelse(wochentag>=5, KW+1, KW),
    Jahr=year(date)
  ) %>% 
  # filter(region!="Zentren_Bund") %>%
  group_by(Hersteller, KW, Jahr, geo) %>% 
  summarise(Lieferung_Praxen=sum(dosen),
            .groups="drop")
fuerpraxen <- 
  bind_rows(fuerpraxen,
            fuerpraxen %>%
              group_by(Hersteller, KW, Jahr) %>% 
              summarise(Lieferung_Praxen=sum(Lieferung_Praxen),
                        .groups="drop") %>% 
              mutate(geo="Gesamt"))
write_json(fuerpraxen, "../data/tabledata/impfsim_lieferungenpraxen.json")

fuerimpfzentren <- impfdashboardde %>% 
  filter(einrichtung=="impfzentren") %>%
  mutate(
    geo=ifelse(region=="DE-BUND", "Zentren_Bund", geo),
    Hersteller=case_when(
      impfstoff=="comirnaty" ~ "BNT/Pfizer",
      impfstoff=="moderna" ~ "Moderna",
      impfstoff=="astra" ~ "AZ",
      impfstoff=="johnson" ~ "J&J",
      TRUE ~ "error"),
    KW=isoweek(date),
    wochentag=wday(date, week_start = 1),
    KW=ifelse(wochentag>=5, KW+1, KW),
    Jahr=year(date)
  ) %>% 
  # filter(region!="Zentren_Bund") %>%
  group_by(Hersteller, KW, Jahr, geo) %>% 
  summarise(Lieferung_IZ=sum(dosen),
            .groups="drop")
fuerimpfzentren <- 
  bind_rows(fuerimpfzentren,
            fuerimpfzentren %>%
              group_by(Hersteller, KW, Jahr) %>% 
              summarise(Lieferung_IZ=sum(Lieferung_IZ),
                        .groups="drop") %>% 
              mutate(geo="Gesamt"))
write_json(fuerimpfzentren, "../data/tabledata/impfsim_lieferungenimpfzentren.json")

fuerandere <- impfdashboardde %>% 
  filter(einrichtung=="bund" | einrichtung=="betriebe") %>%
  mutate(
    geo=ifelse(region=="DE-BUND", "Bund", geo),
    geo=ifelse(region=="DE-Betriebe", "Betriebe", geo),
    Hersteller=case_when(
      impfstoff=="comirnaty" ~ "BNT/Pfizer",
      impfstoff=="moderna" ~ "Moderna",
      impfstoff=="astra" ~ "AZ",
      impfstoff=="johnson" ~ "J&J",
      TRUE ~ "error"),
    KW=isoweek(date),
    wochentag=wday(date, week_start = 1),
    KW=ifelse(wochentag>=5, KW+1, KW),
    Jahr=year(date)
  ) %>% 
  # filter(region!="Zentren_Bund") %>%
  group_by(Hersteller, KW, Jahr, geo) %>% 
  summarise(Lieferung_Bund_oder_Betriebe=sum(dosen),
            .groups="drop")

fuerandere <- 
  bind_rows(fuerandere,
            fuerandere %>%
              group_by(Hersteller, KW, Jahr) %>% 
              summarise(Lieferung_Bund_oder_Betriebe=sum(Lieferung_Bund_oder_Betriebe),
                        .groups="drop") %>% 
              mutate(geo="Gesamt"))
write_json(fuerandere, "../data/tabledata/impfsim_lieferungenandere.json")


# impfungen in impfzentren nach ländern
impfungen_praxen_bl_kw <- impfungen_praxen_bl %>% 
  mutate(KW=isoweek(date), Jahr=ifelse(KW==53, 2020, year(date)), 
         JahrKW=100*Jahr+KW) %>% 
  group_by(KW, Jahr, JahrKW, Bundesland) %>% 
  summarise(Impfungen_Praxen_KW=sum(`BNT/Pfizer`+
                                      `Moderna`+
                                      `AZ`+
                                      `J&J`), .groups="drop") %>% 
  mutate(Impfungen_Praxen_KW=ifelse(is.na(Impfungen_Praxen_KW),
                                          0,
                                          Impfungen_Praxen_KW))
impfungen_praxen_bl_kw <- bind_rows(impfungen_praxen_bl_kw,
                                    impfungen_praxen_bl_kw %>% 
                                      group_by(KW, Jahr, JahrKW) %>% 
                                      summarise(Bundesland="Germany",
                                                Impfungen_Praxen_KW=sum(Impfungen_Praxen_KW),
                                                .groups="drop"))
iz_vergangen_laender <- rki_vacc %>% 
  filter(metric=="dosen_kumulativ") %>% 
  mutate(KW=isoweek(date), Jahr=ifelse(KW==53, 2020, year(date)), 
         JahrKW=100*Jahr+KW) %>% 
  group_by(KW, Jahr, JahrKW, geo) %>% 
  summarise(Impfungen_kum=max(value), .groups="drop") %>% 
  arrange(JahrKW) %>% 
  group_by(geo) %>% 
  mutate(Impfungen_gesamt_KW=Impfungen_kum-lag(Impfungen_kum, default=0)) %>% 
  left_join(impfungen_praxen_bl_kw,
            by=c("KW", "Jahr", "JahrKW", "geo"="Bundesland")
            ) %>% 
  mutate(Impfungen_Praxen_KW=ifelse(is.na(Impfungen_Praxen_KW),
                                    0,
                                    Impfungen_Praxen_KW),
         Impfungen_IZ=Impfungen_gesamt_KW-Impfungen_Praxen_KW) %>% 
  arrange(JahrKW) %>% 
  group_by(geo) %>% 
  mutate(Impfungen_IZ_kum=cumsum(Impfungen_IZ))
# iz_vergangen_laender <-
#   bind_rows(rki_vacc %>% 
#               filter(metric=="dosen_kumulativ" & date<="2021-04-06"),
#             rki_vacc %>% 
#               filter(metric=="dosen_kumulativ_impfstelle_zentral" &
#                        date>="2021-04-07")) %>% 
#   mutate(KW=isoweek(date), Jahr=ifelse(KW==53, 2020, year(date)), 
#          JahrKW=100*Jahr+KW) %>% 
#   group_by(KW, Jahr, JahrKW, geo) %>% 
#   summarise(Impfungen_IZ_kum=max(value), .groups="drop") %>% 
#   arrange(JahrKW) %>% 
#   group_by(geo) %>% 
#   mutate(Impfungen_IZ=Impfungen_IZ_kum-lag(Impfungen_IZ_kum, default=0))
write_json(iz_vergangen_laender, "../data/tabledata/impfsim_impfungenimpfzentren_historisch.json")

# tabelle für SZ
erstzweit_gesamt <- tibble(
  Datum=floor_date(rki_vacc_lastday$date[1], unit="week", week_start = 0) + days(7*0:7),
  Vollgeimpft=c(dosen_verabreicht %>% 
                  filter(geo=="Gesamt") %>% 
                  summarise(dosen_verabreicht_zweit=sum(dosen_verabreicht_zweit, 
                                                        na.rm=TRUE)) %>% 
                  pull(dosen_verabreicht_zweit),
                rep(0, 7)),
  Erstgeimpft=rep(dosen_verabreicht %>% 
                    filter(geo=="Gesamt") %>% 
                    summarise(dosen_verabreicht_erst=sum(dosen_verabreicht_erst, 
                                                          na.rm=TRUE)) %>% 
                    pull(dosen_verabreicht_erst), 8),
  EW_gesamt=rep(83166711, 8),
  EW_ueber18=rep(69488809, 8)
)
impfluecke <- erstzweit_gesamt$Erstgeimpft[1] - erstzweit_gesamt$Vollgeimpft[1]
impfenaktuellewoche <- impfluecke/6 * as.integer(erstzweit_gesamt$Datum[2]-today()+1)/7
erstzweit_gesamt$Vollgeimpft[2] <- round(erstzweit_gesamt$Vollgeimpft[1] +
                                           impfenaktuellewoche)
for (i in 3:7) {
  erstzweit_gesamt$Vollgeimpft[i] <- round(erstzweit_gesamt$Vollgeimpft[i-1] +
                                             impfluecke/6)
}
erstzweit_gesamt$Vollgeimpft[8] <- erstzweit_gesamt$Erstgeimpft[8]

write_csv(erstzweit_gesamt, "../data/tabledata/aktuelle_kapazitaet_erstzweit.csv") # [1:finalrow, ]

