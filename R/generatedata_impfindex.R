library("tidyverse")
library("lubridate")
# library("zicolors")
library("jsonlite")
library(ISOcodes)

## impfungen in praxen nach bundeslaendern von lars
impfungen_praxen_bl <- read_csv("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_bl_date_wirkstoff.csv") %>% 
  select(-X1) %>% 
  rename(`BNT/Pfizer`=`BNT162b2`,
         `Moderna`=`mRNA-1273`,
         `AZ`=`AZD1222`,
         `J&J`=`Ad26.COV2.S`)

## static Bevoelkerung
bev_kreise <- read_delim(
  "data/destatis_12411-0017_rein_2019.csv", 
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
rki_vacc <- tryCatch(
  {
    mytemp <- tempfile()
    rki_vacc_data <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v3/all.csv"
    download.file(rki_vacc_data, mytemp, method = "curl")
    vacc_zahlen <- read_csv(mytemp)
    if (dim(vacc_zahlen)[2] != 5){
      stop("they changed the vacc table")
    } else {
      write_csv(vacc_zahlen, "./data/vacc_zahlen_ard.csv")
      vacc_zahlen
    }
  },
  error=function(e) {
    # read old data
    vacc_zahlen <- read_csv("./data/vacc_zahlen_ard.csv")
    return(vacc_zahlen)
  }
)
rki_vacc <- rki_vacc %>% 
  mutate(region=ifelse(region=="DE", "DE", paste0("DE-", region))) %>% 
  left_join(ISOcodes::ISO_3166_2 %>% 
              select(region=Code,
                     geo=Name), by="region") %>% 
  mutate(geo=ifelse(region=="DE", "Germany", geo),
         geotype=ifelse(region=="DE", "nation", "state"))




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

# last monday
rki_vacc_latestmonday <- rki_vacc %>%
  filter(date==prognosestart-days(1)) %>% # deal with public holidays and/or delayed RKI upload
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
hersteller_laender <- expand_grid(geo=unique(rki_vacc_latestmonday$geo),
                                  erstzweit=c("erst", "zweit"),
                                  hersteller=unique(
                                    dosen_planung_quartale$hersteller)
)

dosen_verabreicht <- rki_vacc_latestmonday %>%
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
              filter(KW<=kwstart-1 | Jahr==2020) %>%
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
  mutate(Stand_letzteKW=isoweek(prognosestart-days(1)),
         Stand_BMG=bmgstand,
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












bmgstand <-as_date("2021-08-05")

kw_abwann_curevac <- 39
# kw_abwannwieder_jj <- 23
# kw_biswann_praxen_az <- 24 # new infos: https://www.kbv.de/html/1150_52484.php


## dynamic parameters 
prognosestart <- floor_date(today(), unit="week", week_start = 1) # as_date("2021-05-03")
kwstart <- isoweek(prognosestart)
prognoseende <- as.Date("2021-12-31")

## read csvs aus pdfs des bmg
dosen_planung_quartale <- read_csv("data/impfdosen_planung_quartale.csv") %>%
  group_by(hersteller) %>% 
  mutate(dosen=ifelse(quartal>1 & jahr>=2021,dosen,cumsum(dosen))) %>%
  filter(quartal>=quarter(prognosestart) & jahr==2021)


# dosen_planung_quartale  <- read_csv("data/impfdosen_planung_quartale.csv")
# dosen_planung_quartale %>% group_by(hersteller)  %>% summarise(total = sum(dosen))


dosen_planung_laender_kw <- read_csv("data/kw_hersteller_laender.csv") # impfzentren

dosen_planung_praxen_kw <- read_csv("data/kw_praxen.csv") # hier nicht nach ländern, dafür auch für die (nahe) zukunft

dosen_planung_praxen_laender_kw <- read_csv( # daten vom pei: nach ländern, aber nur historisch
  "data/kw_hersteller_praxen_laender.csv"
) %>% 
  group_by(Hersteller) %>% 
  mutate(max_KW_PEI_Hersteller=max(KW)) %>% 
  ungroup()
maxpeikw <- max(dosen_planung_praxen_laender_kw$KW)

dosen_planung_betriebsaerztinnen <- read_csv("data/kw_betriebsaerztinnen.csv")

dosen_planung_gesamt_kw <- read_csv("data/kw_gesamt_lieferungen.csv") %>% # zukünftige gesamtlieferungen für brd nach kw
  group_by(Hersteller) %>% 
  mutate(max_KW_gesamt_BMG_Hersteller=max(KW)) %>% 
  ungroup()

maxkwbmgplanung <- max(dosen_planung_gesamt_kw$KW)







bev_u18_laender <- bev_kreise %>% # bevoelkerung über 18
  mutate(blid=floor(`Schlüssel`/1000),
         ueber18=Insgesamt-
           `15 bis unter 18 Jahre`-`10 bis unter 15 Jahre`-
           `6 bis unter 10 Jahre`-`3 bis unter 6 Jahre`-`unter 3 Jahre`) %>%
  group_by(blid) %>%
  summarise(ueber18=sum(ueber18, na.rm=TRUE), .groups="drop") %>%
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

bev_u18_laender <- bind_rows(bev_u18_laender,
                             tibble(blid=0, 
                                    ueber18=sum(bev_u18_laender$ueber18), 
                                    Name="Gesamt"))



## bevoelkerung 12-18
destatis_pop_by_state <- read_csv("data/destatis_pop_by_state.csv")
bev_u18_1860_ue60 <- destatis_pop_by_state %>% 
  mutate(u18_1860_60p=case_when(
    age<12 ~ "unter_12",
    age>=12 & age<18 ~ "12_bis_18",
    age>=18 & age<60 ~ "18_bis_60",
    age>=60 ~ "ueber_60",
    TRUE ~ "error"
  )) %>% 
  group_by(Bundesland, u18_1860_60p) %>% 
  summarise(population_age=sum(pop), .groups="drop")
bev_u18_1860_ue60 <- bind_rows(
  bev_u18_1860_ue60,
  bev_u18_1860_ue60 %>% 
    group_by(u18_1860_60p) %>% 
    summarise(population_age=sum(population_age), .groups="drop") %>% 
    mutate(Bundesland="Gesamt")) %>% 
  pivot_wider(id_cols = Bundesland,
              names_from = u18_1860_60p,
              values_from = population_age,
              names_prefix = "pop_")



## bisher geliefert und rest nach quartalsabsprache (Q1&Q2&Q3)
bishergeliefert_gesamt <- read_csv("data/impfdosen_planung_quartale.csv") %>% 
  filter(quartal<=3 | jahr==2020) %>% 
  group_by(hersteller) %>% 
  summarise(quartalabsprache=sum(dosen), .groups="drop") %>% 
  select(Hersteller=hersteller, quartalabsprache) %>% 
  left_join(dosen_planung_gesamt_kw %>% 
              filter(KW>=kwstart) %>% # & KW<=39
              group_by(Hersteller) %>% 
              summarise(kommtnochQ3=sum(Lieferung_gesamt),
                        .groups="drop"),
            by="Hersteller") %>% 
  mutate(kommtnochQ3=ifelse(is.na(kommtnochQ3), 0, kommtnochQ3)) %>% 
  left_join(bunddashboard_daten %>% 
              filter(geo=="Gesamt") %>% 
              group_by(Hersteller) %>% 
              summarise(bishergeliefert=sum(Lieferung_gesamt), .groups="drop"), 
            by="Hersteller") %>% 
  mutate(bishergeliefert=ifelse(is.na(bishergeliefert), 0, bishergeliefert),
         restq3=pmax(0, quartalabsprache-bishergeliefert-kommtnochQ3))

## zukuenftige Impfdosen nach BMG

dosen_planung_kw <- expand_grid(
  Hersteller=unique(dosen_planung_quartale$hersteller),
  KW=kwstart:maxkwbmgplanung,
  Bundesland=unique(dosen_planung_laender_kw$Bundesland)) %>% 
  left_join(dosen_planung_laender_kw %>% 
              filter(KW>=kwstart),
            by=c("KW", "Hersteller", "Bundesland")) %>% 
  mutate(Lieferung_IZ=ifelse(is.na(Lieferung_IZ), 0, Lieferung_IZ)) %>% 
  left_join(dosen_planung_laender_kw %>% 
              filter(Bundesland!="Gesamt") %>% 
              group_by(Hersteller, KW) %>% 
              summarise(Lieferung_IZ_gesamt=sum(Lieferung_IZ),
                        .groups="drop"),
            by=c("KW", "Hersteller")) %>% 
  mutate(Lieferung_IZ_gesamt=ifelse(is.na(Lieferung_IZ_gesamt), 0, Lieferung_IZ_gesamt)) %>% 
  left_join(dosen_planung_praxen_kw, by=c("Hersteller", "KW")) %>% 
  left_join(dosen_planung_betriebsaerztinnen, by=c("Hersteller", "KW")) %>% 
  left_join(dosen_planung_gesamt_kw, by=c("Hersteller", "KW")) %>% 
  left_join(dosen_planung_praxen_laender_kw, 
            by=c("Hersteller", "KW", "Bundesland")) %>% 
  left_join(bishergeliefert_gesamt,
            by="Hersteller") %>% 
  mutate(Lieferung_Praxen=ifelse(is.na(Lieferung_Praxen), 0, Lieferung_Praxen),
         Lieferung_Praxen_PEI=ifelse(
           is.na(Lieferung_Praxen_PEI), 
           0, Lieferung_Praxen_PEI
         ),
# TRY FIX PARTIALLY NOT KNOWN LIEFERUNGEN
#         Lieferung_gesamt=ifelse(is.na(Lieferung_gesamt), 
#                                 0, 
#                                 Lieferung_gesamt)
         ) %>% 
  left_join(bev_gesamt_laender %>%
              select(Name, population), by=c("Bundesland"="Name")) %>%
  mutate(Lieferung_IZ=ifelse(is.na(Lieferung_Betriebsaerztinnen), 
                             Lieferung_IZ,
                             Lieferung_IZ +
                               round(Lieferung_Betriebsaerztinnen*population/83166711)),
         Lieferung_IZ_gesamt=ifelse(is.na(Lieferung_Betriebsaerztinnen), 
                                    Lieferung_IZ_gesamt,
                                    Lieferung_IZ_gesamt +
                                      Lieferung_Betriebsaerztinnen)) %>% 
  mutate(Lieferung_Praxen=ifelse(
    Lieferung_gesamt>Lieferung_IZ_gesamt & 
      Lieferung_Praxen==0 & 
      Hersteller!="Moderna",
    Lieferung_gesamt-Lieferung_IZ_gesamt,
    Lieferung_Praxen
  )) %>% 
  group_by(Hersteller) %>% 
  mutate(max_KW_PEI_Hersteller=max(max_KW_PEI_Hersteller, na.rm=TRUE),
         max_KW_PEI_Hersteller=ifelse(is.infinite(max_KW_PEI_Hersteller),
                                      0,
                                      max_KW_PEI_Hersteller),
         max_KW_gesamt_BMG_Hersteller=max(max_KW_gesamt_BMG_Hersteller, na.rm=TRUE),
         max_KW_gesamt_BMG_Hersteller=ifelse(is.infinite(max_KW_gesamt_BMG_Hersteller),
                                             0,
                                             max_KW_gesamt_BMG_Hersteller)) %>% 
  mutate(Jahr=2021,
         Lieferung=case_when(
           Hersteller %in% unique(dosen_planung_praxen_laender_kw$Hersteller) &
             KW<=max_KW_PEI_Hersteller ~ 
             Lieferung_IZ + Lieferung_Praxen_PEI,
           Hersteller %in% unique(dosen_planung_praxen_kw$Hersteller) &
             KW>max_KW_PEI_Hersteller & KW<=max_KW_gesamt_BMG_Hersteller ~
             Lieferung_IZ + round(Lieferung_Praxen*population/83166711),
           Hersteller %in% unique(dosen_planung_praxen_kw$Hersteller) &
             KW > max_KW_gesamt_BMG_Hersteller ~ #max_KW_gesamt_BMG_Hersteller
             Lieferung_IZ + round(restq3/(39+1-max_KW_gesamt_BMG_Hersteller)*population/83166711),
           TRUE ~ ifelse(Lieferung_IZ==0, round(Lieferung_gesamt*population/83166711), Lieferung_IZ) + 0
         )
  ) %>% 
  ungroup()

### some checks...
## praxen
sum(dosen_planung_praxen_kw %>% filter(KW<=maxpeikw) %>% pull(Lieferung_Praxen))
sum(dosen_planung_praxen_laender_kw %>% 
      filter(Bundesland=="Gesamt" & KW<=maxpeikw) %>% pull(Lieferung_Praxen_PEI))

## kapazitäten IZ
kapazitaeten <- read_csv("data/Kapazitaeten_Bundeslaender_20210218.csv")





ausgangsdatensatz <- 
  expand_grid(
    Datum=prognosestart+days(seq(0, as.integer(prognoseende-prognosestart), 1)),
    Bundesland=unique(dosen_verabreicht$geo)
  ) %>%
  mutate(kw=isoweek(Datum),
         jahr=year(Datum),
         quartal=case_when(
           Datum<="2021-04-04" ~ 1,
           Datum>="2021-04-05" & Datum<="2021-07-04" ~ 2,
           Datum>="2021-07-05" & Datum<="2021-10-03" ~ 3,
           Datum>="2021-10-04" ~ 4
         ),
         werktag=ifelse(weekdays(Datum) %in% c("Samstag","Sonntag"), 0, 1)) %>%
  full_join(dosen_planung_quartale %>% rename(dosen_quartal=dosen),
            by=c("jahr","quartal")) %>%
  left_join(dosen_planung_kw %>% 
              rename(dosen_kw="Lieferung"),
            by=c("kw"="KW", "hersteller"="Hersteller", "Bundesland")) %>%
  left_join(dosen_verabreicht,
            by=c("hersteller", "Bundesland"="geo")) %>%
  left_join(bev_u18_laender, by=c("Bundesland"="Name")) %>%
  group_by(Bundesland) %>% 
  mutate(population=max(population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(dosen_quartal=round(dosen_quartal/83166711*population)) %>%
  mutate(dosen_kw=case_when(
    quartal==1 & !hersteller%in%c("AZ", "BNT/Pfizer", "Moderna") ~ 0,
    quartal==3 & hersteller=="Curevac" & kw>=kw_abwann_curevac ~
      dosen_quartal/(39-kw_abwann_curevac+1),
    # TRY FIX PARTIALLY UNKNOWN
    # kw<=maxkwbmgplanung & !is.na(Lieferung_gesamt) ~ round(Lieferung_gesamt/83166711*population),
    quartal==4 ~ round(dosen_quartal/13), # ein quartal=13 wochen
    TRUE ~ dosen_kw
  )) %>%
  mutate(dosen_kw=ifelse(is.na(dosen_kw), 0, dosen_kw)) %>%
  # group_by(Bundesland, quartal, hersteller) %>%
  # mutate(ausstehende_dosen=dosen_quartal-
  #          round(sum(dosen_kw, na.rm=TRUE)/7)) %>%
  # ungroup() %>%
  # group_by(Bundesland, hersteller) %>%
  # mutate(letzterwert=max(dosen_kw[quartal==2]), # actually höchster wert
  # ersterwertQ3=dosen_kw[Datum=="2021-07-05"]) %>% ungroup() %>% 
  mutate(ausstehende_dosen=0)

# az zweitimpfung verschieben zu biontech - ab kw 19 nicht mehr gebraucht
# az_erstzweit <- rki_vacc %>%
#   filter(metric %in% c("dosen_erst_astrazeneca_kumulativ",
#                        "dosen_voll_astrazeneca_kumulativ")) %>%
#   pivot_wider(id_cols = c(date, geo),
#               names_from = metric,
#               values_from = value) %>%
#   group_by(geo) %>%
#   arrange(date) %>%
#   mutate( # sometimes the kum. sum decreases...
#     dosen_erst_astrazeneca_kumulativ=cummax(dosen_erst_astrazeneca_kumulativ),
#     dosen_voll_astrazeneca_kumulativ=cummax(dosen_voll_astrazeneca_kumulativ),
#     erst_az=dosen_erst_astrazeneca_kumulativ-
#       lag(dosen_erst_astrazeneca_kumulativ),
#     zweit_az=dosen_voll_astrazeneca_kumulativ-
#       lag(dosen_voll_astrazeneca_kumulativ)
#   ) %>%
#   select(date, geo, erst_az, zweit_az) %>%
#   right_join(expand_grid(geo=unique(.$geo),
#                          date=seq(min(.$date), max(.$date)+12*7, by=1)),
#              by=c("geo", "date")) %>%
#   mutate(zweit_az_ausstehend_vor20210304=case_when(
#     date<=as_date("2021-03-04")+days(9*7) ~ lag(erst_az, 9*7, default=0),
#     TRUE ~ 0
#   ),
#   zweit_az_ausstehend_20210304bis11=case_when(
#     date<=as_date("2021-03-11")+days(9*7) &
#       date>as_date("2021-03-04")+days(9*7) ~ lag(erst_az, 9*7, default=0),
#     TRUE ~ 0
#   ),
#   zweit_az_ausstehend_20210311bis30=case_when(
#     date>as_date("2021-03-11")+days(12*7) &
#       date<=as_date("2021-03-30")+days(12*7)~ lag(erst_az, 12*7, default=0),
#     TRUE ~ 0
#   )) %>%
#   mutate(geo=ifelse(geo=="Germany", "Gesamt", geo))
# 
# az_laender_agg <- az_erstzweit %>%
#   mutate(KW=isoweek(date)) %>%
#   group_by(geo, KW) %>%
#   summarise(
#     zweitimpfung_az_vor20210304 =
#       sum(zweit_az_ausstehend_vor20210304, na.rm=TRUE),
#     zweitimpfung_az_vor20210330 =
#       sum(zweit_az_ausstehend_vor20210304, na.rm=TRUE) +
#       sum(zweit_az_ausstehend_20210304bis11, na.rm=TRUE) +
#       sum(zweit_az_ausstehend_20210311bis30, na.rm=TRUE),
#     .groups="drop") %>%
#   group_by(geo) %>%
#   summarise(zweitimpfung_az_vor20210304=sum(zweitimpfung_az_vor20210304),
#             zweitimpfung_az_vor20210330=sum(zweitimpfung_az_vor20210330))

ausgangsdatensatz_warteschlange <- ausgangsdatensatz %>%
  # left_join(az_laender_agg, by=c("Bundesland"="geo")) %>%
  mutate(warteschlange_zweit_kw=case_when(
    hersteller=="BNT/Pfizer" & kw<=kwstart+5 ~
      round((dosen_verabreicht_erst-dosen_verabreicht_zweit)/6),
    hersteller=="Moderna" & kw<=kwstart+5 ~
      round((dosen_verabreicht_erst-dosen_verabreicht_zweit)/6),
    hersteller=="AZ" & kw<=kwstart+11 ~
      round((dosen_verabreicht_erst-
               dosen_verabreicht_zweit)/12),
    # hersteller=="AZ" & kw<=kwstart+11 ~
    #   round((dosen_verabreicht_erst-
    #            dosen_verabreicht_zweit-
    #            zweitimpfung_az_vor20210304/4)/12),
    TRUE ~ 0
    ),
    warteschlange_zweit_kw=case_when( # noch den kleinen rest addieren/subtrahieren, der durch rundungsfehler entsteht
      hersteller=="BNT/Pfizer" & kw==kwstart ~
        warteschlange_zweit_kw +
        dosen_verabreicht_erst-dosen_verabreicht_zweit-
        round((dosen_verabreicht_erst-dosen_verabreicht_zweit)/6)*6,
      hersteller=="Moderna" & kw==kwstart ~
        warteschlange_zweit_kw +
        dosen_verabreicht_erst-dosen_verabreicht_zweit-
        round((dosen_verabreicht_erst-dosen_verabreicht_zweit)/6)*6,
      hersteller=="AZ" & kw==kwstart ~
        warteschlange_zweit_kw +
        dosen_verabreicht_erst-dosen_verabreicht_zweit-
        round((dosen_verabreicht_erst-
                 dosen_verabreicht_zweit)/12)*12,
      # hersteller=="AZ" & kw<=kwstart+11 ~
      #   round((dosen_verabreicht_erst-
      #            dosen_verabreicht_zweit-
      #            zweitimpfung_az_vor20210304/4)/12),
      TRUE ~ warteschlange_zweit_kw
    )# ,
  # warteschlange_zweit_kw=case_when( # move AZ to BNT
  #   hersteller=="BNT/Pfizer" & kw>=15 & kw<=18 ~
  #     warteschlange_zweit_kw+round(zweitimpfung_az_vor20210304/4),
  #   TRUE ~ warteschlange_zweit_kw
  # ),
  # dosen_verabreicht_erst=case_when(
  #   hersteller=="BNT/Pfizer" ~ dosen_verabreicht_erst +
  #     zweitimpfung_az_vor20210304/4,
  #   hersteller=="AZ" ~ dosen_verabreicht_erst -
  #     zweitimpfung_az_vor20210304/4
  # )
  )

# zwei szenarios fuer quartal 2

zeitreihe_gleichverteilt <- ausgangsdatensatz_warteschlange %>% # 
  # mutate(dosen_kw=case_when(
  #   quartal==2 & hersteller=="Curevac" & 
  #     kw>=kw_abwann_curevac ~
  #     round(ausstehende_dosen/(26-kw_abwann_curevac+1)),
  #   # quartal==2 & hersteller=="AZ" & kw>=kw_biswann_praxen_az+1 ~ 
  #   #   dosen_kw + round(ausstehende_dosen/(26-kw_biswann_praxen_az)), 
  #   # quartal==2 & hersteller=="BNT/Pfizer" & kw>=18 ~ 
  #   #   dosen_kw, 
  #   # quartal==2 & hersteller=="Moderna" & kw>=18 ~ 
  #   #   dosen_kw, 
  #   # quartal==2 & hersteller=="J&J" & kw>=kw_abwannwieder_jj ~ 
  #   #   round(ausstehende_dosen/(26-kw_abwannwieder_jj+1)),
  #   TRUE ~ dosen_kw
  # )) %>%
  mutate(dosen_verabreicht_erst=ifelse(Datum==prognosestart,
                                       dosen_verabreicht_erst, 
                                       0),
         dosen_verabreicht_zweit=ifelse(Datum==prognosestart, 
                                        dosen_verabreicht_zweit,
                                        0),
         dosenspeicher=ifelse(Datum==prognosestart, 
                              dosen_geliefert, 
                              0))

zeitreihe_linearverteilt <- ausgangsdatensatz_warteschlange %>% # 
  # mutate(dosen_kw=case_when(
  #   quartal==2 & hersteller=="Curevac" & 
  #     kw>=kw_abwann_curevac ~
  #     letzterwert + round((kw-kw_abwann_curevac+1)*2*
  #                           (ausstehende_dosen-(26-kw_abwann_curevac+1)*
  #                              letzterwert)/((26-kw_abwann_curevac+1)*
  #                                              (26-kw_abwann_curevac+2))),
  #   # quartal==2 & hersteller=="BNT/Pfizer" & kw>=18 ~ dosen_kw, 
  #   # quartal==2 & hersteller=="Moderna" & kw>=18 ~ dosen_kw,
  #   # quartal==2 & hersteller=="AZ" & kw>=kw_biswann_praxen_az+1 ~ 
  #   #   dosen_kw + round(ausstehende_dosen/(26-kw_biswann_praxen_az)),
  #   # quartal==2 & hersteller=="J&J" & kw>=kw_abwannwieder_jj ~ 
  #   #   letzterwert + round((kw-kw_abwannwieder_jj+1)*2*
  #   #                         (ausstehende_dosen-(26-kw_abwannwieder_jj+1)*
  #   #                            letzterwert)/((26-kw_abwannwieder_jj+1)*
  #   #                                            (26-kw_abwannwieder_jj+2))),
  #   TRUE ~ dosen_kw
  # )) %>%
  mutate(dosen_verabreicht_erst=ifelse(!is.na(dosen_verabreicht_erst), 
                                       dosen_verabreicht_erst, 
                                       0),
         dosen_verabreicht_zweit=ifelse(!is.na(dosen_verabreicht_zweit), 
                                        dosen_verabreicht_zweit,
                                        0),
         dosen_geliefert=ifelse(!is.na(dosen_geliefert), 
                                dosen_geliefert, 
                                0)) %>%
  mutate(dosen_verabreicht_erst=ifelse(Datum==prognosestart, 
                                       dosen_verabreicht_erst, 
                                       0),
         dosen_verabreicht_zweit=ifelse(Datum==prognosestart, 
                                        dosen_verabreicht_zweit, 
                                        0),
         dosenspeicher=ifelse(Datum==prognosestart, 
                              dosen_geliefert, 
                              0)) #,
         # dosenspeicher_lager=dosenspeicher-dosen_verabreicht_erst-dosen_verabreicht_zweit) %>%
  # group_by(Bundesland, hersteller) %>%
  # mutate(dosenspeicher_verteilt=case_when(
  #          hersteller=="BNT/Pfizer" & kw<=11 ~ min(warteschlange_zweit_kw[1], round(dosenspeicher_lager[1]/4)),
  #          hersteller=="Moderna" & kw<=11 ~ min(warteschlange_zweit_kw[1], round(dosenspeicher_lager[1]/4)),
  #          hersteller=="AZ" & kw<=16 ~ min(warteschlange_zweit_kw[1], round(dosenspeicher_lager[1]/9)),
  #          TRUE ~ 0),
  #        dosenspeicher_lager=case_when(
  #          hersteller=="BNT/Pfizer" & Datum==prognosestart ~ dosenspeicher_lager[1]-dosenspeicher_verteilt[1]*4,
  #          hersteller=="Moderna" & Datum==prognosestart ~ dosenspeicher_lager-dosenspeicher_verteilt[1]*4,
  #          hersteller=="AZ" & Datum==prognosestart ~ dosenspeicher_lager-dosenspeicher_verteilt[1]*9,
  #          TRUE ~ 0)) %>%
  # ungroup()
zeitreihe <- bind_rows(zeitreihe_gleichverteilt %>% mutate(Verteilungsszenario="Gleichverteilung"),
                       zeitreihe_linearverteilt %>% mutate(Verteilungsszenario="Linearer Anstieg der Produktion in Q2")) %>%
  select(-dosen_geliefert) %>% # , -letzterwert, -ersterwertQ3) %>% 
  left_join(bev_u18_1860_ue60, by="Bundesland") %>% 
  left_join(stiko_altersgruppen, by="hersteller")

export_newdashboard <- zeitreihe %>%
  filter(wday(Datum, week_start=1)==1) %>% 
  mutate(abstand=round(abstand/7)) %>%
  select(Verteilungsszenario, kw, anwendungen, zugelassen,
         abstand,
         Bundesland, hersteller, ruecklage, prioritaet, altersgruppe,
         dosen_kw, population, ueber18,
         contains("pop_"),
         dosen_verabreicht_erst, dosen_verabreicht_zweit,
         # dosenspeicher_lager, dosenspeicher_verteilt,
         warteschlange_zweit_kw)
write_json(export_newdashboard, "export/tables/impfsim_lieferungen.json")
write_json(dosen_verabreicht, "export/tables/impfsim_start.json")

# für impfindex
fuerpraxen <- read_csv("data/kw_praxen.csv") %>%
  full_join(rki_vacc %>%
              left_join(bev_gesamt_laender %>% 
                          select(population, geo=Name) %>% 
                          mutate(geo=ifelse(geo=="Gesamt", "Germany", geo)),
                        by="geo") %>%
              select(geo, population) %>% distinct(),
            by=character()) %>%
  left_join(dosen_planung_praxen_laender_kw %>% 
              mutate(Bundesland=ifelse(Bundesland=="Gesamt", 
                                       "Germany",
                                       Bundesland)),
            by=c("geo"="Bundesland", "KW", "Hersteller")) %>% 
  mutate(Lieferung_Praxen=ifelse(is.na(Lieferung_Praxen), 0, Lieferung_Praxen),
         Lieferung_Praxen_PEI=ifelse(is.na(Lieferung_Praxen_PEI), 
                                     0, 
                                     Lieferung_Praxen_PEI)) %>% 
  mutate(Lieferung_Praxen=
           ifelse(Hersteller%in%c("BNT/Pfizer", "AZ", "J&J") & KW<=maxpeikw,
                  Lieferung_Praxen_PEI,
                  round(Lieferung_Praxen*population/83166711))) %>%
  mutate(geo=ifelse(geo=="Germany", "Gesamt", geo),
         Jahr=2021)
write_json(fuerpraxen, "export/tables/impfsim_lieferungenpraxen.json")

fuerimpfzentren <- bind_rows(bunddashboard_daten %>% 
                               filter(((KW<=14 & Hersteller!="BNT/Pfizer") | 
                                         (KW<14 & Hersteller=="BNT/Pfizer")) |
                                        Jahr==2020) %>% 
                               left_join(rki_vacc %>%
                                           left_join(bev_gesamt_laender %>% 
                                                       select(population, 
                                                              geo=Name) %>% 
                                                       mutate(geo=ifelse(
                                                         geo=="Gesamt",
                                                         "Germany", 
                                                         geo)
                                                         ),
                                                     by="geo") %>%
                                           select(geo, population) %>% 
                                           distinct(),
                                         by="geo") %>% 
                               mutate(population=ifelse(geo=="Gesamt", 
                                                        83166711,
                                                        population),
                                      Lieferung_IZ=Lieferung_gesamt) %>% 
                               select(-Lieferung_gesamt),
                             dosen_planung_laender_kw %>% 
                               filter(KW!=0) %>% 
                               mutate(geo=Bundesland, Jahr=2021) %>% 
                               select(-Bundesland) %>% 
                               left_join(rki_vacc %>%
                                           left_join(bev_gesamt_laender %>% 
                                                       select(population, 
                                                              geo=Name) %>% 
                                                       mutate(geo=ifelse(
                                                         geo=="Gesamt", 
                                                         "Germany",
                                                         geo)
                                                         ),
                                                     by="geo") %>%
                                           select(geo, population) %>% 
                                           distinct(),
                                         by="geo") %>% 
                               mutate(population=ifelse(geo=="Gesamt",
                                                        83166711, 
                                                        population)))
write_json(fuerimpfzentren, "export/tables/impfsim_lieferungenimpfzentren.json")

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
write_json(iz_vergangen_laender, "export/tables/impfsim_impfungenimpfzentren_historisch.json")

