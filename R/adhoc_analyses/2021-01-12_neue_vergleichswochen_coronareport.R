# testungen alm ev
# höchstwerte HJ1
# tests: kw 26, 22.-28.6., 406699
# positive tests: kw 14, 30.3.-5.4., 30483
# positivrate: kw 14, 9.1%
# testkapazität: kw 26, 911376
# auslastung: kw 14, 63%
# höchstwerte HJ2
# tests: kw 51, 14.-20.12., 1472985
# positive tests: kw 51, 174009
# positivrate: kw 53, 28.12.-3.1.2021, 16.45%
# testkapazität: kw 51, 1819680
# auslastung: kw 44, 26.10.-1.11.2020, 100%

rki_7ti_gesamt <- bind_rows(rki_7ti_bund %>%
                            mutate(id=0),
                          rki_7ti_laender %>%
                            mutate(id=BLID) %>%
                            select(-BLID),
                          rki_7ti_kreise_full %>%
                            mutate(id=as.integer(IdLandkreis)*1000) %>%
                            filter(id>=12000000 | id<11000000) %>%
                            select(-IdLandkreis)) %>%
  left_join(., kreise_regstat_alter %>%
              pivot_longer(cols=contains("ag_"),
                           names_to="Altersgruppe",
                           values_to="Einwohnende") %>%
              mutate(Altersgruppe=case_when(
                Altersgruppe=="ag_6" ~ "80+",
                Altersgruppe=="ag_5" ~ "60-79",
                TRUE ~ "0-59"
              )) %>%
              group_by(id, Altersgruppe) %>%
              summarise(Einwohnende=sum(Einwohnende, na.rm=TRUE),
                        .groups="drop"), by=c("id", "Altersgruppe")) %>%
  group_by(id, JahrKW) %>%
  summarise(STI=round(sum(AnzahlFall)/sum(Einwohnende)*100000)) %>%
  mutate(datesunday=case_when(JahrKW=="2020-53" ~ as.Date("2021-01-03"),
                              JahrKW<"2020-53" ~ as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w"),
                              JahrKW>="2021-01" ~ as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w")+7)
  ) %>%
  filter(datesunday<=lastsunday & id==0)

##
rki_7ti_kreise <- rki %>% 
  filter(Altersgruppe!="unbekannt") %>%
  filter(AnzahlFall>=0) %>%
  mutate(KW=isoweek(Meldedatum),
         Jahr=ifelse(KW==53, 2020, year(Meldedatum)),
         JahrKW=paste0(Jahr, "-", str_pad(KW, 2, pad="0")),
         # Altersgruppe=case_when(
         #   Altersgruppe=="A00-A04" ~ "ag_1",
         #   Altersgruppe=="A05-A14" ~ "ag_2",
         #   Altersgruppe=="A15-A34" ~ "ag_3",
         #   Altersgruppe=="A35-A59" ~ "ag_4",
         #   Altersgruppe=="A60-A79" ~ "ag_5",
         #   Altersgruppe=="A80+" ~ "ag_6"
         Altersgruppe=case_when(
           Altersgruppe=="A80+" ~ "80+",
           Altersgruppe=="A60-A79" ~ "60-79",
           Altersgruppe=="A35-A59" ~ "35-59",
           Altersgruppe=="A15-A34" ~ "15-34",
           Altersgruppe=="A05-A14" ~ "0-14",
           Altersgruppe=="A00-A04" ~ "0-14",
           TRUE ~ "error"
         )) %>%
  group_by(IdLandkreis, JahrKW, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = TRUE), 
            .groups="drop")
rki_7ti_kreise_full <- rki_7ti_kreise %>%
  expand(IdLandkreis, JahrKW, Altersgruppe) %>%
  left_join(., rki_7ti_kreise, by=c("IdLandkreis", "Altersgruppe", "JahrKW")) %>%
  mutate(AnzahlFall=ifelse(is.na(AnzahlFall), 0, AnzahlFall))
rki_7ti_laender <- rki_7ti_kreise_full %>%
  mutate(BLID=floor(as.integer(IdLandkreis)/1000)) %>%
  group_by(BLID, JahrKW, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = TRUE),
            .groups="drop")
rki_7ti_bund <- rki_7ti_laender %>%
  group_by(JahrKW, Altersgruppe) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm = TRUE),
            .groups="drop")
rki_7ti_alle <- bind_rows(rki_7ti_bund %>%
                            mutate(id=0),
                          rki_7ti_laender %>%
                            mutate(id=BLID) %>%
                            select(-BLID),
                          rki_7ti_kreise_full %>%
                            mutate(id=as.integer(IdLandkreis)*1000) %>%
                            filter(id>=12000000 | id<11000000) %>%
                            select(-IdLandkreis)) %>%
  left_join(., kreise_regstat_alter %>%
              pivot_longer(cols=contains("ag_"),
                           names_to="Altersgruppe",
                           values_to="Einwohnende") %>%
              mutate(Altersgruppe=case_when(
                Altersgruppe=="ag_6" ~ "80+",
                Altersgruppe=="ag_5" ~ "60-79",
                Altersgruppe=="ag_4" ~ "35-59",
                Altersgruppe=="ag_3" ~ "15-34",
                Altersgruppe=="ag_2" ~ "0-14",
                Altersgruppe=="ag_1" ~ "0-14",
                TRUE ~ "error"
              )) %>%
              group_by(id, Altersgruppe) %>%
              summarise(Einwohnende=sum(Einwohnende, na.rm=TRUE),
                        .groups="drop"), by=c("id", "Altersgruppe")) %>%
  mutate(STI=round(AnzahlFall/Einwohnende*100000),
         datesunday=case_when(JahrKW=="2020-53" ~ base::as.Date("2021-01-03"),
                              JahrKW<"2020-53" ~ base::as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w"),
                              JahrKW>="2021-01" ~ base::as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w")+7)
  ) %>%
  filter(datesunday<=lastsunday)
##

rki_7ti_ag <- bind_rows(rki_7ti_bund %>%
                            mutate(id=0),
                          rki_7ti_laender %>%
                            mutate(id=BLID) %>%
                            select(-BLID),
                          rki_7ti_kreise_full %>%
                            mutate(id=as.integer(IdLandkreis)*1000) %>%
                            filter(id>=12000000 | id<11000000) %>%
                            select(-IdLandkreis)) %>%
  # mutate(Altersgruppe=case_when(
  #   Altersgruppe=="80+" ~ "60+",
  #   Altersgruppe=="60-79" ~ "60+",
  #   TRUE ~ "0-59"
  # )) %>%
  group_by(id, Altersgruppe, JahrKW) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            .groups="keep") %>%
  left_join(., kreise_regstat_alter %>%
              pivot_longer(cols=contains("ag_"),
                           names_to="Altersgruppe",
                           values_to="Einwohnende") %>%
              mutate(Altersgruppe=case_when(
                Altersgruppe=="ag_6" ~ "80+",
                Altersgruppe=="ag_5" ~ "60-79",
                Altersgruppe=="ag_4" ~ "35-59",
                Altersgruppe=="ag_3" ~ "15-34",
                Altersgruppe=="ag_2" ~ "0-14",
                Altersgruppe=="ag_1" ~ "0-14",
                TRUE ~ "error"
              )) %>%
              group_by(id, Altersgruppe) %>%
              summarise(Einwohnende=sum(Einwohnende, na.rm=TRUE),
                        .groups="drop"), by=c("id", "Altersgruppe")) %>%
  mutate(STI=round(AnzahlFall/Einwohnende*100000),
         datesunday=case_when(JahrKW=="2020-53" ~ as.Date("2021-01-03"),
                              JahrKW<"2020-53" ~ as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w"),
                              JahrKW>="2021-01" ~ as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w")+7)
  ) %>%
  filter(datesunday<=lastsunday & id==0)

regionen_hist3550 <- bind_rows(rki_7ti_bund %>%
                                 mutate(id=0),
                               rki_7ti_laender %>%
                                 mutate(id=BLID) %>%
                                 select(-BLID),
                               rki_7ti_kreise_full %>%
                                 mutate(id=as.integer(IdLandkreis)*1000) %>%
                                 filter(id>=12000000 | id<11000000) %>%
                                 select(-IdLandkreis)) %>%
  mutate(Altersgruppe=case_when(
    Altersgruppe=="80+" ~ "60+",
    Altersgruppe=="60-79" ~ "60+",
    TRUE ~ "0-59"
  )) %>%
  group_by(id, Altersgruppe, JahrKW) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            .groups="keep") %>%
  left_join(., kreise_regstat_alter %>%
              pivot_longer(cols=contains("ag_"),
                           names_to="Altersgruppe",
                           values_to="Einwohnende") %>%
              mutate(Altersgruppe=case_when(
                Altersgruppe=="ag_6" ~ "60+",
                Altersgruppe=="ag_5" ~ "60+",
                TRUE ~ "0-59"
              )) %>%
              group_by(id, Altersgruppe) %>%
              summarise(Einwohnende=sum(Einwohnende, na.rm=TRUE),
                        .groups="drop"), by=c("id", "Altersgruppe")) %>%
  mutate(STI=round(AnzahlFall/Einwohnende*100000),
         datesunday=case_when(JahrKW=="2020-53" ~ as.Date("2021-01-03"),
                              JahrKW<"2020-53" ~ as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w"),
                              JahrKW>="2021-01" ~ as.Date(paste0(JahrKW, "-0"), format="%Y-%W-%w")+7)
  ) %>%
  filter(datesunday<=lastsunday & (id==11 | id>20) & Altersgruppe=="60+") %>%
  group_by(JahrKW) %>%
  summarise(reg35=sum(STI>35, na.rm=TRUE),
            reg50=sum(STI>50, na.rm=TRUE))
# r-wert und 7ti
# höchstwerte HJ1
# rwert: 17.3., 3.02
# inzidenz gesamt: kw 14, 5.4., 43
# unter 60: 29.3., 42
# über 60: kw 14, 52
# 60-79: 5.4., 43
# 80+: 5.4., 81
# regionen >35: kw 14, 202
# regionen >50: kw 14, 152
# höchstwerte HJ2
# rwert: 27.10., 1.47
# inzidenz gesamt: kw 51, 209
# unter 60: kw 51, 20.12., 209
# über 60: kw 51, 208
# 60-79: 20.12., 157
# 80+: 20.12., 371
# regionen >35: kw 51, 392
# regionen >50: kw 51, 382

divi0 <- divi_all %>%
  filter(id==0) %>%
  mutate(wtag=wday(daten_stand)) %>%
  filter(wtag==1) %>%
  mutate(auslastungcovid=faelle_covid_aktuell/ICU_Betten,
         quotefrei=betten_frei/ICU_Betten)

# intensivbetten
# HJ1
# gesamtbetten:  28.6., 32309 oder 17.5. 32236
# belegung covid19 prozent: 3.5., 6.3%
# belegung covid19 zahl: 3.5., 1979
# freie betten prozent: 28.6., 36.6% oder 21.6., 36.7%
# freie betten zahl: 21.6., 10389
# HJ2
# gesamtbetten: 2.8., 33116
# belegung covid19 prozent: kw 52, 27.12., 21%
# belegung covid19 zahl: 27.12., 5562
# freie betten prozent: 20.12, 18.9%
# freie betten zahl: 20.12., 5047

# c19erkrankte
# HJ1
# ohne symptomatik:
# nicht stationaer:
# itsquote:
# ifsg23 neuinfiziert:
# ifsg23 neuhosp:
# ifsg23 neuverstorben:
# ifsg36 neuinfiziert:
# ifsg36 neuinfiziert60:
# ifsg36 neuhosp:
# ifsg36 neuverstorben:
# HJ2
# ohne symptomatik:
# nicht stationaer:
# itsquote:
# ifsg23 neuinfiziert:
# ifsg23 neuhosp:
# ifsg23 neuverstorben:
# ifsg36 neuinfiziert:
# ifsg36 neuinfiziert60:
# ifsg36 neuhosp:
# ifsg36 neuverstorben:

todrki_kw <- rki %>%
  mutate(yearkw=year(Meldedatum)*100+isoweek(Meldedatum)) %>%
  mutate(Altersgruppe3=case_when(
    Altersgruppe=="A80+" ~ "80+",
    Altersgruppe=="A60-A79" ~ "60-79",
    Altersgruppe=="unbekannt" ~ "unbekannt",
    TRUE ~ "0-59"
  )) %>%
  group_by(yearkw, Altersgruppe3) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall, na.rm=TRUE),
            Faelle=sum(AnzahlFall, na.rm = TRUE),
            AnteilTodesfaelle=Todesfaelle/Faelle*100)

todrki_kw_gesamt <- rki %>%
  mutate(yearkw=year(Meldedatum)*100+isoweek(Meldedatum)) %>%
  group_by(yearkw) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall, na.rm=TRUE),
            Faelle=sum(AnzahlFall, na.rm = TRUE),
            AnteilTodesfaelle=Todesfaelle/Faelle*100)

# tod (zahl (prozent))
# HJ1
# c 0-59:
# c 60-79:
# c 80+:
# c gesamt:
# ü 0-59:
# ü 60-79:
# ü 80+:
# ü gesamt:
# HJ2
# c 0-59:
# c 60-79:
# c 80+:
# c gesamt:
# ü 0-59:
# ü 60-79:
# ü 80+:
# ü gesamt:

# vorwarnzeit (tage und land)
# HJ1
# Bund:
# kürzeste:
# längste: 
# HJ2
# Bund:
# kürzeste:
# längste: 

inf_bev_bl <- rki %>%
  group_by(IdBundesland, Bundesland) %>%
  summarise(SumFaelle=sum(AnzahlFall, na.rm=TRUE),
            .groups="keep") %>%
  left_join(., kreise_regstat_alter %>%
              mutate(ewinsgesamt=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6),
            by=c("IdBundesland"="id")) %>%
  mutate(anteil_inf=SumFaelle/ewinsgesamt*100) %>%
  select(Bundesland, anteil_inf, ewinsgesamt)

inf_bev_gesamt <- rki %>%
  summarise(SumFaelle=sum(AnzahlFall, na.rm=TRUE),
            .groups="keep") %>%
  bind_cols(., kreise_regstat_alter %>%
              mutate(ewinsgesamt=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6) %>%
              filter(id==0) %>% select(ewinsgesamt)) %>%
  mutate(anteil_inf=SumFaelle/ewinsgesamt*100)
