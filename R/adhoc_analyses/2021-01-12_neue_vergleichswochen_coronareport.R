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

rki_7ti_60 <- bind_rows(rki_7ti_bund %>%
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
  filter(datesunday<=lastsunday & id==0 & Altersgruppe=="60+")

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
# gesamtbetten:  28.6., 32109 oder 17.5. 32236
# belegung covid19 prozent: 3.5., 6.3%
# belegung covid19 zahl:
# freie betten prozent:
# freie betten zahl:
# HJ2
# gesamtbetten: 2.8., 33116
# belegung covid19 prozent: kw 53, 21.7%
# belegung covid19 zahl:
# freie betten prozent:
# freie betten zahl:

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