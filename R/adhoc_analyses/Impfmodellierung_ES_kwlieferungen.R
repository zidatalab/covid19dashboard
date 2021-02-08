library("tidyverse")
library("lubridate")
library("zicolors")
library("jsonlite")

# Impfmodellierung

# Parameter 
impfstart <- as.Date("2020-12-26")
prognosestart <- as_date("2021-02-08") # lubridate::as_date(now())
startkw <- isoweek(prognosestart)
prognoseende <- as.Date("2021-12-31")
## Impflinge
impflinge_gesamt <- 83166711*(1-0.184)
## Kapazitäten
n_impfzentren <- 400
impfzentren_kapazitaet_gesamt_wt <- 200*1e3
impfzentrum_kapazitaet_wt <- impfzentren_kapazitaet_gesamt_wt/n_impfzentren
n_praxen <- 50000
praxis_kapazitaet_wt <- 20
## Impfdosen

#### Hier werden die Dosen for dem aktuellen Quartal dem aktuellen Quartal 
#### zugerechnet
dosen_planung <- read_csv("R/adhoc_analyses/impfdosen_planung.csv") %>%
  group_by(hersteller) %>% mutate(dosen=ifelse(quartal>quarter(now()) & jahr>=2021,dosen,cumsum(dosen))) %>%
  filter(quartal>=quarter(now()) & jahr==2021)
dosen_verabreicht <- read_csv("R/adhoc_analyses/impfdosen_bisher.csv")
dosen_planung_kw <- read_csv("R/adhoc_analyses/impfdosen_planung_nextkw.csv")


prognosedatensatz <- 
  tibble(Datum=prognosestart+days(seq(0,as.integer(prognoseende-prognosestart), 1))) %>%
  mutate(kw=isoweek(Datum),
         jahr=year(Datum),
         quartal=quarter(Datum),
         werktag=ifelse(weekdays(Datum) %in% c("Samstag","Sonntag"), 0, 1)) %>%
  full_join(.  ,
            dosen_planung %>% rename(dosen_quartal=dosen),
            by=c("jahr","quartal")) %>%
  group_by(hersteller) %>%
  arrange(hersteller, kw) %>%
  left_join(dosen_verabreicht, # %>% select(-dosen_geliefert) %>% mutate(Datum=prognosestart), 
              by=c("hersteller")) %>%
  left_join(dosen_planung_kw %>% 
              rename(dosen_kw="dosen") %>%
              select(-Datum),
            by=c("kw", "quartal", "jahr", "hersteller")) %>%
  mutate(dosen_pro_tag=ifelse(kw<=8 | (hersteller=="AZ" & kw<=9), round(dosen_kw/7), 0)) %>%
  group_by(hersteller) %>% arrange(Datum) %>%
  mutate(dosen_geliefert_temp=dosen_geliefert+cumsum(dosen_pro_tag)) %>%
  ungroup() %>%
  mutate(dosen_pro_tag=ifelse(quartal==1 & kw>=9 & hersteller!="AZ",
                              round((dosen_quartal-dosen_geliefert_temp)/31), 
                              ifelse(quartal==1 & kw>=10 & hersteller=="AZ", round((dosen_quartal-dosen_geliefert_temp)/24), dosen_pro_tag)),
         dosen_kw=ifelse(quartal==1 & kw>=9 & hersteller!="AZ", 
                         dosen_pro_tag*7, 
                         ifelse(quartal==1 & kw>=10 & hersteller=="AZ", dosen_pro_tag*7, dosen_kw)),
         dosen_pro_tag=ifelse(quartal>=3, round(dosen_quartal/92), dosen_pro_tag),
         dosen_kw=ifelse(quartal>=3, 7*dosen_pro_tag, dosen_kw)) %>%
  mutate(dosen_kw=ifelse(is.na(dosen_kw), 0, dosen_kw),
         dosen_pro_tag=ifelse(is.na(dosen_pro_tag), 0, dosen_pro_tag),
         dosen_geliefert=ifelse(is.na(dosen_geliefert), 0, dosen_geliefert))
  # mutate(dosen_kw=ifelse(Datum==prognosestart, dosen_kw-dosen_verabreicht_erst-dosen_verabreicht_zweit, dosen_kw),
  #        dosen_kw=ifelse(is.na(dosen_kw), 0, dosen_kw)) %>%
  # group_by(hersteller) %>%
  # mutate(
  #   dosen_kw_cumsum=cumsum(dosen_kw),
  #   dosen=ifelse(quartal==1 & dosen_kw_cumsum>0,
  #                dosen-dosen_kw_cumsum-dosen_verabreicht_erst[1]-dosen_verabreicht_zweit[1], 
  #                dosen)) #%>%
# full_join(tibble(Datum=prognosestart+days(seq(0,as.integer(prognoseende-prognosestart), 1)))%>%
#             mutate(kw=isoweek(Datum)),
#           by="kw")

zeitreihe_impfdosen <- bind_rows(
  prognosedatensatz %>% mutate(Verteilungsszenario="Gleichverteilung"),
  prognosedatensatz %>% mutate(Verteilungsszenario="Linearer Anstieg der Produktion in Q2")) %>% 
  group_by(Datum, hersteller) %>% arrange(Datum) %>%
  mutate(gewichtungsfaktor = case_when(
    quartal==2 & (month(Datum)==4) & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.20/30 - (0.20/30)*.15 + (0.20/30)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
    quartal==2 & month(Datum)==5 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.35/31 - (0.35/31 )*.15 + (0.35/31)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
    quartal==2 & month(Datum)==6 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.45/30 - (0.45/30 )*.15 + (0.45/30)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
    quartal==2 & Verteilungsszenario=="Gleichverteilung" ~ 1/(30+31+30),
    quartal!=2 ~ 1
  ),
  dosen_pro_tag = ifelse(quartal==2, round(dosen_quartal*gewichtungsfaktor), dosen_pro_tag),
  dosen_kw=ifelse(quartal==2, 7*dosen_pro_tag, dosen_kw)) %>%
  ungroup() %>%
  group_by(Verteilungsszenario, hersteller) %>% arrange(Datum) %>%
  mutate(dosen_geliefert=dosen_geliefert+cumsum(dosen_pro_tag)) %>%
  ungroup() %>%
  mutate(zugelassen=ifelse(hersteller%in%c("BNT/Pfizer", "AZ", "Moderna"), 1, 0))
# CHECK zeitreihe_impfdosen %>% ggplot(aes(x=Datum,y=gewichtungsfaktor,color=Verteilungsszenario)) + geom_line()

export_newdashboard <- zeitreihe_impfdosen %>% 
  filter(jahr>=2021) %>% ungroup() %>%
  select(Verteilungsszenario,kw, dosen.verf=dosen_pro_tag,anwendungen, Datum, zugelassen) %>% group_by(Verteilungsszenario,kw) %>%
  summarise(Dosen=sum(dosen.verf),dosen_zugelassen=sum(dosen.verf[zugelassen==1]), Patienten=sum(dosen.verf/anwendungen), patienten_zugelassen=sum(dosen.verf[zugelassen==1]/anwendungen[zugelassen==1]),mindate=min(Datum),maxdate=max(Datum))
write_json(export_newdashboard, "./data/tabledata/impfsim_data.json")

kapazitaeten <- bind_rows(
  tibble(einrichtung="Impfzentren",
         betriebsart="Regelbetrieb",
         kap_wt=n_impfzentren*impfzentrum_kapazitaet_wt,
         kap_we=n_impfzentren*impfzentrum_kapazitaet_wt),
  tibble(einrichtung="Impfzentren",
         betriebsart="Intensivbetrieb",
         kap_wt=n_impfzentren*impfzentrum_kapazitaet_wt*1.5,
         kap_we=n_impfzentren*impfzentrum_kapazitaet_wt*1.5),
  tibble(einrichtung="Praxen", 
         betriebsart="Regelbetrieb",
         kap_wt=n_praxen*praxis_kapazitaet_wt,
         kap_we=0),
  tibble(einrichtung="Praxen",
         betriebsart="Intensivbetrieb",
         kap_wt=n_praxen*praxis_kapazitaet_wt*2,
         kap_we=0)
)

# Szenarien

szenarien <- 
  bind_rows(
    tibble(szenario="IZ Regelbetrieb", 
           kap_wt=kapazitaeten %>% filter(einrichtung=="Impfzentren" & betriebsart=="Regelbetrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter(einrichtung=="Impfzentren" & betriebsart=="Regelbetrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert)),
    tibble(szenario="IZ Intensivbetrieb", 
           kap_wt=kapazitaeten %>% filter(einrichtung=="Impfzentren" & betriebsart=="Intensivbetrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter(einrichtung=="Impfzentren" & betriebsart=="Intensivbetrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert)),
  
  ) %>% ungroup()


# check 
# sum(andata$dosen.verf/andata$anwendungen)/1e6


# Output-Daten
output <- 
  zeitreihe_impfdosen %>% 
  group_by(Verteilungsszenario,Datum, kw, jahr, quartal, werktag) %>%
  summarise(dosen.verf=sum(dosen_pro_tag),
            .groups="drop") %>%
  ungroup() %>%
  full_join(szenarien,
            by = character()) %>%
  mutate(Kapazitaet=ifelse(werktag, kap_wt, kap_we)) %>%
  select(-kap_wt, -kap_we) %>%
  mutate(IZ_verimpft=ifelse(dosen.verf<=Kapazitaet,dosen.verf,Kapazitaet),
         IZ_Auslastung=100*(dosen.verf/Kapazitaet),
         Zusatzbedarf=ifelse(dosen.verf<=Kapazitaet,0,dosen.verf-IZ_verimpft) ,
         Zusatzbedarf_n_Aerzte=Zusatzbedarf/praxis_kapazitaet_wt)

### erst-zweit-schema mit 0.5 erst, 0.5 zurück
Datumsliste <- sort(unique(prognosedatensatz$Datum))
herstellerliste <- unique(prognosedatensatz$hersteller)
vszenarien <- unique(zeitreihe_impfdosen$Verteilungsszenario)
erstzweit <- tibble()
for (h in herstellerliste) {
  cat(h, "\n")
  for (v in vszenarien) {
    cat("  ", v, "\n")
    h_abstand <- (dosen_planung%>%filter(hersteller==h))$abstand[1]
    hs_erstzweit <- zeitreihe_impfdosen %>% filter(hersteller==h & Verteilungsszenario==v) %>%
      select(-c(kw, jahr, quartal, dosen_quartal, abstand, dosen_kw, dosen_geliefert_temp, gewichtungsfaktor)) %>%
      mutate(erst_neu=0, zweit_neu=0)
    i <- 0
    for (d in Datumsliste) {
      d <- as_date(d)
      # cat(as.character(d), "\n")
      i <- i+1
      if (h_abstand==0) {
        if (as.integer(d-prognosestart)==0) {
          hs_erstzweit[1, "zweit_neu"] <- hs_erstzweit[1, "dosen_pro_tag"]
          hs_erstzweit[1, "erst_neu"] <- hs_erstzweit[1, "zweit_neu"]
          hs_erstzweit[1, "dosen_verabreicht_zweit"] <- hs_erstzweit[1, "dosen_verabreicht_zweit"] + hs_erstzweit[1, "zweit_neu"]
          hs_erstzweit[1, "dosen_verabreicht_erst"] <- hs_erstzweit[1, "dosen_verabreicht_zweit"]
        } else {
          hs_erstzweit[i, "zweit_neu"] <- hs_erstzweit[i, "dosen_pro_tag"]
          hs_erstzweit[i, "erst_neu"] <- hs_erstzweit[i, "zweit_neu"]
          hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
          hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i, "dosen_verabreicht_zweit"]
        }
      } else {
        if (as.integer(d-prognosestart)<h_abstand) {
          if (as.integer(d-prognosestart)==0) {
            hs_erstzweit[1, "erst_neu"] <- round(hs_erstzweit[1, "dosen_pro_tag"]/2)
            hs_erstzweit[1, "zweit_neu"] <- round(hs_erstzweit[1, "dosen_verabreicht_zweit"]/h_abstand)
            hs_erstzweit[1, "dosen_verabreicht_erst"] <- hs_erstzweit[1, "dosen_verabreicht_erst"] + hs_erstzweit[1, "erst_neu"]
            hs_erstzweit[1, "dosen_verabreicht_zweit"] <- hs_erstzweit[1, "dosen_verabreicht_zweit"] + hs_erstzweit[1, "zweit_neu"]
          } else {
            hs_erstzweit[i, "erst_neu"] <- round(hs_erstzweit[i, "dosen_pro_tag"]/2)
            hs_erstzweit[i, "zweit_neu"] <- round(hs_erstzweit[1, "dosen_verabreicht_zweit"]/h_abstand)
            hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i-1, "dosen_verabreicht_erst"] + hs_erstzweit[i, "erst_neu"]
            hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
          }
        } else {
          hs_erstzweit[i, "erst_neu"] <- round(hs_erstzweit[i, "dosen_pro_tag"]/2)
          hs_erstzweit[i, "zweit_neu"] <- hs_erstzweit[i-h_abstand, "erst_neu"]
          hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i-1, "dosen_verabreicht_erst"] + hs_erstzweit[i, "erst_neu"]
          hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
        }
      }
    }
    erstzweit <- bind_rows(erstzweit, hs_erstzweit)
  }
}

zweit_agg_temp <- erstzweit %>%
  mutate(zugelassen=ifelse(hersteller%in%c("BNT/Pfizer", "AZ", "Moderna"), 1, 0)) %>%
  filter(Verteilungsszenario=="Linearer Anstieg der Produktion in Q2")
zweit_agg <- bind_rows(
  zweit_agg_temp %>%
    filter(zugelassen==1) %>%
    group_by(Datum) %>%
    summarise(vollgeimpft=sum(dosen_verabreicht_zweit),
              mineinmalgeimpft=sum(dosen_verabreicht_erst),
              erst_neu_agg=sum(erst_neu),
              .groups="drop") %>%
    mutate(nurzugelassen="nur zugelassen"),
  zweit_agg_temp %>%
    group_by(Datum) %>%
    summarise(vollgeimpft=sum(dosen_verabreicht_zweit),
              mineinmalgeimpft=sum(dosen_verabreicht_erst),
              erst_neu_agg=sum(erst_neu),
              .groups="drop") %>%
    mutate(nurzugelassen="alle Impfstoffe")
  ) %>%
  pivot_longer(cols=c(vollgeimpft, mineinmalgeimpft, erst_neu_agg))


###
### erst-zweit-schema mit voll sofort und zweit abarbeiten
Datumsliste <- sort(unique(prognosedatensatz$Datum))
herstellerliste <- unique(prognosedatensatz$hersteller)
vszenarien <- unique(zeitreihe_impfdosen$Verteilungsszenario)
erstzweit_sofort <- tibble()
for (h in herstellerliste) {
  cat(h, "\n")
  for (v in vszenarien) {
    cat("  ", v, "\n")
    h_abstand <- (dosen_planung%>%filter(hersteller==h))$abstand[1]
    hs_erstzweit <- zeitreihe_impfdosen %>% filter(hersteller==h & Verteilungsszenario==v) %>%
      select(-c(kw, jahr, quartal, dosen_quartal, abstand, dosen_kw, dosen_geliefert_temp, gewichtungsfaktor)) %>%
      mutate(erst_neu=0, zweit_neu=0, zweit_abzuarbeiten=0)
    i <- 0
    for (d in Datumsliste) {
      d <- as_date(d)
      # cat(as.character(d), "\n")
      i <- i+1
      if (h_abstand==0) {
        if (as.integer(d-prognosestart)==0) {
          hs_erstzweit[1, "zweit_neu"] <- hs_erstzweit[1, "dosen_pro_tag"]
          hs_erstzweit[1, "erst_neu"] <- hs_erstzweit[1, "zweit_neu"]
          hs_erstzweit[1, "dosen_verabreicht_zweit"] <- hs_erstzweit[1, "dosen_verabreicht_zweit"] + hs_erstzweit[1, "zweit_neu"]
          hs_erstzweit[1, "dosen_verabreicht_erst"] <- hs_erstzweit[1, "dosen_verabreicht_zweit"]
        } else {
          hs_erstzweit[i, "zweit_neu"] <- hs_erstzweit[i, "dosen_pro_tag"]
          hs_erstzweit[i, "erst_neu"] <- hs_erstzweit[i, "zweit_neu"]
          hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
          hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i, "dosen_verabreicht_zweit"]
        }
      } else {
        if (as.integer(d-prognosestart)<h_abstand) {
          if (as.integer(d-prognosestart)==0) {
            hs_erstzweit[1, "erst_neu"] <- round(hs_erstzweit[1, "dosen_pro_tag"])
            hs_erstzweit[1, "zweit_neu"] <- round(hs_erstzweit[1, "dosen_verabreicht_zweit"]/h_abstand)
            hs_erstzweit[1, "dosen_verabreicht_erst"] <- hs_erstzweit[1, "dosen_verabreicht_erst"] + hs_erstzweit[1, "erst_neu"]
            hs_erstzweit[1, "dosen_verabreicht_zweit"] <- hs_erstzweit[1, "dosen_verabreicht_zweit"] + hs_erstzweit[1, "zweit_neu"]
          } else {
            hs_erstzweit[i, "erst_neu"] <- round(hs_erstzweit[i, "dosen_pro_tag"])
            hs_erstzweit[i, "zweit_neu"] <- round(hs_erstzweit[1, "dosen_verabreicht_zweit"]/h_abstand)
            hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i-1, "dosen_verabreicht_erst"] + hs_erstzweit[i, "erst_neu"]
            hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
          }
        } else {
          if (hs_erstzweit[i-1, "zweit_abzuarbeiten"]>0) {
            aufholen <- min(
              hs_erstzweit[i-1, "zweit_abzuarbeiten"],
              round(hs_erstzweit[i, "dosen_pro_tag"])
            )
            ontime <- min(
              hs_erstzweit[i-h_abstand, "erst_neu"], 
              round(hs_erstzweit[i, "dosen_pro_tag"]) - aufholen
            )
            nichtgeschafft <- hs_erstzweit[i-h_abstand, "erst_neu"] - ontime
            hs_erstzweit[i, "zweit_neu"] <- aufholen + ontime
            hs_erstzweit[i, "zweit_abzuarbeiten"] <- hs_erstzweit[i-1, "zweit_abzuarbeiten"] - aufholen + nichtgeschafft
            hs_erstzweit[i, "erst_neu"] <- max(0, as.numeric(round(hs_erstzweit[i, "dosen_pro_tag"]) - hs_erstzweit[i, "zweit_neu"]))
            hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i-1, "dosen_verabreicht_erst"] + hs_erstzweit[i, "erst_neu"]
            hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
          } else {
            hs_erstzweit[i, "zweit_neu"] <- min(
              hs_erstzweit[i-h_abstand, "erst_neu"], 
              round(hs_erstzweit[i, "dosen_pro_tag"])
              )
            nichtgeschafft <- max(0 , as.numeric(hs_erstzweit[i-h_abstand, "erst_neu"] - round(hs_erstzweit[i, "dosen_pro_tag"])))
            hs_erstzweit[i, "zweit_abzuarbeiten"] <- nichtgeschafft
            hs_erstzweit[i, "erst_neu"] <- max(
              0, 
              as.numeric(round(hs_erstzweit[i, "dosen_pro_tag"]) - hs_erstzweit[i, "zweit_neu"])
              )
            hs_erstzweit[i, "dosen_verabreicht_erst"] <- hs_erstzweit[i-1, "dosen_verabreicht_erst"] + hs_erstzweit[i, "erst_neu"]
            hs_erstzweit[i, "dosen_verabreicht_zweit"] <- hs_erstzweit[i-1, "dosen_verabreicht_zweit"] + hs_erstzweit[i, "zweit_neu"]
          }
        }
      }
    }
    erstzweit_sofort <- bind_rows(erstzweit_sofort, hs_erstzweit)
  }
}

probleme_bei_allesraus <- erstzweit_sofort %>%
  filter(zweit_abzuarbeiten>0) %>% 
  select(Datum, hersteller, dosen_pro_tag, Verteilungsszenario, erst_neu, zweit_neu, zweit_abzuarbeiten) %>%
  mutate(quartal=quarter(Datum))
write_csv(probleme_bei_allesraus, "R/adhoc_analyses/probleme_bei_alles_raus.csv")

zweit_agg_temp_sofort <- erstzweit_sofort %>%
  mutate(zugelassen=ifelse(hersteller%in%c("BNT/Pfizer", "AZ", "Moderna"), 1, 0)) %>%
  filter(Verteilungsszenario=="Linearer Anstieg der Produktion in Q2")
zweit_agg_sofort <- bind_rows(
  zweit_agg_temp_sofort %>%
    filter(zugelassen==1) %>%
    group_by(Datum) %>%
    summarise(vollgeimpft=sum(dosen_verabreicht_zweit),
              mineinmalgeimpft=sum(dosen_verabreicht_erst),
              erst_neu_agg=sum(erst_neu),
              .groups="drop") %>%
    mutate(nurzugelassen="nur zugelassen"),
  zweit_agg_temp_sofort %>%
    group_by(Datum) %>%
    summarise(vollgeimpft=sum(dosen_verabreicht_zweit),
              mineinmalgeimpft=sum(dosen_verabreicht_erst),
              erst_neu_agg=sum(erst_neu),
              .groups="drop") %>%
    mutate(nurzugelassen="alle Impfstoffe")
) %>%
  pivot_longer(cols=c(vollgeimpft, mineinmalgeimpft, erst_neu_agg))


## Durchimpfung
stufen <- tibble(
  stufe=1:6,
  kumanzahl=cumsum(c(8.6, 7, 5.7, 6.9, 8.4, 45))
)

meilensteine <- stufen %>%
  rowwise() %>%
  mutate(
    zweit_zuruecklegen_zugelassen=min(zweit_agg %>%
                                        filter(nurzugelassen=="nur zugelassen" & name=="vollgeimpft" & value>=kumanzahl*1e6) %>%
                                        pull(Datum)),
    zweit_zuruecklegen_alle=min(zweit_agg %>%
                                  filter(nurzugelassen=="alle Impfstoffe" & name=="vollgeimpft" & value>=kumanzahl*1e6) %>%
                                  pull(Datum)),
    erst_zuruecklegen_zugelassen=min(zweit_agg %>%
                                       filter(nurzugelassen=="nur zugelassen" & name=="mineinmalgeimpft" & value>=kumanzahl*1e6) %>%
                                       pull(Datum)),
    erst_zuruecklegen_alle=min(zweit_agg %>%
                                 filter(nurzugelassen=="alle Impfstoffe" & name=="mineinmalgeimpft" & value>=kumanzahl*1e6) %>%
                                 pull(Datum)),
    zweit_allesraus_zugelassen=min(zweit_agg_sofort %>%
                                     filter(nurzugelassen=="nur zugelassen" & name=="vollgeimpft" & value>=kumanzahl*1e6) %>%
                                     pull(Datum)),
    zweit_allesraus_alle=min(zweit_agg_sofort %>%
                               filter(nurzugelassen=="alle Impfstoffe" & name=="vollgeimpft" & value>=kumanzahl*1e6) %>%
                               pull(Datum)),
    erst_allesraus_zugelassen=min(zweit_agg_sofort %>%
                                    filter(nurzugelassen=="nur zugelassen" & name=="mineinmalgeimpft" & value>=kumanzahl*1e6) %>%
                                    pull(Datum)),
    erst_allesraus_alle=min(zweit_agg_sofort %>%
                              filter(nurzugelassen=="alle Impfstoffe" & name=="mineinmalgeimpft" & value>=kumanzahl*1e6) %>%
                              pull(Datum))
  )
write_csv(meilensteine, "R/adhoc_analyses/impfmeilensteine.csv")



ann_text <- data.frame(Datum=as_date(c("2021-09-05", "2021-02-20")), 
                       lab = c("21.9.", "Bev. >18 J."), 
                       nurzugelassen=c("nur zugelassen", "alle Impfstoffe"), 
                       value=c(0, 73e6),
                       name="vollst. geimpft")
stufenlabels <- data_frame(Datum=as_date(meilensteine$zweit_zuruecklegen_zugelassen,
                                         meilensteine$zweit_zuruecklegen_alle,
                                         meilensteine$erst_zuruecklegen_zugelassen,
                                         meilensteine$erst_zuruecklegen_alle),
                           lab=rep(1:6, 4),
                           nurzugelassen=rep(c(rep("nur zugelassen", 6), rep("alle Impfstoffe", 6)), 2),
                           value=rep(meilensteine$kumanzahl, 6)*1e6,
                           name="vollst. geimpft")
durchimpfung.plot <- ggplot(zweit_agg %>% 
                              filter(name!="erst_neu_agg" & Datum<="2021-11-01"),
                            aes(x=Datum, y=value/1e6, col=name)) +
  geom_line(size=2) +
  facet_wrap(.~nurzugelassen, ncol=2) +
  ylim(0, 83166711/1e6) +
  geom_hline(yintercept = impflinge_gesamt/1e6, linetype="dotted") +
  geom_vline(xintercept = as_date("2021-09-21")) +
  geom_text(data = ann_text, aes(label = lab), col="black", size=3.5) +
  # scale_color_discrete() +
  scale_x_date(date_breaks = "2 months", date_labels = "%d. %b.") +
  labs(y="Anzahl in Mio.", col="") +
  scale_color_zi(labels = c("min. 1x geimpft", "vollst. geimpft")) + theme_minimal() + theme(legend.position="bottom")
durchimpfung.plot
ggsave("R/adhoc_analyses/durchimpfung.png", durchimpfung.plot, width = 7, height=7*9/16)




ann_text <- data.frame(Datum=as_date(c("2021-09-05", "2021-02-20")), 
                       lab = c("21.9.", "Bev. >18 J."), 
                       nurzugelassen=c("nur zugelassen", "alle Impfstoffe"), 
                       value=c(0, 73e6),
                       name="vollst. geimpft")
durchimpfung.plot_sofort <- ggplot(zweit_agg_sofort %>% 
                                     filter(name!="erst_neu_agg" & Datum<="2021-11-01"),
                                   aes(x=Datum, y=value/1e6, col=name)) +
  geom_line(size=2) +
  facet_wrap(.~nurzugelassen, ncol=2) +
  ylim(0, 83166711/1e6) +
  geom_hline(yintercept = impflinge_gesamt/1e6, linetype="dotted") +
  geom_vline(xintercept = as_date("2021-09-21")) +
  geom_text(data = ann_text, aes(label = lab), col="black", size=3.5) +
  # scale_color_discrete() +
  scale_x_date(date_breaks = "2 months", date_labels = "%d. %b.") +
  labs(y="Anzahl in Mio.", col="") +
  scale_color_zi(labels = c("min. 1x geimpft", "vollst. geimpft")) + theme_minimal() + theme(legend.position="bottom")
durchimpfung.plot_sofort
ggsave("R/adhoc_analyses/durchimpfung_sofort.png", durchimpfung.plot_sofort, width = 7, height=7*9/16)
