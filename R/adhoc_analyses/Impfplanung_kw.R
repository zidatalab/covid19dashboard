library("tidyverse")
library("lubridate")
library("zicolors")

impfstart <- as.Date("2020-12-26")
prognosestart <- lubridate::as_date(now())
prognoseende <- as.Date("2021-12-31")


#### Hier werden die Dosen for dem aktuellen Quartal dem aktuellen Quartal 
#### zugerechnet
dosen_planung <- read_csv("R/adhoc_analyses/impfdosen_planung.csv") %>%
  group_by(hersteller) %>% mutate(dosen=ifelse(quartal>quarter(now()) & jahr>=2021,dosen,cumsum(dosen))) %>%
  filter(quartal>=quarter(now()) & jahr==2021)
dosen_verabreicht <- read_csv("R/adhoc_analyses/impfdosen_bisher.csv")
dosen_planung_kw <- read_csv("R/adhoc_analyses/impfdosen_planung_nextkw.csv")


prognosedatensatz <- 
#   tibble(
#   kw=5:52,
#   quartal=c(rep(1, 8), rep(2, 13), rep(3, 14), rep(4, 13)),
#   jahr=2021
# ) %>%
  tibble(Datum=prognosestart+days(seq(0,as.integer(prognoseende-prognosestart), 1))) %>%
  mutate(kw=isoweek(Datum),
         jahr=year(Datum),
         quartal=quarter(Datum),
         werktag=ifelse(weekdays(Datum) %in% c("Samstag","Sonntag"), 0, 1)) %>%
  full_join(.  ,
            dosen_planung,
            by=c("jahr","quartal")) %>%
  group_by(hersteller) %>%
  arrange(hersteller, kw) %>%
  left_join(dosen_verabreicht%>%select(-dosen_geliefert) %>%
              mutate(Datum=prognosestart), by=c("hersteller", "Datum")) %>%
  left_join(dosen_planung_kw %>% rename(dosen_kw="dosen"), by=c("kw", "quartal", "jahr", "hersteller", "Datum")) %>%
  mutate(dosen_kw=ifelse(Datum==prognosestart, dosen_kw-dosen_verabreicht_erst-dosen_verabreicht_zweit, dosen_kw),
         dosen_kw=ifelse(is.na(dosen_kw), 0, dosen_kw)) %>%
  group_by(hersteller) %>%
  mutate(
    dosen_kw_cumsum=cumsum(dosen_kw),
    dosen=ifelse(quartal==1 & dosen_kw_cumsum>0,
                      dosen-dosen_kw_cumsum-dosen_verabreicht_erst[1]-dosen_verabreicht_zweit[1], 
                      dosen)) #%>%
  # full_join(tibble(Datum=prognosestart+days(seq(0,as.integer(prognoseende-prognosestart), 1)))%>%
  #             mutate(kw=isoweek(Datum)),
  #           by="kw")

zeitreihe_impfdosen <- bind_rows(
  prognosedatensatz %>% mutate(Verteilungsszenario="Gleichverteilung"),
  prognosedatensatz %>% mutate(Verteilungsszenario="Linearer Anstieg der Produktion in Q2")) %>% 
  group_by(Datum, hersteller) %>% arrange(Datum) %>%
  mutate(gewichtungsfaktor = case_when(
    quartal==1 ~ 1/(31),
    quartal==2 & (month(Datum)==4) & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.20/30 - (0.20/30)*.15 + (0.20/30)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
    quartal==2 & month(Datum)==5 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.35/31 - (0.35/31 )*.15 + (0.35/31)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
    quartal==2 & month(Datum)==6 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.45/30 - (0.45/30 )*.15 + (0.45/30)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
    quartal==2 & Verteilungsszenario=="Gleichverteilung" ~ 1/(30+31+30), 
    quartal==3 ~ 1/(31+31+30),
    quartal==4 ~ 1/(31+30+31)
  ),
  
  dosen.verf = ifelse(kw > 8, dosen*gewichtungsfaktor, dosen/4/7),
  dosen.verf = ifelse(is.na(dosen.verf), 0, dosen.verf)) %>%
  mutate(werktag=ifelse(weekdays(Datum) %in% c("Samstag","Sonntag"), 0, 1))

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
  summarise(dosen.verf=sum(dosen.verf),
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
