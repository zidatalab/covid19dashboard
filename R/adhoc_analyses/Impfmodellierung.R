library("tidyverse")
library("lubridate")
library("zicolors")

# Impfmodellierung

# das Corona-Kabinett hat Modellierungen erbeten, wie sich die Zahl der zu 
# verimpfenden Dosen, die verfügbare Impfkapazität in den Zentren 
#  (Annahmen u.a. unter Normalbetreib / verlängerte Öffnungszeiten / 
# unter 24/7 Betrieb etc) und Zeitpunkt des Wechsels in die vertragsärztliche 
# Versorgung entwickeln könnte. Dabei ist ja auch zu berücksichtigen, welcher 
# Impfstoff sich überhaupt eignet für die vertragsärztliche Versorgung.

# In der Anlage finden Sie unter III. die erwarteten Zahlen bezogen auf die 
# Quartale. Könnten Sie für Q2 und Q3 Modellierungen vornehmen lassen, auch
# da mit verschiedenen Varianten (zB Variante 1 bezogen auf BioNTech:
# Quartalsdosenzahl verteilt sich gleichmäßig auf die drei Monate 
# (also 31,5 Mio. Dosen BioNtech : 3 Monate = 10,5 Mio. Dosen / Monat); 
# Variante 2: 20 % der 31,5 Mio. Dosen in April, weitere 35 % in Mai und 45 % 
# in Juni (oder so)), wie sich Kapazitäten entwickeln müssen und wie sich die
# Phasen von Impfzentrum in Praxen entwickeln könnten?


# Prämissen

# Ärzte sind Überlaufbecken für Impfzentren
# Dosen gehen kaputt

# Outcomes

## 1. Zahl der zu verimpfenden Dosen
## 2. Verfügbare Impfkapazität in den Zentren
## 3. Zeitpunkt des Wechsels in die vertragsärztliche  Versorgung
## 4. Wieviele Personen sollten eingeladen werden? Für Praxen/Zentren?

## Modellierung

## Zeit: 
### Q2 und Q3 nach KW

## Parameter:
## Betriebszeit: Normalbetreib / verlängerte Öffnungszeiten / 24/7 Betrieb 
## Verfügbarkeit: V1 Biontech, V2 

# Datenbasis

# Parameter 

impfstart <- as.Date("2020-12-27")
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
hersteller_liste = c( "az" , "bt" , "cv", "gsk" , "jj" ,  "mod")
hersteller_labels = c("AZ","BNT/Pfizer","Curevac","Sanofi/GSK","J&J","Moderna")
# dosen <- tibble(dosen_bt=c(1.3*1e6, 10.9*1e6, (31.5+8.7)*1e6, (17.6+17.1)*1e6, (2.7+10.8)*1e6),
#                 dosen_mod=c(0, 1.8*1e6, 6.4*1e6, (17.6+9.1)*1e6, (24.6+18.3)*1e6),
#                 dosen_az=c(0, 5.6*1e6, 16.9*1e6, 33.8*1e6, 0*1e6),
#                 dosen_jj=c(0, 0, 10.1*1e6, 22*1e6, 4.6*1e6),
#                 dosen_cv=c(0, 0, 3.5*1e6, 9.4*1e6, 11.7*1e6),
#                 dosen_gsk=c(0, 0, 0, 0, 27.5*1e6),
#                 quartal=c(4, 1, 2, 3, 4),
#                 jahr=c(2020, 2021, 2021, 2021, 2021)) %>%
#   gather(hersteller, dosen, contains("dosen")) %>% 
#   mutate(hersteller=str_replace(hersteller, "dosen_", "")) %>%
#   mutate(anwendungen=ifelse(hersteller!="jj", 2, 1),
#          abstand=case_when(hersteller=="bt"~4*7,
#                            hersteller=="mod"~5*7,
#                            hersteller=="jj"~0,
#                            hersteller=="az"~10*7,
#                            TRUE~3*7),
#          hersteller=factor(hersteller,levels=hersteller_liste,labels =hersteller_labels ))
# dosen %>% mutate(dosen_geliefert=0,dosen_verabreicht=0) %>% write_csv("R/adhoc_analyses/impfdosen.csv")
dosen <- read_csv("R/adhoc_analyses/impfdosen.csv")

# CHECKs 
# dosen %>% group_by(hersteller) %>% 
#   summarise(dosen=sum(dosen),
#             anwendungen=sum(dosen)/mean(anwendungen),
#             abstand=mean(abstand)) 
# 
# dosen %>% group_by(hersteller) %>% 
#   summarise(dosen=sum(dosen),
#             anwendungen=sum(dosen)/mean(anwendungen),
#             abstand=mean(abstand)) %>% ungroup() %>% summarise(anwendungen=sum(anwendungen)) 

# Input-Daten
prognosedatensatz <- tibble(Datum=impfstart+days(seq(0,as.integer(prognoseende-impfstart), 1))) %>%
  mutate(kw=isoweek(Datum),
         jahr=year(Datum),
         quartal=quarter(Datum),
         werktag=ifelse(weekdays(Datum) %in% c("Samstag","Sonntag"), 0, 1))

zeitreihe_impfdosen <- full_join(prognosedatensatz,
                                 dosen,
                                 by=c("jahr","quartal")) %>%
  group_by(hersteller, jahr, quartal) %>%
  mutate(dosen.verf=dosen/n()) %>% ungroup() %>% 
  arrange(hersteller, as.numeric(Datum)) %>% 
  rename("dosen.quartal"=dosen) %>%
  mutate(Patienten.verf=dosen.verf/anwendungen) %>%
  ungroup()

kapazitaeten <- bind_rows(
  tibble(einrichtung="iz",
         betriebsart="Regelbetrieb",
         kap_wt=n_impfzentren*impfzentrum_kapazitaet_wt,
         kap_we=n_impfzentren*impfzentrum_kapazitaet_wt),
  tibble(einrichtung="iz",
         betriebsart="WE-Betrieb",
         kap_wt=n_impfzentren*impfzentrum_kapazitaet_wt*1.5,
         kap_we=n_impfzentren*impfzentrum_kapazitaet_wt*1.5),
  tibble(einrichtung="praxen", 
         betriebsart="Regelbetrieb",
         kap_wt=n_praxen*praxis_kapazitaet_wt,
         kap_we=0),
  tibble(einrichtung="praxen",
         betriebsart="intensivbetrieb",
         kap_wt=n_praxen*praxis_kapazitaet_wt*2,
         kap_we=0)
)

# Szenarien

szenarien <- 
  bind_rows(
    tibble(szenario="1 IZ Regelbetrieb", 
           kap_wt=kapazitaeten %>% filter(einrichtung=="iz" & betriebsart=="Regelbetrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter(einrichtung=="iz" & betriebsart=="Regelbetrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert)),
    tibble(szenario="2 IZ WE-Betrieb", 
           kap_wt=kapazitaeten %>% filter(einrichtung=="iz" & betriebsart=="WE-Betrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter(einrichtung=="iz" & betriebsart=="WE-Betrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert)),
    tibble(szenario="3 IZ Regelbetrieb + Praxen Normalbetrieb", 
           kap_wt=kapazitaeten %>% filter(betriebsart=="Regelbetrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter(betriebsart=="Regelbetrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert)),
    tibble(szenario="4 IZ WE-Betrieb + Praxen Normalbetrieb", 
           kap_wt=kapazitaeten %>% filter((einrichtung=="iz" & betriebsart=="WE-Betrieb") | (einrichtung=="praxen" & betriebsart=="Regelbetrieb")) %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter((einrichtung=="iz" & betriebsart=="WE-Betrieb") | (einrichtung=="praxen" & betriebsart=="Regelbetrieb"))  %>% summarise(wert=sum(kap_we)) %>% pull(wert)),
    tibble(szenario="5 IZ WE-Betrieb + Praxen Intensivbetrieb", 
           kap_wt=kapazitaeten %>% filter(betriebsart=="WE-Betrieb" | betriebsart=="intensivbetrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
           kap_we=kapazitaeten %>% filter(betriebsart=="WE-Betrieb" | betriebsart=="intensivbetrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert))
  ) %>% ungroup()


# check 
# sum(andata$dosen.verf/andata$anwendungen)/1e6


# Output-Daten
output <- 
  zeitreihe_impfdosen %>% 
  group_by(Datum, kw, jahr, quartal, werktag) %>%
  summarise(dosen.verf=sum(dosen.verf),
            Patienten.verf=sum(Patienten.verf),
            .groups="drop") %>%
  ungroup() %>%
  full_join(szenarien,
            by = character()) %>%
  mutate(Kapazitaet=ifelse(werktag, kap_wt, kap_we)) %>%
  select(-kap_wt, -kap_we) 

output.plot <- output %>%
  arrange(szenario, Datum) %>% 
  group_by(szenario) %>%
  mutate(Auslastung=100*(cumsum(dosen.verf)/cumsum(Kapazitaet))) %>%
  filter(jahr>=2021) %>% 
  ggplot(aes(x=Datum, y=Auslastung, color=szenario)) +
  geom_line() + scale_x_date(breaks = "1 month") +
  geom_hline(yintercept=100) +
  scale_y_continuous(limits=c(0, 200)) +
  scale_color_zi() + theme_minimal()


## Übersichten
dosen %>% group_by(hersteller) %>% 
  summarise(dosen=sum(dosen),anwendungen=sum(dosen)/mean(anwendungen),abstand=mean(abstand))
kapazitaeten
szenarien
zeitreihe_impfdosen %>% head()
output.plot

# 1. Q1 Korrektur mit empirischen Dosen, Speicher der Rückstellstellimfpunegn
# 2. Auslastung korrigieren, Unterstützungbedarf IZ durch Ärzte quantifizieren
# 3. Verfügbarkeitszenarien, V1 gleichverteil + emp. , V2 zunehmend + emp.
# 4. Report 1: Impfkapazität
# 5. Rückstellungen , Abgeschlossene Impflinge, 
# 6. Report 2: Durchimpfungsqsquote
# 7. Dashboard
