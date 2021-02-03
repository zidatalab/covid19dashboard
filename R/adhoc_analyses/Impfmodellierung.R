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
impfstart <- as.Date("2020-12-26")
prognosestart <- lubridate::as_date(now())
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

verhaeltnis_ende_anfang <- 4

#### Hier werden die Dosen for dem aktuellen Quartal dem aktuellen Quartal 
#### zugerechnet
dosen_planung <- read_csv("R/adhoc_analyses/impfdosen_planung.csv") %>%
  group_by(hersteller) %>% mutate(dosen=ifelse(quartal>quarter(now()) & jahr>=2021,dosen,cumsum(dosen))) %>%
  filter(quartal>=quarter(now()) & jahr==2021)
dosen_verabreicht <- read_csv("R/adhoc_analyses/impfdosen_bisher.csv")


# Input-Daten
prognosedatensatz <- tibble(Datum=prognosestart+days(seq(0,as.integer(prognoseende-prognosestart), 1))) %>%
  mutate(kw=isoweek(Datum),
         jahr=year(Datum),
         quartal=quarter(Datum),
         werktag=ifelse(weekdays(Datum) %in% c("Samstag","Sonntag"), 0, 1)) %>%
  full_join(.  ,
  dosen_planung,
  by=c("jahr","quartal")) %>%
  group_by(hersteller) %>%
  arrange(hersteller,as.numeric(Datum)) %>%
  left_join(.,dosen_verabreicht,by="hersteller") %>%
  mutate(dosen=ifelse(quarter(Datum)==quarter(now()) & year(Datum)==year(now()),
                      dosen-dosen_geliefert,dosen))

zeitreihe_impfdosen <- bind_rows(
  prognosedatensatz %>% mutate(Verteilungsszenario="Gleichverteilung"),
  prognosedatensatz %>% mutate(Verteilungsszenario="Linear Ansteigen")) %>% 
  group_by(jahr,hersteller,quartal) %>% arrange(as.numeric(Datum)) %>%
  mutate(dosen.verf = dosen/n(),
         monat_in_quartal = month(Datum)-(quarter(Datum)-1)*3,
         gewichtungsfaktor = case_when(monat_in_quartal==1 ~ 0.645161290322581 ,
                                       monat_in_quartal==2 ~ 0.903225806451613,
                                       monat_in_quartal==3 ~ 1.45161290322581),
         gewichtungsfaktor = ifelse(Verteilungsszenario=="Linear Ansteigen",
                                    gewichtungsfaktor,1),
         dosen.verf = dosen.verf*gewichtungsfaktor)
zeitreihe_impfdosen %>% ggplot(aes(x=Datum,y=gewichtungsfaktor,color=Verteilungsszenario)) + geom_line()

zeitreihe_impfdosen %>% group_by(Verteilungsszenario,quartal) %>% 
  
  ggplot(aes(x=quartal,y=gewichtungsfaktor,color=Verteilungsszenario)) +
  geom_line()



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
