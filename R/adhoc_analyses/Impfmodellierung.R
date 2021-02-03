
library("tidyverse")
library("lubridate")

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
impfstart = as.Date("2020-12-20")
prognoseende = as.Date("2021-12-31")

dosen = tibble(dosen_bt=c(1.3*1e6, 10.9*1e6, (31.5+8.7)*1e6,(17.6+17.1)*1e6,(2.7+10.8)*1e6),
               dosen_mod=c(0, 1.8*1e6, 6.4*1e6,(17.6+9.1)*1e6,(24.6+18.3)*1e6),
               dosen_az=c(0, 5.6*1e6, 16.9*1e6,33.8*1e6,0*1e6),
               dosen_jj=c(0, 0, 10.1*1e6,22*1e6,4.6*1e6),
               dosen_cv=c(0, 0, 3.5*1e6,9.4*1e6,11.7*1e6),
               dosen_gsk=c(0, 0, 0,0,27.5*1e6),
                   quartal=c(4,1,2,3,4),
                   jahr=c(2020,2021,2021,2021,2021)) %>%
  gather(hersteller,dosen,contains("dosen")) %>% mutate(hersteller=str_replace(hersteller,"dosen_","")) %>% mutate(anwendungen=ifelse(hersteller!="jj",2,1),abstand=case_when(hersteller=="bt"~4*7,hersteller=="mod"~6*7,hersteller=="jj"~0,TRUE~3*7))



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
  
# Kapazitäten
n_impfzentren <- 400
impfzentrum_kapazitaet_wt <- 200e3/n_impfzentren

n_praxen <- 50000
praxis_kapoaziteat_wt = 10



# Input-Daten
andata <- full_join(prognosedatensatz , dosen, by=c("jahr","quartal")) %>%
  group_by(hersteller,jahr,quartal) %>%
  mutate(dosen.verf= dosen/n()) %>% ungroup() %>% arrange(hersteller,as.numeric(Datum))

# check 
# sum(andata$dosen.verf/andata$anwendungen)/1e6


# Output-Daten
# BROKEN
# output <- andata %>% 
#   mutate(iz_regelbetrieb=n_impfzentren*impfzentrum_kapazitaet_wt*werktag,
#          iz_7tw=n_impfzentren*impfzentrum_kapazitaet_wt) %>% 
#   gather(Betriebsart,Kapazitaet,contains("iz_")) %>%
#   group_by(Betriebsart) %>%
#   mutate(iz_Restdosen=ifelse(cumsum(Kapazitaet)>=cumsum(dosen.verf),0,
#                              cumsum(dosen.verf)-cumsum(Kapazitaet)),
#          iz_Verimpft = cumsum(dosen.verf)- iz_Restdosen,
#          praxen_kapazitaet=n_praxen*praxis_kapoaziteat_wt*werktag,
#          praxen_Restdosen=ifelse(praxen_kapazitaet>=cumsum(iz_Restdosen),0,
#                                  cumsum(cumsum(iz_Restdosen)-praxen_kapazitaet)) )

# DEBUG PLOTS
ggplot(andata) + aes(x=Datum,y=cumsum(dosen.verf)/1e6) + geom_line() + scale_y_continuous(limits=c(0,sum(andata$dosen.verf)/1e6))
ggplot(output) + aes(x=Datum,y=iz_Restdosen/1e6,color=Betriebsart) + geom_line() + scale_y_continuous(limits=c(0,sum(andata$dosen.verf)/1e6))
ggplot(output) + aes(x=Datum,y=praxen_Restdosen/1e6,color=Betriebsart) + geom_line() + scale_y_continuous(limits=c(0,sum(andata$dosen.verf)/1e6))


                          
