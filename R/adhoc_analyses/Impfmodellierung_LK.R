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
  prognosedatensatz %>% mutate(Verteilungsszenario="Linearer Anstieg der Produktion in Q2")) %>% 
  group_by(Datum,hersteller) %>% arrange(as.numeric(Datum)) %>%
  # group_by(jahr,hersteller,quartal) %>% arrange(as.numeric(Datum)) %>%
  mutate(gewichtungsfaktor = case_when(
           quartal==1 ~ 1/as.integer(as_date("2021-03-31")-today()),
           quartal==2 & month(Datum)==4 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.20/30 - (0.20/30)*.15 + (0.20/30)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
           quartal==2 & month(Datum)==5 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.35/31 - (0.35/31 )*.15 + (0.35/31)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
           quartal==2 & month(Datum)==6 & Verteilungsszenario=="Linearer Anstieg der Produktion in Q2" ~ 0.45/30 - (0.45/30 )*.15 + (0.45/30)*.3* as.numeric(mday(Datum)/days_in_month(Datum)),
           quartal==2 & Verteilungsszenario=="Gleichverteilung" ~ 1/(30+31+30), 
           quartal==3 ~ 1/(31+31+30),
           quartal==4 ~ 1/(31+30+31)
         ),
         dosen.verf = dosen*gewichtungsfaktor,
         dosen.verf_plus= dosen_geliefert-dosen_verabreicht_erst-dosen_verabreicht_zweit) %>%
  group_by(Verteilungsszenario,hersteller) %>% arrange(Verteilungsszenario,hersteller,as.numeric(Datum)) %>%
  mutate(dosen.verf=ifelse(row_number()<=21,dosen.verf+dosen.verf_plus/21,dosen.verf)) 


# CHECK zeitreihe_impfdosen %>% ggplot(aes(x=Datum,y=gewichtungsfaktor,color=Verteilungsszenario)) + geom_line()





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
    # tibble(szenario="IZ + Vertragsärzte", 
    #        kap_wt=kapazitaeten %>% filter(einrichtung=="Impfzentren" & betriebsart=="Intensivbetrieb") %>% summarise(wert=sum(kap_wt)) %>% pull(wert),
    #        kap_we=kapazitaeten %>% filter(einrichtung=="Impfzentren" & betriebsart=="Intensivbetrieb") %>% summarise(wert=sum(kap_we)) %>% pull(wert))
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

## Durchimpfung

durchimpfung.base <- zeitreihe_impfdosen %>% group_by(Verteilungsszenario,hersteller) %>% 
   ungroup() %>% select(Verteilungsszenario,Datum,werktag,dosen.verf,hersteller,anwendungen)   %>%  full_join(szenarien,by = character()) %>% 
  mutate(Kapazitaet=ifelse(werktag, kap_wt, kap_we)) %>%
  select(-kap_wt, -kap_we,-werktag) %>% rename("Betriebsszenario"=szenario) %>% 
  left_join(., dosen_verabreicht %>%
              select(-dosen_geliefert),by=c("hersteller")) %>%
  left_join(., dosen_planung %>% group_by(hersteller) %>% count(abstand) %>% select(-n),by=c("hersteller")) %>%
  relocate(Verteilungsszenario,Betriebsszenario,Datum,hersteller) %>%
  arrange(Verteilungsszenario,Betriebsszenario,Datum,hersteller)

Datumsliste = sort(unique(durchimpfung.base$Datum))

for(theDatum in Datumsliste) {
  if (theDatum==Datumsliste[1]){
    i=0
    durchimpfung <- durchimpfung.base
  }
  i = i+1
  
  # Am ersten Tag der Prognose alte Daten holen
  if (i==1){
    tagesdaten   = durchimpfung %>% filter(Datum==as_date(theDatum))  %>%
      group_by(Verteilungsszenario,Betriebsszenario,Datum) %>%
      mutate(Auslastung=sum(dosen.verf)/Kapazitaet,
             Anwendung = round(dosen.verf*(1/Auslastung)),
             Restdosen = dosen.verf-Anwendung) %>% ungroup() 
      
    durchimpfung = durchimpfung %>% filter(Datum!=as_date(theDatum))
    durchimpfung = bind_rows(tagesdaten,durchimpfung)
  }
  # An anderen Tagen vorherigen Tag und aktuellen Tag nehmen
  if (i>1){
     tagesdaten_old = durchimpfung %>% filter(Datum==(as_date(theDatum)-days(1))) %>% 
       select(Verteilungsszenario,hersteller,Betriebsszenario,Restdosen)
     tagesdaten_new = durchimpfung %>% filter(Datum==(as_date(theDatum))) %>% 
       select(-Restdosen) %>%
        left_join(. ,tagesdaten_old, by = c("Verteilungsszenario", "Betriebsszenario"
                                            ,"hersteller" ))  %>%
     mutate(
       dosen.verf=dosen.verf+Restdosen,
       Auslastung=sum(dosen.verf)/Kapazitaet,
              Anwendung = round(dosen.verf*(1/Auslastung)),
              Restdosen = round(dosen.verf-Anwendung)) %>% ungroup() 
    durchimpfung = durchimpfung %>% filter(Datum!=as_date(theDatum))
    durchimpfung = bind_rows(tagesdaten_new,durchimpfung)
  }
}

durchimpfung.ts <- durchimpfung %>% arrange(Verteilungsszenario,Betriebsszenario,hersteller,Datum)

# Schritt 1/2: Erst und Zweitimpfungen aus alten Erstimpfungen abarbeiten für abstand Tage
durchimpfung.ts <- durchimpfung.ts %>% 
  group_by(Verteilungsszenario,Betriebsszenario,hersteller) %>% 
  mutate(Zweit_neu = ifelse(row_number()<abstand,
                            (dosen_verabreicht_erst - dosen_verabreicht_zweit)/abstand,0),
         Erst_neu = ifelse(row_number()<abstand & (Anwendung - Zweit_neu)>0,
                           Anwendung - Zweit_neu,0)) 

# Schritt 3: Folgetage iterativ bestimmen...

# Shortcut 

shortcut <- bind_rows(
  durchimpfung.ts %>% mutate(Patienten=Anwendung/anwendungen) %>% 
  group_by(Verteilungsszenario, Betriebsszenario,Datum) %>% summarise(Patienten=sum(Patienten)) %>% 
  mutate(Patienten=cumsum(Patienten)+
           dosen_verabreicht %>% 
           summarise(Dosen=sum(dosen_verabreicht_zweit)) %>% pull(Dosen)),
  
  durchimpfung.ts %>% filter(Betriebsszenario=="IZ Regelbetrieb") %>% 
    group_by( Verteilungsszenario,Betriebsszenario,Datum) %>% 
    summarise(Patienten=sum(dosen.verf/anwendungen) )  %>% mutate(Patienten=Patienten+dosen_verabreicht %>% 
                                                                    summarise(Dosen=sum(dosen_verabreicht_zweit)) %>% 
                                                                    pull(Dosen)) %>%
  ungroup() %>% mutate(Betriebsszenario="IZ + Vertragsärzte") %>%
  mutate(Patienten)
)

##

shortcut2 <- durchimpfung.ts %>% mutate(Patienten=Anwendung/anwendungen) %>% 
  group_by(Verteilungsszenario, Betriebsszenario,Datum) %>% summarise(Patienten=sum(Patienten)) %>% 
  mutate(Patienten=cumsum(Patienten)+
           dosen_verabreicht %>% 
           summarise(Dosen=sum(dosen_verabreicht_zweit)) %>% pull(Dosen))

## added safety margin 
shortcut%>% mutate(Durchimpfung= 90*(Patienten/impflinge_gesamt)) %>% 
  filter(Durchimpfung<=100) %>% ggplot() + 
  aes(x=Datum,y=Durchimpfung, color=Betriebsszenario) + geom_line(size=2.5) +
  facet_grid(.~Verteilungsszenario) + theme_minimal() + scale_color_zi() +
  scale_x_date(breaks="4 weeks", date_labels = "%d.%m.")+
  labs(y="Durchimpfung der Bevölkerung im Alter 20+ J.") +
  geom_vline(xintercept = as_date("2021-09-11"), linetype="dashed") + 
  geom_hline(yintercept = 0) + theme(legend.position = "bottom")

