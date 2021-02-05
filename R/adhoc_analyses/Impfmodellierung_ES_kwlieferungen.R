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
prognosestart <- as_date("2021-02-01") # lubridate::as_date(now())
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
  left_join(dosen_planung_kw %>% rename(dosen_kw="dosen") %>% select(-Datum), by=c("kw", "quartal", "jahr", "hersteller")) %>%
  mutate(dosen_pro_tag=ifelse(kw<=8, round(dosen_kw/7), 0)) %>%
  group_by(hersteller) %>% arrange(Datum) %>%
  mutate(dosen_geliefert_temp=dosen_geliefert+cumsum(dosen_pro_tag)) %>%
  ungroup() %>%
  mutate(dosen_pro_tag=ifelse(quartal==1 & kw>=9, round((dosen_quartal-dosen_geliefert_temp)/31), dosen_pro_tag),
         dosen_kw=ifelse(quartal==1 & kw>=9, dosen_pro_tag*7, dosen_kw),
         dosen_pro_tag=ifelse(quartal>=3, round(dosen_quartal/92), dosen_pro_tag),
         dosen_kw=ifelse(quartal>=3, 7*dosen_pro_tag, dosen_kw))
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
  ungroup()
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

# output.plot1 <- output %>% mutate(#szenario = paste(szenario,Verteilungsszenario),
#                                   Monat=month(Datum, label=TRUE)) %>%
#   filter(jahr>=2021) %>% 
#   ggplot(aes(x=Monat, y=IZ_Auslastung, fill=szenario)) +
#   facet_wrap(.~Verteilungsszenario) +
#   geom_bar(stat="identity", position = "dodge") + #scale_x_date(breaks = "1 month") +
#   geom_hline(yintercept=100) +
#   scale_fill_zi() + theme_minimal() + theme(legend.position="bottom")
# 
# output.plot1kw <- output %>% 
#   filter(jahr>=2021) %>% 
#   ggplot(aes(x=kw, y=IZ_Auslastung, color=szenario, group=szenario)) +
#   facet_wrap(.~Verteilungsszenario , ncol=1) +
#   geom_line(size=3) + # scale_x_date(breaks = "1 month") +
#   geom_hline(yintercept=100,  linetype="dotted") + labs(color="", x="KW", y="Auslastung der Impfzentren")+ 
#   scale_color_zi() + theme_minimal() + theme(legend.position="bottom")
# 
# output.plot2 <- output %>% mutate(#szenario = paste(szenario,Verteilungsszenario),
#                                   Monat=month(Datum, label=TRUE)) %>%
#   filter(jahr>=2021) %>% 
#   ggplot(aes(x=Monat, y=Zusatzbedarf_n_Aerzte, fill=szenario)) +
#   facet_wrap(.~Verteilungsszenario) +
#   geom_bar(stat="identity", position = "dodge") + # scale_x_date(breaks = "1 month") +
#   geom_hline(yintercept=0,) +
#   scale_fill_zi() + theme_minimal() + theme(legend.position="bottom")
# 
# output.plot2kw <- output %>% mutate(#szenario = paste(szenario,Verteilungsszenario),
#   Monat=month(Datum, label=TRUE)) %>%
#   filter(jahr>=2021) %>% 
#   ggplot(aes(x=kw, y=Zusatzbedarf_n_Aerzte, color=szenario, group=szenario)) +
#   facet_wrap(.~Verteilungsszenario , ncol=1) +
#   geom_line(size=3) + # scale_x_date(breaks = "1 month") +
#   geom_hline(yintercept=0 ) + labs(color="", x="KW", y="Anzahl zuusätzlich notwendiger Ärzte")+ 
#   scale_color_zi() + theme_minimal() + theme(legend.position="bottom")

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
  mutate(Zweit_neu = ifelse(row_number()<=abstand,
                            (dosen_verabreicht_erst - dosen_verabreicht_zweit)/abstand,0),
         Erst_neu = ifelse(row_number()<=abstand & (Anwendung - Zweit_neu)>0,
                           Anwendung - Zweit_neu,0))
di.ts <- tibble()
for (hs in unique(durchimpfung.ts$hersteller)) {
  di.ts.hersteller <- durchimpfung.ts %>%
    filter(hersteller==hs)
  thisabstand <- di.ts.hersteller$abstand[1]
  for (d in seq(length(Datumsliste))) {
    if (d>thisabstand) {
      di.ts.hersteller[d, "Zweit_neu"] <- di.ts.hersteller$Erst_neu[d-thisabstand]
      if (di.ts.hersteller[d, "Anwendung"]-di.ts.hersteller[d, "Zweit_neu"]>0) {
        di.ts.hersteller[d, "Erst_neu"] <- di.ts.hersteller[d, "Anwendung"]-di.ts.hersteller[d, "Zweit_neu"]
      }
    }
  }
  di.ts <- bind_rows(di.ts, di.ts.hersteller)
}

di.ts_LK <- di.ts %>% 
  group_by(Verteilungsszenario ,Betriebsszenario, hersteller) %>% 
  mutate(dosen_verabreicht_erst= dosen_verabreicht_erst+ cumsum(Erst_neu),
         dosen_verabreicht_zweit = dosen_verabreicht_zweit+cumsum(Zweit_neu)) %>%
  group_by(Verteilungsszenario ,Betriebsszenario, Datum) %>% 
  summarise(dosen_verabreicht_erst=sum(dosen_verabreicht_erst/anwendungen),
            dosen_verabreicht_zweit=sum(dosen_verabreicht_zweit/anwendungen))
ggplot(di.ts_LK, aes(x=Datum,y=dosen_verabreicht_zweit,color=Betriebsszenario)) + 
  geom_line() + facet_grid(.~Verteilungsszenario)

# Schritt 3: Folgetage iterativ bestimmen...

# 
# 
# 
# min(Datumsliste)
# 
# durchimpfung.ts <- durchimpfung %>% arrange(Verteilungsszenario,Betriebsszenario,Datum,hersteller) %>%
#   left_join(.,dosen_verabreicht %>% mutate(Datum=min(Datumsliste)) %>% 
#               select(- dosen_geliefert),by=c("hersteller","Datum")) %>%
#   arrange(Verteilungsszenario,Betriebsszenario, hersteller ,Datum ) %>%
#   group_by(Verteilungsszenario,Betriebsszenario,hersteller) %>%
#   left_join(., dosen_planung %>% 
#               group_by(hersteller) %>% count(abstand) %>% select(-n), by="hersteller") %>% 
#   mutate(
#   neu_zweit=ifelse(row_number()<=abstand,
#                   (first(dosen_verabreicht_erst)-
#                      first(dosen_verabreicht_zweit))/abstand,0),
#   neu_erst=ifelse(row_number()<=abstand & (Anwendung-neu_zweit)>=0,
#                                   Anwendung-neu_zweit,Anwendung)) %>%
#   mutate(neu_zweit =  ifelse(row_number()>abstand,neu_zweit+
#            lag(neu_erst,dosen_planung %>% 
#                  filter(hersteller==hersteller) %>% head(1) %>%
#                  pull(abstand) )
#            ,neu_zweit))
#   
# Für jedes Szenario muss ab Zeile Abstand + 1 die 
# Anzahl der Zweitimpfungen iterativ aus den vorangegangenen Erstimpfungen 
# bestimmt werden, die Zahl der neuen Erstimpfungen reduziert sich dann um die 
# ZWeitimpfungen

# 
# for(row in seq(1,dim(testds)[1],1)){
#  thelag =  testds[5,]$abstand
#  testds <- testds %>% 
#    mutate(neu_zweit = ifelse(row_number()>thelag & row_number()=,
#           lag(new_erst,thelag)
#           ) 
# }
# 
# 
# 
# %>%
#   mutate(lag_neu_erst = lag(neu_erst, dosen_planung %>% 
#                               filter(hersteller==hersteller) %>% head(1) %>%
#                               pull(abstand) ),
#          neu_zweit=ifelse(!is.na(lag_neu_erst),neu_zweit+lag_neu_erst,neu_zweit)) 
# )
#                   
#                   
#                   
#                   %>%
#   mutate(neu_zweit=ifelse(row_number()>abstand,lag(neu_erst,abstand-1),neu_zweit))
#          
#          ,
#          neu_erst= ifelse(row_number()>abstand & (Anwendung-neu_zweit)>=0,
#                          Anwendung-neu_zweit,neu_erst)
#   )

  
# 
# 
# durchimpfung.ts <-durchimpfung %>% 
#   mutate(Patienten_IZ=Anwendung/anwendungen,
#          Patienten_IZ_plus=dosen.verf/anwendungen) %>% 
#   group_by(Verteilungsszenario,Betriebsszenario,Datum) %>% 
#   summarise(Patienten_IZ=sum(Patienten_IZ),
#             Patienten_IZ_plus=sum(Patienten_IZ_plus),
#             Anwendung_IZ=sum(Anwendung),
#             Rest_IZ=sum(Restdosen)
#             ) %>%
#   group_by(Verteilungsszenario,Betriebsszenario) %>%
#   arrange(Verteilungsszenario,Betriebsszenario,Datum) %>%
#   mutate(Patienten_IZ_kum=cumsum(Patienten_IZ),
#          Patienten_IZ_plus_kum=cumsum(Patienten_IZ_plus),
#          "Imfquote IZ"      = 100*(Patienten_IZ_kum/impflinge_gesamt),
#          "Imfquote IZ und Vertragsärzte" = 100*(Patienten_IZ_plus_kum/impflinge_gesamt)
#   ) 
# 
# 
# plotdata.imfquote <- durchimpfung.ts %>% select(Verteilungsszenario,Betriebsszenario,Datum,"Imfquote IZ","Imfquote IZ und Vertragsärzte") %>% gather(Merkmal,Wert,4:5)
# 
#  ggplot(plotdata.imfquote %>% filter(Wert<=100),aes(x=Datum,color=Merkmal,y=Wert)) + 
#    geom_line(size=2.5) + theme_minimal() + scale_color_zi() +
#    facet_grid(Betriebsszenario~ Verteilungsszenario)

## Übersichten
dosen_planung %>% group_by(hersteller) %>% 
  summarise(dosen=sum(dosen),anwendungen=sum(dosen)/mean(anwendungen),abstand=mean(abstand))
kapazitaeten
szenarien
zeitreihe_impfdosen %>% head()


# 1. Q1 Korrektur mit empirischen Dosen, Speicher der Rückstellstellimfpunegn
# 2. Auslastung korrigieren, Unterstützungbedarf IZ durch Ärzte quantifizieren
# 3. Verfügbarkeitszenarien, V1 gleichverteil + emp. , V2 zunehmend + emp.
# 4. Report 1: Impfkapazität
# 5. Rückstellungen , Abgeschlossene Impflinge, 
# 6. Report 2: Durchimpfungsqsquote
# 7. Dashboard
