# Packages
library(dplyr)
library(glue)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(DT)
library(DBI)
library(forcats)
library(EpiEstim)
library(plotly)
library(zicolors)
library(deSolve)
library(jsonlite)

# Connect to DB
 conn <- DBI::dbConnect(RPostgres::Postgres(),
                          host   = Sys.getenv("DBHOST"),
                          dbname = Sys.getenv("DBNAME"),
                          user      =  Sys.getenv("DBUSER"),
                          password        = Sys.getenv("DBPASSWORD"),
                          port     = 5432,
                          sslmode = 'require')

  brd_timeseries <- tbl(conn,"brd_timeseries")
  prognosen <- tbl(conn,"prognosen") %>% filter((Tage<=28) | Tage %in% c(30,60,90,120,150,180))
  brdprognosen <- tbl(conn,"prognosen") %>% filter((Tage<=90) & ((ebene=="Kreis") | (name=="Berlin")) )
  rki <- tbl(conn,"rki")
  strukturdaten <- tbl(conn,"strukturdaten")
  aktuell <- tbl(conn,"params")
  trends <- tbl(conn,"trends")
  Datenstand <- tbl(conn,"Stand") %>% collect()
  bundprognose <- prognosen %>% filter(id==0) %>% collect() %>%
    filter(Szenario!="Trend D") %>%
    mutate(Szenario=ifelse(Szenario=="Trend lokal","aktueller Trend",Szenario),
           Szenario=ifelse(Szenario=="Worst Case","Worst Case (R=1,3)",Szenario),
           Datum=as.Date(Datum,format="%d.%m.%Y")) %>%
    filter(Datum<date(now()+weeks(12)))
  labordaten <- tbl(conn, "Labordaten")


rkiall <-  rki %>% select(AnzahlFall,AnzahlTodesfall,Meldedatum,Datenstand,NeuerFall,NeuerTodesfall) %>%
  mutate(NeuerFall=ifelse(NeuerFall==1,"Neue Meldung","Alte Meldung")) %>%
  group_by(Meldedatum,NeuerFall) %>% summarise(AnzahlFall=sum(AnzahlFall,na.rm=T)) %>%
  ungroup() %>% collect() %>%
  mutate(Meldedatum=date(Meldedatum),
         Wochenende=ifelse(wday(Meldedatum)==1 |wday(Meldedatum)==7,"Wochenende",NA))


aktuell <- tbl(conn,"params") %>% collect()

# maps
Kreise.shp <- readRDS("static/Kreise.rds")
Laender.shp <- readRDS("static/Laender.rds")

# Tables
table_patienten <- function(myname="Gesamt",myszenario="Italien"){
  prognosen %>% filter((name==myname) & (Szenario==myszenario)) %>%
    select(-ebene,-id,-name,-Neue_Faelle,
           -neue_icu_patienten,-n_kapazitaet,
           -aezte_ambulant,-durchseuchung) %>%
    collect()  %>%
    mutate(auslastung_icu=round(auslastung_icu),
           auslast_aerzte_covid_ambulant=round(auslast_aerzte_covid_ambulant*100),
           auslast_aerzte_unter60_covid_ambulant=round(auslast_aerzte_unter60_covid_ambulant*100),
           Datum=as.Date(Datum,format="%d.%m.%y"),
           behandlungszeit_pro_Tag_min=round(behandlungszeit_pro_Tag_min/60)) %>%
    select(Datum,
           "Nicht infiziert"=S,
           "Infiziert (symptomatisch)"=I,
           "Infiziert (genesen,verstorben)"=R,
           "Infiziert (jemals)"=Jemals_Infiziert,
           "Veränderung zum Vortrag in %"=Veraenderung_relativ,
           "ICU\n Patienten"=icu_patienten,
           "Stationäre Patienten"=kh_patienten,
           "ICU\n Auslastung in %"=auslastung_icu,
           "Behandlungszeit ambulant in h/Tag"=behandlungszeit_pro_Tag_min,
           "Auslastung Vertragsärzte in %"=auslast_aerzte_covid_ambulant,
           "Auslastung Vertragsärzte unter 60 J. in %"=auslast_aerzte_unter60_covid_ambulant)
}


# Analysis
mitigation_data <- function(myid=0){
  df <- brd_timeseries %>% filter(id==myid) %>% collect()
  if (nrow(df)==0) {
    df <- left_join(trends,strukturdaten %>% filter(id==myid) %>%
                      select(Country=name,id)) %>% filter(!is.na(id))
  }
  df <- df %>% mutate(date=date(date)) %>%
    mutate(I_cases=cases-lag(cases),I_dead=deaths-lag(deaths)) %>%
    filter(!is.na(I_cases) & (date<now()-days(3))) %>%
    filter(I_cases>=0 & I_dead>=0)
    mindate <- min(df$date)
  myconfig <- make_config(list(mean_si = 5,std_si = 4))
  res_parametric_si <- estimate_R(df$I_cases, method="parametric_si", config = myconfig)
  res_parametric_si_deaths <- estimate_R(df$I_dead, method="parametric_si", config = myconfig)
  result <- bind_rows(res_parametric_si$R %>% mutate(Merkmal="Fälle"),
                      res_parametric_si_deaths$R %>% mutate(Merkmal="Todesfälle"))
  as_tibble(result) %>% mutate(date=mindate+days(round(t_start+t_end)/2)) %>%
    select(date,Merkmal,R_Mean=`Mean(R)`,R_std= `Std(R)`)
}

blmitidata <- tibble()
for (theid in seq(0,16,1)){
  thename<-strukturdaten %>% filter(id==theid) %>% collect() %>% head(1) %>% pull(name)
  blmitidata = bind_rows(blmitidata,mitigation_data(theid) %>% mutate(name=thename,id=theid))
}

myblmitidata <- blmitidata %>%
  filter(Merkmal=="Fälle"  & R_Mean<10 & date>=date("2020-03-01"))

mitigationsplot_blvergleich <- function(){
  myplot <- ggplot(myblmitidata %>% rename(R=R_Mean) %>% mutate(R=round(R,digits = 1)),
                   aes(x=date,y=R,group=name,color=name=="Gesamt",
                       text=paste("Region: ",name,"<br>"))) +
    geom_line(data = . %>% filter(name!="Gesamt"),size=1,show.legend = F,color="lightgrey")+
  geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
    scale_color_zi()  +
    theme_minimal() + scale_x_date(date_labels = "%d.%m.") +
    labs(x="",y="Reproduktionszahl R(t)",caption="Vergleich Bund vs. Bundesländer") +
    geom_vline(aes(xintercept=date("2020-03-16")),color="grey") +
    geom_vline(aes(xintercept=date("2020-03-22")),color="grey") +
    geom_vline(aes(xintercept=date("2020-04-17")),color="grey") +
    annotate("text", x = date("2020-03-16"), y = 5, label = "Schulschließungen\n16.3.",color="black",size=3) +
    annotate("text", x = date("2020-03-22"), y = 3, label = "Kontakteinschränkungen\n22.3.",color="black",size=3) +
    annotate("text", x = date("2020-04-17"), y = 3, label = "Lockerung der \nMaßnahmen\n17.4.",color="black",size=3)
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
  }


rki_fallzahl_bl <- function(){
  df <- aktuell %>% filter(id<17)
  neue_faelle <- brd_timeseries %>% filter(id<=17) %>% collect() %>% group_by(id) %>%
    arrange(id,-as.numeric(date(date))) %>% filter(row_number()<=10) %>%
    summarise(newperday=round((first(cases)-last(cases))/10))
  result <- df %>% left_join(.,neue_faelle,by="id") %>%
    mutate(newperday_je100Tsd=round((newperday/Einwohner)*100000),
           cases_je_100Tsd=round((cases/Einwohner)*100000),
           deaths_je_100Tsd=round((deaths/cases)*1000),
           R0=round(R0,digits = 1),
           trend_slope=round(trend_slope,digits = 2)
           # ,trend_slope=case_when(round(trend_slope,digits = 1)>0 ~ paste0("+",format(trend_slope,digits =1,decimal.mark = ",",big.mark = ".")),
           #                       round(trend_slope,digits = 1)==0 ~ paste0(format(trend_slope,digits =1,decimal.mark = ",",big.mark = ".")),
           #                       round(trend_slope,digits = 1)<0 ~ paste0(format(trend_slope,digits =1,decimal.mark = ",",big.mark = ".")))
    )
  result %>% select(Bundesland=name,
                    "Neue Fälle pro Tag"=newperday,
                    "Neue Fälle je 100 Tsd. Einw."=newperday_je100Tsd,
                    "Gesamt"=cases,
                    "Fälle je 100 Tsd. Einw."=cases_je_100Tsd,
                    "Tote"=deaths,"je 1000 Fälle"=deaths_je_100Tsd,
                    "R(t)"=R0)
}

plot_bundprognose <- ggplot(bundprognose %>% mutate("Fälle"=Jemals_Infiziert/1000),aes(x=Datum,y=`Fälle`,color=Szenario)) +
  geom_line(size=2.3) + theme_minimal() + scale_color_zi() +
  theme(legend.position = "bottom") + labs(color="",x="",y="COVID-19-Fälle in Tsd.")+
  scale_x_date(date_labels = "%d.%m.") +
  scale_y_continuous(limits=c(0,floor(max(bundprognose$Jemals_Infiziert)/1000)+1))


# Karten
mapdata_prognosis <- function(myoutcome="I"){
  data<-brdprognosen %>% rename("Outcome"=myoutcome) %>%
    filter(((ebene=="Kreis")|((ebene=="Land") & (name=="Berlin")) )) %>%
    filter(Tage %in% c(1,28)) %>%
    collect() %>%
    mutate(Datum=as.Date(Datum,format="%d.%m.%Y")) %>%
    select(Szenario,id,name,Tage,Datum,"Outcome") %>%
    mutate(Label=make_labels(.,indicator_name = "Outcome"),
           id=ifelse(name=="Berlin",id*1000,floor(id/1000))) %>% arrange(id,Datum)
  return(data)
}


#  Labordaten
laborkapazität <- labordaten  %>% filter(Testtyp=="PCR-Testung") %>% group_by(kw) %>% summarise(kapazitaet=sum(kapazitaet)) %>% arrange(kw) %>% pull() %>% last()


mylabordaten_gesamt <-
  left_join(
    labordaten %>% filter(Testtyp=="PCR-Testung") %>% group_by(Bundesland) %>%
      summarise(Tests=sum(gesamt,na.rm=T),
                Positiv=sum(positiv,na.rm=T)) %>% collect(),
    aktuell %>% filter(id<17) %>%
      rename(Bundesland=name) %>% collect())

mylabordaten_kw <-
  left_join(
    labordaten  %>% filter(Testtyp=="PCR-Testung") %>% group_by(kw) %>%
      summarise(Tests=sum(gesamt,na.rm=T),
                Positiv=sum(positiv,na.rm=T),
                kapazitaet=sum(kapazitaet,na.rm=T),
                id=0) %>% collect(),
    aktuell %>% filter(id==0) %>%
      rename(Bundesland=name) %>% collect()) %>% filter(!is.na(Tests))

rki_kw <- brd_timeseries %>% filter(id==0) %>% collect() %>% mutate(kw=isoweek(date(date))) %>%
  group_by(kw) %>% summarise(Fälle=sum(cases),Todesfälle=sum(deaths)) %>%
  mutate("Neue Fälle"=`Fälle`-lag(`Fälle`))

mylabordaten_kw_plus_rki <- left_join(mylabordaten_kw %>% filter(kw>=11),rki_kw) %>%
  select(id,kw,Bundesland,Einwohner,"Neue Fälle",Tests,Kapazität=kapazitaet) %>%
  mutate(Kapazität=Kapazität*7) %>%
  gather(Merkmal,Wert,5:7) %>% mutate(Wert_je_1000EW=round(Wert/(Einwohner/1000),digits=1))


plot_trend_labortests <- ggplotly(ggplot(mylabordaten_kw_plus_rki ,
                                         aes(x=kw,y=Wert_je_1000EW,color=Merkmal,label=Merkmal)) +
                                    geom_line(size=2) +
                                    labs(x="Kalederwoche",y="Wert je Tsd. Einw.",color="") +
                                    theme_zi_titels() +scale_color_zi())


plot_faelle_zu_tests <- ggplotly(ggplot(mylabordaten_gesamt %>%
                                 mutate("Fälle je Tsd. Einw."=round(cases/(Einwohner/1000),digits=1),
                                        "Tests je Tsd. Einw."=round(Tests/(Einwohner/1000),digits=1)),
                               aes(y=`Fälle je Tsd. Einw.`,
                                   x=`Tests je Tsd. Einw.`,label=Bundesland)) +
  geom_text(color=zi_cols("zigrey")) +
  geom_point(color=zi_cols("ziblue")) +
  labs( y="Fälle je Tsd. Einw.",x="Tests je Tsd. Einw.") +
  theme_zi_titels() +
  scale_y_continuous(limits=c(0,1+max(mylabordaten_gesamt$cases/(mylabordaten_gesamt$Einwohner/1000)))))  %>%
  layout(showlegend = FALSE)

plot_positiv_zu_tests <- ggplotly(mylabordaten_kw %>%
  mutate(Negativ=Tests-Positiv) %>%
  select(kw,Positiv,Negativ) %>%
  gather(Merkmal,Wert,2:3) %>% group_by(kw) %>%
  mutate(Anteil=paste0(round(100*Wert/sum(Wert),digits=0),"%"),
         Wert=Wert/1000) %>% ungroup() %>%
  ggplot(aes(x=kw,fill=Merkmal,y=Wert))+
  geom_bar(post="stack",stat="identity") + scale_fill_zi() +
  theme_zi_titels() + geom_text(aes(label=Anteil),position = "stack") +
  labs(x="KW",y="Anzahl Tests in Tsd.",fill="") )


# Akute infizierte Fälle
vorwarndata <- brd_timeseries %>% filter(id==0) %>% collect()  %>%
  mutate(
    cases=floor(zoo::rollmean(cases, 7, fill=NA)),
    Infected=cases-lag(cases,15)) %>% # Wg. 10 Tage infektiös und symptomatisch + 5 Tage asymptomatisch
  mutate(Rt=(cases-lag(cases,10))/lag(Infected,10)) %>% filter(!is.na(Infected) & !is.na(Rt))  %>%
  mutate(date=date(date),
         Neue_faelle=cases-lag(cases),
         Neue_faelle_Anstieg = Neue_faelle/lag(Neue_faelle),
         Vorwarnzeit= log(16000/Neue_faelle)/log(Neue_faelle_Anstieg),
         Situation = case_when(Vorwarnzeit<0 ~ "grün",
                               (Vorwarnzeit>18 )  ~ "orange",
                               (Vorwarnzeit>=0 & Vorwarnzeit<18)   ~ "rot"),
         Situation=factor(Situation,levels=c("grün","orange","rot"),ordered=T),
         show_val=wday(date)==3) %>% filter(date>=date("2020-03-02"))

akutinfiziert <- ggplot(vorwarndata,aes(x=date,y=Infected,group=1)) +
  geom_vline(aes(xintercept=date("2020-03-16")),color="black",linetype ="dotted") +
  geom_vline(aes(xintercept=date("2020-03-22")),color="black",linetype ="dotted") +
  geom_vline(aes(xintercept=date("2020-04-17")),color="black",linetype ="dotted") +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_zi() + scale_x_date(breaks = "1 week",date_labels = "%d.%m.") +
  annotate("text", x = date("2020-03-16"), y = 22000, label = "Schulschließungen",color="black",size=3) +
  annotate("text", x = date("2020-03-22"), y = 42000, label = "Kontakteinschränkungen",color="black",size=3) +
  annotate("text", x = date("2020-04-17"), y = 43500, label = "Lockerungsbeschluss",color="black",size=3) +
  labs(title="Entwicklung der COVID-19-Epidemie in Deutschland",subtitle = "Aktuelle Zahl akut COVID-19-Infizierter") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

# Vorwarnzeit aktuell
#####################
Bund_Faelle_neu_idl10Tagen <- brd_timeseries %>% filter(id==0) %>% collect() %>%
  mutate(date=date(date)) %>% filter((date>=(now()-days(11)) & id==0) ) %>%
  summarise(neue_Faelle=round((last(cases)-first(cases))/nrow(.))) %>% pull(neue_Faelle)

Belastungsgrenze <- 16340
Reaktionszeit <- 14+7
brdparams <- aktuell %>% filter(id==0) %>% collect()
ngesamt <- brdparams$Einwohner
recovered <- brdparams$recovered + brdparams$deaths
gamma <- 1/10
Ausgangsfallzahl_bestand <- brd_timeseries %>% filter(id==0) %>% collect() %>% mutate(date=date(date)) %>%
  filter((date>=(now()-days(7+2)) & id==0) )  %>% collect() %>% mutate(neu=cases-lag(cases))
Ausgangsfallzahl <-Ausgangsfallzahl_bestand %>%   summarise(neu=round(mean(neu,na.rm = T))) %>% pull(neu)
# infected <- brdparams$cases - (brdparams$recovered + brdparams$deaths)
infected <- round(Bund_Faelle_neu_idl10Tagen/gamma)
recovered <- brdparams$cases - infected

Rt <- seq(1.1, 2, 0.1)
Vorwarnzeit <- rep(0, length(Rt))
Anstieg <- rep(0, length(Rt))

SIR <- function(time, state, parameters, ngesamt, gamma) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/ngesamt * I * S
    dI <- beta/ngesamt * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

sirmodel<- function(ngesamt,  S,   I,   R,  R0,  gamma,  horizont=365) {
  # Set parameters
  ## Infection parameter beta; gamma: recovery parameter
  params <- c("beta" = R0*gamma)
  ## Timeframe
  times      <- seq(0, horizont, by = 1)
  ## Initial numbers
  init       <- c("S"=S, "I"=I, "R"=R)
  ## Time frame
  times      <- seq(0, horizont, by = 1)

  # Solve using ode (General Solver for Ordinary Differential Equations)
  out <- ode(y = init, times = times, func = SIR, parms = params, ngesamt=ngesamt, gamma=gamma)

  # change to data frame and reformat
  out <- as.data.frame(out) %>% select(-time) %>% rename(S=1,I=2,R=3) %>%
    mutate_at(c("S","I","R"),round)
  ## Show data
  return(as_tibble(out))
}

for (i in seq(Rt)) {
  mysir <- sirmodel(ngesamt = ngesamt,
                    S = ngesamt - infected - recovered,
                    I = infected,
                    R = recovered,
                    R0 = Rt[i],
                    gamma = gamma,
                    horizont = 365) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  Vorwarnzeit[i] <- which.max(mysir$Neue_Faelle>Belastungsgrenze)
  Anstieg[i] <- mysir$Neue_Faelle[2]/Ausgangsfallzahl
}

theoretisch <- data.frame(Rt = Rt) %>%
  mutate(Ausgangsfallzahl=Ausgangsfallzahl, Belastungsgrenze=Belastungsgrenze,
         Vorwarnzeit=Vorwarnzeit, Anstieg=Anstieg,
         Reaktionszeit=Reaktionszeit,
         Effektive_Vorwarnzeit=Vorwarnzeit-Reaktionszeit)

mycolorbreaks <- c(14,30,90)
plotdata_Anstieg <- theoretisch %>% select(Rt,Vorwarnzeit,"Effektive Vorwarnzeit"=Effektive_Vorwarnzeit)  %>% gather(Merkmal,Wert,2:3)
plot_Anstiegtheor <- ggplot(plotdata_Anstieg, aes(x=Rt, y=Wert,color=Merkmal)) +
  geom_line(size=2, show.legend = F)+
  theme_zi() + scale_color_zi() +
  scale_y_continuous(limits=c(0,max(theoretisch$Vorwarnzeit)))+
  scale_x_continuous(labels =  function(x) paste0("R=",format(x,decimal.mark = ",")), breaks=Rt)  +
  labs(title="Vorwarnzeit in Tagen nach R(t) ausgehend von aktuell",Bund_Faelle_neu_idl10Tagen, "Neuinfektionen pro Tag")


