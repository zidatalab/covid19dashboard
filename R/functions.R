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

# parameters from literature
icu_days <- 10.1 # aok/divi paper lancet
share_icu <- (266+15395)/207828 # divi intensivregister and rki daily report 30 july 2020

# Connect to DB
# conn <- dbConnect(RSQLite::SQLite(), "../covid-19/data/covid19db.sqlite")
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
  filter(Merkmal=="Fälle"  & R_Mean<10 & date>=date("2020-03-13"))

mitigationsplot_blvergleich <- function(){
  myplot <- ggplot(myblmitidata %>% rename(R=R_Mean) %>% mutate(R=round(R,digits = 1)),
                   aes(x=date,y=R,group=name,color=name=="Gesamt",
                       text=paste("Region: ",name,"<br>"))) +
    geom_hline(yintercept = 1) +
    geom_line(data = . %>% filter(name!="Gesamt"),size=1,show.legend = F,color="lightgrey")+
    geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
    scale_color_zi()  +
    theme_minimal() + scale_x_date(date_labels = "%d.%m.", breaks="2 weeks") +
    labs(x="",y="Reproduktionszahl R(t)",caption="Vergleich Bund vs. Bundesländer") +
    geom_vline(aes(xintercept=date("2020-03-16")),color="grey") +
    geom_vline(aes(xintercept=date("2020-03-22")),color="grey") +
    geom_vline(aes(xintercept=date("2020-04-17")),color="grey") +
    annotate("text", x = date("2020-03-16"), y = 3.3, label = "Schulschließungen\n16.3.",color="black",size=3) +
    annotate("text", x = date("2020-03-22"), y = 2.5, label = "Kontakteinschränkungen\n22.3.",color="black",size=3) +
    annotate("text", x = date("2020-04-17"), y = 2.0, label = "Lockerung der \nMaßnahmen\n17.4.",color="black",size=3) +
    theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())
  myplot %>% ggplotly(tooltip = c("x", "y", "text"))
}

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
  geom_area(fill="#0086C530") +
  geom_vline(aes(xintercept=date("2020-03-16")),color="black",linetype ="dotted") +
  geom_vline(aes(xintercept=date("2020-03-22")),color="black",linetype ="dotted") +
  geom_vline(aes(xintercept=date("2020-04-17")),color="black",linetype ="dotted") +
  geom_hline(aes(yintercept=0),color="black",linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_minimal() +
  scale_x_date(breaks = "2 weeks",date_labels = "%d.%m.") +
  annotate("text", x = date("2020-03-16"), y = 22000, label = "Schulschließungen",color="black",size=3) +
  annotate("text", x = date("2020-03-22"), y = 42000, label = "Kontakteinschränkungen",color="black",size=3) +
  annotate("text", x = date("2020-04-17"), y = 43500, label = "Lockerungsbeschluss",color="black",size=3) +
  labs(y="Anzahl akut infiziert",x = "Datum") +
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))

# Vorwarnzeit aktuell
#####################
# Daten
letzte_7_tage <-  brd_timeseries %>% collect() %>% mutate(date=date(date)) %>%
  group_by(id) %>% arrange(id,-as.numeric(date)) %>%
  filter(row_number()<=7) %>%
  summarise(Faelle_letzte_7_Tage=first(cases)-last(cases)) %>%
  mutate(Faelle_letzte_7_Tage_pro_Tag=round(Faelle_letzte_7_Tage/7))
ausgangsdaten <- aktuell  %>%
  select(id,name,ICU_Betten,Einwohner,ebene,
         cases,R0) %>% filter(ebene!="Staaten" & !is.na(ebene)) %>% select(-ebene) %>%
  left_join(.,letzte_7_tage,by="id") %>%
  mutate(Faelle_letzte_7_Tage_je100TsdEinw=round(Faelle_letzte_7_Tage/(Einwohner/100000)),
         Faelle_letzte_7_Tage_je100TsdEinw=ifelse(Faelle_letzte_7_Tage_je100TsdEinw<0,NA,Faelle_letzte_7_Tage_je100TsdEinw))


# Funktionen
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


# Funktion zur Vorwarnzeit bei festem Rt
vorwarnzeit_berechnen <- function(ngesamt,cases,faelle,Kapazitaet,Rt=1.3){
  gamma=1/10
  infected=faelle/gamma
  recovered= cases-infected
  mysir <- sirmodel(ngesamt = ngesamt,
                    S = ngesamt - infected - recovered,
                    I = infected,
                    R = recovered,
                    R0 = Rt,
                    gamma = gamma,
                    horizont = 365) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  myresult <- NA
  myresult <- mysir %>% mutate(Tage=row_number()-1) %>% filter(Neue_Faelle>=Kapazitaet) %>% head(1) %>% pull(Tage)
  return(myresult)
}


# Ergebnis
vorwarnzeitergebnis <- ausgangsdaten %>%
  mutate(Handlungsgrenze_7_tage=50*(Einwohner/100000),
         Handlungsgrenze_pro_Tag=round(Handlungsgrenze_7_tage/7),
         R0 = ifelse((R0>1) & (Faelle_letzte_7_Tage_pro_Tag==0),NA,R0),
         Kapazitaet=ICU_Betten*0.25/share_icu/icu_days,
         Auslastung_durch_Grenze=round(100*(Handlungsgrenze_pro_Tag/Kapazitaet)))

myTage <- vorwarnzeitergebnis %>% rowwise() %>%
  do(Tage = vorwarnzeit_berechnen(.$Einwohner, .$cases,.$Faelle_letzte_7_Tage_pro_Tag,.$Kapazitaet,1.3)) %>%
  unnest(cols = c(Tage), keep_empty=TRUE)
vorwarnzeitergebnis <- vorwarnzeitergebnis %>%
  mutate(Vorwarnzeit = myTage$Tage, Vorwarnzeit_effektiv=Vorwarnzeit-21)


# Plots
Auslastungsplot<- ggplot(vorwarnzeitergebnis %>% filter(id<17),
                         aes(x=forcats::fct_reorder(name,Auslastung_durch_Grenze),y=Auslastung_durch_Grenze,
                             fill=name=="Gesamt")) +
  geom_bar(stat="identity",show.legend = F) + coord_flip() +
  scale_fill_zi() + labs(subtitle="Auslastung ICU",x="",y="") +
  theme_zi() +
  geom_text(aes(y=5,label=paste0(round(Auslastung_durch_Grenze),"%")),size=2.5,color="white") +
  scale_y_continuous(breaks=seq(0,60,20), limits=c(0,65),
                     labels =  function(x) paste0(x,"%"))
Vorwarnzeitplot <- ggplot(vorwarnzeitergebnis %>% filter(id<17),aes(x=forcats::fct_reorder(name,Auslastung_durch_Grenze),y=Vorwarnzeit)) +
  geom_bar(stat="identity",show.legend = F,fill=zi_cols("ziorange")) + coord_flip() +
  geom_hline(yintercept = 0,color="black") +
  geom_text(aes(y=5,label=paste(Vorwarnzeit,"Tage")),size=2.5,color="white") +
  labs(subtitle="Vorwarnzeit ab Interventionsgrenze",x="",y="") + theme_zi() +
  scale_y_continuous(breaks=seq(0,50,5) #, labels =  function(x) paste0(x," Tage")
  )

# Theoretische Vorwarnzeit
make_theoretischedaten <- function(myid=0) {
fall <- vorwarnzeitergebnis %>% filter(id==myid)
Rt <- seq(1.1, 2, 0.1)
Vorwarnzeit <- rep(0, length(Rt))
Anstieg <- rep(0, length(Rt))
Reaktionszeit <- 21
Belastungsgrenze <- fall$Kapazitaet
gamma=1/10
for (i in seq(Rt)) {
  mysir <- sirmodel(ngesamt = fall$Einwohner,
                    S = fall$Einwohner - fall$cases,
                    I = (fall$Faelle_letzte_7_Tage_pro_Tag)/gamma,
                    R = fall$cases - (fall$Faelle_letzte_7_Tage_pro_Tag)/.1,
                    R0 = Rt[i],
                    gamma =gamma,
                    horizont = 365) %>% mutate(Neue_Faelle=I-lag(I)+R-lag(R))
  Vorwarnzeit[i] <- which.max(mysir$Neue_Faelle>Belastungsgrenze)
}
as_tibble(cbind(Rt,Vorwarnzeit)) %>%
  mutate(id=myid,Effektive_Vorwarnzeit=Vorwarnzeit-Reaktionszeit) %>%
  select(id,Rt,Vorwarnzeit,Effektive_Vorwarnzeit)
}

mycolorbreaks <- c(14,30,90)
plotdata_Anstieg <- make_theoretischedaten(myid=0) %>% select(Rt,Vorwarnzeit,"Effektive Vorwarnzeit"=Effektive_Vorwarnzeit) %>% filter(`Effektive Vorwarnzeit`>=0)  %>% gather(Merkmal,Wert,2:3)
plot_Anstiegtheor <- ggplot(plotdata_Anstieg, aes(x=Rt, y=Wert,color=Merkmal)) +
  geom_line(size=1.5, show.legend = F)+
  geom_point(size=3, show.legend = F)+
  geom_hline(yintercept = 0) +
  theme_minimal() + scale_color_zi() +
  scale_x_continuous(labels =  function(x) paste0(format(x,decimal.mark = ",")),breaks=seq(1.1, 2, 0.1))  +
  labs(y=paste0("Vorwarnzeit in Tagen"))+
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank())



rki_fallzahl_bl <- function(){
  df <- vorwarnzeitergebnis %>% filter(id<17) %>% mutate(cases_je_100Tsd=round(cases/(Einwohner/100000)),
                                                         R0=round(R0,digits = 2))
  df %>% select(Bundesland=name,
                "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
                "Neue Fälle je 100 Tsd. Einw. in 7 Tagen"=Faelle_letzte_7_Tage_je100TsdEinw,
                "Fälle insgesamt"=cases,
                "Fälle je 100 Tsd. Einw."=cases_je_100Tsd,
                "R(t)"=R0,
                "Effektive Vorwarnzeit aktuell"=Vorwarnzeit_effektiv)
}

rki_fallzahl_kreis <- function(){
  df <- vorwarnzeitergebnis %>% filter(id>17 | name=="Berlin") %>% mutate(cases_je_100Tsd=round(cases/(Einwohner/100000)),
                                                         R0=round(R0,digits = 2))
  bundeslaender <- aktuell %>% filter(id>0 & id<17) %>% select(blid=id,Bundesland=name)
  df <- df %>% mutate(blid=ifelse(id>17,floor(id/1000000),id)) %>% left_join(.,bundeslaender) %>% arrange(blid,id)
  df %>% select(Kreis=name,
                Bundesland,
                "Neue Fälle pro Tag"=Faelle_letzte_7_Tage_pro_Tag,
                "Neue Fälle je 100 Tsd. Einw. in 7 Tagen"=Faelle_letzte_7_Tage_je100TsdEinw,
                "Fälle insgesamt"=cases,
                "Fälle je 100 Tsd. Einw."=cases_je_100Tsd,
                "R(t)"=R0,
                "Vorwarnzeit aktuell lokal*"=Vorwarnzeit # needs communication
                )
}


