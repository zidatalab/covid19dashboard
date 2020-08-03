# Make Results for Method Paper
loadfonts(device="win", quiet = TRUE)
source("R/functions.R", encoding = "UTF-8")


# Infected according to RKI


# Plot 1 Akut infizierte
plot_1 <- akutinfiziert +  theme_zi() +
  labs(title="Entwicklung der COVID-19-Pandemie in Deutschland",
       subtitle="Aktuelle Zahl akut COVID-19 Infizierter") +
  theme(axis.title.x = element_blank())
plot_1 

zicolors::finalise_plot(plot_1,height=15,width=3*9,
                        save_filepath="./data/methodpaperresults/plot1.png",
                        source_name = paste0("Datenbasis: Robert Koch-Institut (RKI), Stand ",
                                             format(now(),format="%d.%m.%Y"), ", Fälle wurden geglättet (7-tages-Mittelwert), Annahme: Infektionsdauer 10 Tage + asymptomatische Tage."))

# Kapazitätsrechnungen
no_aktuell_ICU <- aktuell %>% filter(id==0) %>% pull(ICU_Betten)  * .25
paste("Verfügbare COVID-Betten", round(no_aktuell_ICU))
no_akutell_kapazitaet <- no_aktuell_ICU/share_icu
paste("Fallkazaitaet", round(no_akutell_kapazitaet))
no_akutell_kapazitaet_pro_tag <- no_akutell_kapazitaet/icu_days
paste("Fallkazaitaet neue Faelle", round(no_akutell_kapazitaet_pro_tag))
no_aktuell_anteil_ambulant <- 6/7
bedarf_ambulant_ersteinschaetzung <- .8*no_akutell_kapazitaet*no_aktuell_anteil_ambulant
bedarf_ambulant_ersteinschaetzung_risiken <- .2*no_akutell_kapazitaet*no_aktuell_anteil_ambulant
aerzte_pro_tag <- (no_akutell_kapazitaet*no_aktuell_anteil_ambulant *.2) / 7

# Plot 2 Trend Reproduktionszahl
plot_2 <- ggplot(blmitidata %>% filter(id==0 & Merkmal=="Fälle" & date>=date("2020-03-01")) %>% rename(R=R_Mean) %>% mutate(R=round(R,digits = 1)),
                   aes(x=date,y=R,group=name,color=name=="Gesamt",
                       text=paste("Region: ",name,"<br>"))) +
    geom_hline(yintercept = 1) +
    geom_line(data = . %>% filter(name=="Gesamt"),size=2,show.legend = F, color=zi_cols("ziblue"))+
    scale_color_zi()  +
    theme_zi() + scale_x_date(date_labels = "%d.%m.", breaks="2 weeks") +
    geom_vline(aes(xintercept=date("2020-03-16")),color="grey") +
    geom_vline(aes(xintercept=date("2020-03-22")),color="grey") +
    geom_vline(aes(xintercept=date("2020-04-17")),color="grey") +
    annotate("text", x = date("2020-03-16"), y = 3.3, label = "Schulschließungen",color="black",size=3) +
    annotate("text", x = date("2020-03-22"), y = 2.5, label = "Kontakteinschränkungen",color="black",size=3) +
    annotate("text", x = date("2020-04-17"), y = 2.0, label = "Lockerung der \nMaßnahmen",color="black",size=3) +
    theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank()) +
    labs(title="Ausbreitungsgeschwindigkeit von COVID-19 in Deutschland",
         subtitle = "Reproduktionszahl Rt nach Datum")
    
plot_2 
zicolors::finalise_plot(plot_2,height=15,width=3*9,
                        save_filepath="./data/methodpaperresults/plot2.png",
                        source_name = paste0("Datenbasis: Robert Koch-Institut (RKI), Stand ",
                                             format(now(),format="%d.%m.%Y")))

# Plot 3 
plot_3 <- ggplot(zivwz_vs_rkir_verlauf %>% filter(Variable=="Vorwarnzeit"),aes(x=date,y=Wert,color="Wert"))+ geom_line(size=2.5, show.legend = F) + theme_zi() + scale_color_zi() + scale_x_date(date_labels = "%d.%m.", breaks="2 weeks") +
  geom_vline(aes(xintercept=date("2020-03-16")),color="grey") +
  geom_vline(aes(xintercept=date("2020-03-22")),color="grey") +
  geom_vline(aes(xintercept=date("2020-04-17")),color="grey") +
  annotate("text", x = date("2020-03-16"), y = 90, label = "Schulschließungen\n16.3.",color="black",size=3) +
  annotate("text", x = date("2020-03-22"), y = 60, label = "Kontakteinschränkungen\n22.3.",color="black",size=3) +
  annotate("text", x = date("2020-04-17"), y = 90, label = "Lockerung der \nMaßnahmen\n17.4.",color="black",size=3) +
  theme(panel.grid.major.x =   element_blank(),panel.grid.minor.x =   element_blank()) + 
  geom_hline(yintercept = 0) + scale_y_continuous(limits=c(0,120)) + 
  labs(title="Zeitliche Entwicklung der Vorwarnzeit in Deutschland",subtitle = "Effektive Vorwarnzeit bis Kapazitätsgrenze in Tagen ab Stichtag")
plot_3
zicolors::finalise_plot(plot_3,height=15,width=3*9,
                        save_filepath="./data/methodpaperresults/plot3.png",
                        source_name = paste0("Datenbasis: Robert Koch-Institut (RKI), Stand ",
                                             format(now(),format="%d.%m.%Y"),", Berechnungen des Zi auf Basis von Daten des RKI und des DIVI-Intensivregisters."))
