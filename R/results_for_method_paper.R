# Make Results for Method Paper
loadfonts(device="win", quiet = TRUE)
source("R/functions.R", encoding = "UTF-8")


# Infected according to RKI


# Plot 1 
plot_1 <- akutinfiziert +  theme_zi() +
  labs(title="Entwicklung der COVID-19-Pandemie in Deutschland",
       subtitle="Aktuelle Zahl akut COVID-19 Infizierter") +
  theme(axis.title.x = element_blank())
plot_1 

zicolors::finalise_plot(plot_1,height=15,width=3*9,
                        save_filepath="./data/methodpaperresults/plot1.png",
                        source_name = paste0("Datenbasis: Robert Koch-Institut (RKI), Stand ",
                                             format(now(),format="%d.%m.%Y"), ", Fälle wurden geglättet (7-tages-Mittelwert), Annahme: Infektionsdauer 10 Tage + asymptomatische Tage."))

