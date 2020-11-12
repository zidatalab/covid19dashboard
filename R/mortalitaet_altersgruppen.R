# Packages
library(DT)
library(DBI)
library(forcats)
library(EpiEstim)
library(plotly)
library(zicolors)
library(deSolve)
library(jsonlite)
library(readxl)
library(data.table)
library(dplyr)
library(glue)
library(lubridate)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(dtplyr)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

rki <- tbl(conn,"rki") %>% collect()

maxweek <- 43

mort_ag <- rki %>%
  mutate(Meldedatum=as_date(Meldedatum)) %>%
  mutate(Altersgruppe=ifelse(Altersgruppe%in%c("A00-A04", "A05-A14", "A15-A34", "A35-A59"),
                             "0-59 J.", Altersgruppe)) %>%
  mutate(Altersgruppe=ifelse(Altersgruppe=="A80+",
                             "80+ J.", ifelse(Altersgruppe=="A60-A79", 
                                                  "60-79 J.", Altersgruppe))) %>%
  group_by(Kalenderwoche=isoweek(Meldedatum), Altersgruppe) %>%
  summarize(Falltotal=sum(AnzahlFall),
            Todtotal=sum(AnzahlTodesfall),
            .groups="drop") %>%
  mutate(Todtotal=ifelse(Todtotal<0, 0, Todtotal)) %>%
  mutate(`Mortalität`=Todtotal/Falltotal) %>%
  filter(Altersgruppe!="unbekannt" & Kalenderwoche<=maxweek & Kalenderwoche >=10)
mort_ag_plot <- ggplot(mort_ag,
                       aes(x=Kalenderwoche, y=`Mortalität`*100, color=Altersgruppe)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  theme_zi_titels() +
  theme(#axis.title.y = element_blank(),
        legend.position="top",
        legend.title = element_blank()) +
  scale_color_zi() +
  labs(title="Mortalität der COVID-19-Fälle",
       # subtitle = "Anteil Todesfälle in Prozent",
       y="Mortalität in %", 
       x="Kalenderwoche")
  # scale_color_manual(values = c("#CCE7F3", "#0086C5", "#006596"), name = "Altersgruppe")
mort_ag_plot

zicolors::finalise_plot(mort_ag_plot,height=12,width=16,
                        save_filepath="../mortalitaet_altersgruppen.png",
                        source_name = paste0("Datenbasis: Robert Koch-Institut (RKI), Stand ",
                                             format(now(),format="%d.%m.%Y")))


write_csv(mort_ag, "data/mortalitaet_altersgruppen.csv")
