library(tidyverse)
library(lubridate)
library(sf)
library(zicolors)
library(cowplot)
library(readxl)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
deaths <- tbl(conn,"brd_timeseries") %>% filter(id==0) %>% collect()

mindate <- as_date("2020-10-01")

COVID_Verstorbene36_insg <- read_excel("data/COVID_Verstorbene36_insg.xlsx")

plotdata <-COVID_Verstorbene36_insg %>% 
  filter(!is.na(Verstorben_insgesamt)) %>% 
  mutate(date=as_date(as_date(as.numeric(Tag)-1)- 
                        dyears(70)),
         Verstorben_36=as.numeric(Verstorben_36),
         Anteil_an_Verstorben=100*(Verstorben_36/Verstorben_insgesamt)) %>% 
  select(date,contains('Verstorben'))  %>% 
  filter(!is.na(Verstorben_36))

ziplot <- ggplot(plotdata,aes(x=date,
                             y=Anteil_an_Verstorben,color="Anteil") )+ 
  geom_line(show.legend = FALSE, size=2) + 
  geom_point(show.legend = FALSE, size=3.5) + 
  geom_hline(yintercept = 0)+
  scale_color_zi() + theme_zi() + 
  scale_y_continuous(limits=c(0,50))+
  scale_x_date(breaks="7 days", date_labels = "%d.%m.") + 
  labs(title="Anteil der Pflegeheimbewohner an COVID-19-Toten sinkt leicht",
       subtitle="Todesfälle Bewohner von Einr. n. § 36 IfSG an allen Todesfällen in %")

finalise_plot(ziplot, save_filepath = "static/pflegeheimbewohner_an_toten.png",
              width_cm = 16, height_cm = 9,
              source_name = "Datenbasis: Situationsberichte des RKI 1.10.-3.12.")