##### Packages
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
library(zoo)
require(ISOcodes)
library(openxlsx)

impfungen_tc <- read.xlsx("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_dritt_bl.xlsx")

stschnitt <- impfungen_tc %>% 
  filter(Bundesland=="Gesamt") %>% 
  select(Datum, `Erst-Gesamt`) %>% 
  mutate(Datum=as_date(Datum, origin = as_date("1899-12-30"))) %>% 
  mutate(stschnitt=rollmean(`Erst-Gesamt`, 7, fill = 0, align = "left")) %>% 
  filter(Datum>="2021-11-01") %>% 
  select(Datum, stschnitt)

stschnitt_plot <- ggplot(stschnitt,
                         aes(x=Datum, y=stschnitt)) + #
  geom_line() +
  # coord_flip()+
  # geom_line(col=zi_cols("ziblue"), size=3) +
  scale_y_continuous(labels=scales::comma_format(decimal.mark = ",", 
                                                 big.mark = "."),
                     limits=c(0, NA)) +
  theme_zi() +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = "Entwicklung der Erstimpfungen",
       subtitle = "Sieben-Tages-Durchschnitt der Erstimpfungen (Nov. 2021)") +
  scale_x_date(date_breaks="7 days", 
               # breaks=as_date(min(stschnitt$Datum):max(stschnitt$Datum)),
               date_labels = "%d.%m.")  # , limits = as_date(c(min(stschnitt$Datum), max(stschnitt$Datum)))
stschnitt_plot  

# finalise_plot(stschnitt_plot,
#               save_filepath = "data/siebentageschnitterstimpfungenpraxen.png",
#               source_name = "Datenbasis: KBV, Berechnungen: Zi",
#               width_cm = 16, height_cm = 16*3/4 )

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_rki_age <- tbl(conn, "kbv_rki_altersgruppen_kreise") %>%
  collect()

erstimpfungen <- kbv_rki_age %>% 
  group_by(vacc_date, vacc_series, Altersgruppe) %>% 
  summarise(anzahl=sum(anzahl_alleorte, na.rm=TRUE),
            .groups="drop")  %>% 
  group_by(vacc_series, Altersgruppe) %>% 
  arrange(rev(vacc_date)) %>% 
  mutate(stschnitt=rollmean(`anzahl`, 7, fill = 0, align = "left")) %>% 
  ungroup() %>% 
  filter(vacc_date>="2021-11-01") %>% 
  select(vacc_date, vacc_series, Altersgruppe, stschnitt)
  
sts_erstimpfungen_plot <- ggplot(erstimpfungen %>% 
                                   filter(vacc_series==1) %>% 
                                   mutate(Altersgruppe=factor(
                                     Altersgruppe, 
                                     levels=rev(c("60+", "18-59", "12-17", "5-11")),
                                     ordered=TRUE
                                   )),
                         aes(x=vacc_date, y=stschnitt, fill=Altersgruppe)) + #
  geom_area(stat="identity", position="stack") +
  # coord_flip()+
  # geom_line(col=zi_cols("ziblue"), size=3) +
  scale_y_continuous(labels=scales::comma_format(decimal.mark = ",", 
                                                 big.mark = "."),
                     limits=c(0, NA)) +
  theme_zi() +
  # scale_fill_zi() +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = "Entwicklung der Erstimpfungen",
       subtitle = "Sieben-Tages-Durchschnitt der Erstimpfungen") +
  scale_x_date(date_breaks="7 days", 
               # breaks=as_date(min(stschnitt$Datum):max(stschnitt$Datum)),
               date_labels = "%d.%m.")  # , limits = as_date(c(min(stschnitt$Datum), max(stschnitt$Datum)))
sts_erstimpfungen_plot  
