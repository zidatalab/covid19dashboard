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
  select(Datum, `Erst-Praxen`) %>% 
  mutate(Datum=as_date(Datum, origin = as_date("1899-12-30"))) %>% 
  mutate(stschnitt=rollmean(`Erst-Praxen`, 7, fill = 0, align = "left")) %>% 
  filter(Datum>="2021-11-15") %>% 
  select(Datum, stschnitt)

stschnitt_plot <- ggplot(stschnitt,
                         aes(x=Datum, y=stschnitt, label=round(stschnitt))) + #
  geom_bar(stat="identity", fill=zi_cols("ziblue")) +
  coord_flip()+
  # geom_line(col=zi_cols("ziblue"), size=3) +
  scale_y_continuous(labels=scales::comma_format(decimal.mark = ",", 
                                                 big.mark = "."),
                     limits=c(0, NA)) +
  theme_zi() +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = "Anstieg der Erstimpfungen in ärztlichen Praxen",
       subtitle = "Sieben-Tages-Durchschnitt der Erstimpfungen (Nov. 2021)") +
  geom_text(size = 3, hjust=1.1, col="white") +
  scale_x_date(#date_breaks="days", 
               breaks=as_date(min(stschnitt$Datum):max(stschnitt$Datum)),
               date_labels = "%d.%m.")  # , limits = as_date(c(min(stschnitt$Datum), max(stschnitt$Datum)))
stschnitt_plot  

finalise_plot(stschnitt_plot,
              save_filepath = "data/siebentageschnitterstimpfungenpraxen.png",
              source_name = "Datenbasis: KBV, Berechnungen: Zi",
              width_cm = 16, height_cm = 16*3/4 )
