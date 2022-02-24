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

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_age <- tbl(conn,"kbv_rki_altersgruppen_kreise") %>%
  collect()

kbv_impfstoff <- tbl(conn,"kbv_rki_impfstoffe_laender") %>%
  collect()

(98209*6+12354*20+236*5+3823*10)/((1614+98209+13951)*6+(87+12354+4938)*20+(2+236+95)*5+(3823+1000)*10)
(13951*6+4938*20+95*5+1000*10)/((1614+98209+13951)*6+(87+12354+4938)*20+(2+236+95)*5+(3823+1000)*10)
(1614*6+87*20+2*5)/((1614+98209+13951)*6+(87+12354+4938)*20+(2+236+95)*5+(3823+1000)*10)

lastday <- kbv_impfstoff %>% 
  filter(vacc_date==max(vacc_date)) %>% 
  group_by(vacc_series) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
            .groups="drop")

seit10feb <- kbv_impfstoff %>% 
  filter(vacc_date>="2022-02-10") %>% 
  group_by(vacc_series) %>% 
  summarise(anzahl_praxen=sum(anzahl_praxen, na.rm=TRUE),
            .groups="drop")

impfungen_tc <- read.xlsx("https://ziwebstorage.blob.core.windows.net/publicdata/zeitreihe_impfungen_aerzte_dritt_bl.xlsx")

stschnitt <- impfungen_tc %>% 
  filter(Bundesland=="Gesamt") %>% 
  # select(Datum, `Erst-Gesamt`) %>% 
  mutate(Datum=as_date(Datum, origin = as_date("1899-12-30")),
         "Gesamt-Praxen"=`Erst-Praxen`+`Zweit-Praxen`+`Dritt-Praxen`+`Viert-Praxen`) %>% 
  mutate(across(contains("Praxen"), ~rollmean(.x, 7, fill = 0, align = "left"))) %>% 
  filter(Datum>="2021-11-01") %>% 
  select(Datum, contains("Praxen"))

impfungen_tc <- impfungen_tc %>% 
  filter(Bundesland=="Gesamt") %>% 
  # select(Datum, `Erst-Gesamt`) %>% 
  mutate(Datum=as_date(Datum, origin = as_date("1899-12-30")),
         "Gesamt-Praxen"=`Erst-Praxen`+`Zweit-Praxen`+`Dritt-Praxen`+`Viert-Praxen`) %>% 
  filter(Datum>="2021-11-01") #%>% 
  # select(Datum, contains("Praxen"))
  
