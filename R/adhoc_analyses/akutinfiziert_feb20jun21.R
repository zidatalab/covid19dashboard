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

##### Source files
source("R/aux_functions.R")

## barmer
infektperiode <- 8
icu_days <- 21
infekt2icudays <- 14

##### Connect to DB
# conn <- dbConnect(RSQLite::SQLite(), "../covid-19/data/covid19db.sqlite")
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

##### get data
## get data from db
brd_timeseries <- tbl(conn,"brd_timeseries") %>% 
  collect() %>% 
  mutate(date=base::as.Date(date))

## akut infizierte (X Tage infektiös nach Meldung)
akutinfiziert_data <- brd_timeseries %>% filter(id==0) %>%
  mutate(cases_rm=floor(zoo::rollmean(cases, 7, fill=NA)),
         cases=ifelse(is.na(cases_rm), cases, cases_rm),
         Infected=cases-lag(cases, infektperiode)) %>% #
  filter(!is.na(Infected)) %>%
  mutate(date=date(date)) %>%
  # filter(date>=date("2020-03-02")) %>%
  select(date, Infected)

akutinfiziert_plot <- ggplot(akutinfiziert_data %>% 
                               filter(date>="2020-02-01" & date<="2021-06-30"),
                             aes(x=date, y=Infected,group=1)) +
  geom_area(fill="#0086C530") +
  geom_hline(aes(yintercept=0), color="black", linetype ="solid") +
  geom_line(size=2, show.legend = F, color=zi_cols("ziblue")) +
  scale_color_manual(values = c("#B1C800","#E49900" ,"darkred")) +
  theme_minimal() +
  scale_x_date(breaks = "3 months",date_labels = "%d.%m.%y") +
  labs(y="Anzahl akut infiziert",x = "Datum") +
  theme(panel.grid.major.x =element_blank(), panel.grid.minor.x=element_blank()) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark=",", scientific = FALSE))
akutinfiziert_plot
