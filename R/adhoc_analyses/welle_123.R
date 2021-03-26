##### Packages
library(DT)
library(DBI)
library(forcats)
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

##### Source files
source("R/aux_functions.R")

##### Connect to DB
# conn <- dbConnect(RSQLite::SQLite(), "../covid-19/data/covid19db.sqlite")
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

rki <- tbl(conn,"rki") %>% collect()
brd_timeseries <- tbl(conn,"brd_timeseries") %>% collect() %>% mutate(date=base::as.Date(date))

wellen_data <- rki %>%
  mutate(Datum=as_date(Meldedatum)) %>% 
  filter(Datum<max(Datum)) %>% 
  group_by(Datum) %>%
  summarise(dailycases=sum(AnzahlFall[NeuerFall>=0]), .groups = "drop") %>% 
  mutate(dailycases_rm=round(zoo::rollmean(dailycases, 7, fill=NA, align="right")),
         dailycases_rm=ifelse(is.na(dailycases_rm), dailycases, dailycases_rm)) %>% 
  mutate(anstieg=dailycases_rm/lag(dailycases_rm, 1, 1),
         daycount=seq(length(unique(Datum))),
         anstiege=case_when(
           Datum>="2020-03-01" & Datum<="2020-03-27" ~ "Welle 1",
           Datum>="2020-09-25" & Datum<="2020-11-07" ~ "Welle 2",
           Datum>="2021-03-05" & Datum<=max(Datum) ~ "Welle 3",
           TRUE ~ ""
         )) # %>% 
  # filter(Datum>="2020-03-07" & Datum<="2020-03-21") # %>%
  # filter(Datum>="2020-10-01") # %>%
  # filter(Datum>="2021-03-05") # %>%
  
ggplot(wellen_data,
       aes(x=Datum, y=dailycases_rm, color=anstiege)) +
  geom_line(aes(group=NA))

mult_lm_1 <- lm(log(dailycases_rm) ~ 0 + daycount, data=wellen_data %>%
                  filter(anstiege=="Welle 1") %>% 
                  mutate(daycount=daycount-min(daycount),
                         dailycases_rm=dailycases_rm-min(dailycases_rm)+1))
coef(mult_lm_1)

mult_lm_2 <- lm(log(dailycases_rm) ~ 0 + daycount, data=wellen_data %>%
                  filter(anstiege=="Welle 2") %>% 
                  mutate(daycount=daycount-min(daycount),
                         dailycases_rm=dailycases_rm-min(dailycases_rm)+1))
coef(mult_lm_2)

mult_lm_3 <- lm(log(dailycases_rm) ~ 0 + daycount, data=wellen_data %>%
                  filter(anstiege=="Welle 3") %>% 
                  mutate(daycount=daycount-min(daycount),
                         dailycases_rm=dailycases_rm-min(dailycases_rm)+1))
coef(mult_lm_3)


# inflection points?
library(inflection)

findiplist(wellen_data$daycount[1:100], wellen_data$dailycases_rm[1:100], 0)
findiplist(wellen_data$daycount[1:200], wellen_data$dailycases_rm[1:200], 0)
