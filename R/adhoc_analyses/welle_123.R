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
           Datum>="2021-03-05" & Datum<=max(Datum)-4 ~ "Welle 3",
           TRUE ~ ""
         )) # %>% 
  # filter(Datum>="2020-03-07" & Datum<="2020-03-21") # %>%
  # filter(Datum>="2020-10-01") # %>%
  # filter(Datum>="2021-03-05") # %>%
  
# Fälle
wellen_data %>% filter(anstiege!="") %>% group_by(anstiege) %>% arrange(anstiege,Datum) %>% mutate(Wellentag=row_number())%>%
  ggplot(aes(x=Wellentag,y=dailycases_rm,color=anstiege)) + geom_line(size=3) + 
  theme_minimal() + labs(y="Fälle pro Tag",color="Welle") +
  theme(legend.position = "bottom")

ggsave("~/Desktop/plot_1.png", width=20,height = 20*9/16,dpi=300,units="cm")

# Anstieg
wellen_data %>% filter(anstiege!="") %>% group_by(anstiege) %>% arrange(anstiege,Datum) %>% mutate(Wellentag=row_number())%>%
  ggplot(aes(x=Wellentag,y=100*(anstieg-1),color=anstiege)) + geom_line(size=3) + 
  theme_minimal() + labs(y="Anstieg zum Vortag in % (log Skala)",color="Welle") +
  theme(legend.position = "bottom") + scale_y_log10()

ggsave("~/Desktop/plot_2.png", width=20,height = 20*9/16,dpi=300,units="cm")


# Regression log(Faelle)
larsmodell <- wellen_data %>% filter(anstiege!="") %>% group_by(anstiege) %>% 
  arrange(anstiege,Datum) %>% mutate(Wellentag=row_number()) %>% 
  lm(log(dailycases_rm) ~ 1 + Wellentag  *I(anstiege), data=. )

# Plot vorhergesagte Fälle
tibble(expand.grid(Wellentag=seq(1,30,1),anstiege=unique(wellen_data %>% filter(anstiege!="") %>% 
                                                           count(anstiege) %>% pull(anstiege)))) %>% 
  mutate(yhat=exp(predict(larsmodell,newdata = .))) %>% 
  ggplot(aes(x=Wellentag,y=yhat,color=anstiege)) + geom_line(size=3) + 
  theme_minimal() + labs(y="Vorhergesagte Fälle pro Tag",color="Welle") +
  theme(legend.position = "bottom")

ggsave("~/Desktop/plot_3.png", width=20,height = 20*9/16,dpi=300,units="cm")