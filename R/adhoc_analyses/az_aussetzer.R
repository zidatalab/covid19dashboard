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

rki_vacc <- tryCatch(
  {
    mytemp <- tempfile()
    rki_vacc_data <- "https://raw.githubusercontent.com/n0rdlicht/rki-vaccination-scraper/main/data/de-vaccinations.csv"
    download.file(rki_vacc_data, mytemp, method = "curl")
    vacc_zahlen <- read_csv(mytemp)
    if (dim(vacc_zahlen)[2] != 8){
      stop("they changed the vacc table")
    } else {
      write_csv(vacc_zahlen, "./data/vacc_zahlen.csv")
      vacc_zahlen
    }
  },
  error=function(e) {
    # read old data
    vacc_zahlen <- read_csv("./data/vacc_zahlen.csv")
    return(vacc_zahlen)
  }
)

mindate <- min(rki_vacc$date)
maxdate <- max(rki_vacc$date)
all_dates <- seq(mindate, maxdate, by=1)

rkisum <- rki_vacc %>% 
  filter(key%in%c("sum_initial_biontech", "sum_initial_moderna", "sum_initial_astrazeneca",
                  "sum_booster_biontech", "sum_booster_moderna", "sum_booster_astrazeneca") &
           geo=="Germany") %>%
  
  pivot_wider(id_cols="date", names_from = "key", values_from = "value", values_fill = 0) %>%
  mutate(biontech=sum_initial_biontech+sum_booster_biontech,
         moderna=sum_initial_moderna+sum_booster_moderna,
         astrazeneca=sum_initial_astrazeneca+sum_booster_astrazeneca) %>%
  pivot_longer(cols = c(biontech, moderna, astrazeneca), names_to = "impfstoff", values_to = "impfungen") %>%
  # filter(date>=min(date)+days(1)) %>%
  select(date, impfstoff, impfungen)

rkilagged <- rki_vacc %>% 
  filter(key%in%c("sum_initial_biontech", "sum_initial_moderna", "sum_initial_astrazeneca",
                  "sum_booster_biontech", "sum_booster_moderna", "sum_booster_astrazeneca") &
           geo=="Germany") %>%
  
  pivot_wider(id_cols="date", names_from = "key", values_from = "value", values_fill = 0) %>%
  mutate(biontech=sum_initial_biontech+sum_booster_biontech - (lag(sum_initial_biontech, default=0)+lag(sum_booster_biontech, default=0)),
         moderna=sum_initial_moderna+sum_booster_moderna - (lag(sum_initial_moderna, default=0)+lag(sum_booster_moderna, default=0)),
         astrazeneca=sum_initial_astrazeneca+sum_booster_astrazeneca - (lag(sum_initial_astrazeneca,default=0)+lag(sum_booster_astrazeneca,default=0))) %>%
  pivot_longer(cols = c(biontech, moderna, astrazeneca), names_to = "impfstoff", values_to = "impfungen") %>%
  filter(date>=min(date)+days(1)) %>%
  select(date, impfstoff, impfungen)

aussetzer_plot <- ggplot(rkisum, aes(x=date, y=impfungen, col=impfstoff)) +
  geom_line()
aussetzer_plot

