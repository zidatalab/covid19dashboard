library(readr)
library(tidyverse)
library(lubridate)
library(zicolors)
library(extrafont)
library(zoo)
owid_covid_data <- read_csv("data/owid-covid-data.csv")
owid_currenticu_data <- read_csv("data/current-covid-patients-icu.csv")
owid_hospadm_data <- read_csv("data/weekly-hospital-admissions-covid.csv")

case_data <- owid_covid_data %>% 
  filter(location=="United Kingdom" | location=="Germany") %>% 
  select(date, population,
         location,
         contains("new_deaths"),
         contains("new_cases"))

currenticu_data <- owid_currenticu_data %>% 
  filter(Entity=="United Kingdom" | Entity=="Germany")

gesamt <- case_data %>% 
  select(date, new_deaths, new_cases, location) %>% 
  left_join(currenticu_data %>% 
              select(Day, `Daily ICU occupancy`, Entity),
            by=c("date"="Day", "location"="Entity"))

gesamt_wona <- gesamt %>% 
  drop_na()

# ccf(gesamt_wona$new_deaths, gesamt_wona$new_cases)
# ccf(gesamt_wona$`Daily hospital occupancy`, gesamt_wona$new_cases)
# ccf(gesamt_wona$`Daily ICU occupancy`, gesamt_wona$new_cases)

gesamt_long <- gesamt %>% 
  group_by(location) %>% 
  mutate(new_cases=rollmean(new_cases, 7, align="right", fill=NA),
         new_deaths=rollmean(new_deaths, 7, align="right", fill=NA),
         new_cases_14=lag(new_cases, 14)) %>% 
  pivot_longer(cols=c(new_cases, new_deaths, `Daily ICU occupancy`)) %>% 
  # mutate(rate_to_cases14=log(value/new_cases_14))
  mutate(rate_to_cases14=value/new_cases_14) %>% 
  filter(date!="2020-07-16" & date!="2020-07-15" &
           date>="2020-08-01")

plot <- ggplot(gesamt_long,
                  aes(x=date, y=value, col=location)) +
  geom_line() +
  ylim(0, NA) +
  facet_wrap(~name, ncol=3, scales = "free_y") +
  theme_zi()
plot

plot_rate14 <- ggplot(gesamt_long,
                  aes(x=date, y=rate_to_cases14, col=location)) +
  geom_line() +
  ylim(0, NA) +
  facet_wrap(~name, ncol=3, scales = "free_y") +
  theme_zi()
plot_rate14
