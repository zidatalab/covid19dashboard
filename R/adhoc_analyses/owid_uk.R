library(readr)
library(tidyverse)
library(lubridate)
library(zicolors)
library(extrafont)
owid_covid_data <- read_csv("data/owid-covid-data.csv")
owid_currenthosp_data <- read_csv("data/current-covid-patients-hospital.csv")
owid_hospadm_data <- read_csv("data/weekly-hospital-admissions-covid.csv")
owid_currenticu_data <- read_csv("data/current-covid-patients-icu.csv")


uk_case_data <- owid_covid_data %>% 
  filter(location=="United Kingdom") %>% 
  select(date, population, 
         contains("new_deaths"),
         contains("new_cases"))

uk_currenthosp_data <- owid_currenthosp_data %>% 
  filter(Entity=="United Kingdom")
uk_hospadm_data <- owid_hospadm_data %>% 
  filter(Entity=="United Kingdom")
uk_currenticu_data <- owid_currenticu_data %>% 
  filter(Entity=="United Kingdom")

uk_gesamt <- uk_case_data %>% 
  select(date, new_deaths, new_cases) %>% 
  left_join(uk_currenthosp_data %>% 
              select(Day, `Daily hospital occupancy`),
            by=c("date"="Day")) %>% 
  # left_join(uk_hospadm_data %>% 
  #             select(Day, `Weekly new hospital admissions`),
  #           by=c("date"="Day")) %>% 
  left_join(uk_currenticu_data %>% 
              select(Day, `Daily ICU occupancy`),
            by=c("date"="Day"))

uk_gesamt_wona <- uk_gesamt %>% 
  drop_na()

ccf(uk_gesamt_wona$new_deaths, uk_gesamt_wona$new_cases)
ccf(uk_gesamt_wona$`Daily hospital occupancy`, uk_gesamt_wona$new_cases)
ccf(uk_gesamt_wona$`Daily ICU occupancy`, uk_gesamt_wona$new_cases)

uk_gesamt_long <- uk_gesamt %>% 
  mutate(new_cases_14=lag(new_cases, 14)) %>% 
  pivot_longer(cols=new_deaths:`Daily ICU occupancy`) %>% 
  mutate(rate_to_cases14=log(value/new_cases_14))

uk_plot <- ggplot(uk_gesamt_long %>% 
                    filter(name!="Weekly new hospital admissions"),
                  aes(x=date, y=value)) +
  geom_line() +
  facet_wrap(~name, ncol=2, scales = "free_y") +
  theme_zi()
uk_plot

uk_plot_rate14 <- ggplot(uk_gesamt_long %>% 
                    filter(name!="Weekly new hospital admissions"),
                  aes(x=date, y=rate_to_cases14)) +
  geom_line() +
  facet_wrap(~name, ncol=2, scales = "free_y") +
  theme_zi()
uk_plot_rate14
