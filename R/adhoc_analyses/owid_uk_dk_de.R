library(readr)
library(tidyverse)
library(lubridate)
library(zicolors)
library(extrafont)
library(zoo)

owid_covid_data <- read_csv("data/owid-covid-data.csv")
owid_currenticu_data <- read_csv("data/current-covid-patients-icu.csv")
owid_tests_data <- read_csv("data/daily-tests-per-thousand-people-smoothed-7-day.csv")
owid_vacc_data <- read_csv("data/daily-covid-vaccination-doses-per-capita.csv")
owid_weeklyhosp_data <- read_csv("data/weekly-hospital-admissions-covid.csv")


pop <- tibble(location=c("United Kingdom", "Denmark", "Germany"),
              bev=c(66800000, 5840045, 83155031))

case_data <- owid_covid_data %>% 
  filter(location=="United Kingdom" | location=="Germany" | location=="Denmark") %>% 
  select(date, population,
         location,
         contains("new_deaths"),
         contains("new_cases"))

currenticu_data <- owid_currenticu_data %>% 
  filter(Entity=="United Kingdom" | Entity=="Germany"|Entity=="Denmark")

test_data <- owid_tests_data %>% 
  filter(Entity=="United Kingdom" | Entity=="Germany"|Entity=="Denmark")

vacc_data <- owid_vacc_data %>% 
  filter(Entity=="United Kingdom" | Entity=="Germany"|Entity=="Denmark")

hosp_data <- owid_weeklyhosp_data %>% 
  filter(Entity=="United Kingdom" | Entity=="Germany"|Entity=="Denmark")

gesamt <- case_data %>% 
  select(date, new_deaths, new_cases, location) %>% 
  left_join(pop, by="location") %>% 
  left_join(currenticu_data %>% 
              select(Day, `Daily ICU occupancy`, Entity),
            by=c("date"="Day", "location"="Entity")) %>% 
  left_join(test_data %>% 
              select(Day, new_tests_per_thousand_7day_smoothed, Entity),
            by=c("date"="Day", "location"="Entity")) %>% 
  left_join(vacc_data %>% 
              select(Day, new_vaccinations_smoothed_per_million, Entity),
            by=c("date"="Day", "location"="Entity")) %>% 
  left_join(hosp_data %>% 
              select(Day, `Weekly new hospital admissions`, Entity),
            by=c("date"="Day", "location"="Entity")) %>%
  mutate(icuoccupancy_per_million=`Daily ICU occupancy`/bev*1000000,
         hospadmissions_per_million=`Weekly new hospital admissions`/7/bev*1000000,
         test_per_million=new_tests_per_thousand_7day_smoothed*1000)

gesamt_wona <- gesamt %>% 
  drop_na()

# ccf(gesamt_wona$new_deaths, gesamt_wona$new_cases)
# ccf(gesamt_wona$`Daily hospital occupancy`, gesamt_wona$new_cases)
# ccf(gesamt_wona$`Daily ICU occupancy`, gesamt_wona$new_cases)

gesamt_long <- gesamt %>% 
  group_by(location) %>% 
  mutate(new_cases_per_million=rollmean(new_cases, 7, align="right", fill=NA)/bev*1000000,
         new_deaths_per_million=rollmean(new_deaths, 7, align="right", fill=NA)/bev*1000000) %>%
  ungroup() %>% 
  pivot_longer(cols=c(hospadmissions_per_million, new_vaccinations_smoothed_per_million, new_cases_per_million, new_deaths_per_million, test_per_million, icuoccupancy_per_million)) %>% 
  filter(date>="2021-06-30")%>% 
  filter(!(name=="new_deaths_per_million" & date>=as_date("2021-10-13")-days(14))) %>% 
  mutate(name=case_when(
    name=="icuoccupancy_per_million" ~ "ICU-Belegung",
    name=="new_cases_per_million" ~ "Neue Fälle",
    name=="hospadmissions_per_million" ~ "Krankenhauseinweisungen",
    name=="new_deaths_per_million" ~ "Neue Todesfälle",
    name=="test_per_million" ~ "Tests",
    name=="new_vaccinations_smoothed_per_million" ~ "Neue Impfungen",
    TRUE ~ "Errorname"
  )) %>% 
  mutate(Land=case_when(
    location=="Denmark" ~ "Dänemark",
    location=="United Kingdom" ~ "Vereinigtes Königreich",
    location=="Germany" ~ "Deutschland",
    TRUE ~ "Errorlocation"
  )) %>% 
  select(-c(location, new_deaths, new_cases, bev, `Daily ICU occupancy`, new_tests_per_thousand_7day_smoothed, `Weekly new hospital admissions`)) %>% 
  drop_na()

plot <- ggplot(gesamt_long,
                  aes(x=date, y=value, col=Land)) +
  geom_line(size=1.5) +
  ylim(0, NA) +
  facet_wrap(~name, ncol=2, scales = "free_y") +
  theme_zi() +
  scale_color_zi() +
  labs(title="COVID-19-Kennzahlen 2021 pro 1 Mio. Einwohnende",
       subtitle="Aufhebung der Maßnahmen in UK am 19.7. (grün),\nin Dänemark am 11.9. (blau)") +
  geom_vline(xintercept=as_date("2021-07-19"), col=zi_cols("zigreen")) +
  geom_vline(xintercept=as_date("2021-09-11"), col=zi_cols("ziblue"))
plot
