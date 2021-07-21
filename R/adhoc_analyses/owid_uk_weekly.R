library(readr)
library(tidyverse)
library(lubridate)
library(zicolors)
library(extrafont)
owid_covid_data <- read_csv("data/owid-covid-data.csv")
# owid_currenthosp_data <- read_csv("data/current-covid-patients-hospital.csv")
owid_hospadm_data <- read_csv("data/weekly-hospital-admissions-covid.csv")
# owid_currenticu_data <- read_csv("data/current-covid-patients-icu.csv")

uk_case_data <- owid_covid_data %>% 
  filter(location=="United Kingdom" & date<="2021-07-18" & date>="2020-07-13") %>% 
  select(date, population, 
         contains("new_deaths"),
         contains("new_cases")) %>% 
  mutate(KW=isoweek(date)) %>% 
  group_by(KW) %>% 
  summarise(weekly_cases=sum(new_cases, na.rm=TRUE),
            weekly_deaths=sum(new_deaths, na.rm=TRUE),
            Day=max(date),
            .groups = "drop")

uk_hospadm_data <- owid_hospadm_data %>%
  filter(Entity=="United Kingdom")

uk_gesamt <- uk_case_data %>% 
  select(Day, weekly_deaths, weekly_cases) %>% 
  left_join(uk_hospadm_data %>%
              select(Day, `Weekly new hospital admissions`),
            by=c("Day"))

# uk_gesamt_wona <- uk_gesamt %>% 
#   drop_na()

# ccf(uk_gesamt_wona$new_deaths, uk_gesamt_wona$new_cases)
# ccf(uk_gesamt_wona$`Daily hospital occupancy`, uk_gesamt_wona$new_cases)
# ccf(uk_gesamt_wona$`Daily ICU occupancy`, uk_gesamt_wona$new_cases)



uk_gesamt_long <- uk_gesamt %>% 
  mutate(new_cases_14=lag(weekly_cases, 2)) %>% 
  pivot_longer(cols=weekly_deaths:`Weekly new hospital admissions`) %>% 
  # mutate(rate_to_cases14=log(value/new_cases_14))
  mutate(rate_to_cases14=value/new_cases_14)  %>% 
  filter(Day>"2020-07-26") %>% 
  drop_na()


point <- scales::format_format(big.mark=".", decimal.mark=",", scientific=FALSE)


uk_plot <- ggplot(uk_gesamt_long %>% 
                    filter(name!="weekly_deaths") %>% 
                    mutate(name=case_when(
                      name=="weekly_cases" ~ "Fälle",
                      name=="Weekly new hospital admissions" ~ "Hospitalisierungen"
                    )),
                  aes(x=Day, y=value)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = point, limits = c(0, NA)) +
  scale_x_date() +
  facet_wrap(~name, ncol=2, scales = "free_y") +
  labs(y="", x="", col="",
       subtitle="Wöchentliche Fälle und Hospitalisierungen in UK"
  ) +
  theme_zi()
uk_plot

uk_plot_rate14 <- ggplot(uk_gesamt_long %>% 
                           filter(name=="Weekly new hospital admissions"),
                  aes(x=Day, y=100*rate_to_cases14)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=0:3*5, labels=paste0(0:3*5, " %"), limits=c(0, NA)) +
  labs(y="", x="", col="",
       subtitle="Verhältnis Hospitalisierungen zu Fällen vor 14 Tagen in UK"
  ) +
  # facet_wrap(~name, ncol=3, scales = "free_y") +
  theme_zi()
uk_plot_rate14
