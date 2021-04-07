library(MASS) # be aware of "select"
library(tidyverse)
library(lubridate)
library(ggalluvial)
library(sf)
library(zicolors)
library(cowplot)
library(broom)
library(zoo)
select <- dplyr::select

deprivation <- Kreis <- read_csv("https://raw.githubusercontent.com/lekroll/GISD/master/Revisions/2018/Bund/Kreis/Kreis_2014.csv", 
                                 locale = locale(date_names = "de", encoding = "WINDOWS-1252"))
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
strukturdaten <- tbl(conn,"strukturdaten") %>% filter(ebene=="Kreis") %>% collect()
last_brd <- tbl(conn,"brd_timeseries") %>% filter(id==0) %>% 
  collect() %>% mutate(date=as_date(date)) %>%
  left_join(tbl(conn,"divi_all") %>% filter(id==0) %>% collect()
            %>%
              mutate(date=as_date(daten_stand)),by=c("id","date")) %>%
  filter(!is.na(ICU_Betten)) 

plotdata <- last_brd %>% select(date,cases,faelle_covid_aktuell_beatmet) %>% 
  arrange(date) %>%
mutate(INC_cases=cases-lag(cases)) %>%
  filter(!is.na(INC_cases)) %>%
  mutate(
    jahr=year(date),
    quartal = paste0(year(date),"-",quarter(date)),
    kw=fct_reorder(as_factor(isoweek(date)),
                   paste0(year(date),"-",sprintf("%02d",isoweek(date))))) %>% 
  filter(date>="2020-05-04") %>%
  group_by(quartal,kw) %>%
  summarise(Datum=mean(date),
            ICU_Faelle=mean(faelle_covid_aktuell_beatmet),
            INC_cases=sum(INC_cases)) 
  
# Plot
ggplot(plotdata , aes(y=100*ICU_Faelle/lag(INC_cases,2),x=Datum,color=quartal)) +
  geom_line(show.legend = F,size=2.5) + 
  labs(subtitle="ICU Fälle (beatmet) auf inzidente Fälle vor 2 Wochen in %") + 
  theme_zi()+ scale_y_continuous(limits = c(0, NA)) + 
  scale_x_date(date_labels = "%d.%m.%y",
               breaks = "6 weeks")+scale_color_zi(reverse = TRUE)

# Modellrechnung 
dosen <- 1.77947e6
# icu_beatmet = 0.03935916
icu_beatmet <- plotdata %>% ungroup() %>% summarise(VH =mean(ICU_Faelle/lag(INC_cases,2),na.rm=TRUE)) %>% pull(VH)
fatality <- 73656/2581329
trombosis_risk <- 7/1.6e6
inc <- 83.7/100000

paste(round(trombosis_risk*dosen), "Mgl. Trombosen")
paste(round(fatality*inc*dosen), "Todesfaelle")
paste(round(icu_beatmet*inc*dosen), "ICU Beatmungen")
paste(round(fatality*inc*2*dosen), "Todesfaelle bei doppelter Inzidenz")





