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

bundeslaender_table<- read_json("./data/tabledata/bundeslaender_table.json",
                                simplifyVector = TRUE)
bundeslaender_r_und_vwz_data <- read_json("./data/plotdata/bundeslaender_r_und_vwz.json",
                                          simplifyVector = TRUE) %>%
  mutate(Datum=as_date(Datum))

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
rki <- tbl(conn,"rki") %>% collect()
rki <- rki %>% mutate(Meldedatum=as_date(Meldedatum))


bltabelle <- bind_rows(
  bundeslaender_table %>%
    filter(Bundesland=="Gesamt"),
  bundeslaender_table %>%
    filter(Bundesland!="Gesamt") %>%
    arrange(Bundesland)
) %>%
  select(Bundesland, `R(t)`, `7-Tage-Inzidenz`, `7-Tage-Inzidenz 60+`, Vorwarnzeit=`Vorwarnzeit aktuell`)

vwztabelle <- tibble(
  Vorwarnzeit=c(
    "Bundesdurchschnitt",
    "kürzeste",
    "längste"
  ),
  Vorwoche=c(
    paste0(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
                 pull(Wert),
           " Tage"), # Bundesdurchschnitt
    paste0(min(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
             filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" &
                      Wert==min(bundeslaender_r_und_vwz_data %>%
                                  filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                                  pull(Wert))) %>%
             pull(name), ", ")), # kürzeste
    paste0(max(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
                           filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" &
                                    Wert==max(bundeslaender_r_und_vwz_data %>%
                                                filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                                                pull(Wert))) %>%
                           pull(name), ", ")) # längste
  ),
  dieseWoche=c(
    paste0(bundeslaender_r_und_vwz_data %>%
             filter(Datum==max(Datum) & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
             pull(Wert),
           " Tage"), # Bundesdurchschnitt
    paste0(min(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
             filter(Datum==max(Datum) & Variable=="Vorwarnzeit" &
                      Wert==min(bundeslaender_r_und_vwz_data %>%
                                  filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                                  pull(Wert))) %>%
             pull(name), ", ")), # kürzeste
    paste0(max(bundeslaender_r_und_vwz_data %>%
                 filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                 pull(Wert)),
           " Tage\n",
           glue_collapse(bundeslaender_r_und_vwz_data %>%
                           filter(Datum==max(Datum) & Variable=="Vorwarnzeit" &
                                    Wert==max(bundeslaender_r_und_vwz_data %>%
                                                filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
                                                pull(Wert))) %>%
                           pull(name), ", ")) # längste
  ),
  Veraenderung=c(
    bundeslaender_r_und_vwz_data %>%
      filter(Datum==max(Datum) & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
      pull(Wert) - bundeslaender_r_und_vwz_data %>%
      filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit" & name=="Gesamt") %>%
      pull(Wert), # Bundesdurchschnitt
    min(bundeslaender_r_und_vwz_data %>%
          filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
          pull(Wert)) - min(bundeslaender_r_und_vwz_data %>%
                              filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                              pull(Wert)), # kürzeste
    max(bundeslaender_r_und_vwz_data %>%
          filter(Datum==max(Datum) & Variable=="Vorwarnzeit") %>%
          pull(Wert)) - max(bundeslaender_r_und_vwz_data %>%
                              filter(Datum==max(Datum)-7 & Variable=="Vorwarnzeit") %>%
                              pull(Wert)) # längste
  )
)

rki <- rki %>% mutate(KW=isoweek(Meldedatum))
thisKW <- max(rki$KW)
sterbeKW <- thisKW-4
vorsterbeKW <- sterbeKW-1
sterbestichtag <- max(rki%>%filter(KW==sterbeKW)%>%pull(Meldedatum))
vorsterbestichtag <- max(rki%>%filter(KW==vorsterbeKW)%>%pull(Meldedatum))
sterberki <- rki %>% filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
  group_by(Altersgruppe) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop") %>%
  filter(Altersgruppe!="unbekannt") %>%
  bind_rows(., rki %>% filter(Meldedatum<=sterbestichtag & Meldedatum>=sterbestichtag-6) %>%
              summarise(Altersgruppe="Gesamt", Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop"))
vorsterberki <- rki %>% filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
  group_by(Altersgruppe) %>%
  summarise(Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop") %>%
  filter(Altersgruppe!="unbekannt") %>%
  bind_rows(., rki %>% filter(Meldedatum<=vorsterbestichtag & Meldedatum>=vorsterbestichtag-6) %>%
              summarise(Altersgruppe="Gesamt", Todesfaelle=sum(AnzahlTodesfall), Sterblichkeit=sum(AnzahlTodesfall)/sum(AnzahlFall), .groups="drop"))

sterbetabelle <- tibble(
  `Todesfälle & Sterblichkeit`=c(
    "0 bis 4 Jahre",
    "5 bis 14 Jahre",
    "15 bis 34 Jahre",
    "35 bis 59 Jahre",
    "60 bis 79 Jahre",
    "80 Jahre +",
    "Gesamt"
  ),
  Vorwoche=c(
    vorsterberki %>% pull(Todesfaelle)
  ),
  Vorwoche_sterblichkeit=c(
    vorsterberki %>% pull(Sterblichkeit)
  ),
  KWX=c(
    sterberki %>% pull(Todesfaelle)
  ),
  KWX_sterblichkeit=c(
    sterberki %>% pull(Sterblichkeit)
  ),
  Veraenderung=paste0(round(100*(KWX-Vorwoche)/Vorwoche, 1), "%")
) %>%
  mutate(Vorwoche=ifelse(Vorwoche==0, 0, paste0(Vorwoche, " (", format(round(100*Vorwoche_sterblichkeit, 1), decimal.mark=","), "%)")),
         KWX=ifelse(KWX==0, 0, paste0(KWX, " (", format(round(100*KWX_sterblichkeit, 1), decimal.mark=","), "%)"))) %>%
  select(`Todesfälle & Sterblichkeit`, Vorwoche, KWX, Veraenderung)
