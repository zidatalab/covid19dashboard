library(tidyverse)
library(lubridate)
library(readr)
library(DBI)
library("zicolors")
library("ggrepel")
library("readxl")
library("zoo")

url <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"
destfile <- "~/Downloads/sonderauswertung_sterbefaelle.xlsx"
curl::curl_download(url, destfile)
sterbefaelle_tage <- read_excel(destfile, 
                                sheet = "D_2016_2020_Tage",
                                skip = 8,
                                na="X") %>%
  rename(Jahr="...1") %>% 
  pivot_longer(cols=-Jahr, names_to="Tag", values_to="Faelle")
  # gather(Tag,Faelle,2:ncol(.))

sterbefaelle_kw <- bind_rows(read_excel(destfile, 
                              sheet = "D_2016_2020_KW_AG_Männlich", 
                              skip = 8) %>% mutate(sex="maennlich"),
                             read_excel(destfile, 
                                        sheet = "D_2016_2020_KW_AG_Weiblich", 
                                        skip = 8) %>% mutate(sex="weiblich")) %>%
  select(-"Nr.") %>% 
  rename("Jahr"="...2", "Alter"= "unter … Jahren" ) %>%
  relocate(Jahr,Alter,sex) %>% 
  pivot_longer(cols=-c("Jahr", "Alter", "sex"), names_to="KW", values_to="Tote")
  # gather(KW,Tote,4:ncol(.))



sterbefaelle_kw.rec <- 
  left_join(sterbefaelle_kw %>% filter(Jahr==2020 & !is.na(Tote)),
            sterbefaelle_kw %>% filter(Jahr<2020) %>%
              group_by(Alter,KW,sex) %>% 
              summarise(Tote_2016_2019=mean(Tote,na.rm=T), .groups="drop"),
            by=c("KW","Alter","sex")) %>% 
  mutate(
    KW=as.numeric(KW),
    Vergleich=(Tote/Tote_2016_2019)*100,
    startage=as.numeric(ifelse(grepl("-", Alter), stringr::str_split_fixed(Alter,"-",2)[,1], NA)),
    stopage=as.numeric(ifelse(grepl("-", Alter), stringr::str_split_fixed(Alter,"-",2)[,2], NA)),
    agegrp = case_when(
      stopage<=60 ~ 1,
      startage>=60 & startage<=75 ~ 2,
      startage>=80 ~ 3,
      Alter=="95 u. mehr" ~ 3,
      Alter=="Insgesamt" ~ 4
      ),
    agegrp=factor(agegrp,
                  ordered = T,
                  levels=c(1,2,3,4),
                  labels=c("0-59","60-79","80+","Gesamt"))
    ) 


min_kw <- min(sterbefaelle_kw.rec$KW)
max_kw <- max(sterbefaelle_kw.rec$KW)

myplot <- sterbefaelle_kw.rec %>% 
  group_by(agegrp,KW) %>% 
  filter(agegrp!="Gesamt") %>% 
  summarise(Vergleich=100*(sum(Tote)/sum(Tote_2016_2019)),
            .groups="drop") %>% 
  filter(!is.na(agegrp)) %>% 
  ggplot(aes(x=KW,y=Vergleich,group=agegrp,color=agegrp)) + 
  geom_line(show.legend = F, size=1.5) + 
  scale_color_zi() +
  theme_zi_titels() +
  geom_label(data = . %>% 
               group_by(agegrp) %>%
               arrange(-KW) %>% 
               filter(KW>40) %>%
               filter(row_number()==1),
             aes(label=agegrp),
             show.legend = F) + 
  geom_hline(yintercept = 100) + 
  labs(y="Todesfälle in % von 2016-19", 
       x="Kalenderwoche")


myplot_1_alt <- sterbefaelle_kw.rec %>% 
  group_by(agegrp,KW) %>%
  filter(agegrp!="Gesamt") %>%
  summarise(Vergleich=100*(sum(Tote)/sum(Tote_2016_2019)),
            .groups="drop") %>%
  filter(!is.na(agegrp)) %>%  
  group_by(agegrp) %>% 
  arrange(KW) %>%
  mutate(Datum=as_date("2020-01-01")+weeks(as.numeric(KW)),
         Vergleich=rollmean(Vergleich, k = 3, fill = NA)) %>%
  ggplot(aes(x=Datum,y=Vergleich,group=agegrp,color=agegrp)) + 
  geom_line(show.legend = F, size=1.5) + 
  scale_color_zi() +
  theme_zi_titels() + 
  geom_label(data=. %>% 
               filter(!is.na(Vergleich)) %>% 
               group_by(agegrp) %>%
               arrange(-as.numeric(Datum)) %>% 
               filter(row_number()==1),
             aes(label=agegrp),show.legend = FALSE) +
  geom_hline(yintercept = 100) + 
  labs(y="Todesfälle in % von 2016-19", 
       x="Datum") + scale_x_date(breaks="4 weeks",date_labels = "%d.%m.")


myplot_1_alt_abs <- sterbefaelle_kw.rec %>% group_by(agegrp,KW) %>% 
  filter(agegrp!="Gesamt") %>% 
  summarise(Vergleich_abs=sum(Tote-Tote_2016_2019)) %>% 
  filter(!is.na(agegrp)) %>%   group_by(agegrp) %>% arrange(KW) %>%
  mutate(Datum=as_date("2020-01-01")+weeks(as.numeric(KW)),
         Vergleich_abs=rollmean(Vergleich_abs , k = 3, fill = NA)) %>%
  ggplot(aes(x=Datum,y=Vergleich_abs,group=agegrp,color=agegrp)) + 
  geom_hline(yintercept = 0) + 
  geom_line(show.legend = F, size=1.5) +  scale_color_zi() +
  theme_zi() + 
  geom_text_repel(force=5,nudge_x = 20,direction = "y", data=. %>% filter(!is.na(Vergleich_abs))%>% 
               group_by(agegrp) %>% arrange(-as.numeric(Datum)) %>% filter(row_number()==1) ,
             aes(label=paste(agegrp,"J.")),show.legend = FALSE)+
  labs(title="Sterblichkeit nach Altersgruppen im Kontext von COVID-19",
        subtitle="Abweichung der Todesfälle vom Durchschnitt 2016-19 pro KW (rollierender MW)", 
       x="Datum") + scale_x_date(breaks="4 weeks",date_labels = "%d.%m.")


finalise_plot(myplot_1_alt_abs,
              source_name = "Datenbasis: Stat. Bundesamt (2020) Sonderauswertung Sterbefälle 2016-2020",
              save_filepath = "~/Desktop/Trend_abs.png", width_cm=20,height_cm=20*(9/16))





finalise_plot(myplot,
              source_name = "Datenbasis: Stat. Bundesamt (2020) Sonderauswertung Sterbefälle 2016-2020",
              save_filepath = "~/Desktop/Trend.png", width_cm=20,height_cm=20*(9/16))



myplot2 <- sterbefaelle_kw.rec %>% 
  mutate(date=as.Date("2020-01-01")+weeks(KW-1)) %>% 
  filter(date>=as.Date("2020-11-01")) %>% 
  group_by(sex,agegrp) %>% 
  summarise(Vergleich=100*(sum(Tote)/sum(Tote_2016_2019)),
            min=min(KW),max=max(KW)) %>% 
  mutate(Geschlecht=ifelse(sex=='maennlich',"männlich","weiblich")) %>%
  ggplot(aes(x=agegrp,y=Vergleich,fill=Geschlecht)) + 
  geom_bar(stat="identity",show.legend = F) +  scale_fill_zi() +
  theme_zi_titels() + 
  facet_grid(.~Geschlecht)+
  geom_hline(yintercept = 100) + 
  labs(y="Todesfälle in % von 2016-19", 
       x="")

finalise_plot(myplot2,
              source_name = "Datenbasis: Stat. Bundesamt (2020) Sonderauswertung Sterbefälle 2016-2020, KW 45 und 46, November 2020.",
              save_filepath = "~/Desktop/November.png", width_cm=20,height_cm=20*(9/16))

