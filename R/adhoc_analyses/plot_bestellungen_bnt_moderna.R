library(tidyverse)
library(DBI)
library(tidyverse)
library(zicolors)

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

kbv_bestell <- DBI::dbReadTable(conn,"kbv_bestelldaten") %>% collect()


plot_roh <- kbv_bestell %>% 
filter(Bestellwoche >= 41) %>%
mutate(Impfstoff=case_when(Impfstoff=="BT"~'BioNTech',Impfstoff=="Moderna"~"Moderna")) %>%
filter(!is.na(Impfstoff)) %>%
mutate(Lieferwoche=ifelse(Bestellwoche>46,Bestellwoche,Bestellwoche+1)) %>%
  group_by(Lieferwoche,Impfstoff) %>%
  summarise(Bestellung=sum(Bestellung)) %>%
  ungroup() %>% group_by(Lieferwoche) %>% 
  mutate(cumBestellung=sum(Bestellung)) %>%
ggplot(aes(x=Lieferwoche,y=Bestellung/1e6,fill=Impfstoff)) +
geom_bar(stat="identity") + scale_fill_zi() + theme_zi() +
geom_text(data=. %>% filter(Impfstoff!="Moderna"), 
          aes(y=0.5+cumBestellung/1e6,label=paste(format(cumBestellung/1e6,digits=1,decimal.mark=","),"Mio.")),
          family="Calibri") +
labs(title = "Bestellungen von Moderna und BioNTech in den Arztpraxen",
subtitle = "Anzahl bestellter Dosen in Mio. für Kalenderwoche",fill="") +
theme(legend.position="bottom")+
scale_x_continuous(breaks=seq(1,48,1)) +
scale_y_continuous(labels = paste(format(seq(0,10,2.5),decimal.mark = ",",digits=2),"Mio."), breaks = seq(0,10,2.5))

finalise_plot(plot_roh,
              save_filepath = "moderna_barchar.png",
              source_name = "Datenbasis: KBV",
              width_cm = 20, height_cm = 20*3/4 )


