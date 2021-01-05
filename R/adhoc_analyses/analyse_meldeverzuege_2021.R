library(tidyverse)
library(lubridate)
library(ggalluvial)
library(sf)
library(zicolors)
library(cowplot)
library(ggbeeswarm)
mindate <- as_date("2020-11-01")

startdate <- as_date("2020-11-25")
enddate <- as_date("2021-01-03")

tagehorizont <- 3

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
dbListTables(conn)
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()

rkicounts <- tibble()

# tt_query <- system.time({testarchive_teil <- tbl(conn, "rki_archive") %>% 
#   filter(Datenstand=="2020-12-06") %>% 
#   collect() })

lastrki <- tbl(conn, "rki_archive") %>% 
  filter(Datenstand==enddate) %>%
  filter(Meldedatum>=mindate) %>%
  collect()
allkreisedates <- expand(lastrki, IdLandkreis, Meldedatum)
uniquekreise <- unique(as.integer(lastrki$IdLandkreis)*1000)
for (thisdate in seq(startdate, enddate, 1)) {
  thisdate <- as_date(thisdate)
  cat(thisdate, "\n")
  # thisrki <- read_csv(paste0("./data/rki_", as_date(thisdate), ".csv"))
  thisrki <- tbl(conn, "rki_archive") %>% 
    filter(Datenstand==thisdate) %>% 
    filter(Meldedatum>=mindate) %>%
    select(AnzahlFall, Meldedatum, IdLandkreis) %>% collect()
  thisrki <- thisrki %>%
    full_join(., allkreisedates %>% filter(Meldedatum<=max(thisrki$Meldedatum)), by=c("IdLandkreis", "Meldedatum")) %>%
    mutate(IdLandkreis=as.numeric(IdLandkreis)*1000) %>%
    mutate(IdLandkreis=ifelse(IdLandkreis<12000000&IdLandkreis>=11000000, 11000000, IdLandkreis)) %>%
    left_join(., strukturdaten %>% select(id, EW_insgesamt), by=c("IdLandkreis"="id")) %>%
    group_by(IdLandkreis, Meldedatum) %>%
    summarise(Faelle=sum(AnzahlFall, na.rm=TRUE),
              EW_insgesamt=median(EW_insgesamt),
              .groups="drop")
  thisrki2 <- inner_join(thisrki,
                         thisrki %>%
                           select(IdLandkreis, Meldedatum, Faelle), 
                         by = "IdLandkreis") %>% 
    mutate(datediff = as_date(Meldedatum.x) - as_date(Meldedatum.y)) %>%
    filter(datediff >= 0 & datediff <= 6) %>% 
    group_by(IdLandkreis, Meldedatum.x) %>% 
    summarise(Siebentagefaelle = sum(Faelle.y, na.rm=TRUE),
              Siebentageinzidenz = median(Siebentagefaelle/EW_insgesamt*100000, na.rm=TRUE),
              Faelle=median(Faelle.x, na.rm=TRUE),
              .groups="drop") %>%
    mutate(Rkidatum=as_date(thisdate))
  rkicounts <- bind_rows(rkicounts,
                         thisrki2)
}

# gemittelte analyse
multi_quantmeldeverzuege <- tibble()
for (d in 1:28) {
  cat(d, "\n")
  quantmeldeverzuege_d <- rkicounts %>%
    filter(IdLandkreis%in%c(uniquekreise, 11000000)) %>%
    filter(Meldedatum.x==startdate-1+(d-1)) %>%
    group_by(IdLandkreis) %>%
    mutate(diffSiebentageinzidenz=Siebentageinzidenz-min(Siebentageinzidenz, na.rm = TRUE),
           STI_wochespaeter= Siebentageinzidenz[Rkidatum==Meldedatum.x+7+1], # lead(Siebentageinzidenz, 7),
           Faelle_wochespaeter= Faelle[Rkidatum==Meldedatum.x+7+1], # lead(Faelle, 7),
           prozentFaelle=Faelle/max(Faelle_wochespaeter, 1)*100, # max(Faelle[Rkidatum==enddate], 1, na.rm = TRUE)*100, # max(Faelle, na.rm = TRUE)*100,
           prozentSTI=Siebentageinzidenz/STI_wochespaeter*100, # Siebentageinzidenz[Rkidatum==enddate]*100, # max(Siebentageinzidenz, na.rm=TRUE)*100,
           jump=ifelse((STI_wochespaeter>=50) & # max(Siebentageinzidenz, na.rm = TRUE)>=50) &
                         Siebentageinzidenz<50, TRUE, FALSE),# (min(Siebentageinzidenz, na.rm = TRUE)<50),TRUE,FALSE),
           Inzidenzlevel=ifelse(Siebentageinzidenz<35, "<35",
                                ifelse(Siebentageinzidenz<50, "35-50",
                                       "50+"))) %>%
    mutate(Inzidenzlevel=factor(Inzidenzlevel, levels=c("<35", "35-50", "50+"), 
                                ordered=TRUE),
           d=d)
  multi_quantmeldeverzuege <- bind_rows(multi_quantmeldeverzuege,
                                        quantmeldeverzuege_d)
}

# bund nachmeldung niveau
meldeverzuege_bund <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            .groups="drop")

ggplot(meldeverzuege_bund, 
       aes(x=datediff, y=mean_prozentSTI, fill=mean_prozentSTI)) +
  geom_bar(stat="identity") + # , fill=zi_cols("ziblue")
  theme_zi() +
  theme(legend.position = "none") +
  scale_fill_continuous(limits=c(50,100)) +
  scale_y_continuous(breaks=(0:10)*10, labels=paste0((0:10)*10, "%")) +
  scale_x_continuous(breaks=1:tagehorizont, labels=paste0("Tag ", 1:tagehorizont)) +
  labs(title="Unvollständige Sieben-Tage-Inzidenz\ndurch Nachmeldungen (BRD gesamt)", subtitle="Prozent der tatsächlichen Siebentageinzidenz")

ggplot(meldeverzuege_bund, 
       aes(x=datediff, y=mean_prozentFaelle, fill=mean_prozentFaelle)) +
  geom_bar(stat="identity") +
  theme_zi() +
  theme(legend.position = "none") +
  scale_fill_continuous(limits=c(0,100)) +
  scale_y_continuous(breaks=(0:10)*10, labels=paste0((0:10)*10, "%")) +
  scale_x_continuous(breaks=1:tagehorizont, labels=paste0("Tag ", 1:tagehorizont)) +
  labs(title="Nachmeldungen von COVID-19-Fällen\n(BRD gesamt)", subtitle="(nach)gemeldete Fälle in Prozent der tatsächlichen Fälle")

# bund nachmeldung nach wochentag
meldeverzuege_bund_wday <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x),
         wochentag=wday(Meldedatum.x)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff, wochentag) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            .groups="drop")

ggplot(meldeverzuege_bund_wday, 
       aes(x=wochentag, y=mean_prozentSTI, group=datediff, fill=mean_prozentSTI)) +
  geom_bar(stat="identity", #fill=zi_cols("ziblue"),
           color="darkgrey",
           position = "dodge") +
  theme_zi() +
  theme(legend.position = "none") +
  scale_fill_continuous(limits=c(50,100)) +
  scale_y_continuous(breaks=(0:10)*10, labels=paste0((0:10)*10, "%")) +
  scale_x_continuous(breaks=1:7, labels=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")) +
  labs(title="Unvollständige Sieben-Tage-Inzidenz\nnach Wochentagen (BRD gesamt)", subtitle="Prozent der tatsächlichen Siebentageinzidenz")

# bundeslaender nachmeldung
meldeverzuege_laender <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x),
         blid=floor(IdLandkreis/1000000)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff, blid) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            .groups="drop")
ggplot(meldeverzuege_laender, 
       aes(x=datediff, y=mean_prozentSTI, fill=mean_prozentSTI)) +
  geom_bar(stat="identity") +
  facet_wrap(vars(blid)) +
  theme_zi() +
  theme(legend.position = "none") +
  scale_fill_continuous(limits=c(50,100.2)) +
  scale_y_continuous(breaks=(0:2)*50, labels=paste0((0:2)*50, "%")) +
  scale_x_continuous(breaks=1:tagehorizont, labels=1:tagehorizont) +
  labs(title="Unvollständige Sieben-Tage-Inzidenz\ndurch Nachmeldungen (Bundesländer)", subtitle="Prozent der tatsächlichen Siebentageinzidenz")

# kreise nachmledung niveau boxplot
meldeverzuege_kreise <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff, IdLandkreis) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            .groups="drop")

ggplot(meldeverzuege_kreise,
       aes(x=factor(datediff), y=mean_prozentSTI)) +
  geom_beeswarm(cex=0.2, size=0.2) +
  theme_zi() +
  scale_x_discrete(labels=paste0("Tag ", 1:tagehorizont)) +
  scale_y_continuous(breaks=(0:2)*50, labels=paste0((0:2)*50, "%")) +
  labs(title="Unvollständige Sieben-Tage-Inzidenz\ndurch Nachmeldungen (Kreise)", subtitle="Prozent der tatsächlichen Siebentageinzidenz")

# karte prozent sti
KRS <- read_sf("./data/shp/kreise.shp")
REG <- KRS %>%
  group_by(RS) %>%
  count() %>%
  ungroup() %>%
  mutate(IdLandkreis=as.numeric(RS)*1000) %>%
  left_join(., meldeverzuege_kreise, by="IdLandkreis")
BL <- KRS %>%
  group_by(SN_L) %>%
  summarise(geometry = sf::st_union(geometry),
            .groups="drop")
plot_karte_sti <-
  REG %>%
  filter(datediff==1) %>%
  ggplot() + 
  geom_sf(aes(fill=mean_prozentSTI),
          lwd=0.1, color="#dfdfdf") +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  # scale_fill_gradient(low="#C800B1", high="#0086C5") +
  theme_void()
plot_karte_sti 

plot_karte_sti_cat <-
  REG %>%
  filter(datediff==1) %>%
  mutate(sti_cat=case_when(
    mean_prozentSTI<70 ~ "unter 70%",
    mean_prozentSTI>=70 & mean_prozentSTI<80 ~ "70-80%",
    mean_prozentSTI>=80 & mean_prozentSTI<90 ~ "80-90%",
    mean_prozentSTI>=90 & mean_prozentSTI<95 ~ "90-95%",
    mean_prozentSTI>=95 ~ "über 95%"
  ),
  sti_cat=factor(sti_cat, ordered = TRUE, levels = c("unter 70%", "70-80%", "80-90%", "90-95%", "über 95%"))) %>%
  ggplot() + 
  geom_sf(aes(fill=sti_cat),
          lwd=0.1, color="#dfdfdf") +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  theme_void() +
  scale_fill_zi("blue", discrete = TRUE) +
  labs(fill=paste0("Niveau der berichteten\nSieben-Tage-Inzidenz"))
plot_karte_sti_cat



#####











quantmeldeverzuege <- rkicounts %>%
  filter(Meldedatum.x==startdate-1) %>%
  group_by(IdLandkreis) %>%
  mutate(diffSiebentageinzidenz=Siebentageinzidenz-min(Siebentageinzidenz, na.rm = TRUE),
         prozentFaelle=Faelle/max(Faelle, na.rm = TRUE)*100,
         jump=ifelse((max(Siebentageinzidenz, na.rm = TRUE)>=50) &
                       (min(Siebentageinzidenz, na.rm = TRUE)<50),TRUE,FALSE),
         Inzidenzlevel=ifelse(Siebentageinzidenz<35, "<35",
                              ifelse(Siebentageinzidenz<50, "35-50",
                                     "50+"))) %>%
  mutate(Inzidenzlevel=factor(Inzidenzlevel, levels=c("<35", "35-50", "50+"), 
                              ordered=TRUE))

# Deutschlandkarte
KRS <- read_sf("./data/shp/kreise.shp")
REG <- KRS %>%
  group_by(RS) %>%
  count() %>%
  ungroup() %>%
  mutate(IdLandkreis=as.numeric(RS)*1000) %>%
  left_join(., quantmeldeverzuege, by="IdLandkreis")
BL <- KRS %>%
  group_by(SN_L) %>%
  summarise(geometry = sf::st_union(geometry))

# final one
c_levels = c("keine Änderung","+1 bis +5","+6 bis +19",
             "+20 bis +49","mehr als +50")
plot1 <-
  REG %>%
  filter(Rkidatum %in% as_date(enddate)) %>%
  mutate(change_kat=case_when(diffSiebentageinzidenz==0~"keine Änderung",
                              diffSiebentageinzidenz>0 &
                                diffSiebentageinzidenz<=5  ~"+1 bis +5",
                              diffSiebentageinzidenz>5 &
                              diffSiebentageinzidenz<20  ~"+6 bis +19",
                              diffSiebentageinzidenz>=20 &
                                diffSiebentageinzidenz<50  ~"+20 bis +49",
                              diffSiebentageinzidenz>=50   ~"mehr als +50"),
         change_kat=factor(change_kat,ordered = T, levels = c_levels)) %>%
  ggplot() + 
  geom_sf(aes(fill=change_kat),
          lwd=0.1, color="#dfdfdf") +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  theme_void() + scale_fill_zi("blue", discrete = TRUE, reverse = T)+
  labs(fill=paste0("Veränderung der\n7-Tages-Inzidenz\n nach ",
                   as.numeric(days(enddate-startdate))/60/60/24," Tagen")) 
plot1 

# final two
plot2.df <- REG %>% filter(Rkidatum %in% as_date(enddate)) %>%
  mutate(veraenderung=
           case_when((Siebentageinzidenz)<50 ~ "<50",
                     (Siebentageinzidenz-diffSiebentageinzidenz)>=50 ~ 
                       ">50 bekannt",
                     Siebentageinzidenz>=50 & 
                       (Siebentageinzidenz-diffSiebentageinzidenz)<50 ~ 
                       ">50 unbekannt"))

plot2 <-
  plot2.df %>%
  ggplot(.) +
  geom_sf(aes(fill=veraenderung),lwd=0.1, na.rm = TRUE,color="#dfdfdf") +
  scale_fill_manual(values = c("white", "#c59e9e", "#c50000")) +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  theme_void() +
  labs(fill="Überschreitung\nvon Grenzwerten\nnach Korrektur")
plot2

title <- ggdraw() +
  draw_label(
    "Verzögerte Meldungen der Gesundheitsämter verschieben Inzidenzen in Deutschland",
    fontfamily = "Calibri",
    fontface = 'bold',
    size = 14,
    x = 0.02  ,
    hjust = 0
  ) +
  theme(    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0))
subtitle <- ggdraw() +
  draw_label(
    "Änderung der 7-Tages-Inzidenz vor und nach Berücksichtigung von Nachmeldungen",
    fontfamily = "Calibri",
    size = 14,
    x = 0.02,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 0))

full_plot<- plot_grid(title,NULL,subtitle,NULL,plot1,plot2, 
                      ncol=2,rel_heights = c(.08,.05,1))


finalise_plot(full_plot,"static/Verzoegerung_Effekt.png",
              source_name = 
                paste0("Datenbasis: Meldedaten des RKI für den ",
                       format(startdate,"%d.%m.%Y")," mit dem Datenstand ",
                       format(startdate+days(1),"%d.%m.%Y")," bzw. ",
                       format(enddate,"%d.%m.%Y"),"."),
              width_cm = 23,height_cm = 23*(9/16))

# prozent verzug nach tagen
quantmeldeverzuege_byday <- quantmeldeverzuege %>%
  group_by(Rkidatum) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            .groups="drop")

ggplot(quantmeldeverzuege_byday %>% filter(Rkidatum<=as_date("2020-11-29")), aes(x=Rkidatum, y=mean_prozentFaelle)) +
  geom_bar(stat="identity", fill=zi_cols("ziblue")) +
  theme_zi() +
  scale_x_date(date_labels=paste0("Tag ", c(5, 1:4))) +
  labs(title="Nachmeldungen von COVID-19-Fällen", subtitle="gemeldete Fälle in Prozent der \nnach einer Woche bekannten Fälle")

ggplot(quantmeldeverzuege %>% filter(Rkidatum<=as_date("2020-11-29")), aes(group=factor(Rkidatum), y=prozentFaelle)) +
  geom_boxplot() +
  theme_zi() +
  # scale_x_discrete(labels=paste0("Tag ", c(5, 1:4))) +
  labs(title="Nachmeldungen von COVID-19-Fällen", subtitle="gemeldete Fälle in Prozent der \nnach einer Woche bekannten Fälle")

# nach wie viel tagen 95 prozent der fälle (kreisweise)
quantmeldeverzuege_95bykreis <- quantmeldeverzuege %>%
  group_by(IdLandkreis) %>%
  summarise(wann95=min(Rkidatum[prozentFaelle>=95]),
            .groups="drop") %>%
  mutate(Tag95=as.integer(wann95-as_date(startdate)))

plot3 <-
  REG %>%
  left_join(., quantmeldeverzuege_95bykreis, by="IdLandkreis") %>%
  filter(Rkidatum %in% as_date(enddate)) %>%
  ggplot() + 
  geom_sf(aes(fill=Tag95),
          lwd=0.1, color="#dfdfdf") +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  theme_void() + scale_fill_zi("green", discrete = TRUE, reverse = T)+
  labs(fill=paste0("Erreichen von 95% der\ngemeldeten Fälle")) 
plot3

