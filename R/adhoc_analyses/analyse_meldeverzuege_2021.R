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

tagehorizont <- 7

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
           prozentFaelle=ifelse(Faelle_wochespaeter==0, ifelse(Faelle==0, 1, NA), Faelle/Faelle_wochespaeter)*100, # max(Faelle[Rkidatum==enddate], 1, na.rm = TRUE)*100, # max(Faelle, na.rm = TRUE)*100,
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

meldeverzuege_bund_plot <- ggplot(meldeverzuege_bund, 
       aes(x=datediff, y=mean_prozentFaelle, fill=mean_prozentFaelle)) +
  geom_bar(stat="identity") +
  theme_zi() +
  theme(legend.position = "none") +
  scale_fill_continuous(limits=c(0,100)) +
  scale_y_continuous(breaks=(0:10)*10, labels=paste0((0:10)*10, "%")) +
  scale_x_continuous(breaks=1:tagehorizont, labels=paste0("Tag ", 1:tagehorizont)) +
  labs(title="Meldeverzug von COVID-19-Fällen\n(BRD gesamt, gemittelt)",
       subtitle="vorliegende Fälle zum Meldedatum in Prozent\nder gemeldeten Fälle nach einer Woche")

finalise_plot(meldeverzuege_bund_plot, "static/meldeverzuege_bund.png",
              source_name = 
                paste0("Datenbasis: Meldedaten des RKI für den ",
                       format(startdate-1, "%d.%m.%Y"),
                       " bis ",
                       format(startdate-1+27, "%d.%m.%Y"),
                       "\nmit den Datenständen ",
                       format(startdate, "%d.%m.%Y"),
                       " bis ",
                       format(startdate-1+27+7, "%d.%m.%Y"),"."),
              width_cm = 23,height_cm = 23*(9/16))

# bund nachmeldung nach wochentag
meldeverzuege_bund_wday <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x),
         wochentag=wday(Meldedatum.x)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff, wochentag) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            .groups="drop")

# ggplot(meldeverzuege_bund_wday, 
#        aes(x=wochentag, y=mean_prozentSTI, group=datediff, fill=mean_prozentSTI)) +
#   geom_bar(stat="identity", #fill=zi_cols("ziblue"),
#            color="darkgrey",
#            position = "dodge") +
#   theme_zi() +
#   theme(legend.position = "none") +
#   scale_fill_continuous(limits=c(50,100)) +
#   scale_y_continuous(breaks=(0:10)*10, labels=paste0((0:10)*10, "%")) +
#   scale_x_continuous(breaks=1:7, labels=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")) +
#   labs(title="Unvollständige Sieben-Tage-Inzidenz\nnach Wochentagen (BRD gesamt)", subtitle="Prozent der tatsächlichen Siebentageinzidenz")
## --> keine nennenswerten unterschiede zwischen wochentagen auf brd-ebene

# bundeslaender nachmeldung
meldeverzuege_laender <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x),
         blid=floor(IdLandkreis/1000000)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff, blid) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            mean_STI=mean(Siebentageinzidenz, na.rm=TRUE),
            .groups="drop")
# ggplot(meldeverzuege_laender, 
#        aes(x=datediff, y=mean_prozentSTI, fill=mean_prozentSTI)) +
#   geom_bar(stat="identity") +
#   facet_wrap(vars(blid)) +
#   theme_zi() +
#   theme(legend.position = "none") +
#   scale_fill_continuous(limits=c(50,100.2)) +
#   scale_y_continuous(breaks=(0:2)*50, labels=paste0((0:2)*50, "%")) +
#   scale_x_continuous(breaks=1:tagehorizont, labels=1:tagehorizont) +
#   labs(title="Unvollständige Sieben-Tage-Inzidenz\ndurch Nachmeldungen (Bundesländer)", subtitle="Prozent der tatsächlichen Siebentageinzidenz")
mappingBL <- c(
  "01"="SH",
  "02"="HH",
  "03"="NI",
  "04"="HB",
  "05"="NW",
  "06"="HE",
  "07"="RP",
  "08"="BW",
  "09"="BY",
  "10"="SL",
  "11"="BE",
  "12"="BB",
  "13"="MV",
  "14"="SN",
  "15"="ST",
  "16"="TH"
)
ggplot(meldeverzuege_laender %>% filter(datediff==1),
       aes(x = mean_prozentSTI, y = mean_STI)) +
  # geom_point() +
  geom_text(aes(label=mappingBL), check_overlap = TRUE) +
  ylim(0, 400) +
  scale_x_continuous(breaks=(15:20)*5, labels=paste0((15:20)*5, "%")) +
  theme_zi()


# kreise nachmledung niveau boxplot
meldeverzuege_kreise <- multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x)) %>%
  filter(datediff<=tagehorizont) %>%
  group_by(datediff, IdLandkreis) %>%
  summarise(mean_prozentFaelle=mean(prozentFaelle, na.rm=TRUE),
            mean_prozentSTI=mean(prozentSTI, na.rm=TRUE),
            mean_STI=mean(Siebentageinzidenz, na.rm=TRUE),
            .groups="drop")

# ausreisser 9775000 
View(multi_quantmeldeverzuege %>%
  mutate(datediff=as.integer(Rkidatum-Meldedatum.x)) %>%
  filter(IdLandkreis==9775000 & datediff==7))
View(rkicounts %>%
       filter(IdLandkreis==9775000) %>%
       group_by(Rkidatum) %>%
       summarise(Faellegesamt=sum(Faelle, na.rm = TRUE),
                 .groups="drop"))
# --> am 8.12. sind im Landkreis Neu-Ulm 180 Fälle verschwunden/umdatiert?

mk_d1 <- meldeverzuege_kreise %>% filter(datediff==1)
mm_prozentSTI <- mean(mk_d1$mean_prozentSTI)
mm_STI <- mean(mk_d1$mean_STI)
belastungkreise_niveausti <- ggplot(mk_d1,
       aes(x = mean_prozentSTI, y = mean_STI)) +
  geom_point(col=zi_cols("zidarkblue")) +
  geom_hline(yintercept = mm_STI) +
  geom_vline(xintercept = mm_prozentSTI) +
  scale_x_continuous(breaks=(6:10)*10, labels=paste0((6:10)*10, "%")) +
  theme_zi() +
  labs(x="Niveau",
       y="Sieben-Tage-Inzidenz",
       title="Kein Zusammenhang zwischen Höhe der\nSieben-Tage-Inzidenz und Nachmeldungen",
       subtitle="durchschnittliche Inzidenz und erreichtes Niveau auf Kreisebene")
cor((meldeverzuege_kreise %>% filter(datediff==1))$mean_prozentSTI,
    (meldeverzuege_kreise %>% filter(datediff==1))$mean_STI)

finalise_plot(belastungkreise_niveausti, "static/belastungkreise_niveausti.png",
              source_name = 
                paste0("Datenbasis: Meldedaten des RKI für den ",
                       format(startdate-1, "%d.%m.%Y"),
                       " bis ",
                       format(startdate-1+27, "%d.%m.%Y"),
                       "\nmit den Datenständen ",
                       format(startdate, "%d.%m.%Y"),
                       " bis ",
                       format(startdate-1+27+7, "%d.%m.%Y"),
                       "."),
              width_cm = 23,height_cm = 23*(9/16))


## karte prozent sti
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
  theme_void() +
  scale_fill_zi("blue", discrete = FALSE) +
  labs(fill=paste0("Niveau der berichteten\nSieben-Tage-Inzidenz"))
plot_karte_sti 

plot_karte_sti_cat <-
  REG %>%
  filter(datediff==1) %>%
  mutate(sti_cat=case_when(
    mean_prozentSTI<70 ~ "über 30%",
    mean_prozentSTI>=70 & mean_prozentSTI<80 ~ "20-30%",
    mean_prozentSTI>=80 & mean_prozentSTI<90 ~ "10-20%",
    mean_prozentSTI>=90 & mean_prozentSTI<95 ~ "5-10%",
    mean_prozentSTI>=95 ~ "unter 5%"
  ),
  sti_cat=factor(sti_cat, ordered = TRUE, levels = c("über 30%", "20-30%", "10-20%", "5-10%", "unter 5%"))) %>%
  ggplot() + 
  geom_sf(aes(fill=sti_cat),
          lwd=0.1, color="#dfdfdf") +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  theme_void() +
  scale_fill_zi("blue", discrete = TRUE) +
  labs(fill=paste0("Anteil fehlende Fälle"),
       title="Regionale Unterschiede bei der Berechnung der\nSieben-Tage-Inzidenz durch Nachmeldungen")
plot_karte_sti_cat

finalise_plot(plot_karte_sti_cat, "static/niveau_sti_karte.png",
              source_name = 
                paste0("Datenbasis: Meldedaten des RKI für den ",
                       format(startdate-1, "%d.%m.%Y"),
                       " bis ",
                       format(startdate-1+27, "%d.%m.%Y"),
                       "\nmit den Datenständen ",
                       format(startdate, "%d.%m.%Y"),
                       " bis ",
                       format(startdate-1+27+7, "%d.%m.%Y"),
                       ", Karte: GeoBasis-DE / BKG 2020."),
              width_cm = 23/2, height_cm = 23*(9/16))
