library(tidyverse)
library(lubridate)
library(ggalluvial)
library(sf)
library(zicolors)
library(cowplot)
mindate <- as_date("2020-11-01")

startdate <- as_date("2020-11-24")
enddate <- as_date("2020-11-26")

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
DBI::dbDisconnect(conn)
rkicounts <- tibble()

lastrki <- read_csv(paste0("./data/rki_", as_date(enddate), ".csv")) %>%
  filter(Meldedatum>=mindate)
allkreisedates <- expand(lastrki, IdLandkreis, Meldedatum)

for (thisdate in seq(startdate, enddate, 1)) {
  thisrki <- read_csv(paste0("./data/rki_", as_date(thisdate), ".csv")) %>%
    filter(Meldedatum>=mindate) %>%
    select(AnzahlFall, Meldedatum, IdLandkreis) %>%
    full_join(., allkreisedates %>% filter(Meldedatum<=thisdate), by=c("IdLandkreis", "Meldedatum")) %>%
    mutate(IdLandkreis=as.numeric(IdLandkreis)*1000) %>%
    mutate(IdLandkreis=ifelse(IdLandkreis<12000000&IdLandkreis>=11000000, 11000000, IdLandkreis)) %>%
    left_join(., strukturdaten %>% select(id, EW_insgesamt), by=c("IdLandkreis"="id")) %>%
    group_by(IdLandkreis, Meldedatum) %>%
    summarise(Faelle=sum(AnzahlFall, na.rm=TRUE),
              EW_insgesamt=median(EW_insgesamt),
              .groups="drop")
  thisrki2 <- inner_join(thisrki,
                         thisrki %>%
                           select(IdLandkreis, Meldedatum, Faelle), by = "IdLandkreis") %>% 
    mutate(datediff = as_date(Meldedatum.x) - as_date(Meldedatum.y)) %>%
    filter(datediff >= 0 & datediff <= 6) %>% 
    group_by(IdLandkreis, Meldedatum.x) %>% 
    summarise(Siebentagefaelle = sum(Faelle.y, na.rm=TRUE),
              Siebentageinzidenz = median(Siebentagefaelle/EW_insgesamt*100000),
              Faelle=median(Faelle.x),
              .groups="drop") %>%
    mutate(Rkidatum=as_date(thisdate))
  rkicounts <- bind_rows(rkicounts,
                         thisrki2)
}

quantmeldeverzuege <- rkicounts %>%
  filter(Meldedatum.x==startdate) %>%
  group_by(IdLandkreis) %>%
  mutate(diffSiebentageinzidenz=Siebentageinzidenz-min(Siebentageinzidenz),
         jump=ifelse((max(Siebentageinzidenz)>=50) & (min(Siebentageinzidenz)<50),TRUE,FALSE),
         Inzidenzlevel=ifelse(Siebentageinzidenz<35, "<35",
                              ifelse(Siebentageinzidenz<50, "35-50",
                                     "50+"))) %>%
  mutate(Inzidenzlevel=factor(Inzidenzlevel, levels=c("<35", "35-50", "50+"), ordered=TRUE))

ggplot(quantmeldeverzuege, aes(x=Rkidatum, y=diffSiebentageinzidenz)) +
  geom_line(aes(group=as_factor(IdLandkreis)), alpha=0.1) + 
  stat_summary(fun="mean", geom="point")

ggplot(quantmeldeverzuege, aes(group=Rkidatum, y=diffSiebentageinzidenz)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0, 50))

ggplot(quantmeldeverzuege, aes(x=Rkidatum, fill=Inzidenzlevel)) +
  geom_bar(position="dodge")

ggplot(quantmeldeverzuege %>% filter(Rkidatum<="2020-11-29" & jump==TRUE),
       aes(x = Rkidatum, stratum = Inzidenzlevel, alluvium = IdLandkreis,
           fill = Inzidenzlevel, label = Inzidenzlevel)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "backfront") +
  geom_stratum() +
  theme(legend.position = "bottom")

# deutschlandkarte
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
c_levels = c("keine Änderung","+1 bis +5","+6 bis +19","+20 bis +49","mehr als +50")
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
  ggplot() + # %>% mutate(diffSiebentageinzidenz=ifelse(diffSiebentageinzidenz==0, NA, diffSiebentageinzidenz))
  geom_sf(aes(fill=change_kat),
          lwd=0.2) +
  geom_sf(data=BL, lwd=0.4, alpha=0) +
  theme_void() +
  scale_fill_manual(values=c("#ededed","#F5FAFD", "#CCE7F3" ,"#0086C5","#006596")) +
  labs(fill=paste0("Veränderung der\n7-Tages-Inzidenz\n nach ",as.numeric(days(enddate-startdate))/60/60/24," Tagen")) 
plot1 

# final two
plot2 <-
  REG %>% filter(Rkidatum %in% as_date(enddate)) %>%
  ggplot(.) +
  # %>% mutate(diffSiebentageinzidenz=ifelse(diffSiebentageinzidenz==0, NA, diffSiebentageinzidenz))
  geom_sf(aes(fill=jump),lwd=0.1, na.rm = TRUE) +
  scale_fill_manual(values = c("white", "#E49900"), labels=c("keine Änderung", ">50")) +
  geom_sf(data=BL, lwd=0.4, alpha=0) +
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
    paste0("Änderung der 7-Tages-Inzidenz bei Berücksichtigung von Nachmeldungen ",
           format(startdate,"%d.%m.")," vs. ",format(enddate,"%d.%m.")),
    fontfamily = "Calibri",
    size = 12,
    x = 0.02,
    hjust = 0
  ) +
  theme(    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0))

full_plot<- plot_grid(title,NULL,subtitle,NULL,plot1,plot2, ncol=2,rel_heights = c(.08,.05,1))
full_plot

ggsave(full_plot,filename = "Verzoegerung_Effekt.png", width = 10,height=10*9/16,dpi=600)


# ggplot(REG %>% filter(Rkidatum %in% as_date(c("2020-11-24", "2020-11-29")))) +
#   geom_sf(aes(fill=Inzidenzlevel), lwd=0.2) +
#   scale_fill_brewer() +
#   facet_wrap(~Rkidatum) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = 'transparent'),
#         axis.text=element_blank())
# 
# ggplot(REG %>% filter(Rkidatum %in% as_date(c("2020-11-29")))) + # %>% mutate(diffSiebentageinzidenz=ifelse(diffSiebentageinzidenz==0, NA, diffSiebentageinzidenz))
#   geom_sf(aes(fill=diffSiebentageinzidenz), lwd=0.2) +
#   theme_minimal() +
#   geom_sf(data=st_centroid(REG %>% filter(Jump==1)) %>%
#             mutate(Inzidenzlevel=REG %>% filter(Jump==1) %>% pull(Inzidenzlevel)), aes(color=Inzidenzlevel), size=0.5, shape=8) + # midpoints of the kreise
#   scale_fill_gradient(low="#FFFFFF", high="#00008B") +
#   scale_color_brewer() +
#   theme(panel.grid.major = element_line(colour = 'transparent'),
#         axis.text=element_blank())

