library(tidyverse)
library(lubridate)
library(ggalluvial)
library(sf)

mindate <- as_date("2020-11-01")

startdate <- as_date("2020-11-26")
enddate <- as_date("2020-11-29")

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
         Inzidenzlevel=ifelse(Siebentageinzidenz<35, "<35",
                              ifelse(Siebentageinzidenz<50, "35-50",
                                     ifelse(Siebentageinzidenz<70, "50-70",
                                            ifelse(Siebentageinzidenz<200, "70-200",
                                                   "200+"))))) %>%
  mutate(Inzidenzlevel=factor(Inzidenzlevel, levels=c("<35", "35-50", "50-70", "70-200", "200+"), ordered=TRUE))
jumpmeldeverzuge <- quantmeldeverzuege %>%
  group_by(IdLandkreis) %>%
  summarise(Jump=n_distinct(Inzidenzlevel)-1,
            .groups="drop")
jumpkreise <- jumpmeldeverzuge %>% filter(Jump>0) %>% pull(IdLandkreis)

ggplot(quantmeldeverzuege, aes(x=Rkidatum, y=diffSiebentageinzidenz)) +
  geom_line(aes(group=as_factor(IdLandkreis)), alpha=0.1) + 
  stat_summary(fun="mean", geom="point")

ggplot(quantmeldeverzuege, aes(x=Rkidatum, fill=Inzidenzlevel)) +
  geom_bar(position="dodge")

ggplot(quantmeldeverzuege %>% filter(Rkidatum<="2020-11-26" & IdLandkreis%in%jumpkreise),
       aes(x = Rkidatum, stratum = Inzidenzlevel, alluvium = IdLandkreis,
           fill = Inzidenzlevel, label = Inzidenzlevel)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "backfront") +
  geom_stratum() +
  theme(legend.position = "bottom")

# deutschlandkarte
KRS <- read_sf("./data/shp/kreise.shp")
ggplot(KRS)+geom_sf() 
ggplot(KRS, aes(fill=SN_L))+geom_sf() # colored by Bundesland!
REG <- KRS %>%
  group_by(RS) %>%
  count() %>%
  ungroup() %>%
  mutate(IdLandkreis=as.numeric(RS)*1000) %>%
  left_join(., quantmeldeverzuege, by="IdLandkreis") %>%
  mutate(Jump=factor(ifelse(IdLandkreis%in%jumpkreise, 1, 0)),
         Inz50200=ifelse(!Inzidenzlevel%in%c("<35", "35-50"), 
                         ifelse(Inzidenzlevel=="200+",
                                "über 200",
                                "über 50"),
                         "unter 50"))
BL <- KRS %>%
  group_by(SN_L) %>%
  summarise(geometry = sf::st_union(geometry))

# final one
ggplot() + # %>% mutate(diffSiebentageinzidenz=ifelse(diffSiebentageinzidenz==0, NA, diffSiebentageinzidenz))
  geom_sf(data=REG %>%
            filter(Rkidatum %in% as_date(c("2020-11-29"))),
          aes(fill=diffSiebentageinzidenz),
          lwd=0.2) +
  geom_sf(data=BL, lwd=0.4, alpha=0) +
  theme_minimal() +
  scale_fill_gradient(low="#FFFFFF", high="#889C05") +
  labs(fill="Veränderung 7-T.-I.") +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text=element_blank())

# final two
ggplot(REG %>% filter(Rkidatum %in% as_date(c("2020-11-29")))) + # %>% mutate(diffSiebentageinzidenz=ifelse(diffSiebentageinzidenz==0, NA, diffSiebentageinzidenz))
  geom_sf(lwd=0.1) +
  theme_minimal() +
  geom_sf(data=REG %>% filter(Jump==1 & Inz50200=="über 50"), fill="#CCE7F3", lwd=0.2) +
  geom_sf(data=REG %>% filter(Jump==1 & Inz50200=="über 200"), fill="#006596", lwd=0.2) +
  # scale_fill_brewer() +
  geom_sf(data=BL, lwd=0.4, alpha=0) +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        axis.text=element_blank())

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

