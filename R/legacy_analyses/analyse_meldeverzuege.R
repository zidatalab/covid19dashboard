library(tidyverse)
library(lubridate)

mindate <- as_date("2020-11-01")

startdate <- as_date("2020-11-24")
enddate <- as_date("2020-11-28")

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

for (thisdate in seq(startdate, enddate, 1)) {
  thisrki <- read_csv(paste0("./data/rki_", as_date(thisdate), ".csv")) %>%
    filter(Meldedatum>=mindate) %>%
    select(IdBundesland, AnzahlFall, Meldedatum, IdLandkreis) %>%
    mutate(IdLandkreis=as.numeric(IdLandkreis)*1000) %>%
    mutate(IdLandkreis=ifelse(IdLandkreis<12000000&IdLandkreis>=11000000, 11000000, IdLandkreis)) %>%
    left_join(., strukturdaten %>% select(id, EW_insgesamt), by=c("IdLandkreis"="id")) %>%
    group_by(IdLandkreis, Meldedatum) %>%
    summarise(Faelle=sum(AnzahlFall),
              IdBundesland=median(IdBundesland),
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
  mutate(diffSiebentageinzidenz=Siebentageinzidenz-min(Siebentageinzidenz))

ggplot(quantmeldeverzuege, aes(x=Rkidatum, y=diffSiebentageinzidenz)) +
  geom_line(aes(group=as_factor(IdLandkreis)), alpha=0.1) + 
  stat_summary(fun="mean", geom="point")
