conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')

params <- tbl(conn,"params") %>% select(name, EW_insgesamt) %>% collect()

eumapping <- tibble(english=c(
  "France",
  "Spain",
  "United Kingdom",
  "Italy",
  "Germany",
  "Poland",
  "Belgium",
  "Czechia", 
  "Netherlands",
  "Romania",
  "Portugal",
  "Austria",
  "Sweden",
  "Hungary",
  "Bulgaria",
  "Croatia",
  "Slovakia",
  "Greece",
  "Denmark",
  "Ireland",
  "Slovenia",
  "Lithuania",
  "Norway",
  "Luxembourg",
  "Finland",
  "Latvia",
  "Estonia",
  "Cyprus",
  "Malta",
  "Iceland",
  "Liechtenstein"
), german=c(
  "Frankreich",
  "Spanien",
  "Vereinigtes Königreich",
  "Italien",
  "Deutschland",
  "Polen",
  "Belgien",
  "Tschechien", 
  "Niederlande",
  "Rumänien",
  "Portugal",
  "Österreich",
  "Schweden",
  "Ungarn",
  "Bulgarien",
  "Kroatien",
  "Slowakei",
  "Griechenland",
  "Dänemark",
  "Irland",
  "Slowenien",
  "Litauen",
  "Norwegen",
  "Luxemburg",
  "Finnland",
  "Lettland",
  "Estland",
  "Zypern",
  "Malta",
  "Island",
  "Liechtenstein"
), cgroup=c(
  "big5",
  "big5",
  "big5",
  "big5",
  "big5",
  "eastern",
  "benelux",
  "eastern", 
  "benelux",
  "eastern",
  "mediterranean",
  "western",
  "scandinavian",
  "eastern",
  "eastern",
  "eastern",
  "eastern",
  "mediterranean",
  "scandinavian",
  "western",
  "eastern",
  "baltic",
  "scandinavian",
  "benelux",
  "scandinavian",
  "baltic",
  "baltic",
  "mediterranean",
  "mediterranean",
  "western",
  "western"
)
)

international <- tbl(conn,"trends") %>%
  filter(Country %in% c(
    "France",
    "Spain",
    "United Kingdom",
    "Italy",
    "Germany",
    "Poland",
    "Belgium",
    "Czechia", 
    "Netherlands",
    "Romania",
    "Portugal",
    "Austria",
    "Sweden",
    "Hungary",
    "Bulgaria",
    "Croatia",
    "Slovakia",
    "Greece",
    "Denmark",
    "Ireland",
    "Slovenia",
    "Lithuania",
    "Norway",
    "Luxembourg",
    "Finland",
    "Latvia",
    "Estonia",
    "Cyprus",
    "Malta",
    "Iceland",
    "Liechtenstein"
  )) %>%
  collect() %>%
  mutate(date=as_date(date)) %>%
  left_join(., params, by=c("Country"="name"))

eumaxdate <- as_date("2020-12-27")
eu_7ti_kw <- international %>%
  filter(date <= eumaxdate) %>%
  mutate(KW=isoweek(date)) %>%
  group_by(Country, KW) %>%
  summarise(STI=sum(max(cases, na.rm=TRUE)-min(cases, na.rm=TRUE), na.rm=TRUE)/EW_insgesamt*100000,
            .groups="drop") %>%
  distinct() %>%
  left_join(., eumapping, by=c("Country"="english"))

for (cg in unique(eu_7ti_kw$cgroup)) {
  print(ggplot(eu_7ti_kw %>% filter(cgroup==cg),
         aes(x=KW, y=STI, col=Country)) +
    geom_line())
}
