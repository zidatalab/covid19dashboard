# trends <- tbl(conn, "trends") %>% collect()
# strukturdaten <- tbl(conn,"strukturdaten") %>% collect()
maxdate <- as_date(max(trends$date))
trends_sti <- trends %>%
  mutate(date=as_date(date)) %>%
  filter(date>=maxdate-6) %>%
  left_join(strukturdaten, by=c("Country"="name")) %>%
  group_by(Country) %>%
  summarise(STI=sum(incident_cases)/EW_insgesamt[1]*100000,
            .groups="drop")
