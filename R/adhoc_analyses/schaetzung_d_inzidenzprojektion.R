brd_sti_all <- rki %>%
  group_by(Meldedatum) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            einwohnende=kreise_regstat_alter %>%
              filter(id==0) %>%
              mutate(einwohnende=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6) %>%
              pull(einwohnende),
            .groups="drop") %>%
  mutate(STI=round((AnzahlFall+lag(AnzahlFall, 1)+lag(AnzahlFall, 2)+lag(AnzahlFall, 3)+
                      lag(AnzahlFall, 4)+lag(AnzahlFall, 5)+lag(AnzahlFall, 6))/einwohnende*100000))

sti_r <- brd_sti_all %>%
  left_join(rki_reformat_r_ts, by=c("Meldedatum"="date")) %>%
  left_join(rwert_bund_data %>% filter(name=="Gesamt") %>% select(date, R),
            by=c("Meldedatum"="date")) %>%
  mutate(r_rolling7=zoo::rollmean(R, 7, fill=NA, align="right"),
         sti_rate=STI/lag(STI, 7),
         d_estimate=7/log(sti_rate)*log(R),
         x_estimate=7/log(sti_rate))

hist(sti_r$d_estimate)
md <- mean(sti_r$d_estimate[is.finite(sti_r$d_estimate)])
sdd <- sd(sti_r$d_estimate[is.finite(sti_r$d_estimate)])

md + c(-1, 0, +1)*1.965*sdd

ggplot(sti_r %>%
         select(Meldedatum, `RKI-R-Wert`, sti_rate) %>%
         filter(Meldedatum>="2020-03-17") %>%
         pivot_longer(c("RKI-R-Wert", "sti_rate")),
       aes(x=Meldedatum, y=value, col=name)) +
  geom_line() +
  ylim(0, NA) +
  geom_hline(yintercept=c(0.5, 0.7))

ggplot(sti_r %>%
         select(Meldedatum, `RKI-R-Wert`, sti_rate) %>%
         filter(Meldedatum>="2020-03-17"),
       aes(x=`RKI-R-Wert`, y=sti_rate)) +
  geom_point() +
  geom_vline(xintercept=0.7) +
  geom_hline(yintercept = 0.5)

# by kw
brd_sti_all_kw <- rki %>%
  filter(Meldedatum<="2020-12-27") %>%
  mutate(kw=isoweek(Meldedatum)) %>%
  group_by(kw) %>%
  summarise(AnzahlFall=sum(AnzahlFall, na.rm=TRUE),
            datesunday=base::as.Date(paste0("2020-", kw[1], "-0"), format="%Y-%W-%w"),
            einwohnende=kreise_regstat_alter %>%
              filter(id==0) %>%
              mutate(einwohnende=ag_1+ag_2+ag_3+ag_4+ag_5+ag_6) %>%
              pull(einwohnende),
            .groups="drop") %>%
  mutate(STI=round((AnzahlFall)/einwohnende*100000))



sti_r_kw <- brd_sti_all_kw %>%
  left_join(rwert_bund_data %>% filter(name=="Gesamt") %>% select(date, R) %>%
              mutate(r_rolling7=zoo::rollmean(R, 7, fill=NA, align="right")),
            by=c("datesunday"="date")) %>%
  mutate(sti_rate=STI/lag(STI),
         d_estimate_rolling7=7/log(sti_rate)*log(r_rolling7),
         d_estimate=7/log(sti_rate)*log(R))
