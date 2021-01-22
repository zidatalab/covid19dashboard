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
