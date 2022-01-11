kbv_rki_gesamt <- kbv_rki %>% 
  filter(vacc_series==2) %>% 
  group_by(vacc_date) %>% 
  summarise(anzahl_alleorte_2=sum(anzahl_alleorte, na.rm=TRUE),
            .groups="drop")

library(ggplot2)
ggplot(kbv_rki_gesamt,
       aes(x=vacc_date, y=anzahl_alleorte_2)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  scale_x_date(date_breaks = "1 month")
