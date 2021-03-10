library(sf)
library(zicolors)

KRS <- read_sf("../../data/shp/kreise.shp")
BL <- KRS %>%
  group_by(SN_L) %>%
  summarise(geometry = sf::st_union(geometry),
            .groups="drop")

plot_karte_krs_bl <-
  KRS %>%
  ggplot() +
  geom_sf(aes(fill=BEZ),
          lwd=0.1, color="#dfdfdf") +
  geom_sf(data=BL, lwd=0.2, alpha=0, color="black") +
  theme_void() +
  scale_fill_zi("blue", discrete = TRUE)
plot_karte_krs_bl 
