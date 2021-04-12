library(tidyverse)
library(ggrepel)

# Params

R0 <- 4.3 # B.1.1.7  3.8 + 0.5 https://www.aerzteblatt.de/nachrichten/120883/RKI-Verbreitung-der-B-1-1-7-Virusvariante-bei-sechs-Prozent
DHIT<- 1-1/R0
VaccAccept <- .85
VaccEFF <- 0.8


Zeitreihe <- readr::read_csv("Zeitreihe.csv")


herdimmunity_data <- Zeitreihe %>% 
  select(date,impflinge=population,
         geimpft="Anteil Durchimpfung")  %>%
  mutate(population=83.2e6,
         impfquote=geimpft/100*impflinge/population,
         R_eff=R0*(1-(impfquote/DHIT) ),
         R_eff=ifelse(R_eff<0,0,R_eff))

