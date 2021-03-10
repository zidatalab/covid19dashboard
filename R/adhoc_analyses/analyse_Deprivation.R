library(MASS) # be aware of "select"
library(tidyverse)
library(lubridate)
library(ggalluvial)
library(sf)
library(zicolors)
library(cowplot)
library(broom)

select <- dplyr::select

deprivation <- Kreis <- read_csv("https://raw.githubusercontent.com/lekroll/GISD/master/Revisions/2018/Bund/Kreis/Kreis_2014.csv", 
                                 locale = locale(date_names = "de", encoding = "WINDOWS-1252"))
conn <- DBI::dbConnect(RPostgres::Postgres(),
                       host   = Sys.getenv("DBHOST"),
                       dbname = Sys.getenv("DBNAME"),
                       user      =  Sys.getenv("DBUSER"),
                       password        = Sys.getenv("DBPASSWORD"),
                       port     = 5432,
                       sslmode = 'require')
strukturdaten <- tbl(conn,"strukturdaten") %>% filter(ebene=="Kreis") %>% collect()
last_brd <- tbl(conn,"brd_timeseries") %>% filter(id==0) %>% collect() %>% tail(1)
joined_current <- tbl(conn,"brd_timeseries") %>% 
  filter(id>=local(min(strukturdaten$id)) &
           id<=local(max(strukturdaten$id)) & date==local(last_brd$date[1])) %>%
  collect() %>% 
  left_join(.,strukturdaten,by="id") %>%
  left_join(.,deprivation %>% mutate(id=Kreiskennziffer*1000) %>% select(id,contains("GISD")),by="id")

# Plot
ggplot(joined_current,aes(x=GISD_Score,y=deaths*100/cases)) + geom_smooth(method="gam") + geom_point()

# Model
models <- vector("list")
models$poisson_nooffset <- glm(formula = deaths ~ 1 + GISD_Score +  cases +  I(Einwohner/flaeche)+ICU_Betten,family = "poisson", data = joined_current)
models$poisson_withoffset <- glm(formula = deaths ~ 1 + GISD_Score +  offset(log(cases)) +  I(Einwohner/flaeche)+ICU_Betten,family = "poisson", data = joined_current)
models$negbinom <- glm.nb(formula = deaths ~ 1 + GISD_Score +  offset(log(cases)) +  I(Einwohner/flaeche)+ICU_Betten, data = joined_current)
lapply(models, AIC)

models$negbinom %>% tidy(exponentiate=TRUE)
