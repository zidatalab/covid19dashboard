library(readxl)
library(dplyr)
regionalstatistik_bev_2019 <- read_excel("./data/legacy_data/regionalstatistik_bev_2019_21AG_mw_Kreise_12411-03-03-4.xlsx", 
                                         col_names = c("id", "name", "altersgruppe",
                                                       "insgesamt_alle", "insgesamt_m", "insgesamt_w",
                                                       "deutsche_alle", "deutsche_m", "deutsche_w",
                                                       "auslaenderinnen_alle", "auslaenderinnen_m", "auslaenderinnen_w"),
                                         col_types = c("text", "text", "text",
                                                       "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric",
                                                       "numeric", "numeric", "numeric"),
                                         skip = 6, n_max = 11836,
                                         na=c('.', '-')) %>%
  fill(c(id, name), .direction = "down") %>%
  mutate(id=as.integer(ifelse(id=="DG", 0, id)))
