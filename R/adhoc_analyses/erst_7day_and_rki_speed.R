library(tidyverse)
library(readr)

uri <- "https://ziwebstorage.blob.core.windows.net/publicdata/impfungen_erst_zweit_vgl_praxen_andere.csv"

df <- tibble(read_csv2(uri))

df %>% 
  mutate("7dayerstpraxen"=
           round((`Erst-Praxen`-lag(`Erst-Praxen`,8))/7)) %>%
  filter(`Erst-Praxen`>0) %>% 
  ggplot(aes(x=date,y=`7dayerstpraxen`)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

df7day <- df %>% select(date,contains("Erst-")) %>% gather(Ort,Anzahl_Erst,2:3) %>% arrange(Ort,date) %>%
  group_by(Ort) %>% mutate(erst_7day=round((Anzahl_Erst-lag(Anzahl_Erst,8))/7)) %>%
  filter(!is.na(erst_7day)) 

ggplot(df7day,aes(x=date,fill=Ort,y=erst_7day/1000)) + geom_area(position = "stack")