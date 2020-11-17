source("functions.R")

# Define Model
# Source https://github.com/ellisp/blog-source/blob/master/_working/0178-covid-prevalence-inference.R


estimate_cases <- function(observed_cases, pos_rate, m, k){
  y <- observed_cases * m*pos_rate ^ k
  return(y)
}

# Annahmen
# Todesfälle = 1% Todesfälle unter aktuellen Fällen
# Testquote = Je höher die Testquote desto geringer die Untererfassung
# Positivrate  = Je geringer die Positivrate desto geringer die Untererfassung
case_prediction <- brd_timeseries %>% filter(id==0) %>% select(date,deaths) %>% collect() %>%
  mutate(kw=isoweek(as.Date(date))) %>% group_by(kw) %>% summarise(deaths=mean(deaths)) %>% ungroup() %>%
  mutate(Einwohner= strukturdaten %>% filter(id==0) %>% pull(Einwohner),
         `Schätzung mit Todesfälle`=((lead(deaths,1)-lead(deaths,0))/0.01)) %>% filter(kw<isoweek(now())) %>%
  left_join(brd_testungen,.,by="kw") %>%
  mutate(m_emp = testungen/(Einwohner/1200),
         `Schätzung mit Fälle`= positiv,
         `Schätzung mit Testquote`= estimate_cases(positiv, positivrate, m = m_emp, k = 0),
         `Schätzung mit Positivrate`  = estimate_cases(positiv, positivrate, m = 7, k = 0.1))

plotdata <- case_prediction %>% select(kw,Einwohner,contains("Schätzung")) %>% pivot_longer(contains("Schätzung"), names_to="Merkmal", values_to="Wert") %>%
  mutate(Merkmal=str_remove(Merkmal,"Schätzung mit ")) %>%
  mutate(`Inzidenz je 100 Tsd. Einw.`= round(Wert/(Einwohner/100000),1),
         Datum= as.Date(paste(2020,kw,1,sep=":"), format = "%Y:%W:%u") - 3.5)

testsplot_longterm <- ggplot(plotdata,
                             aes(x=Datum, y=`Inzidenz je 100 Tsd. Einw.`,color=Merkmal)) +
    geom_line(size=2) + theme_zi() + scale_color_zi("main4colors") + labs(color="",subtitle = "Langfristige Entwicklung")

testsplot__shortterm <- ggplot(plotdata %>% filter(kw>(isoweek(now())-12)),
                               aes(x=Datum, y=`Inzidenz je 100 Tsd. Einw.`,color=Merkmal)) +
  geom_line(size=2) + theme_zi() +
  scale_color_zi("main4colors") + labs(color="",subtitle = "letzte 3 Monate") +
  scale_x_date(date_labels = "%d.%m." , breaks="2 weeks")

ggplotly(testsplot_longterm)
ggplotly(testsplot__shortterm)

exportplot <- testsplot_longterm + labs(title="Geschätzte COVID-19 Inzidenz auf Basis von Testungen und Todesfällen",
                                    subtitle="Vergleich der vorhergesagten Inzidenz je 100 Tsd. Einw.",
                                    caption="Datenbasis: Robert Koch-Institut, Stand: 12.8.2020")
