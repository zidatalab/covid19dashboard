library(openxlsx)
library(readxl)

risiko_kreise_80 <- read_excel("~/Dokumente/covid-19/zigitlab/covid19dashboard/data/risiko_kreise_80.xlsx", 
                               sheet = "Daten", skip = 3) %>%
  mutate(Wert=parse_number(Wert, locale = readr::locale(decimal_mark = ",")),
         Bundeswert=parse_number(Bundeswert, locale = readr::locale(decimal_mark = ",")))

risiko_kreise_6079 <- read_excel("~/Dokumente/covid-19/zigitlab/covid19dashboard/data/risiko_kreise_6079.xlsx", 
                               sheet = "Daten", skip = 3) %>%
  mutate(Wert=parse_number(Wert, locale = readr::locale(decimal_mark = ",")),
         Bundeswert=parse_number(Bundeswert, locale = readr::locale(decimal_mark = ",")))

risiko_kreise_1559 <- read_excel("~/Dokumente/covid-19/zigitlab/covid19dashboard/data/risiko_kreise_1559.xlsx", 
                               sheet = "Daten", skip = 3) %>%
  mutate(Wert=parse_number(Wert, locale = readr::locale(decimal_mark = ",")),
         Bundeswert=parse_number(Bundeswert, locale = readr::locale(decimal_mark = ",")))

risiko_gesamt <- left_join(risiko_kreise_80 %>%
                             mutate(`80+`=Wert, Bundeswert80=Bundeswert) %>%
                             select(Region, `Regions-ID`, `80+`),
                           risiko_kreise_6079 %>%
                             mutate(`60-79`=Wert, Bundeswert6079=Bundeswert) %>%
                             select(`Regions-ID`, `60-79`),
                           by="Regions-ID") %>%
  left_join(risiko_kreise_1559 %>%
              mutate(`15-59`=Wert, Bundeswert1559=Bundeswert) %>%
              select(`Regions-ID`, `15-59`),
            by="Regions-ID") %>%
  pivot_longer(cols=`80+`:`15-59`,
               names_to="Altersgruppe",
               values_to="AnzahlRisikogruppe") %>%
  mutate(`Regions-ID`=ifelse(`Regions-ID`==3152 | `Regions-ID`==3156, 3159, `Regions-ID`),
         Region=ifelse(`Regions-ID`==3159, "LK Göttingen", Region)) %>%
  group_by(`Regions-ID`, Region, Altersgruppe) %>%
  summarise(AnzahlRisikogruppe=sum(AnzahlRisikogruppe, na.rm=TRUE),
            .groups="drop")

rki_covidtode <- rki_original %>%
  mutate(IdLandkreis=as.integer(ifelse(IdLandkreis>="11000"&IdLandkreis<"12000","11000", IdLandkreis)),
         Altersgruppe=case_when(
           Altersgruppe=="A00-A04" | Altersgruppe=="A05-A14" ~ "0-15",
           Altersgruppe=="A15-A34" | Altersgruppe<="A35-A59" ~ "15-59",
           Altersgruppe=="A60-A79" ~ "60-79",
           Altersgruppe=="A80+" ~ "80+",
           TRUE ~ "unbekannt"
         )) %>%
  group_by(IdLandkreis, Altersgruppe) %>%
  summarise(AnzahlTodesfall=sum(AnzahlTodesfall[NeuerTodesfall>=0], na.rm=TRUE),
            Landkreis=Landkreis[1],
            .groups="drop")

rki_va <- full_join(risiko_gesamt, rki_covidtode, by=c("Regions-ID"="IdLandkreis", "Altersgruppe")) %>%
  select(-Region) %>%
  rename(ID_Landkreis=`Regions-ID`) %>%
  mutate(AnzahlRisikogruppe=ifelse(is.na(AnzahlRisikogruppe), 0, AnzahlRisikogruppe))

rki_va_gesamt <- rki_va %>%
  group_by(ID_Landkreis, Landkreis) %>%
  summarise(AnzahlRisikogruppe=sum(AnzahlRisikogruppe),
            AnzahlTodesfall=sum(AnzahlTodesfall),
            .groups="drop") %>%
  mutate(Verhaeltnis_TF_pro100k_RG=round(AnzahlTodesfall/AnzahlRisikogruppe*100000, 1))

rki_va_altersgruppen <- rki_va %>%
  filter(Altersgruppe!="unbekannt" & Altersgruppe!="0-15") %>%
  group_by(ID_Landkreis, Landkreis, Altersgruppe) %>%
  summarise(AnzahlRisikogruppe=sum(AnzahlRisikogruppe),
            AnzahlTodesfall=sum(AnzahlTodesfall),
            .groups="drop") %>%
  mutate(Verhaeltnis_TF_pro100k_RG=round(AnzahlTodesfall/AnzahlRisikogruppe*100000, 1))


list_of_datasets <- list("Gesamt (Todesfälle und Risikogruppen)"=rki_va_gesamt,
                         "Altersgruppen (Todesfälle und Risikogruppen)"=rki_va_altersgruppen)
write.xlsx(list_of_datasets, file = paste0("../data/kreise_versorgungsatlas_risikogruppen_vs_rki_todesfaelle.xlsx"))
