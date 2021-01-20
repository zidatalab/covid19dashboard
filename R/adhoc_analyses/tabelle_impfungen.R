bl_table <- read_json("../../data/tabledata/bundeslaender_table.json", simplifyVector = TRUE) %>%
  select(Bundesland, `geimpfte Bevölkerung %`, `7-Tage-Inzidenz`, `7-Tage-Inzidenz 80+`, `Fälle insgesamt`) %>%
  distinct() %>%
  mutate(id=0:16)

pflegeheimbewohnende_2019_bundeslaender <- read_delim("../../data/pflegeheimbewohnende_2019_bundeslaender.csv", 
                                                      ";", escape_double = FALSE, trim_ws = TRUE)

bl_impftable <- bl_table %>%
  left_join(., kreise_regstat_alter, by="id") %>%
  left_join(., pflegeheimbewohnende_2019_bundeslaender, by=c("id"="Bundesland_ID")) %>%
  left_join(., vaccinations %>%
              filter(datum==max(vaccinations$datum)) %>%
              pivot_wider(id_cols=c("region"), names_from="metric", values_from="value"),
            by=c("Kuerzel"="region")) %>%
  mutate(AnteilInfiziert=`Fälle insgesamt`/(ag_1+ag_2+ag_3+ag_4+ag_5+ag_6)*100,
         geimpftAnteilPHB=pflegeheimbewohnerin/vollstationaere_dauerpflege*100,
         geimpftAnteilAlter=indikation_nach_alter/ag_6*100) %>%
  select(Bundesland.x, vollstationaere_dauerpflege, ag_6, `geimpfte Bevölkerung %`, `7-Tage-Inzidenz`, `7-Tage-Inzidenz 80+`, 
         impfungen_kumulativ, AnteilInfiziert, geimpftAnteilPHB, geimpftAnteilAlter)
library(openxlsx)
write.xlsx(bl_impftable, "../../data/kbvreport_export/impftabelle_vorschlag.xlsx")
