# source("functions.R")

## Kreise 7TI über 50 bzw 100, und unter 10, 0
sum(kreise_table$`7-Tage-Inzidenz`>=100)
sum(kreise_table$`7-Tage-Inzidenz`>=50)
sum(kreise_table$`7-Tage-Inzidenz`<=10)
sum(kreise_table$`7-Tage-Inzidenz`<1)
min(kreise_table$`7-Tage-Inzidenz`)
