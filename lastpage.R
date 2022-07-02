#lastpage
#zeigt wo die personen jeweils abgebrochen haben, R01

library(readr)
lastpage_csv <- read_delim("ProcessedData/lastpage.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
#absolute häufigkeit
table.all <- table(lastpage_csv$lastpage)
table.all
round(prop.table(table.all),2)

#based on R01(treatmen (as t) vs controllgroup (as c))
lastpage.t <- lastpage_csv$lastpage[lastpage_csv$R01 == 1]
lastpage.c <- lastpage_csv$lastpage[lastpage_csv$R01 == 2]

#absolute häufigkeiten
table.t <- table(lastpage.t)
table.c <- table(lastpage.c)

#relative häufigkeiten
round(prop.table(table.t),2)
round(prop.table(table.c),2)