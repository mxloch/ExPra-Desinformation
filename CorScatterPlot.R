#Korellation Scatterplot
#1. Fsumme und Grouptime
#2. M01 & grouptime
#3. M02 & grouptime


library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

library("ggplot2")
library(dplyr)

#1. Fsumme und Grouptime
qplot(x = FSumme, y = groupTime17759, data = survey_892955, xlab = "Richtige Antworten", ylab = "Beschäftigungszeit") + 
  geom_smooth(method = "lm", formula = y ~ x)

#2. M01 & grouptime
count(survey_892955)
survey_892955 <- subset(survey_892955, survey_892955$M01_int != 999)
count(survey_892955)

qplot(x = M01_int, y = groupTime17759, data = survey_892955, xlab = "Kontrolle", ylab = "Beschäftigungszeit") + 
  geom_smooth(method = "lm", formula = y ~ x)

#3. M02 & grouptime
qplot(x = M02_int, y = groupTime17759, data = survey_892955, xlab = "Kontrollüberzeugung", ylab = "Beschäftigungszeit") + 
  geom_smooth(method = "lm", formula = y ~ x)

#4.M02 & FSumme
qplot(x = FSumme, y = M02_int, data = survey_892955, xlab = "Richtige Antworten", ylab = "Kontrollüberzeugung") + 
  geom_smooth(method = "lm", formula = y ~ x)


