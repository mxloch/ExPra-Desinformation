#Erstellt eine CorrelationsMatrix
#eventuell grafische Darstellung

#install.packages("corrplot")
library(dplyr)
library(ggplot2)
library(corrplot)

#import daten
library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#only use int/double data
survey_int <- subset(survey_892955, select = c("Sex_int", "D01", "education_int", "R01", "M01_int", "M02_int", "S0212_int", "S0222_int", "FSumme", "E02_int", "groupTime17759"))

#delete NULL values
count(survey_int)
survey_int <- subset(survey_int, survey_int$M01_int != 999)
count(survey_int)
survey_int <- subset(survey_int, survey_int$M02_int != 999)
count(survey_int)
survey_int <- subset(survey_int, survey_int$education_int != 999)
count(survey_int)

library(Hmisc)
rcorr(as.matrix(survey_int))

#visualtization
corrplot(cor(survey_int), method="number")