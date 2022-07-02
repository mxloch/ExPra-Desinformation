#Verteilungen
#Histogramm grouptime17759
#Histogramm FSumme

library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#separiere die beiden R01=1 & R01=2
surveyR01_1 <- subset(survey_892955, survey_892955$R01 == 1)
surveyR01_2 <- subset(survey_892955, survey_892955$R01 == 2)

#Histogramm für grouptime17759
hist(survey_892955$groupTime17759, freq = F, breaks = 20,
     xlab = "", ylab = "", main = "")

hist(surveyR01_1$groupTime17759, freq = F, breaks = 20,
     xlab = "", ylab = "", main = "")

hist(surveyR01_2$groupTime17759, freq = F, breaks = 20,
     xlab = "", ylab = "", main = "")

#Histogramm für FSumme
hist(survey_892955$FSumme, freq = F, breaks = 20,
     xlab = "", ylab = "", main = "")

hist(surveyR01_1$FSumme, freq = F, breaks = 20,
     xlab = "", ylab = "", main = "")

hist(surveyR01_2$FSumme, freq = F, breaks = 20,
     xlab = "", ylab = "", main = "")
