#Aufklärungstext nochmal anzeigen
#Auswertung, ob es Unterschiede zwischen den Gruppen gibt (E02_int)
#Chi-Quadrat-Test

library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")
#Anzeige Mittelwerte für E02_int
#mean of E02int
survey_892955%>%
  group_by(R01)%>% 
  summarise(Mean=mean(E02_int))

chisq.test(survey_892955$R01, survey_892955$E02_int)