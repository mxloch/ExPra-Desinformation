#Analyse der richtigen Antworten pro Proband (FSumme)
#Umfasst:
#Mittelwertsvergleiche deskriptiv
#Median-Test


#install.packages("dplyr") 
library(dplyr)
library(ggplot2)

#import daten
library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#Mittelwertsvergleiche deskriptiv
survey_892955%>%
  group_by(R01)%>% 
  summarise(Mean=mean(FSumme), Max=max(FSumme), Min=min(FSumme), Median=median(FSumme), Std=sd(FSumme))

#Boxplot FSumme
boxplot(survey_892955$FSumme~survey_892955$R01)

#nomralverteilung, qqplot
qqnorm(completed_data$FSumme, xlab = "Theoretische Quantile", ylab = "Grouptime17759") 
qqline(completed_data$FSumme, col = "red")

#Kolmogorov Smirnov Test
z.FSumme <- scale(survey_892955$FSumme)
ks.test(x = z.FSumme, y = pnorm)

#FTest
var.test(surveyR01_1$FSumme, surveyR01_2$FSumme)

#t-test based on FSumme(treatmen (as t) vs controllgroup (as c))
FSumme.t <- survey_892955$FSumme[survey_892955$R01 == 1]
FSumme.c <- survey_892955$FSumme[survey_892955$R01 == 2]
t.test(FSumme.t, FSumme.c, alternative='less', var.equal = TRUE, conf.level = 0.95)