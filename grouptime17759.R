#Analyse der Zeit mit der sich die Probanden mit dem Stimuli auseinandergesetzt (grouptime17759) haben
#Umfasst:
#Mittelwertsvergleiche deskriptiv
#Boxplot als grafische Darstellung
#Auf Normalverteilung prüfen (qqplot, Shapiro-Wilk Test, Kolmogorov Smirnov Test)
#auf Varianzhomogenität prüfen
#Inferenzstatistik mittels t-test

#install.packages("dplyr") 
library(dplyr)
library(ggplot2)

#import daten
library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#median by group for groupTime17759 - Faktencheck
survey_892955%>%
  group_by(R01)%>% 
  summarise(Mean=mean(groupTime17759), Max=max(groupTime17759), Min=min(groupTime17759), Median=median(groupTime17759), Std=sd(groupTime17759))

#boxplot grouptime17759
boxplot(survey_892955$groupTime17759)

#boxplot grouptime17759
boxplot(survey_892955$groupTime17759~survey_892955$R01)

#nomralverteilung, qqplot
qqnorm(completed_data$groupTime17759, xlab = "Theoretische Quantile", ylab = "Grouptime17759") 
qqline(completed_data$groupTime17759, col = "red")

#Kolmogorov Smirnov Test
z.grouptime17759 <- scale(survey_892955$groupTime17759)
ks.test(x = z.grouptime17759, y = pnorm)

#separiere die beiden R01=1 & R01=2
surveyR01_1 <- subset(survey_892955, survey_892955$R01 == 1)
surveyR01_2 <- subset(survey_892955, survey_892955$R01 == 2)

#F-Test
var.test(surveyR01_1$groupTime17759, surveyR01_2$groupTime17759)

#t-test based on R01(treatmen (as t) vs controllgroup (as c))
grouptime17759.t <- survey_892955$groupTime17759[survey_892955$R01 == 1]
grouptime17759.c <- survey_892955$groupTime17759[survey_892955$R01 == 2]
t.test(grouptime17759.t, grouptime17759.c, alternative='less', var.equal = TRUE, conf.level = 0.95)
