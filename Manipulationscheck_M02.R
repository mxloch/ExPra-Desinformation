#manipulationscheck
#deskriptiv: Medianvergleich 
#boxplot
#U-Test von Mann-Whitney

#install.packages("coin")
library(coin)

library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#delete null values (codiert als 999)
count(survey_892955)
survey_892955 <- subset(survey_892955, survey_892955$M02_int != 999)
count(survey_892955)

#Median, Mean, Standardabweichung
survey_892955%>%
  group_by(R01)%>% 
  summarise(Mean=mean(M02_int),Median=median(M02_int), Std=sd(M02_int))

survey_892955%>%
  summarise(Mean=mean(M02_int),Median=median(M02_int), Std=sd(M02_int))

#boxplot
boxplot(M02_int~R01, data = survey_892955)

wilcox.test(M02_int~R01, data = survey_892955, exact = FALSE, correct = FALSE, conf.int = FALSE)

#t-test
M02.t <- survey_892955$M02_int[survey_892955$R01 == 1]
M02.c <- survey_892955$M02_int[survey_892955$R01 == 2]
t.test(M02.t, M02.c, var.equal = TRUE, conf.level = 0.95)

#t.test(M02.t, M02.c, var.equal = TRUE, alternative='less', conf.level = 0.95)
