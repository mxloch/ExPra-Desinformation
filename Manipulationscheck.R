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
survey_892955 <- subset(survey_892955, survey_892955$M01_int != 999)
count(survey_892955)

#Median & Mean
survey_892955%>%
  group_by(R01)%>% 
  summarise(Mean=mean(M01_int),Median=median(M01_int))

#boxplot
boxplot(M01_int~R01, data = survey_892955)

wilcox.test(M01_int~R01, data = survey_892955, exact = FALSE, correct = FALSE, conf.int = FALSE)

outlierR01_M01 <- subset(survey_892955, survey_892955$R01 == 1  & survey_892955$M01_int >= 4)
View(outlierR01_M01)
outlierR02_M01 <- subset(survey_892955, survey_892955$R01 == 2  & survey_892955$M01_int <= 2)
View(outlierR02_M01)
survey_892955 <- subset(survey_892955, survey_892955$education_int != 999 & survey_892955$Sex_int != 999)