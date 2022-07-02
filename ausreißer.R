#Ausreißer
#am ende exestieren 4 dataframes von Ausreißern:
#outlierR01_M01 Ausreißer bei Manipulationscheck von R01=1
#outlierR02_M01 Ausreißer bei Manipulationscheck von R01=2
#outlier_grouptime_R011 bei groupTime17759 von R01=1
#outlier_grouptime_R012 bei groupTime17759 von R01=2

library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#separiere die beiden R01=1 & R01=2
surveyR01_1 <- subset(survey_892955, survey_892955$R01 == 1)
surveyR01_2 <- subset(survey_892955, survey_892955$R01 == 2)

#Ausreißer beim Maninputlationscheck
#lösche NULLValues
survey_892955 <- subset(survey_892955, survey_892955$M01_int != 999)
summary(surveyR01_1$M01_int)
summary(surveyR01_2$M01_int)

#Ausreißer bei M01_int
boxplot(M01_int~R01, data = survey_892955)

outlierR01_M01 <- subset(survey_892955, survey_892955$R01 == 1  & survey_892955$M01_int >= 4)
View(outlierR01_M01)
outlierR02_M01 <- subset(survey_892955, survey_892955$R01 == 2  & survey_892955$M01_int <= 2)
View(outlierR02_M01)

#Ausreißer bei grouptime17759
summary(surveyR01_1$groupTime17759)
summary(surveyR01_2$groupTime17759)
#Boxplot Grouptime
boxplot(survey_892955$groupTime17759~survey_892955$R01)
#save values
Values_boxplot_R01_1 <- boxplot(surveyR01_1$groupTime17759)
Values_boxplot_R01_2 <- boxplot(surveyR01_2$groupTime17759)
#show values
Values_boxplot_R01_1$out
Values_boxplot_R01_2$out

#save data record
outlier_grouptime_R011 <- subset(surveyR01_1, surveyR01_1$groupTime17759 %in% Values_boxplot_R01_1$out)
outlier_grouptime_R012 <- subset(surveyR01_2, surveyR01_2$groupTime17759 %in% Values_boxplot_R01_2$out)
View(outlier_grouptime_R011)
View(outlier_grouptime_R012)