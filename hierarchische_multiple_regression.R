#Hiearchische multiple Regression:
#Kontrollvariablen (Bildung, Geschlecht: dummy codiert) als erster Block aufgenommen.
#eigentliche UV (Keine Kontrolle) in einem zweiten Block hinzugefügt.
#am Ende nochmal das Model mit z-standartisierte Variablen
#ist jedoch nicht zulässig, da ordinalskalierte und nominalskalierte Variablen

#vorbereitung
#Gechlecht in int umwandeln (D02)
#Bildung in int umwandeln (D03)

library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#delete null values (codiert als 999)
count(survey_892955)
survey_892955 <- subset(survey_892955, survey_892955$education_int != 999 & survey_892955$Sex_int != 999)
count(survey_892955)

#transfer R01 to Dummycodierung
R_dummy <- c()
i <- 1
for (R01 in survey_892955$R01) {
  R_dummy[i] = R01 - 1
  i = i +1
}
survey_892955$R01 <- R_dummy

#model
model0 <- lm(groupTime17759~education_int, data = survey_892955)
model1 <- lm(groupTime17759~Sex_int+education_int, data = survey_892955)
model2 <- lm(groupTime17759~Sex_int+education_int+R01, data = survey_892955)
model_generalControl <- lm(groupTime17759~M02_int, data=survey_892955)

summary(model0)
summary(model1)
summary(model2)
summary(model_generalControl)


#rsquared
summary(model1)$r.squared
summary(model2)$r.squared

#ab hier wahrscheinlich nicht zulässig
#zwerte
survey_892955$z_groupTime17759 <- scale(survey_892955$groupTime17759)
survey_892955$z_Sex_int <- scale(survey_892955$Sex_int)
survey_892955$z_education_int <- scale(survey_892955$education_int)
survey_892955$z_R01 <- scale(survey_892955$R01)

#model standartisiert
z_model1 <- lm(z_groupTime17759~z_Sex_int+z_education_int, data = survey_892955)
z_model2 <- lm(z_groupTime17759~z_Sex_int+z_education_int+z_R01, data = survey_892955)

summary(z_model1)
summary(z_model2)

#rsquare
summary(z_model1)$r.squared
summary(z_model2)$r.squared