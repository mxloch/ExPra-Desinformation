#overview relevante daten

#Boxplot Daten
boxplot(survey_892955$groupTime17759)

#Boxplot Grouptime
boxplot(survey_892955$groupTime17759~survey_892955$R01)

#boxplot Manipulationscheck
boxplot(M01_int~R01, data = survey_892955)