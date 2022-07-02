#visualiserung
#Boxplot von grouptTime

library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#Boxplot Grouptime
boxplot(survey_892955$groupTime17759~survey_892955$R01,
        xlab = "Gruppen", ylab = "Zeit in Sekunden",  names = c("Geringe Kontrolle","Hohe Kontrolle"))

#hist
hist(surveyR01_1$groupTime17759, freq = F, breaks = 5)
hist(surveyR01_2$groupTime17759, freq = F, breaks = 5)

#Boxplot Manipulationscheck M01
#lösche NULLValues
survey_892955 <- subset(survey_892955, survey_892955$M01_int != 999)
summary(surveyR01_1$M01_int)
summary(surveyR01_2$M01_int)

#Boxplot M01_int
boxplot(M01_int~R01, data = survey_892955,
        xlab = "Gruppen", ylab = "Kontrolle ",  names = c("Geringe Kontrolle","Hohe Kontrolle"))

#Boxplot bei M02_int
boxplot(M02_int~R01, data = survey_892955,
        xlab = "Gruppen", ylab = "Kontrollüberzeugung",  names = c("Geringe Kontrolle","Hohe Kontrolle"))

#daten neu einlesen
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")
#boxplot Fsumme
boxplot(FSumme~R01, data = survey_892955,
        xlab = "Gruppen", ylab = "Summe richtiger Antworten",  names = c("Geringe Kontrolle","Hohe Kontrolle"))


#histogram FSumme

hist(surveyR01_1$FSumme, freq = F, breaks = 5,
     xlab = "Summe richtiger Antworten", ylab = "Relative Häufigkeit", main = "Summe richtiger Antworten der Gruppe \"geringe Kontrolle\""
     ,axes=TRUE)

hist(surveyR01_1$FSumme, freq = F, breaks = 5,
     xlab = "Summe richtiger Antworten", ylab = "Relative Häufigkeit", main = "Summe richtiger Antworten der Gruppe \"geringe Kontrolle\""
     ,axes=FALSE)
axis(2, at=seq(0, 0.4, 0.1))

hist(surveyR01_2$FSumme, freq = F, breaks = 5,
     xlab = "Summe richtiger Antworten", ylab = "Relative Häufigkeit", main = "Summe richtiger Antworten der Gruppe \"hohe Kontrolle\"")

