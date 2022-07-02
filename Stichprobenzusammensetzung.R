#Bildungstand & Alter

library(dplyr)
#import daten
library(readr)
survey_892955 <- read.table(file='ProcessedData/survey_892955.csv', header=TRUE, sep = ";", dec = ",")

#bildungsstand
survey_892955 %>% count(D03)

#geschlecht
survey_892955 %>% count(D02)

#alter
survey_892955 %>% count(D01)

#R01
survey_892955 %>% count(R01)

#altersegemente
alter <- c()
i <- 1

for (D01 in survey_892955$D01) {
  if (D01 < 25)
    alter[i] = "18 - 24 Jahre"
  else if (D01 > 24 & D01 < 35)
    alter[i] = "25 - 34 Jahre"
  else if (D01 > 34 & D01 < 45)
    alter[i] = "35 - 44 Jahre"
  else if (D01 > 44 & D01 < 55)
    alter[i] = "45 - 54 Jahre"
  else if (D01 > 54 & D01 < 65)
    alter[i] = "55 - 64 Jahre"
  else {
    alter[i] = "65+ Jahre"
  }
  i = i+1
}

table(alter)