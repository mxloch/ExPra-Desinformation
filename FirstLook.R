#install.packages("dplyr") 
library(dplyr)
library(ggplot2)

#import daten
library(readr)
survey_892955_R_data_file <- read_delim("Rohdaten/survey_892955_R_data_file_010522.csv", 
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(survey_892955_R_data_file)

#data preprocessing


#filter by completed survey
completed_data <- filter(survey_892955_R_data_file, survey_892955_R_data_file['lastpage'] == 12)
completed_data <- subset(completed_data, select = c("id", "submitdate", "lastpage", "D02", "D01", "D03", "R01", "P11", "P12", "M01[SQ001]", "M02[SQ001]", "S02[012]", "S02[022]", "F01", "F02", "F03", "F04", "F05", "E02", "groupTime17759"))
#completed_data <- filter(completed_data, completed_data['id'] != 93)

#turn mobile(D05) into boolean

#turn answers from F01-F05 into int
F01_int <- c(); F02_int <- c(); F03_int <- c(); F04_int <- c(); F05_int <- c()

#replace NULL Values
completed_data$F01[which(is.na(completed_data$F01))] <- 'Null Value'
completed_data$F02[which(is.na(completed_data$F02))] <- 'Null Value'
completed_data$F03[which(is.na(completed_data$F03))] <- 'Null Value'
completed_data$F04[which(is.na(completed_data$F04))] <- 'Null Value'
completed_data$F05[which(is.na(completed_data$F05))] <- 'Null Value'

#F01
i <- 1
for (F01 in completed_data$F01) {
  if (F01 == 'F1A2'){
    F01_int[i] = 1
  }else {
    F01_int[i] = 0
  }
  i <- i + 1}


#F02
i <- 1
for (F02 in completed_data$F02) {
  if (F02 == 'F2A2'){
    F02_int[i] = 1
  }else {
    F02_int[i] = 0
  }
  i <- i + 1}

#F03
i <- 1
for (F03 in completed_data$F03) {
  if (F03 == 'F3A3'){
    F03_int[i] = 1
  }else {
    F03_int[i] = 0
  }
  i <- i + 1}

#F04
i <- 1
for (F04 in completed_data$F04) {
  if (F04 == 'F4A1'){
    F04_int[i] = 1
  }else {
    F04_int[i] = 0
  }
  i <- i + 1}

#F05
i <- 1
for (F05 in completed_data$F05) {
  if (F05 == 'F5A1'){
    F05_int[i] = 1
  }else {
    F05_int[i] = 0
  }
  i <- i + 1}

#add F01_int - F05_int to dataframe
completed_data$F01_int <- F01_int
completed_data$F02_int <- F02_int
completed_data$F03_int <- F03_int
completed_data$F04_int <- F04_int
completed_data$F05_int <- F05_int

#sum F01-F05
completed_data$FSumme <-ave(completed_data$F01_int + completed_data$F02_int + completed_data$F03_int + completed_data$F04_int + completed_data$F05_int, completed_data$id, FUN=cumsum)

completed_data%>%
  group_by(R01)%>% 
  summarise(Mean=mean(FSumme), Max=max(FSumme), Min=min(FSumme), Median=median(FSumme), Std=sd(FSumme))

#median by group for groupTime17759 - Faktencheck
completed_data%>%
  group_by(R01)%>% 
  summarise(Mean=mean(groupTime17759), Max=max(groupTime17759), Min=min(groupTime17759), Median=median(groupTime17759), Std=sd(groupTime17759))


#Transfer E02 into integer and group by
#replace NULL
completed_data$E02[which(is.na(completed_data$E02))] <- 'E2A2'

#create counter, create vector
i <- 1

#for loop E02 and add value to vector
E02_int <- c()
for (E02 in completed_data$E02) {
  if (E02 == 'E2A1'){
    E02_int[i] = 1
  }else {
    E02_int[i] = 0
  }
  i <- i + 1}

#add vector to dataframe
completed_data$E02_int <- E02_int

#mean of E02int
completed_data%>%
  group_by(R01)%>% 
  summarise(Mean=mean(E02_int))

#boxplot grouptime17759
boxplot(completed_data$groupTime17759~completed_data$R01)

#histogram FSumme
hist(completed_data$FSumme)

#nomralverteilung
qqnorm(completed_data$groupTime17759, xlab = "Theoretische Quantile", ylab = "Grouptime17759") 
qqline(completed_data$groupTime17759, col = "red")

#count R01
survey_892955_R_data_file %>% count(R01)
completed_data %>% count(R01)

#correlation between 
