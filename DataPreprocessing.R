#install.packages("dplyr") 
library(dplyr)
library(ggplot2)

#import daten
library(readr)
survey_892955_R_data_file <- read_delim("Rohdaten/survey_892955_R_data_file_010522.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

#filter by completed survey
lastpage_csv <- subset(survey_892955_R_data_file, !is.na(survey_892955_R_data_file$lastpage), select = c("id", "lastpage", "R01"))
completed_data <- filter(survey_892955_R_data_file, survey_892955_R_data_file['lastpage'] >= 9)
completed_data <- subset(completed_data, select = c("id", "submitdate", "lastpage", "D02", "D01", "D03", "R01", "P11", "P12", "M01[SQ001]", "M02[SQ001]", "S02[012]", "S02[022]", "F01", "F02", "F03", "F04", "F05", "E02", "groupTime17759"))
#completed_data <- filter(completed_data, completed_data['id'] != 93)

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

#delete NULL Values in P11 && P12
count(completed_data)

completed_data <- subset(completed_data, !(is.na(completed_data$P11) & is.na(completed_data$P12)))

count(completed_data)

#Gechlecht in int umwandeln (D02)
Sex_int <- c()
i <- 1
for (D02 in completed_data$D02) {
  if (D02 == 'D2A1'){
    Sex_int[i] = 0
  }else if (D02 == 'D2A2')
    Sex_int[i] = 1
  else {
    Sex_int[i] = 999
  }
  i <- i + 1}
completed_data$Sex_int <- Sex_int

#Bildung in int umwandeln (D03)
completed_data$D03[which(is.na(completed_data$D03))] <- 'NULL'

Education_int <- c()
i <- 1
for (D03 in completed_data$D03) {
  if (D03 == 'DQ031')
    Education_int[i] = 0
  else if (D03 == 'DQ032')
    Education_int[i] = 1
  else if (D03 == 'DQ033')
    Education_int[i] = 2
  else if (D03 == 'DQ034')
    Education_int[i] = 3
  else if (D03 == 'DQ036')
    Education_int[i] = 4
  else if (D03 == 'DQ037')
    Education_int[i] = 5
  else {
    Education_int[i] = 999
  }
  i <- i + 1}

completed_data$education_int <- Education_int

#Manipulationscheck
#replace NULL
completed_data$`M01[SQ001]`[which(is.na(completed_data$`M01[SQ001]`))] <- 'NULL'
#transfer
M01_int <- c()
i <- 1
for (M01 in completed_data$`M01[SQ001]`) {
  if (M01 == 'AO01')
    M01_int[i] = 1
  else if (M01 == 'AO02') 
    M01_int[i] = 2
  else if (M01 == 'AO03')
    M01_int[i] = 3
  else if (M01 == 'AO04')
    M01_int[i] = 4    
  else if (M01 == 'AO05')
    M01_int[i] = 5  
  else if (M01 == 'AO06')
    M01_int[i] = 6  
  else if (M01 == 'AO07')
    M01_int[i] = 7  
  else{
    M01_int[i] = 999
    }
  i = i+1
}

completed_data$M01_int <- M01_int

#Allgemeine Kontrollüberzeugung
#replace NULL
completed_data$`M02[SQ001]`[which(is.na(completed_data$`M02[SQ001]`))] <- 'NULL'
#transfer
M02_int <- c()
i <- 1
for (M02 in completed_data$`M02[SQ001]`) {
  if (M02 == 'AO01')
    M02_int[i] = 1
  else if (M02 == 'AO02') 
    M02_int[i] = 2
  else if (M02 == 'AO03')
    M02_int[i] = 3
  else if (M02 == 'AO04')
    M02_int[i] = 4    
  else if (M02 == 'AO05')
    M02_int[i] = 5  
  else if (M02 == 'AO06')
    M02_int[i] = 6  
  else if (M02 == 'AO07')
    M02_int[i] = 7  
  else{
    M02_int[i] = 999
  }
  i = i+1
}

completed_data$M02_int <- M02_int

#Glaubwürdigkeit These 1
#replace NULL
completed_data$`S02[012]`[which(is.na(completed_data$`S02[012]`))] <- 'NULL'
#transfer
S0212_int <- c()
i = 1

for (S02 in completed_data$`S02[012]`) {
  if (S02 == 'AO01')
    S0212_int[i] = 1
  else if (S02 == 'AO02') 
    S0212_int[i] = 2
  else if (S02 == 'AO03')
    S0212_int[i] = 3
  else if (S02 == 'AO04')
    S0212_int[i] = 4    
  else{
    S0212_int[i] = 2.5
  }
  i = i+1
}

completed_data$S0212_int <- S0212_int

#Glaubwürdigkeit These 2
#replace NULL
completed_data$`S02[022]`[which(is.na(completed_data$`S02[022]`))] <- 'NULL'
#transfer
S0222_int <- c()
i = 1

for (S02 in completed_data$`S02[022]`) {
  if (S02 == 'AO01')
    S0222_int[i] = 1
  else if (S02 == 'AO02') 
    S0222_int[i] = 2
  else if (S02 == 'AO03')
    S0222_int[i] = 3
  else if (S02 == 'AO04')
    S0222_int[i] = 4    
  else{
    S0222_int[i] = 2.5
  }
  i = i+1
}

completed_data$S0222_int <- S0222_int

#write as new csv
write_csv2(completed_data, "ProcessedData/survey_892955.csv")

#create second csv for lastpage
write_csv2(lastpage_csv, "ProcessedData/lastpage.csv")