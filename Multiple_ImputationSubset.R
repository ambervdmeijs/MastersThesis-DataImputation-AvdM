
# Installing packages -----------------------------------------------------------------------------------------------------------------
#install.packages("mice")
#install.packages("readxl")
#install.packages("devtools")
#install.packages("dplyr")
#install.packages("plyr")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("mice")
library("readxl")
library("devtools")
# install_github("jabiru/tictoc")
library("tictoc")
library("dplyr")
library("plyr")


## Loading ipums data set to environment ----------------------------------------------------------------------------------------------------
ipums <- get(load("ipums.Rdata"))


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


# Setting correct and total to '0' 
SMI2.1_correct <- SMI2.2_correct <- SMI2.3_correct <- SMI5.1_correct <- SMI5.2_correct <- SMI5.3_correct <- SMI10.1_correct <- SMI10.2_correct <- SMI10.3_correct <- 0
SMI2.1_total <- SMI2.2_total <- SMI2.3_total <- SMI5.1_total <- SMI5.2_total <- SMI5.3_total <- SMI10.1_total <- SMI10.2_total <- SMI10.3_total <- 0


## Multiple Imputation with 'mice' ---------------------------------------------------------------------------------------------------

# 2% data sets

# 2.1 data set
MCAR2.1 <- get(load(file = "MCAR2_1.Rdata")) # plus get(load(sub_MCAR2.1)
sub_MCAR2.1 <- MCAR2.1[my_rows, c(1:12)]
SMI_MCAR2.1 <- data.frame(sub_MCAR2.1)
names(SMI_MCAR2.1) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR2.1)) {
  SMI_MCAR2.1[, i] <- as.factor(SMI_MCAR2.1[, i])
}
rm(MCAR2.1)
rm(sub_MCAR2.1)

tic("Multiple Imputation 2.1 processing time...")
Smultiple_imp2.1 <- mice(SMI_MCAR2.1, m = 5)
toc(log = TRUE)
Smultiple_imputation2.1 <- complete(Smultiple_imp2.1)
rm(SMI_MCAR2.1)

df_Smultiple_imputation2.1 <- as.data.frame(Smultiple_imputation2.1)
save(df_Smultiple_imputation2.1, file = "Smultiple_imputation21.Rdata")
rm(Smultiple_imp2.1)

SMI2.1_correct <- SMI2.1_correct + sum(sub_ipums == df_Smultiple_imputation2.1)
SMI2.1_total <- SMI2.1_total + sum(!is.na(df_Smultiple_imputation2.1))
rm(df_Smultiple_imputation2.1)

# 2.2 data set 
MCAR2.2 <- get(load(file = "MCAR2_2.Rdata"))
sub_MCAR2.2 <- MCAR2.2[my_rows, c(1:12)]
SMI_MCAR2.2 <- data.frame(sub_MCAR2.2)
names(SMI_MCAR2.2) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR2.2)) {
  SMI_MCAR2.2[, i] <- as.factor(SMI_MCAR2.2[, i])
}
rm(MCAR2.2)
rm(sub_MCAR2.2)

tic("Multiple Imputation 2.2 processing time...")
Smultiple_imp2.2 <- mice(SMI_MCAR2.2, m = 5)
toc(log = TRUE)
Smultiple_imputation2.2 <- complete(Smultiple_imp2.2)
rm(SMI_MCAR2.2)

df_Smultiple_imputation2.2 <- as.data.frame(Smultiple_imputation2.2)
save(df_Smultiple_imputation2.2, file = "Smultiple_imputation22.Rdata")
rm(Smultiple_imp2.2)

SMI2.2_correct <- SMI2.2_correct + sum(sub_ipums == df_Smultiple_imputation2.2)
SMI2.2_total <- SMI2.2_total + sum(!is.na(df_Smultiple_imputation2.2))
rm(df_Smultiple_imputation2.2)

# 2.3 data set 
MCAR2.3 <- get(load(file = "MCAR2_3.Rdata"))
sub_MCAR2.3 <- MCAR2.3[my_rows, c(1:12)]
SMI_MCAR2.3 <- data.frame(sub_MCAR2.3)
names(SMI_MCAR2.3) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR2.3)) {
  SMI_MCAR2.3[, i] <- as.factor(SMI_MCAR2.3[, i])
}
rm(MCAR2.3)
rm(sub_MCAR2.3)

tic("Multiple Imputation 2.3 processing time...")
Smultiple_imp2.3 <- mice(SMI_MCAR2.3, m = 5)
toc(log = TRUE)
Smultiple_imputation2.3 <- complete(Smultiple_imp2.3)
rm(SMI_MCAR2.3)

df_Smultiple_imputation2.3 <- as.data.frame(Smultiple_imputation2.3)
save(df_Smultiple_imputation2.3, file = "Smultiple_imputation23.Rdata")
rm(Smultiple_imp2.3)

SMI2.3_correct <- SMI2.3_correct + sum(sub_ipums == df_Smultiple_imputation2.3)
SMI2.3_total <- SMI2.3_total + sum(!is.na(df_Smultiple_imputation2.3))
rm(df_Smultiple_imputation2.3)


# 5% data sets 

# 5.1 data set
MCAR5.1 <- get(load(file = "MCAR5_1.Rdata"))
sub_MCAR5.1 <- MCAR5.1[my_rows, c(1:12)]
SMI_MCAR5.1 <- data.frame(sub_MCAR5.1)
names(SMI_MCAR5.1) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR5.1)) {
  SMI_MCAR5.1[, i] <- as.factor(SMI_MCAR5.1[, i])
}
rm(MCAR5.1)
rm(sub_MCAR5.1)

tic("Multiple Imputation 5.1 processing time...")
Smultiple_imp5.1 <- mice(SMI_MCAR5.1, m = 5)
toc(log = TRUE)
Smultiple_imputation5.1 <- complete(Smultiple_imp5.1)
rm(SMI_MCAR5.1)

df_Smultiple_imputation5.1 <- as.data.frame(Smultiple_imputation5.1)
save(df_Smultiple_imputation5.1, file = "Smultiple_imputation51.Rdata")
rm(Smultiple_imp5.1)

SMI5.1_correct <- SMI5.1_correct + sum(sub_ipums == df_Smultiple_imputation5.1)
SMI5.1_total <- SMI5.1_total + sum(!is.na(df_Smultiple_imputation5.1))
rm(df_Smultiple_imputation5.1)

# 5.2 data set 
MCAR5.2 <- get(load(file = "MCAR5_2.Rdata"))
sub_MCAR5.2 <- MCAR5.2[my_rows, c(1:12)]
SMI_MCAR5.2 <- data.frame(sub_MCAR5.2)
names(SMI_MCAR5.2) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR5.2)) {
  SMI_MCAR5.2[, i] <- as.factor(SMI_MCAR5.2[, i])
}
rm(MCAR5.2)
rm(sub_MCAR5.2)

tic("Multiple Imputation 5.2 processing time...")
Smultiple_imp5.2 <- mice(SMI_MCAR5.2, m = 5)
toc(log = TRUE)
Smultiple_imputation5.2 <- complete(Smultiple_imp5.2)
rm(SMI_MCAR5.2)

df_Smultiple_imputation5.2 <- as.data.frame(Smultiple_imputation5.2)
save(df_Smultiple_imputation5.2, file = "Smultiple_imputation52.Rdata")
rm(Smultiple_imp5.2)

SMI5.2_correct <- SMI5.2_correct + sum(sub_ipums == df_Smultiple_imputation5.2)
SMI5.2_total <- SMI5.2_total + sum(!is.na(df_Smultiple_imputation5.2))
rm(df_Smultiple_imputation5.2)

# 5.3 data set 
MCAR5.3 <- get(load(file = "MCAR5_3.Rdata"))
sub_MCAR5.3 <- MCAR5.3[my_rows, c(1:12)]
SMI_MCAR5.3 <- data.frame(sub_MCAR5.3)
names(SMI_MCAR5.3) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR5.3)) {
  SMI_MCAR5.3[, i] <- as.factor(SMI_MCAR5.3[, i])
}
rm(MCAR5.3)
rm(sub_MCAR5.3)

tic("Multiple Imputation 5.3 processing time...")
Smultiple_imp5.3 <- mice(SMI_MCAR5.3, m = 5)
toc(log = TRUE)
Smultiple_imputation5.3 <- complete(Smultiple_imp5.3)
rm(SMI_MCAR5.3)

df_Smultiple_imputation5.3 <- as.data.frame(Smultiple_imputation5.3)
save(df_Smultiple_imputation5.3, file = "Smultiple_imputation53.Rdata")
rm(Smultiple_imp5.3)

SMI5.3_correct <- SMI5.3_correct + sum(sub_ipums == df_Smultiple_imputation5.3)
SMI5.3_total <- SMI5.3_total + sum(!is.na(df_Smultiple_imputation5.3))
rm(df_Smultiple_imputation5.3)

# 10% data sets 

# 10.1 data set
MCAR10.1 <- get(load(file = "MCAR10_1.Rdata"))
sub_MCAR10.1 <- MCAR10.1[my_rows, c(1:12)]
SMI_MCAR10.1 <- data.frame(sub_MCAR10.1)
names(SMI_MCAR10.1) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR10.1)) {
  SMI_MCAR10.1[, i] <- as.factor(SMI_MCAR10.1[, i])
}
rm(MCAR10.1)
rm(sub_MCAR10.1)

tic("Multiple Imputation 10.1 processing time...")
Smultiple_imp10.1 <- mice(SMI_MCAR10.1, m = 5)
toc(log = TRUE)
Smultiple_imputation10.1 <- complete(Smultiple_imp10.1)
rm(SMI_MCAR10.1)

df_Smultiple_imputation10.1 <- as.data.frame(Smultiple_imputation10.1)
save(df_Smultiple_imputation10.1, file = "Smultiple_imputation101.Rdata")
rm(Smultiple_imp10.1)

SMI10.1_correct <- SMI10.1_correct + sum(sub_ipums == df_Smultiple_imputation10.1)
SMI10.1_total <- SMI10.1_total + sum(!is.na(df_Smultiple_imputation10.1))
rm(df_Smultiple_imputation10.1)

# 10.2 data set 
MCAR10.2 <- get(load(file = "MCAR10_2.Rdata"))
sub_MCAR10.2 <- MCAR10.2[my_rows, c(1:12)]
SMI_MCAR10.2 <- data.frame(sub_MCAR10.2)
names(SMI_MCAR10.2) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR10.2)) {
  SMI_MCAR10.2[, i] <- as.factor(SMI_MCAR10.2[, i])
}
rm(MCAR10.2)
rm(sub_MCAR10.2)

tic("Multiple Imputation 10.2 processing time...")
Smultiple_imp10.2 <- mice(SMI_MCAR10.2, m = 5)
toc(log = TRUE)
Smultiple_imputation10.2 <- complete(Smultiple_imp10.2)
rm(SMI_MCAR10.2)

df_Smultiple_imputation10.2 <- as.data.frame(Smultiple_imputation10.2)
save(df_Smultiple_imputation10.2, file = "Smultiple_imputation102.Rdata")
rm(Smultiple_imp10.2)

SMI10.2_correct <- SMI10.2_correct + sum(sub_ipums == df_Smultiple_imputation10.2)
SMI10.2_total <- SMI10.2_total + sum(!is.na(df_Smultiple_imputation10.2))
rm(df_Smultiple_imputation10.2)

# 10.3 data set 
MCAR10.3 <- get(load(file = "MCAR2_3.Rdata"))
sub_MCAR10.3 <- MCAR10.3[my_rows, c(1:12)]
SMI_MCAR10.3 <- data.frame(sub_MCAR10.3)
names(SMI_MCAR10.3) <- gsub(" ", "_", names(sub_ipums), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR10.3)) {
  SMI_MCAR10.3[, i] <- as.factor(SMI_MCAR10.3[, i])
}
rm(MCAR10.3)
rm(sub_MCAR10.3)

tic("Multiple Imputation 10.3 processing time...")
Smultiple_imp10.3 <- mice(SMI_MCAR10.3, m = 5)
toc(log = TRUE)
Smultiple_imputation10.3 <- complete(Smultiple_imp10.3)
rm(SMI_MCAR10.3)

df_Smultiple_imputation10.3 <- as.data.frame(Smultiple_imputation10.3)
save(df_Smultiple_imputation10.3, file = "Smultiple_imputation23.Rdata")
rm(Smultiple_imp10.3)

SMI10.3_correct <- SMI10.3_correct + sum(sub_ipums == df_Smultiple_imputation10.3)
SMI10.3_total <- SMI10.3_total + sum(!is.na(df_Smultiple_imputation10.3))
rm(df_Smultiple_imputation10.3)



# Check if all values are imputed 
# anyNA(c(df_multiple_imputation2.1, df_multiple_imputation2.2, df_multiple_imputation2.3, df_multiple_imputation5.1, df_multiple_imputation5.2, 
# df_multiple_imputation5.3, df_multiple_imputation10.1, df_multiple_imputation10.2, df_multiple_imputation10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------


# Computing the accuracy of imputation 
SMI2.1_accuracy <- SMI2.1_correct / SMI2.1_total
SMI2.2_accuracy <- SMI2.2_correct / SMI2.2_total
SMI2.3_accuracy <- SMI2.3_correct / SMI2.3_total

SMI5.1_accuracy <- SMI5.1_correct / SMI5.1_total
SMI5.2_accuracy <- SMI5.2_correct / SMI5.2_total
SMI5.3_accuracy <- SMI5.3_correct / SMI5.3_total

SMI10.1_accuracy <- SMI10.1_correct / SMI10.1_total
SMI10.2_accuracy <- SMI10.2_correct / SMI10.2_total
SMI10.3_accuracy <- SMI10.3_correct / SMI10.3_total


## Computing Bias -------------------------------------------------------------------------------------------------------


## Loading frequencies tabel
Geslacht <- get(load(file = "Freq_Geslacht.Rdata"))
Leeftijd <- get(load(file = "Freq_Leeftijd.Rdata"))
HH_Pos <- get(load(file = "Freq_HH_Pos.Rdata"))
HH_grootte <- get(load(file = "Freq_HH_grootte.Rdata"))
Woonregio_vorig_jaar <- get(load(file = "Freq_Woonregio.Rdata"))
Nationaliteit <- get(load(file = "Freq_Nationaliteit.Rdata"))
Geboorteland <- get(load(file = "Freq_Geboorteland.Rdata"))
Onderwijsniveau <- get(load(file = "Freq_Onderwijsniveau.Rdata"))
Econ_status <- get(load(file = "Freq_Econstatus.Rdata"))
Econ_status$Econ._status <- as.factor(Econ_status$Econ._status)
Beroep <- get(load(file = "Freq_Beroep.Rdata"))
SBI <- get(load(file = "Freq_SBI.Rdata"))
Burg_Staat <- get(load(file = "Freq_Burgstaat.Rdata"))
Burg_Staat$Burg._Staat <- as.factor(Burg_Staat$Burg._Staat)


# Computing Bias 
Bias <- function(df1, df2, column){
  Bdf <- merge(df1, df2, by = column, all = TRUE)
  Bdf$result <- Bdf["freq.x"] - Bdf["freq.y"]
  Bdf$percentage <- (Bdf["result"]/Bdf["freq.y"])*100
  Bdf$percentageABS <- abs(Bdf$percentage)
  return(Bdf)
}

Bias2 <- function(df1, df2, column){
  Bdf <- merge(df1, df2, by = column, all = TRUE)
  Bdf[1][is.na(Bdf[1])] <- 0 
  Bdf$freq.x[is.na(Bdf$freq.x)] <- 0
  Bdf$freq.y[is.na(Bdf$freq.y)] <- 0
  #Bdf$result <- Bdf["freq.x"] - Bdf["freq.y"]
  Bdf$result <- ifelse(Bdf$freq.y == 0, Bdf$freq.x, Bdf$freq.x - Bdf$freq.y)
  Bdf$percentage <- ifelse(Bdf$freq.y == 0, 100.000000, (Bdf$result/Bdf$freq.y)*100)
  #Bdf$percentage <- (Bdf["result"]/Bdf["freq.y"])*100
  Bdf$percentageABS <- abs(Bdf$percentage)
  return(Bdf)
}



# 2% data set

# Counting values
S10MI2.1_Geslacht <- plyr::count(df_Smultiple_imputation2.1, 'Geslacht')
S10MI2.1_Leeftijd <- plyr::count(df_Smultiple_imputation2.1, 'Leeftijd')
S10MI2.1_HH_Pos <- plyr::count(df_Smultiple_imputation2.1, 'HH_Pos')
S10MI2.1_HH_grootte <- plyr::count(df_Smultiple_imputation2.1, 'HH_grootte')
S10MI2.1_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation2.1, 'Woonregio_vorig_jaar')
S10MI2.1_Nationaliteit <- plyr::count(df_Smultiple_imputation2.1, 'Nationaliteit')
S10MI2.1_Geboorteland <- plyr::count(df_Smultiple_imputation2.1, 'Geboorteland')
S10MI2.1_Onderwijsniveau <- plyr::count(df_Smultiple_imputation2.1, 'Onderwijsniveau')
S10MI2.1_Econ_status <- plyr::count(df_Smultiple_imputation2.1, 'Econ._status')
S10MI2.1_Beroep <- plyr::count(df_Smultiple_imputation2.1, 'Beroep')
S10MI2.1_SBI <- plyr::count(df_Smultiple_imputation2.1, 'SBI')
S10MI2.1_Burg_Staat <- plyr::count(df_Smultiple_imputation2.1, 'Burg._Staat')

S10MI2.2_Geslacht <- plyr::count(df_Smultiple_imputation2.2, 'Geslacht')
S10MI2.2_Leeftijd <- plyr::count(df_Smultiple_imputation2.2, 'Leeftijd')
S10MI2.2_HH_Pos <- plyr::count(df_Smultiple_imputation2.2, 'HH_Pos')
S10MI2.2_HH_grootte <- plyr::count(df_Smultiple_imputation2.2, 'HH_grootte')
S10MI2.2_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation2.2, 'Woonregio_vorig_jaar')
S10MI2.2_Nationaliteit <- plyr::count(df_Smultiple_imputation2.2, 'Nationaliteit')
S10MI2.2_Geboorteland <- plyr::count(df_Smultiple_imputation2.2, 'Geboorteland')
S10MI2.2_Onderwijsniveau <- plyr::count(df_Smultiple_imputation2.2, 'Onderwijsniveau')
S10MI2.2_Econ_status <- plyr::count(df_Smultiple_imputation2.2, 'Econ._status')
S10MI2.2_Beroep <- plyr::count(df_Smultiple_imputation2.2, 'Beroep')
S10MI2.2_SBI <- plyr::count(df_Smultiple_imputation2.2, 'SBI')
S10MI2.2_Burg_Staat <- plyr::count(df_Smultiple_imputation2.2, 'Burg._Staat')

S10MI2.3_Geslacht <- plyr::count(df_Smultiple_imputation2.3, 'Geslacht')
S10MI2.3_Leeftijd <- plyr::count(df_Smultiple_imputation2.3, 'Leeftijd')
S10MI2.3_HH_Pos <- plyr::count(df_Smultiple_imputation2.3, 'HH_Pos')
S10MI2.3_HH_grootte <- plyr::count(df_Smultiple_imputation2.3, 'HH_grootte')
S10MI2.3_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation2.3, 'Woonregio_vorig_jaar')
S10MI2.3_Nationaliteit <- plyr::count(df_Smultiple_imputation2.3, 'Nationaliteit')
S10MI2.3_Geboorteland <- plyr::count(df_Smultiple_imputation2.3, 'Geboorteland')
S10MI2.3_Onderwijsniveau <- plyr::count(df_Smultiple_imputation2.3, 'Onderwijsniveau')
S10MI2.3_Econ_status <- plyr::count(df_Smultiple_imputation2.3, 'Econ._status')
S10MI2.3_Beroep <- plyr::count(df_Smultiple_imputation2.3, 'Beroep')
S10MI2.3_SBI <- plyr::count(df_Smultiple_imputation2.3, 'SBI')
S10MI2.3_Burg_Staat <- plyr::count(df_Smultiple_imputation2.3, 'Burg._Staat')

rm(df_Smultiple_imputation2.1, df_Smultiple_imputation2.2, df_Smultiple_imputation2.3)

# 2.1 data set 
BS10MI2.1_Geslacht <- Bias(S10MI2.1_Geslacht, Geslacht, "Geslacht")
BOS10MI2.1_Geslacht <- sum(BS10MI2.1_Geslacht$percentageABS)/nrow(BS10MI2.1_Geslacht)

BS10MI2.1_Leeftijd <- Bias(S10MI2.1_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI2.1_Leeftijd <- sum(BS10MI2.1_Leeftijd$percentageABS)/nrow(BS10MI2.1_Leeftijd)

BS10MI2.1_HH_Pos <- Bias(S10MI2.1_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI2.1_HH_Pos <- sum(BS10MI2.1_HH_Pos$percentageABS)/nrow(BS10MI2.1_HH_Pos)

BS10MI2.1_HH_grootte <- Bias(S10MI2.1_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI2.1_HH_grootte <- sum(BS10MI2.1_HH_grootte$percentageABS)/nrow(BS10MI2.1_HH_grootte)

BS10MI2.1_Woonregio_vorig_jaar <- Bias(S10MI2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI2.1_Woonregio_vorig_jaar <- sum(BS10MI2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI2.1_Woonregio_vorig_jaar)

BS10MI2.1_Nationaliteit <- Bias(S10MI2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI2.1_Nationaliteit <- sum(BS10MI2.1_Nationaliteit$percentageABS)/nrow(BS10MI2.1_Nationaliteit)

BS10MI2.1_Geboorteland <- Bias(S10MI2.1_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI2.1_Geboorteland <- sum(BS10MI2.1_Geboorteland$percentageABS)/nrow(BS10MI2.1_Geboorteland)

BS10MI2.1_Onderwijsniveau <- Bias(S10MI2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI2.1_Onderwijsniveau <- sum(BS10MI2.1_Onderwijsniveau$percentageABS)/nrow(BS10MI2.1_Onderwijsniveau)

BS10MI2.1_Econ_status <- Bias2(S10MI2.1_Econ_status, Econ_status, "Econ._status")
BOS10MI2.1_Econ_status <- sum(BS10MI2.1_Econ_status$percentageABS)/nrow(BS10MI2.1_Econ_status)

BS10MI2.1_Beroep <- Bias(S10MI2.1_Beroep, Beroep, "Beroep")
BOS10MI2.1_Beroep <- sum(BS10MI2.1_Beroep$percentageABS)/nrow(BS10MI2.1_Beroep)

BS10MI2.1_SBI <- Bias(S10MI2.1_SBI, SBI, "SBI")
BOS10MI2.1_SBI <- sum(BS10MI2.1_SBI$percentageABS)/nrow(BS10MI2.1_SBI)

BS10MI2.1_Burg_Staat <- Bias2(S10MI2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI2.1_Burg_Staat <- sum(BS10MI2.1_Burg_Staat$percentageABS)/nrow(BS10MI2.1_Burg_Staat)

BiasOverallS10MI2.1 <- sum(BOS10MI2.1_Geslacht, BOS10MI2.1_Leeftijd, BOS10MI2.1_HH_Pos, BOS10MI2.1_HH_grootte, BOS10MI2.1_Woonregio_vorig_jaar, 
                        BOS10MI2.1_Nationaliteit, BOS10MI2.1_Geboorteland, BOS10MI2.1_Onderwijsniveau, BOS10MI2.1_Econ_status, 
                        BOS10MI2.1_Beroep, BOS10MI2.1_SBI, BOS10MI2.1_Burg_Staat) / 12

rm(S10MI2.1_Geslacht, S10MI2.1_Leeftijd, S10MI2.1_HH_Pos, S10MI2.1_HH_grootte, S10MI2.1_Woonregio_vorig_jaar, 
   S10MI2.1_Nationaliteit, S10MI2.1_Geboorteland, S10MI2.1_Onderwijsniveau, S10MI2.1_Econ_status, 
   S10MI2.1_Beroep, S10MI2.1_SBI, S10MI2.1_Burg_Staat)
rm(BS10MI2.1_Geslacht, BS10MI2.1_Leeftijd, BS10MI2.1_HH_Pos, BS10MI2.1_HH_grootte, BS10MI2.1_Woonregio_vorig_jaar, 
   BS10MI2.1_Nationaliteit, BS10MI2.1_Geboorteland, BS10MI2.1_Onderwijsniveau, BS10MI2.1_Econ_status, 
   BS10MI2.1_Beroep, BS10MI2.1_SBI, BS10MI2.1_Burg_Staat)
rm(BOS10MI2.1_Geslacht, BOS10MI2.1_Leeftijd, BOS10MI2.1_HH_Pos, BOS10MI2.1_HH_grootte, BOS10MI2.1_Woonregio_vorig_jaar, 
   BOS10MI2.1_Nationaliteit, BOS10MI2.1_Geboorteland, BOS10MI2.1_Onderwijsniveau, BOS10MI2.1_Econ_status, 
   BOS10MI2.1_Beroep, BOS10MI2.1_SBI, BOS10MI2.1_Burg_Staat)

# 2.2 data set 
BS10MI2.2_Geslacht <- Bias(S10MI2.2_Geslacht, Geslacht, "Geslacht")
BOS10MI2.2_Geslacht <- sum(BS10MI2.2_Geslacht$percentageABS)/nrow(BS10MI2.2_Geslacht)

BS10MI2.2_Leeftijd <- Bias(S10MI2.2_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI2.2_Leeftijd <- sum(BS10MI2.2_Leeftijd$percentageABS)/nrow(BS10MI2.2_Leeftijd)

BS10MI2.2_HH_Pos <- Bias(S10MI2.2_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI2.2_HH_Pos <- sum(BS10MI2.2_HH_Pos$percentageABS)/nrow(BS10MI2.2_HH_Pos)

BS10MI2.2_HH_grootte <- Bias(S10MI2.2_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI2.2_HH_grootte <- sum(BS10MI2.2_HH_grootte$percentageABS)/nrow(BS10MI2.2_HH_grootte)

BS10MI2.2_Woonregio_vorig_jaar <- Bias(S10MI2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI2.2_Woonregio_vorig_jaar <- sum(BS10MI2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI2.2_Woonregio_vorig_jaar)

BS10MI2.2_Nationaliteit <- Bias(S10MI2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI2.2_Nationaliteit <- sum(BS10MI2.2_Nationaliteit$percentageABS)/nrow(BS10MI2.2_Nationaliteit)

BS10MI2.2_Geboorteland <- Bias(S10MI2.2_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI2.2_Geboorteland <- sum(BS10MI2.2_Geboorteland$percentageABS)/nrow(BS10MI2.2_Geboorteland)

BS10MI2.2_Onderwijsniveau <- Bias(S10MI2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI2.2_Onderwijsniveau <- sum(BS10MI2.2_Onderwijsniveau$percentageABS)/nrow(BS10MI2.2_Onderwijsniveau)

BS10MI2.2_Econ_status <- Bias2(S10MI2.2_Econ_status, Econ_status, "Econ._status")
BOS10MI2.2_Econ_status <- sum(BS10MI2.2_Econ_status$percentageABS)/nrow(BS10MI2.2_Econ_status)

BS10MI2.2_Beroep <- Bias(S10MI2.2_Beroep, Beroep, "Beroep")
BOS10MI2.2_Beroep <- sum(BS10MI2.2_Beroep$percentageABS)/nrow(BS10MI2.2_Beroep)

BS10MI2.2_SBI <- Bias(S10MI2.2_SBI, SBI, "SBI")
BOS10MI2.2_SBI <- sum(BS10MI2.2_SBI$percentageABS)/nrow(BS10MI2.2_SBI)

BS10MI2.2_Burg_Staat <- Bias2(S10MI2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI2.2_Burg_Staat <- sum(BS10MI2.2_Burg_Staat$percentageABS)/nrow(BS10MI2.2_Burg_Staat)

BiasOverallS10MI2.2 <- sum(BOS10MI2.2_Geslacht, BOS10MI2.2_Leeftijd, BOS10MI2.2_HH_Pos, BOS10MI2.2_HH_grootte, BOS10MI2.2_Woonregio_vorig_jaar, 
                        BOS10MI2.2_Nationaliteit, BOS10MI2.2_Geboorteland, BOS10MI2.2_Onderwijsniveau, BOS10MI2.2_Econ_status, 
                        BOS10MI2.2_Beroep, BOS10MI2.2_SBI, BOS10MI2.2_Burg_Staat) / 12

rm(S10MI2.2_Geslacht, S10MI2.2_Leeftijd, S10MI2.2_HH_Pos, S10MI2.2_HH_grootte, S10MI2.2_Woonregio_vorig_jaar, 
   S10MI2.2_Nationaliteit, S10MI2.2_Geboorteland, S10MI2.2_Onderwijsniveau, S10MI2.2_Econ_status, 
   S10MI2.2_Beroep, S10MI2.2_SBI, S10MI2.2_Burg_Staat)
rm(BS10MI2.2_Geslacht, BS10MI2.2_Leeftijd, BS10MI2.2_HH_Pos, BS10MI2.2_HH_grootte, BS10MI2.2_Woonregio_vorig_jaar, 
   BS10MI2.2_Nationaliteit, BS10MI2.2_Geboorteland, BS10MI2.2_Onderwijsniveau, BS10MI2.2_Econ_status, 
   BS10MI2.2_Beroep, BS10MI2.2_SBI, BS10MI2.2_Burg_Staat)
rm(BOS10MI2.2_Geslacht, BOS10MI2.2_Leeftijd, BOS10MI2.2_HH_Pos, BOS10MI2.2_HH_grootte, BOS10MI2.2_Woonregio_vorig_jaar, 
   BOS10MI2.2_Nationaliteit, BOS10MI2.2_Geboorteland, BOS10MI2.2_Onderwijsniveau, BOS10MI2.2_Econ_status, 
   BOS10MI2.2_Beroep, BOS10MI2.2_SBI, BOS10MI2.2_Burg_Staat)

# 2.3 data set 
BS10MI2.3_Geslacht <- Bias(S10MI2.3_Geslacht, Geslacht, "Geslacht")
BOS10MI2.3_Geslacht <- sum(BS10MI2.3_Geslacht$percentageABS)/nrow(BS10MI2.3_Geslacht)

BS10MI2.3_Leeftijd <- Bias(S10MI2.3_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI2.3_Leeftijd <- sum(BS10MI2.3_Leeftijd$percentageABS)/nrow(BS10MI2.3_Leeftijd)

BS10MI2.3_HH_Pos <- Bias(S10MI2.3_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI2.3_HH_Pos <- sum(BS10MI2.3_HH_Pos$percentageABS)/nrow(BS10MI2.3_HH_Pos)

BS10MI2.3_HH_grootte <- Bias(S10MI2.3_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI2.3_HH_grootte <- sum(BS10MI2.3_HH_grootte$percentageABS)/nrow(BS10MI2.3_HH_grootte)

BS10MI2.3_Woonregio_vorig_jaar <- Bias(S10MI2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI2.3_Woonregio_vorig_jaar <- sum(BS10MI2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI2.3_Woonregio_vorig_jaar)

BS10MI2.3_Nationaliteit <- Bias(S10MI2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI2.3_Nationaliteit <- sum(BS10MI2.3_Nationaliteit$percentageABS)/nrow(BS10MI2.3_Nationaliteit)

BS10MI2.3_Geboorteland <- Bias(S10MI2.3_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI2.3_Geboorteland <- sum(BS10MI2.3_Geboorteland$percentageABS)/nrow(BS10MI2.3_Geboorteland)

BS10MI2.3_Onderwijsniveau <- Bias(S10MI2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI2.3_Onderwijsniveau <- sum(BS10MI2.3_Onderwijsniveau$percentageABS)/nrow(BS10MI2.3_Onderwijsniveau)

BS10MI2.3_Econ_status <- Bias2(S10MI2.3_Econ_status, Econ_status, "Econ._status")
BOS10MI2.3_Econ_status <- sum(BS10MI2.3_Econ_status$percentageABS)/nrow(BS10MI2.3_Econ_status)

BS10MI2.3_Beroep <- Bias(S10MI2.3_Beroep, Beroep, "Beroep")
BOS10MI2.3_Beroep <- sum(BS10MI2.3_Beroep$percentageABS)/nrow(BS10MI2.3_Beroep)

BS10MI2.3_SBI <- Bias(S10MI2.3_SBI, SBI, "SBI")
BOS10MI2.3_SBI <- sum(BS10MI2.3_SBI$percentageABS)/nrow(BS10MI2.3_SBI)

BS10MI2.3_Burg_Staat <- Bias2(S10MI2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI2.3_Burg_Staat <- sum(BS10MI2.3_Burg_Staat$percentageABS)/nrow(BS10MI2.3_Burg_Staat)

BiasOverallS10MI2.3 <- sum(BOS10MI2.3_Geslacht, BOS10MI2.3_Leeftijd, BOS10MI2.3_HH_Pos, BOS10MI2.3_HH_grootte, BOS10MI2.3_Woonregio_vorig_jaar, 
                        BOS10MI2.3_Nationaliteit, BOS10MI2.3_Geboorteland, BOS10MI2.3_Onderwijsniveau, BOS10MI2.3_Econ_status, 
                        BOS10MI2.3_Beroep, BOS10MI2.3_SBI, BOS10MI2.3_Burg_Staat) / 12

rm(S10MI2.3_Geslacht, S10MI2.3_Leeftijd, S10MI2.3_HH_Pos, S10MI2.3_HH_grootte, S10MI2.3_Woonregio_vorig_jaar, 
   S10MI2.3_Nationaliteit, S10MI2.3_Geboorteland, S10MI2.3_Onderwijsniveau, S10MI2.3_Econ_status, 
   S10MI2.3_Beroep, S10MI2.3_SBI, S10MI2.3_Burg_Staat)
rm(BS10MI2.3_Geslacht, BS10MI2.3_Leeftijd, BS10MI2.3_HH_Pos, BS10MI2.3_HH_grootte, BS10MI2.3_Woonregio_vorig_jaar, 
   BS10MI2.3_Nationaliteit, BS10MI2.3_Geboorteland, BS10MI2.3_Onderwijsniveau, BS10MI2.3_Econ_status, 
   BS10MI2.3_Beroep, BS10MI2.3_SBI, BS10MI2.3_Burg_Staat)
rm(BOS10MI2.3_Geslacht, BOS10MI2.3_Leeftijd, BOS10MI2.3_HH_Pos, BOS10MI2.3_HH_grootte, BOS10MI2.3_Woonregio_vorig_jaar, 
   BOS10MI2.3_Nationaliteit, BOS10MI2.3_Geboorteland, BOS10MI2.3_Onderwijsniveau, BOS10MI2.3_Econ_status, 
   BOS10MI2.3_Beroep, BOS10MI2.3_SBI, BOS10MI2.3_Burg_Staat)


# 5% data sets

# Counting values
S10MI5.1_Geslacht <- plyr::count(df_Smultiple_imputation5.1, 'Geslacht')
S10MI5.1_Leeftijd <- plyr::count(df_Smultiple_imputation5.1, 'Leeftijd')
S10MI5.1_HH_Pos <- plyr::count(df_Smultiple_imputation5.1, 'HH_Pos')
S10MI5.1_HH_grootte <- plyr::count(df_Smultiple_imputation5.1, 'HH_grootte')
S10MI5.1_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation5.1, 'Woonregio_vorig_jaar')
S10MI5.1_Nationaliteit <- plyr::count(df_Smultiple_imputation5.1, 'Nationaliteit')
S10MI5.1_Geboorteland <- plyr::count(df_Smultiple_imputation5.1, 'Geboorteland')
S10MI5.1_Onderwijsniveau <- plyr::count(df_Smultiple_imputation5.1, 'Onderwijsniveau')
S10MI5.1_Econ_status <- plyr::count(df_Smultiple_imputation5.1, 'Econ._status')
S10MI5.1_Beroep <- plyr::count(df_Smultiple_imputation5.1, 'Beroep')
S10MI5.1_SBI <- plyr::count(df_Smultiple_imputation5.1, 'SBI')
S10MI5.1_Burg_Staat <- plyr::count(df_Smultiple_imputation5.1, 'Burg._Staat')

S10MI5.2_Geslacht <- plyr::count(df_Smultiple_imputation5.2, 'Geslacht')
S10MI5.2_Leeftijd <- plyr::count(df_Smultiple_imputation5.2, 'Leeftijd')
S10MI5.2_HH_Pos <- plyr::count(df_Smultiple_imputation5.2, 'HH_Pos')
S10MI5.2_HH_grootte <- plyr::count(df_Smultiple_imputation5.2, 'HH_grootte')
S10MI5.2_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation5.2, 'Woonregio_vorig_jaar')
S10MI5.2_Nationaliteit <- plyr::count(df_Smultiple_imputation5.2, 'Nationaliteit')
S10MI5.2_Geboorteland <- plyr::count(df_Smultiple_imputation5.2, 'Geboorteland')
S10MI5.2_Onderwijsniveau <- plyr::count(df_Smultiple_imputation5.2, 'Onderwijsniveau')
S10MI5.2_Econ_status <- plyr::count(df_Smultiple_imputation5.2, 'Econ._status')
S10MI5.2_Beroep <- plyr::count(df_Smultiple_imputation5.2, 'Beroep')
S10MI5.2_SBI <- plyr::count(df_Smultiple_imputation5.2, 'SBI')
S10MI5.2_Burg_Staat <- plyr::count(df_Smultiple_imputation5.2, 'Burg._Staat')

S10MI5.3_Geslacht <- plyr::count(df_Smultiple_imputation5.3, 'Geslacht')
S10MI5.3_Leeftijd <- plyr::count(df_Smultiple_imputation5.3, 'Leeftijd')
S10MI5.3_HH_Pos <- plyr::count(df_Smultiple_imputation5.3, 'HH_Pos')
S10MI5.3_HH_grootte <- plyr::count(df_Smultiple_imputation5.3, 'HH_grootte')
S10MI5.3_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation5.3, 'Woonregio_vorig_jaar')
S10MI5.3_Nationaliteit <- plyr::count(df_Smultiple_imputation5.3, 'Nationaliteit')
S10MI5.3_Geboorteland <- plyr::count(df_Smultiple_imputation5.3, 'Geboorteland')
S10MI5.3_Onderwijsniveau <- plyr::count(df_Smultiple_imputation5.3, 'Onderwijsniveau')
S10MI5.3_Econ_status <- plyr::count(df_Smultiple_imputation5.3, 'Econ._status')
S10MI5.3_Beroep <- plyr::count(df_Smultiple_imputation5.3, 'Beroep')
S10MI5.3_SBI <- plyr::count(df_Smultiple_imputation5.3, 'SBI')
S10MI5.3_Burg_Staat <- plyr::count(df_Smultiple_imputation5.3, 'Burg._Staat')

rm(df_Smultiple_imputation5.1, df_Smultiple_imputation5.2, df_Smultiple_imputation5.3)


# 5.1 data set 
BS10MI5.1_Geslacht <- Bias(S10MI5.1_Geslacht, Geslacht, "Geslacht")
BOS10MI5.1_Geslacht <- sum(BS10MI5.1_Geslacht$percentageABS)/nrow(BS10MI5.1_Geslacht)

BS10MI5.1_Leeftijd <- Bias(S10MI5.1_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI5.1_Leeftijd <- sum(BS10MI5.1_Leeftijd$percentageABS)/nrow(BS10MI5.1_Leeftijd)

BS10MI5.1_HH_Pos <- Bias(S10MI5.1_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI5.1_HH_Pos <- sum(BS10MI5.1_HH_Pos$percentageABS)/nrow(BS10MI5.1_HH_Pos)

BS10MI5.1_HH_grootte <- Bias(S10MI5.1_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI5.1_HH_grootte <- sum(BS10MI5.1_HH_grootte$percentageABS)/nrow(BS10MI5.1_HH_grootte)

BS10MI5.1_Woonregio_vorig_jaar <- Bias(S10MI5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI5.1_Woonregio_vorig_jaar <- sum(BS10MI5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI5.1_Woonregio_vorig_jaar)

BS10MI5.1_Nationaliteit <- Bias(S10MI5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI5.1_Nationaliteit <- sum(BS10MI5.1_Nationaliteit$percentageABS)/nrow(BS10MI5.1_Nationaliteit)

BS10MI5.1_Geboorteland <- Bias(S10MI5.1_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI5.1_Geboorteland <- sum(BS10MI5.1_Geboorteland$percentageABS)/nrow(BS10MI5.1_Geboorteland)

BS10MI5.1_Onderwijsniveau <- Bias(S10MI5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI5.1_Onderwijsniveau <- sum(BS10MI5.1_Onderwijsniveau$percentageABS)/nrow(BS10MI5.1_Onderwijsniveau)

BS10MI5.1_Econ_status <- Bias2(S10MI5.1_Econ_status, Econ_status, "Econ._status")
BOS10MI5.1_Econ_status <- sum(BS10MI5.1_Econ_status$percentageABS)/nrow(BS10MI5.1_Econ_status)

BS10MI5.1_Beroep <- Bias(S10MI5.1_Beroep, Beroep, "Beroep")
BOS10MI5.1_Beroep <- sum(BS10MI5.1_Beroep$percentageABS)/nrow(BS10MI5.1_Beroep)

BS10MI5.1_SBI <- Bias(S10MI5.1_SBI, SBI, "SBI")
BOS10MI5.1_SBI <- sum(BS10MI5.1_SBI$percentageABS)/nrow(BS10MI5.1_SBI)

BS10MI5.1_Burg_Staat <- Bias2(S10MI5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI5.1_Burg_Staat <- sum(BS10MI5.1_Burg_Staat$percentageABS)/nrow(BS10MI5.1_Burg_Staat)

BiasOverallS10MI5.1 <- sum(BOS10MI5.1_Geslacht, BOS10MI5.1_Leeftijd, BOS10MI5.1_HH_Pos, BOS10MI5.1_HH_grootte, BOS10MI5.1_Woonregio_vorig_jaar, 
                        BOS10MI5.1_Nationaliteit, BOS10MI5.1_Geboorteland, BOS10MI5.1_Onderwijsniveau, BOS10MI5.1_Econ_status, 
                        BOS10MI5.1_Beroep, BOS10MI5.1_SBI, BOS10MI5.1_Burg_Staat) / 12

rm(S10MI5.1_Geslacht, S10MI5.1_Leeftijd, S10MI5.1_HH_Pos, S10MI5.1_HH_grootte, S10MI5.1_Woonregio_vorig_jaar, 
   S10MI5.1_Nationaliteit, S10MI5.1_Geboorteland, S10MI5.1_Onderwijsniveau, S10MI5.1_Econ_status, 
   S10MI5.1_Beroep, S10MI5.1_SBI, S10MI5.1_Burg_Staat)
rm(BS10MI5.1_Geslacht, BS10MI5.1_Leeftijd, BS10MI5.1_HH_Pos, BS10MI5.1_HH_grootte, BS10MI5.1_Woonregio_vorig_jaar, 
   BS10MI5.1_Nationaliteit, BS10MI5.1_Geboorteland, BS10MI5.1_Onderwijsniveau, BS10MI5.1_Econ_status, 
   BS10MI5.1_Beroep, BS10MI5.1_SBI, BS10MI5.1_Burg_Staat)
rm(BOS10MI5.1_Geslacht, BOS10MI5.1_Leeftijd, BOS10MI5.1_HH_Pos, BOS10MI5.1_HH_grootte, BOS10MI5.1_Woonregio_vorig_jaar, 
   BOS10MI5.1_Nationaliteit, BOS10MI5.1_Geboorteland, BOS10MI5.1_Onderwijsniveau, BOS10MI5.1_Econ_status, 
   BOS10MI5.1_Beroep, BOS10MI5.1_SBI, BOS10MI5.1_Burg_Staat)

# 5.2 data set 
BS10MI5.2_Geslacht <- Bias(S10MI5.2_Geslacht, Geslacht, "Geslacht")
BOS10MI5.2_Geslacht <- sum(BS10MI5.2_Geslacht$percentageABS)/nrow(BS10MI5.2_Geslacht)

BS10MI5.2_Leeftijd <- Bias(S10MI5.2_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI5.2_Leeftijd <- sum(BS10MI5.2_Leeftijd$percentageABS)/nrow(BS10MI5.2_Leeftijd)

BS10MI5.2_HH_Pos <- Bias(S10MI5.2_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI5.2_HH_Pos <- sum(BS10MI5.2_HH_Pos$percentageABS)/nrow(BS10MI5.2_HH_Pos)

BS10MI5.2_HH_grootte <- Bias(S10MI5.2_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI5.2_HH_grootte <- sum(BS10MI5.2_HH_grootte$percentageABS)/nrow(BS10MI5.2_HH_grootte)

BS10MI5.2_Woonregio_vorig_jaar <- Bias(S10MI5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI5.2_Woonregio_vorig_jaar <- sum(BS10MI5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI5.2_Woonregio_vorig_jaar)

BS10MI5.2_Nationaliteit <- Bias(S10MI5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI5.2_Nationaliteit <- sum(BS10MI5.2_Nationaliteit$percentageABS)/nrow(BS10MI5.2_Nationaliteit)

BS10MI5.2_Geboorteland <- Bias(S10MI5.2_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI5.2_Geboorteland <- sum(BS10MI5.2_Geboorteland$percentageABS)/nrow(BS10MI5.2_Geboorteland)

BS10MI5.2_Onderwijsniveau <- Bias(S10MI5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI5.2_Onderwijsniveau <- sum(BS10MI5.2_Onderwijsniveau$percentageABS)/nrow(BS10MI5.2_Onderwijsniveau)

BS10MI5.2_Econ_status <- Bias2(S10MI5.2_Econ_status, Econ_status, "Econ._status")
BOS10MI5.2_Econ_status <- sum(BS10MI5.2_Econ_status$percentageABS)/nrow(BS10MI5.2_Econ_status)

BS10MI5.2_Beroep <- Bias(S10MI5.2_Beroep, Beroep, "Beroep")
BOS10MI5.2_Beroep <- sum(BS10MI5.2_Beroep$percentageABS)/nrow(BS10MI5.2_Beroep)

BS10MI5.2_SBI <- Bias(S10MI5.2_SBI, SBI, "SBI")
BOS10MI5.2_SBI <- sum(BS10MI5.2_SBI$percentageABS)/nrow(BS10MI5.2_SBI)

BS10MI5.2_Burg_Staat <- Bias2(S10MI5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI5.2_Burg_Staat <- sum(BS10MI5.2_Burg_Staat$percentageABS)/nrow(BS10MI5.2_Burg_Staat)

BiasOverallS10MI5.2 <- sum(BOS10MI5.2_Geslacht, BOS10MI5.2_Leeftijd, BOS10MI5.2_HH_Pos, BOS10MI5.2_HH_grootte, BOS10MI5.2_Woonregio_vorig_jaar, 
                        BOS10MI5.2_Nationaliteit, BOS10MI5.2_Geboorteland, BOS10MI5.2_Onderwijsniveau, BOS10MI5.2_Econ_status, 
                        BOS10MI5.2_Beroep, BOS10MI5.2_SBI, BOS10MI5.2_Burg_Staat) / 12

rm(S10MI5.2_Geslacht, S10MI5.2_Leeftijd, S10MI5.2_HH_Pos, S10MI5.2_HH_grootte, S10MI5.2_Woonregio_vorig_jaar, 
   S10MI5.2_Nationaliteit, S10MI5.2_Geboorteland, S10MI5.2_Onderwijsniveau, S10MI5.2_Econ_status, 
   S10MI5.2_Beroep, S10MI5.2_SBI, S10MI5.2_Burg_Staat)
rm(BS10MI5.2_Geslacht, BS10MI5.2_Leeftijd, BS10MI5.2_HH_Pos, BS10MI5.2_HH_grootte, BS10MI5.2_Woonregio_vorig_jaar, 
   BS10MI5.2_Nationaliteit, BS10MI5.2_Geboorteland, BS10MI5.2_Onderwijsniveau, BS10MI5.2_Econ_status, 
   BS10MI5.2_Beroep, BS10MI5.2_SBI, BS10MI5.2_Burg_Staat)
rm(BOS10MI5.2_Geslacht, BOS10MI5.2_Leeftijd, BOS10MI5.2_HH_Pos, BOS10MI5.2_HH_grootte, BOS10MI5.2_Woonregio_vorig_jaar, 
   BOS10MI5.2_Nationaliteit, BOS10MI5.2_Geboorteland, BOS10MI5.2_Onderwijsniveau, BOS10MI5.2_Econ_status, 
   BOS10MI5.2_Beroep, BOS10MI5.2_SBI, BOS10MI5.2_Burg_Staat)

# 5.3 data set 
BS10MI5.3_Geslacht <- Bias(S10MI5.3_Geslacht, Geslacht, "Geslacht")
BOS10MI5.3_Geslacht <- sum(BS10MI5.3_Geslacht$percentageABS)/nrow(BS10MI5.3_Geslacht)

BS10MI5.3_Leeftijd <- Bias(S10MI5.3_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI5.3_Leeftijd <- sum(BS10MI5.3_Leeftijd$percentageABS)/nrow(BS10MI5.3_Leeftijd)

BS10MI5.3_HH_Pos <- Bias(S10MI5.3_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI5.3_HH_Pos <- sum(BS10MI5.3_HH_Pos$percentageABS)/nrow(BS10MI5.3_HH_Pos)

BS10MI5.3_HH_grootte <- Bias(S10MI5.3_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI5.3_HH_grootte <- sum(BS10MI5.3_HH_grootte$percentageABS)/nrow(BS10MI5.3_HH_grootte)

BS10MI5.3_Woonregio_vorig_jaar <- Bias(S10MI5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI5.3_Woonregio_vorig_jaar <- sum(BS10MI5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI5.3_Woonregio_vorig_jaar)

BS10MI5.3_Nationaliteit <- Bias(S10MI5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI5.3_Nationaliteit <- sum(BS10MI5.3_Nationaliteit$percentageABS)/nrow(BS10MI5.3_Nationaliteit)

BS10MI5.3_Geboorteland <- Bias(S10MI5.3_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI5.3_Geboorteland <- sum(BS10MI5.3_Geboorteland$percentageABS)/nrow(BS10MI5.3_Geboorteland)

BS10MI5.3_Onderwijsniveau <- Bias(S10MI5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI5.3_Onderwijsniveau <- sum(BS10MI5.3_Onderwijsniveau$percentageABS)/nrow(BS10MI5.3_Onderwijsniveau)

BS10MI5.3_Econ_status <- Bias2(S10MI5.3_Econ_status, Econ_status, "Econ._status")
BOS10MI5.3_Econ_status <- sum(BS10MI5.3_Econ_status$percentageABS)/nrow(BS10MI5.3_Econ_status)

BS10MI5.3_Beroep <- Bias(S10MI5.3_Beroep, Beroep, "Beroep")
BOS10MI5.3_Beroep <- sum(BS10MI5.3_Beroep$percentageABS)/nrow(BS10MI5.3_Beroep)

BS10MI5.3_SBI <- Bias(S10MI5.3_SBI, SBI, "SBI")
BOS10MI5.3_SBI <- sum(BS10MI5.3_SBI$percentageABS)/nrow(BS10MI5.3_SBI)

BS10MI5.3_Burg_Staat <- Bias2(S10MI5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI5.3_Burg_Staat <- sum(BS10MI5.3_Burg_Staat$percentageABS)/nrow(BS10MI5.3_Burg_Staat)

BiasOverallS10MI5.3 <- sum(BOS10MI5.3_Geslacht, BOS10MI5.3_Leeftijd, BOS10MI5.3_HH_Pos, BOS10MI5.3_HH_grootte, BOS10MI5.3_Woonregio_vorig_jaar, 
                        BOS10MI5.3_Nationaliteit, BOS10MI5.3_Geboorteland, BOS10MI5.3_Onderwijsniveau, BOS10MI5.3_Econ_status, 
                        BOS10MI5.3_Beroep, BOS10MI5.3_SBI, BOS10MI5.3_Burg_Staat) / 12

rm(S10MI5.3_Geslacht, S10MI5.3_Leeftijd, S10MI5.3_HH_Pos, S10MI5.3_HH_grootte, S10MI5.3_Woonregio_vorig_jaar, 
   S10MI5.3_Nationaliteit, S10MI5.3_Geboorteland, S10MI5.3_Onderwijsniveau, S10MI5.3_Econ_status, 
   S10MI5.3_Beroep, S10MI5.3_SBI, S10MI5.3_Burg_Staat)
rm(BS10MI5.3_Geslacht, BS10MI5.3_Leeftijd, BS10MI5.3_HH_Pos, BS10MI5.3_HH_grootte, BS10MI5.3_Woonregio_vorig_jaar, 
   BS10MI5.3_Nationaliteit, BS10MI5.3_Geboorteland, BS10MI5.3_Onderwijsniveau, BS10MI5.3_Econ_status, 
   BS10MI5.3_Beroep, BS10MI5.3_SBI, BS10MI5.3_Burg_Staat)
rm(BOS10MI5.3_Geslacht, BOS10MI5.3_Leeftijd, BOS10MI5.3_HH_Pos, BOS10MI5.3_HH_grootte, BOS10MI5.3_Woonregio_vorig_jaar, 
   BOS10MI5.3_Nationaliteit, BOS10MI5.3_Geboorteland, BOS10MI5.3_Onderwijsniveau, BOS10MI5.3_Econ_status, 
   BOS10MI5.3_Beroep, BOS10MI5.3_SBI, BOS10MI5.3_Burg_Staat)


# 10% data sets

# Counting values
S10MI10.1_Geslacht <- plyr::count(df_Smultiple_imputation10.1, 'Geslacht')
S10MI10.1_Leeftijd <- plyr::count(df_Smultiple_imputation10.1, 'Leeftijd')
S10MI10.1_HH_Pos <- plyr::count(df_Smultiple_imputation10.1, 'HH_Pos')
S10MI10.1_HH_grootte <- plyr::count(df_Smultiple_imputation10.1, 'HH_grootte')
S10MI10.1_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation10.1, 'Woonregio_vorig_jaar')
S10MI10.1_Nationaliteit <- plyr::count(df_Smultiple_imputation10.1, 'Nationaliteit')
S10MI10.1_Geboorteland <- plyr::count(df_Smultiple_imputation10.1, 'Geboorteland')
S10MI10.1_Onderwijsniveau <- plyr::count(df_Smultiple_imputation10.1, 'Onderwijsniveau')
S10MI10.1_Econ_status <- plyr::count(df_Smultiple_imputation10.1, 'Econ._status')
S10MI10.1_Beroep <- plyr::count(df_Smultiple_imputation10.1, 'Beroep')
S10MI10.1_SBI <- plyr::count(df_Smultiple_imputation10.1, 'SBI')
S10MI10.1_Burg_Staat <- plyr::count(df_Smultiple_imputation10.1, 'Burg._Staat')

S10MI10.2_Geslacht <- plyr::count(df_Smultiple_imputation10.2, 'Geslacht')
S10MI10.2_Leeftijd <- plyr::count(df_Smultiple_imputation10.2, 'Leeftijd')
S10MI10.2_HH_Pos <- plyr::count(df_Smultiple_imputation10.2, 'HH_Pos')
S10MI10.2_HH_grootte <- plyr::count(df_Smultiple_imputation10.2, 'HH_grootte')
S10MI10.2_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation10.2, 'Woonregio_vorig_jaar')
S10MI10.2_Nationaliteit <- plyr::count(df_Smultiple_imputation10.2, 'Nationaliteit')
S10MI10.2_Geboorteland <- plyr::count(df_Smultiple_imputation10.2, 'Geboorteland')
S10MI10.2_Onderwijsniveau <- plyr::count(df_Smultiple_imputation10.2, 'Onderwijsniveau')
S10MI10.2_Econ_status <- plyr::count(df_Smultiple_imputation10.2, 'Econ._status')
S10MI10.2_Beroep <- plyr::count(df_Smultiple_imputation10.2, 'Beroep')
S10MI10.2_SBI <- plyr::count(df_Smultiple_imputation10.2, 'SBI')
S10MI10.2_Burg_Staat <- plyr::count(df_Smultiple_imputation10.2, 'Burg._Staat')

S10MI10.3_Geslacht <- plyr::count(df_Smultiple_imputation10.3, 'Geslacht')
S10MI10.3_Leeftijd <- plyr::count(df_Smultiple_imputation10.3, 'Leeftijd')
S10MI10.3_HH_Pos <- plyr::count(df_Smultiple_imputation10.3, 'HH_Pos')
S10MI10.3_HH_grootte <- plyr::count(df_Smultiple_imputation10.3, 'HH_grootte')
S10MI10.3_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation10.3, 'Woonregio_vorig_jaar')
S10MI10.3_Nationaliteit <- plyr::count(df_Smultiple_imputation10.3, 'Nationaliteit')
S10MI10.3_Geboorteland <- plyr::count(df_Smultiple_imputation10.3, 'Geboorteland')
S10MI10.3_Onderwijsniveau <- plyr::count(df_Smultiple_imputation10.3, 'Onderwijsniveau')
S10MI10.3_Econ_status <- plyr::count(df_Smultiple_imputation10.3, 'Econ._status')
S10MI10.3_Beroep <- plyr::count(df_Smultiple_imputation10.3, 'Beroep')
S10MI10.3_SBI <- plyr::count(df_Smultiple_imputation10.3, 'SBI')
S10MI10.3_Burg_Staat <- plyr::count(df_Smultiple_imputation10.3, 'Burg._Staat')

rm(df_Smultiple_imputation10.1, df_Smultiple_imputation10.2, df_Smultiple_imputation10.3)


# 10.1 data set 
BS10MI10.1_Geslacht <- Bias(S10MI10.1_Geslacht, Geslacht, "Geslacht")
BOS10MI10.1_Geslacht <- sum(BS10MI10.1_Geslacht$percentageABS)/nrow(BS10MI10.1_Geslacht)

BS10MI10.1_Leeftijd <- Bias(S10MI10.1_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI10.1_Leeftijd <- sum(BS10MI10.1_Leeftijd$percentageABS)/nrow(BS10MI10.1_Leeftijd)

BS10MI10.1_HH_Pos <- Bias(S10MI10.1_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI10.1_HH_Pos <- sum(BS10MI10.1_HH_Pos$percentageABS)/nrow(BS10MI10.1_HH_Pos)

BS10MI10.1_HH_grootte <- Bias(S10MI10.1_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI10.1_HH_grootte <- sum(BS10MI10.1_HH_grootte$percentageABS)/nrow(BS10MI10.1_HH_grootte)

BS10MI10.1_Woonregio_vorig_jaar <- Bias(S10MI10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI10.1_Woonregio_vorig_jaar <- sum(BS10MI10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI10.1_Woonregio_vorig_jaar)

BS10MI10.1_Nationaliteit <- Bias(S10MI10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI10.1_Nationaliteit <- sum(BS10MI10.1_Nationaliteit$percentageABS)/nrow(BS10MI10.1_Nationaliteit)

BS10MI10.1_Geboorteland <- Bias(S10MI10.1_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI10.1_Geboorteland <- sum(BS10MI10.1_Geboorteland$percentageABS)/nrow(BS10MI10.1_Geboorteland)

BS10MI10.1_Onderwijsniveau <- Bias(S10MI10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI10.1_Onderwijsniveau <- sum(BS10MI10.1_Onderwijsniveau$percentageABS)/nrow(BS10MI10.1_Onderwijsniveau)

BS10MI10.1_Econ_status <- Bias2(S10MI10.1_Econ_status, Econ_status, "Econ._status")
BOS10MI10.1_Econ_status <- sum(BS10MI10.1_Econ_status$percentageABS)/nrow(BS10MI10.1_Econ_status)

BS10MI10.1_Beroep <- Bias(S10MI10.1_Beroep, Beroep, "Beroep")
BOS10MI10.1_Beroep <- sum(BS10MI10.1_Beroep$percentageABS)/nrow(BS10MI10.1_Beroep)

BS10MI10.1_SBI <- Bias(S10MI10.1_SBI, SBI, "SBI")
BOS10MI10.1_SBI <- sum(BS10MI10.1_SBI$percentageABS)/nrow(BS10MI10.1_SBI)

BS10MI10.1_Burg_Staat <- Bias2(S10MI10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI10.1_Burg_Staat <- sum(BS10MI10.1_Burg_Staat$percentageABS)/nrow(BS10MI10.1_Burg_Staat)

BiasOverallS10MI10.1 <- sum(BOS10MI10.1_Geslacht, BOS10MI10.1_Leeftijd, BOS10MI10.1_HH_Pos, BOS10MI10.1_HH_grootte, BOS10MI10.1_Woonregio_vorig_jaar, 
                         BOS10MI10.1_Nationaliteit, BOS10MI10.1_Geboorteland, BOS10MI10.1_Onderwijsniveau, BOS10MI10.1_Econ_status, 
                         BOS10MI10.1_Beroep, BOS10MI10.1_SBI, BOS10MI10.1_Burg_Staat) / 12

rm(S10MI10.1_Geslacht, S10MI10.1_Leeftijd, S10MI10.1_HH_Pos, S10MI10.1_HH_grootte, S10MI10.1_Woonregio_vorig_jaar, 
   S10MI10.1_Nationaliteit, S10MI10.1_Geboorteland, S10MI10.1_Onderwijsniveau, S10MI10.1_Econ_status, 
   S10MI10.1_Beroep, S10MI10.1_SBI, S10MI10.1_Burg_Staat)
rm(BS10MI10.1_Geslacht, BS10MI10.1_Leeftijd, BS10MI10.1_HH_Pos, BS10MI10.1_HH_grootte, BS10MI10.1_Woonregio_vorig_jaar, 
   BS10MI10.1_Nationaliteit, BS10MI10.1_Geboorteland, BS10MI10.1_Onderwijsniveau, BS10MI10.1_Econ_status, 
   BS10MI10.1_Beroep, BS10MI10.1_SBI, BS10MI10.1_Burg_Staat)
rm(BOS10MI10.1_Geslacht, BOS10MI10.1_Leeftijd, BOS10MI10.1_HH_Pos, BOS10MI10.1_HH_grootte, BOS10MI10.1_Woonregio_vorig_jaar, 
   BOS10MI10.1_Nationaliteit, BOS10MI10.1_Geboorteland, BOS10MI10.1_Onderwijsniveau, BOS10MI10.1_Econ_status, 
   BOS10MI10.1_Beroep, BOS10MI10.1_SBI, BOS10MI10.1_Burg_Staat)

# 10.2 data set 
BS10MI10.2_Geslacht <- Bias(S10MI10.2_Geslacht, Geslacht, "Geslacht")
BOS10MI10.2_Geslacht <- sum(BS10MI10.2_Geslacht$percentageABS)/nrow(BS10MI10.2_Geslacht)

BS10MI10.2_Leeftijd <- Bias(S10MI10.2_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI10.2_Leeftijd <- sum(BS10MI10.2_Leeftijd$percentageABS)/nrow(BS10MI10.2_Leeftijd)

BS10MI10.2_HH_Pos <- Bias(S10MI10.2_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI10.2_HH_Pos <- sum(BS10MI10.2_HH_Pos$percentageABS)/nrow(BS10MI10.2_HH_Pos)

BS10MI10.2_HH_grootte <- Bias(S10MI10.2_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI10.2_HH_grootte <- sum(BS10MI10.2_HH_grootte$percentageABS)/nrow(BS10MI10.2_HH_grootte)

BS10MI10.2_Woonregio_vorig_jaar <- Bias(S10MI10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI10.2_Woonregio_vorig_jaar <- sum(BS10MI10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI10.2_Woonregio_vorig_jaar)

BS10MI10.2_Nationaliteit <- Bias(S10MI10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI10.2_Nationaliteit <- sum(BS10MI10.2_Nationaliteit$percentageABS)/nrow(BS10MI10.2_Nationaliteit)

BS10MI10.2_Geboorteland <- Bias(S10MI10.2_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI10.2_Geboorteland <- sum(BS10MI10.2_Geboorteland$percentageABS)/nrow(BS10MI10.2_Geboorteland)

BS10MI10.2_Onderwijsniveau <- Bias(S10MI10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI10.2_Onderwijsniveau <- sum(BS10MI10.2_Onderwijsniveau$percentageABS)/nrow(BS10MI10.2_Onderwijsniveau)

BS10MI10.2_Econ_status <- Bias2(S10MI10.2_Econ_status, Econ_status, "Econ._status")
BOS10MI10.2_Econ_status <- sum(BS10MI10.2_Econ_status$percentageABS)/nrow(BS10MI10.2_Econ_status)

BS10MI10.2_Beroep <- Bias(S10MI10.2_Beroep, Beroep, "Beroep")
BOS10MI10.2_Beroep <- sum(BS10MI10.2_Beroep$percentageABS)/nrow(BS10MI10.2_Beroep)

BS10MI10.2_SBI <- Bias(S10MI10.2_SBI, SBI, "SBI")
BOS10MI10.2_SBI <- sum(BS10MI10.2_SBI$percentageABS)/nrow(BS10MI10.2_SBI)

BS10MI10.2_Burg_Staat <- Bias2(S10MI10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI10.2_Burg_Staat <- sum(BS10MI10.2_Burg_Staat$percentageABS)/nrow(BS10MI10.2_Burg_Staat)

BiasOverallS10MI10.2 <- sum(BOS10MI10.2_Geslacht, BOS10MI10.2_Leeftijd, BOS10MI10.2_HH_Pos, BOS10MI10.2_HH_grootte, BOS10MI10.2_Woonregio_vorig_jaar, 
                         BOS10MI10.2_Nationaliteit, BOS10MI10.2_Geboorteland, BOS10MI10.2_Onderwijsniveau, BOS10MI10.2_Econ_status, 
                         BOS10MI10.2_Beroep, BOS10MI10.2_SBI, BOS10MI10.2_Burg_Staat) / 12

rm(S10MI10.2_Geslacht, S10MI10.2_Leeftijd, S10MI10.2_HH_Pos, S10MI10.2_HH_grootte, S10MI10.2_Woonregio_vorig_jaar, 
   S10MI10.2_Nationaliteit, S10MI10.2_Geboorteland, S10MI10.2_Onderwijsniveau, S10MI10.2_Econ_status, 
   S10MI10.2_Beroep, S10MI10.2_SBI, S10MI10.2_Burg_Staat)
rm(BS10MI10.2_Geslacht, BS10MI10.2_Leeftijd, BS10MI10.2_HH_Pos, BS10MI10.2_HH_grootte, BS10MI10.2_Woonregio_vorig_jaar, 
   BS10MI10.2_Nationaliteit, BS10MI10.2_Geboorteland, BS10MI10.2_Onderwijsniveau, BS10MI10.2_Econ_status, 
   BS10MI10.2_Beroep, BS10MI10.2_SBI, BS10MI10.2_Burg_Staat)
rm(BOS10MI10.2_Geslacht, BOS10MI10.2_Leeftijd, BOS10MI10.2_HH_Pos, BOS10MI10.2_HH_grootte, BOS10MI10.2_Woonregio_vorig_jaar, 
   BOS10MI10.2_Nationaliteit, BOS10MI10.2_Geboorteland, BOS10MI10.2_Onderwijsniveau, BOS10MI10.2_Econ_status, 
   BOS10MI10.2_Beroep, BOS10MI10.2_SBI, BOS10MI10.2_Burg_Staat)

# 10.3 data set 
BS10MI10.3_Geslacht <- Bias(S10MI10.3_Geslacht, Geslacht, "Geslacht")
BOS10MI10.3_Geslacht <- sum(BS10MI10.3_Geslacht$percentageABS)/nrow(BS10MI10.3_Geslacht)

BS10MI10.3_Leeftijd <- Bias(S10MI10.3_Leeftijd, Leeftijd, "Leeftijd")
BOS10MI10.3_Leeftijd <- sum(BS10MI10.3_Leeftijd$percentageABS)/nrow(BS10MI10.3_Leeftijd)

BS10MI10.3_HH_Pos <- Bias(S10MI10.3_HH_Pos, HH_Pos, "HH_Pos")
BOS10MI10.3_HH_Pos <- sum(BS10MI10.3_HH_Pos$percentageABS)/nrow(BS10MI10.3_HH_Pos)

BS10MI10.3_HH_grootte <- Bias(S10MI10.3_HH_grootte, HH_grootte, "HH_grootte")
BOS10MI10.3_HH_grootte <- sum(BS10MI10.3_HH_grootte$percentageABS)/nrow(BS10MI10.3_HH_grootte)

BS10MI10.3_Woonregio_vorig_jaar <- Bias(S10MI10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MI10.3_Woonregio_vorig_jaar <- sum(BS10MI10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MI10.3_Woonregio_vorig_jaar)

BS10MI10.3_Nationaliteit <- Bias(S10MI10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MI10.3_Nationaliteit <- sum(BS10MI10.3_Nationaliteit$percentageABS)/nrow(BS10MI10.3_Nationaliteit)

BS10MI10.3_Geboorteland <- Bias(S10MI10.3_Geboorteland, Geboorteland, "Geboorteland")
BOS10MI10.3_Geboorteland <- sum(BS10MI10.3_Geboorteland$percentageABS)/nrow(BS10MI10.3_Geboorteland)

BS10MI10.3_Onderwijsniveau <- Bias(S10MI10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MI10.3_Onderwijsniveau <- sum(BS10MI10.3_Onderwijsniveau$percentageABS)/nrow(BS10MI10.3_Onderwijsniveau)

BS10MI10.3_Econ_status <- Bias2(S10MI10.3_Econ_status, Econ_status, "Econ._status")
BOS10MI10.3_Econ_status <- sum(BS10MI10.3_Econ_status$percentageABS)/nrow(BS10MI10.3_Econ_status)

BS10MI10.3_Beroep <- Bias(S10MI10.3_Beroep, Beroep, "Beroep")
BOS10MI10.3_Beroep <- sum(BS10MI10.3_Beroep$percentageABS)/nrow(BS10MI10.3_Beroep)

BS10MI10.3_SBI <- Bias(S10MI10.3_SBI, SBI, "SBI")
BOS10MI10.3_SBI <- sum(BS10MI10.3_SBI$percentageABS)/nrow(BS10MI10.3_SBI)

BS10MI10.3_Burg_Staat <- Bias2(S10MI10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MI10.3_Burg_Staat <- sum(BS10MI10.3_Burg_Staat$percentageABS)/nrow(BS10MI10.3_Burg_Staat)

BiasOverallS10MI10.3 <- sum(BOS10MI10.3_Geslacht, BOS10MI10.3_Leeftijd, BOS10MI10.3_HH_Pos, BOS10MI10.3_HH_grootte, BOS10MI10.3_Woonregio_vorig_jaar, 
                         BOS10MI10.3_Nationaliteit, BOS10MI10.3_Geboorteland, BOS10MI10.3_Onderwijsniveau, BOS10MI10.3_Econ_status, 
                         BOS10MI10.3_Beroep, BOS10MI10.3_SBI, BOS10MI10.3_Burg_Staat) / 12

rm(S10MI10.3_Geslacht, S10MI10.3_Leeftijd, S10MI10.3_HH_Pos, S10MI10.3_HH_grootte, S10MI10.3_Woonregio_vorig_jaar, 
   S10MI10.3_Nationaliteit, S10MI10.3_Geboorteland, S10MI10.3_Onderwijsniveau, S10MI10.3_Econ_status, 
   S10MI10.3_Beroep, S10MI10.3_SBI, S10MI10.3_Burg_Staat)
rm(BS10MI10.3_Geslacht, BS10MI10.3_Leeftijd, BS10MI10.3_HH_Pos, BS10MI10.3_HH_grootte, BS10MI10.3_Woonregio_vorig_jaar, 
   BS10MI10.3_Nationaliteit, BS10MI10.3_Geboorteland, BS10MI10.3_Onderwijsniveau, BS10MI10.3_Econ_status, 
   BS10MI10.3_Beroep, BS10MI10.3_SBI, BS10MI10.3_Burg_Staat)
rm(BOS10MI10.3_Geslacht, BOS10MI10.3_Leeftijd, BOS10MI10.3_HH_Pos, BOS10MI10.3_HH_grootte, BOS10MI10.3_Woonregio_vorig_jaar, 
   BOS10MI10.3_Nationaliteit, BOS10MI10.3_Geboorteland, BOS10MI10.3_Onderwijsniveau, BOS10MI10.3_Econ_status, 
   BOS10MI10.3_Beroep, BOS10MI10.3_SBI, BOS10MI10.3_Burg_Staat)


