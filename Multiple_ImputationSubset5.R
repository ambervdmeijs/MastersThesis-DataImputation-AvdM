
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
library("plyr")
library("dplyr")


## Loading ipums data set to environment ----------------------------------------------------------------------------------------------------
sub_ipums5 <- get(load("sub_ipums5.Rdata"))


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


# Setting correct and total to '0' 
SMI2.1_correct <- SMI2.2_correct <- SMI2.3_correct <- SMI5.1_correct <- SMI5.2_correct <- SMI5.3_correct <- SMI10.1_correct <- SMI10.2_correct <- SMI10.3_correct <- 0
SMI2.1_total <- SMI2.2_total <- SMI2.3_total <- SMI5.1_total <- SMI5.2_total <- SMI5.3_total <- SMI10.1_total <- SMI10.2_total <- SMI10.3_total <- 0


## Multiple Imputation with 'mice' ---------------------------------------------------------------------------------------------------

# 2% data sets

# 2.1 data set
sub5_MCAR2.1 <- get(load(file = "sub5_MCAR21.Rdata"))
SMI_MCAR2.1 <- data.frame(sub5_MCAR2.1)
names(SMI_MCAR2.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR2.1)) {
  SMI_MCAR2.1[, i] <- as.factor(SMI_MCAR2.1[, i])
}
rm(sub5_MCAR2.1)

tic("Multiple Imputation 2.1 processing time...")
S5multiple_imp2.1 <- mice(SMI_MCAR2.1, m = 5)
toc(log = TRUE)
S5multiple_imputation2.1 <- complete(S5multiple_imp2.1)
rm(SMI_MCAR2.1)

df_Smultiple_imputation2.1 <- as.data.frame(S5multiple_imputation2.1)
save(df_Smultiple_imputation2.1, file = "S5multiple_imputation21.Rdata")
rm(S5multiple_imp2.1)

SMI2.1_correct <- SMI2.1_correct + sum(sub_ipums5 == df_Smultiple_imputation2.1)
SMI2.1_total <- SMI2.1_total + sum(!is.na(df_Smultiple_imputation2.1))
rm(df_Smultiple_imputation2.1)

SM2.1_notcor <- 0
SM2.1_notcor <- SM2.1_notcor + sum(sub_ipums5 != df_Smultiple_imputation2.1)
save(SM2.1_notcor, file = "MI21_S5_notcorrect.Rdata")

# 2.2 data set 
sub5_MCAR2.2 <- get(load(file = "sub5_MCAR22.Rdata"))
SMI_MCAR2.2 <- data.frame(sub5_MCAR2.2)
names(SMI_MCAR2.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR2.2)) {
  SMI_MCAR2.2[, i] <- as.factor(SMI_MCAR2.2[, i])
}
rm(sub5_MCAR2.2)

tic("Multiple Imputation 2.2 processing time...")
S5multiple_imp2.2 <- mice(SMI_MCAR2.2, m = 5)
toc(log = TRUE)
S5multiple_imputation2.2 <- complete(S5multiple_imp2.2)
rm(SMI_MCAR2.2)

df_Smultiple_imputation2.2 <- as.data.frame(S5multiple_imputation2.2)
save(df_Smultiple_imputation2.2, file = "S5multiple_imputation22.Rdata")
rm(S5multiple_imp2.2)

SMI2.2_correct <- SMI2.2_correct + sum(sub_ipums5 == df_Smultiple_imputation2.2)
SMI2.2_total <- SMI2.2_total + sum(!is.na(df_Smultiple_imputation2.2))
rm(df_Smultiple_imputation2.2)

SM2.2_notcor <- 0
SM2.2_notcor <- SM2.2_notcor + sum(sub_ipums5 != df_Smultiple_imputation2.2)
save(SM2.2_notcor, file = "MI22_S5_notcorrect.Rdata")

# 2.3 data set 
sub5_MCAR2.3 <- get(load(file = "sub5_MCAR23.Rdata"))
SMI_MCAR2.3 <- data.frame(sub5_MCAR2.3)
names(SMI_MCAR2.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR2.3)) {
  SMI_MCAR2.3[, i] <- as.factor(SMI_MCAR2.3[, i])
}
rm(sub5_MCAR2.3)

tic("Multiple Imputation 2.3 processing time...")
S5multiple_imp2.3 <- mice(SMI_MCAR2.3, m = 5)
toc(log = TRUE)
S5multiple_imputation2.3 <- complete(S5multiple_imp2.3)
rm(SMI_MCAR2.3)

df_Smultiple_imputation2.3 <- as.data.frame(S5multiple_imputation2.3)
save(df_Smultiple_imputation2.3, file = "S5multiple_imputation23.Rdata")
rm(S5multiple_imp2.3)

SMI2.3_correct <- SMI2.3_correct + sum(sub_ipums5 == df_Smultiple_imputation2.3)
SMI2.3_total <- SMI2.3_total + sum(!is.na(df_Smultiple_imputation2.3))
rm(df_Smultiple_imputation2.3)

SM2.3_notcor <- 0
SM2.3_notcor <- SM2.3_notcor + sum(sub_ipums5 != df_Smultiple_imputation2.3)
save(SM2.3_notcor, file = "MI23_S5_notcorrect.Rdata")

# 5% data sets 

# 5.1 data set
sub5_MCAR5.1 <- get(load(file = "sub5_MCAR51.Rdata"))
SMI_MCAR5.1 <- data.frame(sub5_MCAR5.1)
names(SMI_MCAR5.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR5.1)) {
  SMI_MCAR5.1[, i] <- as.factor(SMI_MCAR5.1[, i])
}
rm(sub5_MCAR5.1)

tic("Multiple Imputation 5.1 processing time...")
S5multiple_imp5.1 <- mice(SMI_MCAR5.1, m = 5)
toc(log = TRUE)
S5multiple_imputation5.1 <- complete(S5multiple_imp5.1)
rm(SMI_MCAR5.1)

df_Smultiple_imputation5.1 <- as.data.frame(S5multiple_imputation5.1)
save(df_Smultiple_imputation5.1, file = "S5multiple_imputation51.Rdata")
rm(S5multiple_imp5.1)

SMI5.1_correct <- SMI5.1_correct + sum(sub_ipums5 == df_Smultiple_imputation5.1)
SMI5.1_total <- SMI5.1_total + sum(!is.na(df_Smultiple_imputation5.1))
rm(df_Smultiple_imputation5.1)

SM5.1_notcor <- 0
SM5.1_notcor <- SM5.1_notcor + sum(sub_ipums5 != df_Smultiple_imputation5.1)
save(SM5.1_notcor, file = "MI51_S5_notcorrect.Rdata")

# 5.2 data set 
sub5_MCAR5.2 <- get(load(file = "sub5_MCAR52.Rdata"))
SMI_MCAR5.2 <- data.frame(sub5_MCAR5.2)
names(SMI_MCAR5.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR5.2)) {
  SMI_MCAR5.2[, i] <- as.factor(SMI_MCAR5.2[, i])
}
rm(sub5_MCAR5.2)

tic("Multiple Imputation 5.2 processing time...")
S5multiple_imp5.2 <- mice(SMI_MCAR5.2, m = 5)
toc(log = TRUE)
S5multiple_imputation5.2 <- complete(S5multiple_imp5.2)
rm(SMI_MCAR5.2)

df_Smultiple_imputation5.2 <- as.data.frame(S5multiple_imputation5.2)
save(df_Smultiple_imputation5.2, file = "S5multiple_imputation52.Rdata")
rm(S5multiple_imp5.2)

SMI5.2_correct <- SMI5.2_correct + sum(sub_ipums5 == df_Smultiple_imputation5.2)
SMI5.2_total <- SMI5.2_total + sum(!is.na(df_Smultiple_imputation5.2))
rm(df_Smultiple_imputation5.2)

SM5.2_notcor <- 0
SM5.2_notcor <- SM5.2_notcor + sum(sub_ipums5 != df_Smultiple_imputation5.2)
save(SM5.2_notcor, file = "MI52_S5_notcorrect.Rdata")

# 5.3 data set 
sub5_MCAR5.3 <- get(load(file = "sub5_MCAR53.Rdata"))
SMI_MCAR5.3 <- data.frame(sub5_MCAR5.3)
names(SMI_MCAR5.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR5.3)) {
  SMI_MCAR5.3[, i] <- as.factor(SMI_MCAR5.3[, i])
}
rm(sub5_MCAR5.3)

tic("Multiple Imputation 5.3 processing time...")
S5multiple_imp5.3 <- mice(SMI_MCAR5.3, m = 5)
toc(log = TRUE)
S5multiple_imputation5.3 <- complete(S5multiple_imp5.3)
rm(SMI_MCAR5.3)

df_Smultiple_imputation5.3 <- as.data.frame(S5multiple_imputation5.3)
save(df_Smultiple_imputation5.3, file = "S5multiple_imputation53.Rdata")
rm(S5multiple_imp5.3)

SMI5.3_correct <- SMI5.3_correct + sum(sub_ipums5 == df_Smultiple_imputation5.3)
SMI5.3_total <- SMI5.3_total + sum(!is.na(df_Smultiple_imputation5.3))
rm(df_Smultiple_imputation5.3)

SM5.3_notcor <- 0
SM5.3_notcor <- SM5.3_notcor + sum(sub_ipums5 != df_Smultiple_imputation5.3)
save(SM5.3_notcor, file = "MI53_S5_notcorrect.Rdata")

# 10% data sets 

# 10.1 data set
sub5_MCAR10.1 <- get(load(file = "sub5_MCAR101.Rdata"))
SMI_MCAR10.1 <- data.frame(sub5_MCAR10.1)
names(SMI_MCAR10.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR10.1)) {
  SMI_MCAR10.1[, i] <- as.factor(SMI_MCAR10.1[, i])
}
rm(sub5_MCAR10.1)

tic("Multiple Imputation 10.1 processing time...")
S5multiple_imp10.1 <- mice(SMI_MCAR10.1, m = 5)
toc(log = TRUE)
S5multiple_imputation10.1 <- complete(S5multiple_imp10.1)
rm(SMI_MCAR10.1)

df_Smultiple_imputation10.1 <- as.data.frame(S5multiple_imputation10.1)
save(df_Smultiple_imputation10.1, file = "S5multiple_imputation101.Rdata")
rm(S5multiple_imp10.1)

SMI10.1_correct <- SMI10.1_correct + sum(sub_ipums5 == df_Smultiple_imputation10.1)
SMI10.1_total <- SMI10.1_total + sum(!is.na(df_Smultiple_imputation10.1))
rm(df_Smultiple_imputation10.1)

SM10.1_notcor <- 0
SM10.1_notcor <- SM10.1_notcor + sum(sub_ipums5 != df_Smultiple_imputation10.1)
save(SM10.1_notcor, file = "MI101_S5_notcorrect.Rdata")

# 10.2 data set 
sub5_MCAR10.2 <- get(load(file = "sub5_MCAR102.Rdata"))
SMI_MCAR10.2 <- data.frame(sub5_MCAR10.2)
names(SMI_MCAR10.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR10.2)) {
  SMI_MCAR10.2[, i] <- as.factor(SMI_MCAR10.2[, i])
}
rm(sub5_MCAR10.2)

tic("Multiple Imputation 10.2 processing time...")
S5multiple_imp10.2 <- mice(SMI_MCAR10.2, m = 5)
toc(log = TRUE)
S5multiple_imputation10.2 <- complete(S5multiple_imp10.2)
rm(SMI_MCAR10.2)

df_Smultiple_imputation10.2 <- as.data.frame(S5multiple_imputation10.2)
save(df_Smultiple_imputation10.2, file = "S5multiple_imputation102.Rdata")
rm(S5multiple_imp10.2)

SMI10.2_correct <- SMI10.2_correct + sum(sub_ipums5 == df_Smultiple_imputation10.2)
SMI10.2_total <- SMI10.2_total + sum(!is.na(df_Smultiple_imputation10.2))
rm(df_Smultiple_imputation10.2)

SM10.2_notcor <- 0
SM10.2_notcor <- SM10.2_notcor + sum(sub_ipums5 != df_Smultiple_imputation10.2)
save(SM10.2_notcor, file = "MI102_S5_notcorrect.Rdata")

# 10.3 data set 
sub5_MCAR10.3 <- get(load(file = "sub5_MCAR103.Rdata"))
SMI_MCAR10.3 <- data.frame(sub5_MCAR10.3)
names(SMI_MCAR10.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SMI_MCAR10.3)) {
  SMI_MCAR10.3[, i] <- as.factor(SMI_MCAR10.3[, i])
}
rm(sub5_MCAR10.3)

tic("Multiple Imputation 10.3 processing time...")
S5multiple_imp10.3 <- mice(SMI_MCAR10.3, m = 5)
toc(log = TRUE)
S5multiple_imputation10.3 <- complete(S5multiple_imp10.3)
rm(SMI_MCAR10.3)

df_Smultiple_imputation10.3 <- as.data.frame(S5multiple_imputation10.3)
save(df_Smultiple_imputation10.3, file = "S5multiple_imputation103.Rdata")
rm(S5multiple_imp10.3)

SMI10.3_correct <- SMI10.3_correct + sum(sub_ipums5 == df_Smultiple_imputation10.3)
SMI10.3_total <- SMI10.3_total + sum(!is.na(df_Smultiple_imputation10.3))
rm(df_Smultiple_imputation10.3)

SM10.3_notcor <- 0
SM10.3_notcor <- SM10.3_notcor + sum(sub_ipums5 != df_Smultiple_imputation10.3)
save(SM10.3_notcor, file = "MI103_S5_notcorrect.Rdata")

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


## Computing Bias --------------------------------------------------------------------------------------------------------------------


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
S5MI2.1_Geslacht <- plyr::count(df_Smultiple_imputation2.1, 'Geslacht')
S5MI2.1_Leeftijd <- plyr::count(df_Smultiple_imputation2.1, 'Leeftijd')
S5MI2.1_HH_Pos <- plyr::count(df_Smultiple_imputation2.1, 'HH_Pos')
S5MI2.1_HH_grootte <- plyr::count(df_Smultiple_imputation2.1, 'HH_grootte')
S5MI2.1_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation2.1, 'Woonregio_vorig_jaar')
S5MI2.1_Nationaliteit <- plyr::count(df_Smultiple_imputation2.1, 'Nationaliteit')
S5MI2.1_Geboorteland <- plyr::count(df_Smultiple_imputation2.1, 'Geboorteland')
S5MI2.1_Onderwijsniveau <- plyr::count(df_Smultiple_imputation2.1, 'Onderwijsniveau')
S5MI2.1_Econ_status <- plyr::count(df_Smultiple_imputation2.1, 'Econ._status')
S5MI2.1_Beroep <- plyr::count(df_Smultiple_imputation2.1, 'Beroep')
S5MI2.1_SBI <- plyr::count(df_Smultiple_imputation2.1, 'SBI')
S5MI2.1_Burg_Staat <- plyr::count(df_Smultiple_imputation2.1, 'Burg._Staat')

S5MI2.2_Geslacht <- plyr::count(df_Smultiple_imputation2.2, 'Geslacht')
S5MI2.2_Leeftijd <- plyr::count(df_Smultiple_imputation2.2, 'Leeftijd')
S5MI2.2_HH_Pos <- plyr::count(df_Smultiple_imputation2.2, 'HH_Pos')
S5MI2.2_HH_grootte <- plyr::count(df_Smultiple_imputation2.2, 'HH_grootte')
S5MI2.2_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation2.2, 'Woonregio_vorig_jaar')
S5MI2.2_Nationaliteit <- plyr::count(df_Smultiple_imputation2.2, 'Nationaliteit')
S5MI2.2_Geboorteland <- plyr::count(df_Smultiple_imputation2.2, 'Geboorteland')
S5MI2.2_Onderwijsniveau <- plyr::count(df_Smultiple_imputation2.2, 'Onderwijsniveau')
S5MI2.2_Econ_status <- plyr::count(df_Smultiple_imputation2.2, 'Econ._status')
S5MI2.2_Beroep <- plyr::count(df_Smultiple_imputation2.2, 'Beroep')
S5MI2.2_SBI <- plyr::count(df_Smultiple_imputation2.2, 'SBI')
S5MI2.2_Burg_Staat <- plyr::count(df_Smultiple_imputation2.2, 'Burg._Staat')

S5MI2.3_Geslacht <- plyr::count(df_Smultiple_imputation2.3, 'Geslacht')
S5MI2.3_Leeftijd <- plyr::count(df_Smultiple_imputation2.3, 'Leeftijd')
S5MI2.3_HH_Pos <- plyr::count(df_Smultiple_imputation2.3, 'HH_Pos')
S5MI2.3_HH_grootte <- plyr::count(df_Smultiple_imputation2.3, 'HH_grootte')
S5MI2.3_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation2.3, 'Woonregio_vorig_jaar')
S5MI2.3_Nationaliteit <- plyr::count(df_Smultiple_imputation2.3, 'Nationaliteit')
S5MI2.3_Geboorteland <- plyr::count(df_Smultiple_imputation2.3, 'Geboorteland')
S5MI2.3_Onderwijsniveau <- plyr::count(df_Smultiple_imputation2.3, 'Onderwijsniveau')
S5MI2.3_Econ_status <- plyr::count(df_Smultiple_imputation2.3, 'Econ._status')
S5MI2.3_Beroep <- plyr::count(df_Smultiple_imputation2.3, 'Beroep')
S5MI2.3_SBI <- plyr::count(df_Smultiple_imputation2.3, 'SBI')
S5MI2.3_Burg_Staat <- plyr::count(df_Smultiple_imputation2.3, 'Burg._Staat')

rm(df_Smultiple_imputation2.1, df_Smultiple_imputation2.2, df_Smultiple_imputation2.3)

# 2.1 data set 
BS5MI2.1_Geslacht <- Bias(S5MI2.1_Geslacht, Geslacht, "Geslacht")
BOS5MI2.1_Geslacht <- sum(BS5MI2.1_Geslacht$percentageABS)/nrow(BS5MI2.1_Geslacht)

BS5MI2.1_Leeftijd <- Bias(S5MI2.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI2.1_Leeftijd <- sum(BS5MI2.1_Leeftijd$percentageABS)/nrow(BS5MI2.1_Leeftijd)

BS5MI2.1_HH_Pos <- Bias(S5MI2.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI2.1_HH_Pos <- sum(BS5MI2.1_HH_Pos$percentageABS)/nrow(BS5MI2.1_HH_Pos)

BS5MI2.1_HH_grootte <- Bias(S5MI2.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI2.1_HH_grootte <- sum(BS5MI2.1_HH_grootte$percentageABS)/nrow(BS5MI2.1_HH_grootte)

BS5MI2.1_Woonregio_vorig_jaar <- Bias(S5MI2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI2.1_Woonregio_vorig_jaar <- sum(BS5MI2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI2.1_Woonregio_vorig_jaar)

BS5MI2.1_Nationaliteit <- Bias(S5MI2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI2.1_Nationaliteit <- sum(BS5MI2.1_Nationaliteit$percentageABS)/nrow(BS5MI2.1_Nationaliteit)

BS5MI2.1_Geboorteland <- Bias(S5MI2.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI2.1_Geboorteland <- sum(BS5MI2.1_Geboorteland$percentageABS)/nrow(BS5MI2.1_Geboorteland)

BS5MI2.1_Onderwijsniveau <- Bias(S5MI2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI2.1_Onderwijsniveau <- sum(BS5MI2.1_Onderwijsniveau$percentageABS)/nrow(BS5MI2.1_Onderwijsniveau)

BS5MI2.1_Econ_status <- Bias2(S5MI2.1_Econ_status, Econ_status, "Econ._status")
BOS5MI2.1_Econ_status <- sum(BS5MI2.1_Econ_status$percentageABS)/nrow(BS5MI2.1_Econ_status)

BS5MI2.1_Beroep <- Bias(S5MI2.1_Beroep, Beroep, "Beroep")
BOS5MI2.1_Beroep <- sum(BS5MI2.1_Beroep$percentageABS)/nrow(BS5MI2.1_Beroep)

BS5MI2.1_SBI <- Bias(S5MI2.1_SBI, SBI, "SBI")
BOS5MI2.1_SBI <- sum(BS5MI2.1_SBI$percentageABS)/nrow(BS5MI2.1_SBI)

BS5MI2.1_Burg_Staat <- Bias2(S5MI2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI2.1_Burg_Staat <- sum(BS5MI2.1_Burg_Staat$percentageABS)/nrow(BS5MI2.1_Burg_Staat)

BiasOverallS5MI2.1 <- sum(BOS5MI2.1_Geslacht, BOS5MI2.1_Leeftijd, BOS5MI2.1_HH_Pos, BOS5MI2.1_HH_grootte, BOS5MI2.1_Woonregio_vorig_jaar, 
                           BOS5MI2.1_Nationaliteit, BOS5MI2.1_Geboorteland, BOS5MI2.1_Onderwijsniveau, BOS5MI2.1_Econ_status, 
                           BOS5MI2.1_Beroep, BOS5MI2.1_SBI, BOS5MI2.1_Burg_Staat) / 12

rm(S5MI2.1_Geslacht, S5MI2.1_Leeftijd, S5MI2.1_HH_Pos, S5MI2.1_HH_grootte, S5MI2.1_Woonregio_vorig_jaar, 
   S5MI2.1_Nationaliteit, S5MI2.1_Geboorteland, S5MI2.1_Onderwijsniveau, S5MI2.1_Econ_status, 
   S5MI2.1_Beroep, S5MI2.1_SBI, S5MI2.1_Burg_Staat)
rm(BS5MI2.1_Geslacht, BS5MI2.1_Leeftijd, BS5MI2.1_HH_Pos, BS5MI2.1_HH_grootte, BS5MI2.1_Woonregio_vorig_jaar, 
   BS5MI2.1_Nationaliteit, BS5MI2.1_Geboorteland, BS5MI2.1_Onderwijsniveau, BS5MI2.1_Econ_status, 
   BS5MI2.1_Beroep, BS5MI2.1_SBI, BS5MI2.1_Burg_Staat)
rm(BOS5MI2.1_Geslacht, BOS5MI2.1_Leeftijd, BOS5MI2.1_HH_Pos, BOS5MI2.1_HH_grootte, BOS5MI2.1_Woonregio_vorig_jaar, 
   BOS5MI2.1_Nationaliteit, BOS5MI2.1_Geboorteland, BOS5MI2.1_Onderwijsniveau, BOS5MI2.1_Econ_status, 
   BOS5MI2.1_Beroep, BOS5MI2.1_SBI, BOS5MI2.1_Burg_Staat)

# 2.2 data set 
BS5MI2.2_Geslacht <- Bias(S5MI2.2_Geslacht, Geslacht, "Geslacht")
BOS5MI2.2_Geslacht <- sum(BS5MI2.2_Geslacht$percentageABS)/nrow(BS5MI2.2_Geslacht)

BS5MI2.2_Leeftijd <- Bias(S5MI2.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI2.2_Leeftijd <- sum(BS5MI2.2_Leeftijd$percentageABS)/nrow(BS5MI2.2_Leeftijd)

BS5MI2.2_HH_Pos <- Bias(S5MI2.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI2.2_HH_Pos <- sum(BS5MI2.2_HH_Pos$percentageABS)/nrow(BS5MI2.2_HH_Pos)

BS5MI2.2_HH_grootte <- Bias(S5MI2.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI2.2_HH_grootte <- sum(BS5MI2.2_HH_grootte$percentageABS)/nrow(BS5MI2.2_HH_grootte)

BS5MI2.2_Woonregio_vorig_jaar <- Bias(S5MI2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI2.2_Woonregio_vorig_jaar <- sum(BS5MI2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI2.2_Woonregio_vorig_jaar)

BS5MI2.2_Nationaliteit <- Bias(S5MI2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI2.2_Nationaliteit <- sum(BS5MI2.2_Nationaliteit$percentageABS)/nrow(BS5MI2.2_Nationaliteit)

BS5MI2.2_Geboorteland <- Bias(S5MI2.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI2.2_Geboorteland <- sum(BS5MI2.2_Geboorteland$percentageABS)/nrow(BS5MI2.2_Geboorteland)

BS5MI2.2_Onderwijsniveau <- Bias(S5MI2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI2.2_Onderwijsniveau <- sum(BS5MI2.2_Onderwijsniveau$percentageABS)/nrow(BS5MI2.2_Onderwijsniveau)

BS5MI2.2_Econ_status <- Bias2(S5MI2.2_Econ_status, Econ_status, "Econ._status")
BOS5MI2.2_Econ_status <- sum(BS5MI2.2_Econ_status$percentageABS)/nrow(BS5MI2.2_Econ_status)

BS5MI2.2_Beroep <- Bias(S5MI2.2_Beroep, Beroep, "Beroep")
BOS5MI2.2_Beroep <- sum(BS5MI2.2_Beroep$percentageABS)/nrow(BS5MI2.2_Beroep)

BS5MI2.2_SBI <- Bias(S5MI2.2_SBI, SBI, "SBI")
BOS5MI2.2_SBI <- sum(BS5MI2.2_SBI$percentageABS)/nrow(BS5MI2.2_SBI)

BS5MI2.2_Burg_Staat <- Bias2(S5MI2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI2.2_Burg_Staat <- sum(BS5MI2.2_Burg_Staat$percentageABS)/nrow(BS5MI2.2_Burg_Staat)

BiasOverallS5MI2.2 <- sum(BOS5MI2.2_Geslacht, BOS5MI2.2_Leeftijd, BOS5MI2.2_HH_Pos, BOS5MI2.2_HH_grootte, BOS5MI2.2_Woonregio_vorig_jaar, 
                           BOS5MI2.2_Nationaliteit, BOS5MI2.2_Geboorteland, BOS5MI2.2_Onderwijsniveau, BOS5MI2.2_Econ_status, 
                           BOS5MI2.2_Beroep, BOS5MI2.2_SBI, BOS5MI2.2_Burg_Staat) / 12

rm(S5MI2.2_Geslacht, S5MI2.2_Leeftijd, S5MI2.2_HH_Pos, S5MI2.2_HH_grootte, S5MI2.2_Woonregio_vorig_jaar, 
   S5MI2.2_Nationaliteit, S5MI2.2_Geboorteland, S5MI2.2_Onderwijsniveau, S5MI2.2_Econ_status, 
   S5MI2.2_Beroep, S5MI2.2_SBI, S5MI2.2_Burg_Staat)
rm(BS5MI2.2_Geslacht, BS5MI2.2_Leeftijd, BS5MI2.2_HH_Pos, BS5MI2.2_HH_grootte, BS5MI2.2_Woonregio_vorig_jaar, 
   BS5MI2.2_Nationaliteit, BS5MI2.2_Geboorteland, BS5MI2.2_Onderwijsniveau, BS5MI2.2_Econ_status, 
   BS5MI2.2_Beroep, BS5MI2.2_SBI, BS5MI2.2_Burg_Staat)
rm(BOS5MI2.2_Geslacht, BOS5MI2.2_Leeftijd, BOS5MI2.2_HH_Pos, BOS5MI2.2_HH_grootte, BOS5MI2.2_Woonregio_vorig_jaar, 
   BOS5MI2.2_Nationaliteit, BOS5MI2.2_Geboorteland, BOS5MI2.2_Onderwijsniveau, BOS5MI2.2_Econ_status, 
   BOS5MI2.2_Beroep, BOS5MI2.2_SBI, BOS5MI2.2_Burg_Staat)

# 2.3 data set 
BS5MI2.3_Geslacht <- Bias(S5MI2.3_Geslacht, Geslacht, "Geslacht")
BOS5MI2.3_Geslacht <- sum(BS5MI2.3_Geslacht$percentageABS)/nrow(BS5MI2.3_Geslacht)

BS5MI2.3_Leeftijd <- Bias(S5MI2.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI2.3_Leeftijd <- sum(BS5MI2.3_Leeftijd$percentageABS)/nrow(BS5MI2.3_Leeftijd)

BS5MI2.3_HH_Pos <- Bias(S5MI2.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI2.3_HH_Pos <- sum(BS5MI2.3_HH_Pos$percentageABS)/nrow(BS5MI2.3_HH_Pos)

BS5MI2.3_HH_grootte <- Bias(S5MI2.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI2.3_HH_grootte <- sum(BS5MI2.3_HH_grootte$percentageABS)/nrow(BS5MI2.3_HH_grootte)

BS5MI2.3_Woonregio_vorig_jaar <- Bias(S5MI2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI2.3_Woonregio_vorig_jaar <- sum(BS5MI2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI2.3_Woonregio_vorig_jaar)

BS5MI2.3_Nationaliteit <- Bias(S5MI2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI2.3_Nationaliteit <- sum(BS5MI2.3_Nationaliteit$percentageABS)/nrow(BS5MI2.3_Nationaliteit)

BS5MI2.3_Geboorteland <- Bias(S5MI2.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI2.3_Geboorteland <- sum(BS5MI2.3_Geboorteland$percentageABS)/nrow(BS5MI2.3_Geboorteland)

BS5MI2.3_Onderwijsniveau <- Bias(S5MI2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI2.3_Onderwijsniveau <- sum(BS5MI2.3_Onderwijsniveau$percentageABS)/nrow(BS5MI2.3_Onderwijsniveau)

BS5MI2.3_Econ_status <- Bias2(S5MI2.3_Econ_status, Econ_status, "Econ._status")
BOS5MI2.3_Econ_status <- sum(BS5MI2.3_Econ_status$percentageABS)/nrow(BS5MI2.3_Econ_status)

BS5MI2.3_Beroep <- Bias(S5MI2.3_Beroep, Beroep, "Beroep")
BOS5MI2.3_Beroep <- sum(BS5MI2.3_Beroep$percentageABS)/nrow(BS5MI2.3_Beroep)

BS5MI2.3_SBI <- Bias(S5MI2.3_SBI, SBI, "SBI")
BOS5MI2.3_SBI <- sum(BS5MI2.3_SBI$percentageABS)/nrow(BS5MI2.3_SBI)

BS5MI2.3_Burg_Staat <- Bias2(S5MI2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI2.3_Burg_Staat <- sum(BS5MI2.3_Burg_Staat$percentageABS)/nrow(BS5MI2.3_Burg_Staat)

BiasOverallS5MI2.3 <- sum(BOS5MI2.3_Geslacht, BOS5MI2.3_Leeftijd, BOS5MI2.3_HH_Pos, BOS5MI2.3_HH_grootte, BOS5MI2.3_Woonregio_vorig_jaar, 
                           BOS5MI2.3_Nationaliteit, BOS5MI2.3_Geboorteland, BOS5MI2.3_Onderwijsniveau, BOS5MI2.3_Econ_status, 
                           BOS5MI2.3_Beroep, BOS5MI2.3_SBI, BOS5MI2.3_Burg_Staat) / 12

rm(S5MI2.3_Geslacht, S5MI2.3_Leeftijd, S5MI2.3_HH_Pos, S5MI2.3_HH_grootte, S5MI2.3_Woonregio_vorig_jaar, 
   S5MI2.3_Nationaliteit, S5MI2.3_Geboorteland, S5MI2.3_Onderwijsniveau, S5MI2.3_Econ_status, 
   S5MI2.3_Beroep, S5MI2.3_SBI, S5MI2.3_Burg_Staat)
rm(BS5MI2.3_Geslacht, BS5MI2.3_Leeftijd, BS5MI2.3_HH_Pos, BS5MI2.3_HH_grootte, BS5MI2.3_Woonregio_vorig_jaar, 
   BS5MI2.3_Nationaliteit, BS5MI2.3_Geboorteland, BS5MI2.3_Onderwijsniveau, BS5MI2.3_Econ_status, 
   BS5MI2.3_Beroep, BS5MI2.3_SBI, BS5MI2.3_Burg_Staat)
rm(BOS5MI2.3_Geslacht, BOS5MI2.3_Leeftijd, BOS5MI2.3_HH_Pos, BOS5MI2.3_HH_grootte, BOS5MI2.3_Woonregio_vorig_jaar, 
   BOS5MI2.3_Nationaliteit, BOS5MI2.3_Geboorteland, BOS5MI2.3_Onderwijsniveau, BOS5MI2.3_Econ_status, 
   BOS5MI2.3_Beroep, BOS5MI2.3_SBI, BOS5MI2.3_Burg_Staat)


# 5% data sets

# Counting values
S5MI5.1_Geslacht <- plyr::count(df_Smultiple_imputation5.1, 'Geslacht')
S5MI5.1_Leeftijd <- plyr::count(df_Smultiple_imputation5.1, 'Leeftijd')
S5MI5.1_HH_Pos <- plyr::count(df_Smultiple_imputation5.1, 'HH_Pos')
S5MI5.1_HH_grootte <- plyr::count(df_Smultiple_imputation5.1, 'HH_grootte')
S5MI5.1_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation5.1, 'Woonregio_vorig_jaar')
S5MI5.1_Nationaliteit <- plyr::count(df_Smultiple_imputation5.1, 'Nationaliteit')
S5MI5.1_Geboorteland <- plyr::count(df_Smultiple_imputation5.1, 'Geboorteland')
S5MI5.1_Onderwijsniveau <- plyr::count(df_Smultiple_imputation5.1, 'Onderwijsniveau')
S5MI5.1_Econ_status <- plyr::count(df_Smultiple_imputation5.1, 'Econ._status')
S5MI5.1_Beroep <- plyr::count(df_Smultiple_imputation5.1, 'Beroep')
S5MI5.1_SBI <- plyr::count(df_Smultiple_imputation5.1, 'SBI')
S5MI5.1_Burg_Staat <- plyr::count(df_Smultiple_imputation5.1, 'Burg._Staat')

S5MI5.2_Geslacht <- plyr::count(df_Smultiple_imputation5.2, 'Geslacht')
S5MI5.2_Leeftijd <- plyr::count(df_Smultiple_imputation5.2, 'Leeftijd')
S5MI5.2_HH_Pos <- plyr::count(df_Smultiple_imputation5.2, 'HH_Pos')
S5MI5.2_HH_grootte <- plyr::count(df_Smultiple_imputation5.2, 'HH_grootte')
S5MI5.2_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation5.2, 'Woonregio_vorig_jaar')
S5MI5.2_Nationaliteit <- plyr::count(df_Smultiple_imputation5.2, 'Nationaliteit')
S5MI5.2_Geboorteland <- plyr::count(df_Smultiple_imputation5.2, 'Geboorteland')
S5MI5.2_Onderwijsniveau <- plyr::count(df_Smultiple_imputation5.2, 'Onderwijsniveau')
S5MI5.2_Econ_status <- plyr::count(df_Smultiple_imputation5.2, 'Econ._status')
S5MI5.2_Beroep <- plyr::count(df_Smultiple_imputation5.2, 'Beroep')
S5MI5.2_SBI <- plyr::count(df_Smultiple_imputation5.2, 'SBI')
S5MI5.2_Burg_Staat <- plyr::count(df_Smultiple_imputation5.2, 'Burg._Staat')

S5MI5.3_Geslacht <- plyr::count(df_Smultiple_imputation5.3, 'Geslacht')
S5MI5.3_Leeftijd <- plyr::count(df_Smultiple_imputation5.3, 'Leeftijd')
S5MI5.3_HH_Pos <- plyr::count(df_Smultiple_imputation5.3, 'HH_Pos')
S5MI5.3_HH_grootte <- plyr::count(df_Smultiple_imputation5.3, 'HH_grootte')
S5MI5.3_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation5.3, 'Woonregio_vorig_jaar')
S5MI5.3_Nationaliteit <- plyr::count(df_Smultiple_imputation5.3, 'Nationaliteit')
S5MI5.3_Geboorteland <- plyr::count(df_Smultiple_imputation5.3, 'Geboorteland')
S5MI5.3_Onderwijsniveau <- plyr::count(df_Smultiple_imputation5.3, 'Onderwijsniveau')
S5MI5.3_Econ_status <- plyr::count(df_Smultiple_imputation5.3, 'Econ._status')
S5MI5.3_Beroep <- plyr::count(df_Smultiple_imputation5.3, 'Beroep')
S5MI5.3_SBI <- plyr::count(df_Smultiple_imputation5.3, 'SBI')
S5MI5.3_Burg_Staat <- plyr::count(df_Smultiple_imputation5.3, 'Burg._Staat')

rm(df_Smultiple_imputation5.1, df_Smultiple_imputation5.2, df_Smultiple_imputation5.3)


# 5.1 data set 
BS5MI5.1_Geslacht <- Bias(S5MI5.1_Geslacht, Geslacht, "Geslacht")
BOS5MI5.1_Geslacht <- sum(BS5MI5.1_Geslacht$percentageABS)/nrow(BS5MI5.1_Geslacht)

BS5MI5.1_Leeftijd <- Bias(S5MI5.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI5.1_Leeftijd <- sum(BS5MI5.1_Leeftijd$percentageABS)/nrow(BS5MI5.1_Leeftijd)

BS5MI5.1_HH_Pos <- Bias(S5MI5.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI5.1_HH_Pos <- sum(BS5MI5.1_HH_Pos$percentageABS)/nrow(BS5MI5.1_HH_Pos)

BS5MI5.1_HH_grootte <- Bias(S5MI5.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI5.1_HH_grootte <- sum(BS5MI5.1_HH_grootte$percentageABS)/nrow(BS5MI5.1_HH_grootte)

BS5MI5.1_Woonregio_vorig_jaar <- Bias(S5MI5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI5.1_Woonregio_vorig_jaar <- sum(BS5MI5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI5.1_Woonregio_vorig_jaar)

BS5MI5.1_Nationaliteit <- Bias(S5MI5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI5.1_Nationaliteit <- sum(BS5MI5.1_Nationaliteit$percentageABS)/nrow(BS5MI5.1_Nationaliteit)

BS5MI5.1_Geboorteland <- Bias(S5MI5.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI5.1_Geboorteland <- sum(BS5MI5.1_Geboorteland$percentageABS)/nrow(BS5MI5.1_Geboorteland)

BS5MI5.1_Onderwijsniveau <- Bias(S5MI5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI5.1_Onderwijsniveau <- sum(BS5MI5.1_Onderwijsniveau$percentageABS)/nrow(BS5MI5.1_Onderwijsniveau)

BS5MI5.1_Econ_status <- Bias2(S5MI5.1_Econ_status, Econ_status, "Econ._status")
BOS5MI5.1_Econ_status <- sum(BS5MI5.1_Econ_status$percentageABS)/nrow(BS5MI5.1_Econ_status)

BS5MI5.1_Beroep <- Bias(S5MI5.1_Beroep, Beroep, "Beroep")
BOS5MI5.1_Beroep <- sum(BS5MI5.1_Beroep$percentageABS)/nrow(BS5MI5.1_Beroep)

BS5MI5.1_SBI <- Bias(S5MI5.1_SBI, SBI, "SBI")
BOS5MI5.1_SBI <- sum(BS5MI5.1_SBI$percentageABS)/nrow(BS5MI5.1_SBI)

BS5MI5.1_Burg_Staat <- Bias2(S5MI5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI5.1_Burg_Staat <- sum(BS5MI5.1_Burg_Staat$percentageABS)/nrow(BS5MI5.1_Burg_Staat)

BiasOverallS5MI5.1 <- sum(BOS5MI5.1_Geslacht, BOS5MI5.1_Leeftijd, BOS5MI5.1_HH_Pos, BOS5MI5.1_HH_grootte, BOS5MI5.1_Woonregio_vorig_jaar, 
                           BOS5MI5.1_Nationaliteit, BOS5MI5.1_Geboorteland, BOS5MI5.1_Onderwijsniveau, BOS5MI5.1_Econ_status, 
                           BOS5MI5.1_Beroep, BOS5MI5.1_SBI, BOS5MI5.1_Burg_Staat) / 12

rm(S5MI5.1_Geslacht, S5MI5.1_Leeftijd, S5MI5.1_HH_Pos, S5MI5.1_HH_grootte, S5MI5.1_Woonregio_vorig_jaar, 
   S5MI5.1_Nationaliteit, S5MI5.1_Geboorteland, S5MI5.1_Onderwijsniveau, S5MI5.1_Econ_status, 
   S5MI5.1_Beroep, S5MI5.1_SBI, S5MI5.1_Burg_Staat)
rm(BS5MI5.1_Geslacht, BS5MI5.1_Leeftijd, BS5MI5.1_HH_Pos, BS5MI5.1_HH_grootte, BS5MI5.1_Woonregio_vorig_jaar, 
   BS5MI5.1_Nationaliteit, BS5MI5.1_Geboorteland, BS5MI5.1_Onderwijsniveau, BS5MI5.1_Econ_status, 
   BS5MI5.1_Beroep, BS5MI5.1_SBI, BS5MI5.1_Burg_Staat)
rm(BOS5MI5.1_Geslacht, BOS5MI5.1_Leeftijd, BOS5MI5.1_HH_Pos, BOS5MI5.1_HH_grootte, BOS5MI5.1_Woonregio_vorig_jaar, 
   BOS5MI5.1_Nationaliteit, BOS5MI5.1_Geboorteland, BOS5MI5.1_Onderwijsniveau, BOS5MI5.1_Econ_status, 
   BOS5MI5.1_Beroep, BOS5MI5.1_SBI, BOS5MI5.1_Burg_Staat)

# 5.2 data set 
BS5MI5.2_Geslacht <- Bias(S5MI5.2_Geslacht, Geslacht, "Geslacht")
BOS5MI5.2_Geslacht <- sum(BS5MI5.2_Geslacht$percentageABS)/nrow(BS5MI5.2_Geslacht)

BS5MI5.2_Leeftijd <- Bias(S5MI5.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI5.2_Leeftijd <- sum(BS5MI5.2_Leeftijd$percentageABS)/nrow(BS5MI5.2_Leeftijd)

BS5MI5.2_HH_Pos <- Bias(S5MI5.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI5.2_HH_Pos <- sum(BS5MI5.2_HH_Pos$percentageABS)/nrow(BS5MI5.2_HH_Pos)

BS5MI5.2_HH_grootte <- Bias(S5MI5.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI5.2_HH_grootte <- sum(BS5MI5.2_HH_grootte$percentageABS)/nrow(BS5MI5.2_HH_grootte)

BS5MI5.2_Woonregio_vorig_jaar <- Bias(S5MI5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI5.2_Woonregio_vorig_jaar <- sum(BS5MI5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI5.2_Woonregio_vorig_jaar)

BS5MI5.2_Nationaliteit <- Bias(S5MI5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI5.2_Nationaliteit <- sum(BS5MI5.2_Nationaliteit$percentageABS)/nrow(BS5MI5.2_Nationaliteit)

BS5MI5.2_Geboorteland <- Bias(S5MI5.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI5.2_Geboorteland <- sum(BS5MI5.2_Geboorteland$percentageABS)/nrow(BS5MI5.2_Geboorteland)

BS5MI5.2_Onderwijsniveau <- Bias(S5MI5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI5.2_Onderwijsniveau <- sum(BS5MI5.2_Onderwijsniveau$percentageABS)/nrow(BS5MI5.2_Onderwijsniveau)

BS5MI5.2_Econ_status <- Bias2(S5MI5.2_Econ_status, Econ_status, "Econ._status")
BOS5MI5.2_Econ_status <- sum(BS5MI5.2_Econ_status$percentageABS)/nrow(BS5MI5.2_Econ_status)

BS5MI5.2_Beroep <- Bias(S5MI5.2_Beroep, Beroep, "Beroep")
BOS5MI5.2_Beroep <- sum(BS5MI5.2_Beroep$percentageABS)/nrow(BS5MI5.2_Beroep)

BS5MI5.2_SBI <- Bias(S5MI5.2_SBI, SBI, "SBI")
BOS5MI5.2_SBI <- sum(BS5MI5.2_SBI$percentageABS)/nrow(BS5MI5.2_SBI)

BS5MI5.2_Burg_Staat <- Bias2(S5MI5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI5.2_Burg_Staat <- sum(BS5MI5.2_Burg_Staat$percentageABS)/nrow(BS5MI5.2_Burg_Staat)

BiasOverallS5MI5.2 <- sum(BOS5MI5.2_Geslacht, BOS5MI5.2_Leeftijd, BOS5MI5.2_HH_Pos, BOS5MI5.2_HH_grootte, BOS5MI5.2_Woonregio_vorig_jaar, 
                           BOS5MI5.2_Nationaliteit, BOS5MI5.2_Geboorteland, BOS5MI5.2_Onderwijsniveau, BOS5MI5.2_Econ_status, 
                           BOS5MI5.2_Beroep, BOS5MI5.2_SBI, BOS5MI5.2_Burg_Staat) / 12

rm(S5MI5.2_Geslacht, S5MI5.2_Leeftijd, S5MI5.2_HH_Pos, S5MI5.2_HH_grootte, S5MI5.2_Woonregio_vorig_jaar, 
   S5MI5.2_Nationaliteit, S5MI5.2_Geboorteland, S5MI5.2_Onderwijsniveau, S5MI5.2_Econ_status, 
   S5MI5.2_Beroep, S5MI5.2_SBI, S5MI5.2_Burg_Staat)
rm(BS5MI5.2_Geslacht, BS5MI5.2_Leeftijd, BS5MI5.2_HH_Pos, BS5MI5.2_HH_grootte, BS5MI5.2_Woonregio_vorig_jaar, 
   BS5MI5.2_Nationaliteit, BS5MI5.2_Geboorteland, BS5MI5.2_Onderwijsniveau, BS5MI5.2_Econ_status, 
   BS5MI5.2_Beroep, BS5MI5.2_SBI, BS5MI5.2_Burg_Staat)
rm(BOS5MI5.2_Geslacht, BOS5MI5.2_Leeftijd, BOS5MI5.2_HH_Pos, BOS5MI5.2_HH_grootte, BOS5MI5.2_Woonregio_vorig_jaar, 
   BOS5MI5.2_Nationaliteit, BOS5MI5.2_Geboorteland, BOS5MI5.2_Onderwijsniveau, BOS5MI5.2_Econ_status, 
   BOS5MI5.2_Beroep, BOS5MI5.2_SBI, BOS5MI5.2_Burg_Staat)

# 5.3 data set 
BS5MI5.3_Geslacht <- Bias(S5MI5.3_Geslacht, Geslacht, "Geslacht")
BOS5MI5.3_Geslacht <- sum(BS5MI5.3_Geslacht$percentageABS)/nrow(BS5MI5.3_Geslacht)

BS5MI5.3_Leeftijd <- Bias(S5MI5.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI5.3_Leeftijd <- sum(BS5MI5.3_Leeftijd$percentageABS)/nrow(BS5MI5.3_Leeftijd)

BS5MI5.3_HH_Pos <- Bias(S5MI5.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI5.3_HH_Pos <- sum(BS5MI5.3_HH_Pos$percentageABS)/nrow(BS5MI5.3_HH_Pos)

BS5MI5.3_HH_grootte <- Bias(S5MI5.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI5.3_HH_grootte <- sum(BS5MI5.3_HH_grootte$percentageABS)/nrow(BS5MI5.3_HH_grootte)

BS5MI5.3_Woonregio_vorig_jaar <- Bias(S5MI5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI5.3_Woonregio_vorig_jaar <- sum(BS5MI5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI5.3_Woonregio_vorig_jaar)

BS5MI5.3_Nationaliteit <- Bias(S5MI5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI5.3_Nationaliteit <- sum(BS5MI5.3_Nationaliteit$percentageABS)/nrow(BS5MI5.3_Nationaliteit)

BS5MI5.3_Geboorteland <- Bias(S5MI5.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI5.3_Geboorteland <- sum(BS5MI5.3_Geboorteland$percentageABS)/nrow(BS5MI5.3_Geboorteland)

BS5MI5.3_Onderwijsniveau <- Bias(S5MI5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI5.3_Onderwijsniveau <- sum(BS5MI5.3_Onderwijsniveau$percentageABS)/nrow(BS5MI5.3_Onderwijsniveau)

BS5MI5.3_Econ_status <- Bias2(S5MI5.3_Econ_status, Econ_status, "Econ._status")
BOS5MI5.3_Econ_status <- sum(BS5MI5.3_Econ_status$percentageABS)/nrow(BS5MI5.3_Econ_status)

BS5MI5.3_Beroep <- Bias(S5MI5.3_Beroep, Beroep, "Beroep")
BOS5MI5.3_Beroep <- sum(BS5MI5.3_Beroep$percentageABS)/nrow(BS5MI5.3_Beroep)

BS5MI5.3_SBI <- Bias(S5MI5.3_SBI, SBI, "SBI")
BOS5MI5.3_SBI <- sum(BS5MI5.3_SBI$percentageABS)/nrow(BS5MI5.3_SBI)

BS5MI5.3_Burg_Staat <- Bias2(S5MI5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI5.3_Burg_Staat <- sum(BS5MI5.3_Burg_Staat$percentageABS)/nrow(BS5MI5.3_Burg_Staat)

BiasOverallS5MI5.3 <- sum(BOS5MI5.3_Geslacht, BOS5MI5.3_Leeftijd, BOS5MI5.3_HH_Pos, BOS5MI5.3_HH_grootte, BOS5MI5.3_Woonregio_vorig_jaar, 
                           BOS5MI5.3_Nationaliteit, BOS5MI5.3_Geboorteland, BOS5MI5.3_Onderwijsniveau, BOS5MI5.3_Econ_status, 
                           BOS5MI5.3_Beroep, BOS5MI5.3_SBI, BOS5MI5.3_Burg_Staat) / 12

rm(S5MI5.3_Geslacht, S5MI5.3_Leeftijd, S5MI5.3_HH_Pos, S5MI5.3_HH_grootte, S5MI5.3_Woonregio_vorig_jaar, 
   S5MI5.3_Nationaliteit, S5MI5.3_Geboorteland, S5MI5.3_Onderwijsniveau, S5MI5.3_Econ_status, 
   S5MI5.3_Beroep, S5MI5.3_SBI, S5MI5.3_Burg_Staat)
rm(BS5MI5.3_Geslacht, BS5MI5.3_Leeftijd, BS5MI5.3_HH_Pos, BS5MI5.3_HH_grootte, BS5MI5.3_Woonregio_vorig_jaar, 
   BS5MI5.3_Nationaliteit, BS5MI5.3_Geboorteland, BS5MI5.3_Onderwijsniveau, BS5MI5.3_Econ_status, 
   BS5MI5.3_Beroep, BS5MI5.3_SBI, BS5MI5.3_Burg_Staat)
rm(BOS5MI5.3_Geslacht, BOS5MI5.3_Leeftijd, BOS5MI5.3_HH_Pos, BOS5MI5.3_HH_grootte, BOS5MI5.3_Woonregio_vorig_jaar, 
   BOS5MI5.3_Nationaliteit, BOS5MI5.3_Geboorteland, BOS5MI5.3_Onderwijsniveau, BOS5MI5.3_Econ_status, 
   BOS5MI5.3_Beroep, BOS5MI5.3_SBI, BOS5MI5.3_Burg_Staat)


# 10% data sets

# Counting values
S5MI10.1_Geslacht <- plyr::count(df_Smultiple_imputation10.1, 'Geslacht')
S5MI10.1_Leeftijd <- plyr::count(df_Smultiple_imputation10.1, 'Leeftijd')
S5MI10.1_HH_Pos <- plyr::count(df_Smultiple_imputation10.1, 'HH_Pos')
S5MI10.1_HH_grootte <- plyr::count(df_Smultiple_imputation10.1, 'HH_grootte')
S5MI10.1_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation10.1, 'Woonregio_vorig_jaar')
S5MI10.1_Nationaliteit <- plyr::count(df_Smultiple_imputation10.1, 'Nationaliteit')
S5MI10.1_Geboorteland <- plyr::count(df_Smultiple_imputation10.1, 'Geboorteland')
S5MI10.1_Onderwijsniveau <- plyr::count(df_Smultiple_imputation10.1, 'Onderwijsniveau')
S5MI10.1_Econ_status <- plyr::count(df_Smultiple_imputation10.1, 'Econ._status')
S5MI10.1_Beroep <- plyr::count(df_Smultiple_imputation10.1, 'Beroep')
S5MI10.1_SBI <- plyr::count(df_Smultiple_imputation10.1, 'SBI')
S5MI10.1_Burg_Staat <- plyr::count(df_Smultiple_imputation10.1, 'Burg._Staat')

S5MI10.2_Geslacht <- plyr::count(df_Smultiple_imputation10.2, 'Geslacht')
S5MI10.2_Leeftijd <- plyr::count(df_Smultiple_imputation10.2, 'Leeftijd')
S5MI10.2_HH_Pos <- plyr::count(df_Smultiple_imputation10.2, 'HH_Pos')
S5MI10.2_HH_grootte <- plyr::count(df_Smultiple_imputation10.2, 'HH_grootte')
S5MI10.2_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation10.2, 'Woonregio_vorig_jaar')
S5MI10.2_Nationaliteit <- plyr::count(df_Smultiple_imputation10.2, 'Nationaliteit')
S5MI10.2_Geboorteland <- plyr::count(df_Smultiple_imputation10.2, 'Geboorteland')
S5MI10.2_Onderwijsniveau <- plyr::count(df_Smultiple_imputation10.2, 'Onderwijsniveau')
S5MI10.2_Econ_status <- plyr::count(df_Smultiple_imputation10.2, 'Econ._status')
S5MI10.2_Beroep <- plyr::count(df_Smultiple_imputation10.2, 'Beroep')
S5MI10.2_SBI <- plyr::count(df_Smultiple_imputation10.2, 'SBI')
S5MI10.2_Burg_Staat <- plyr::count(df_Smultiple_imputation10.2, 'Burg._Staat')

S5MI10.3_Geslacht <- plyr::count(df_Smultiple_imputation10.3, 'Geslacht')
S5MI10.3_Leeftijd <- plyr::count(df_Smultiple_imputation10.3, 'Leeftijd')
S5MI10.3_HH_Pos <- plyr::count(df_Smultiple_imputation10.3, 'HH_Pos')
S5MI10.3_HH_grootte <- plyr::count(df_Smultiple_imputation10.3, 'HH_grootte')
S5MI10.3_Woonregio_vorig_jaar <- plyr::count(df_Smultiple_imputation10.3, 'Woonregio_vorig_jaar')
S5MI10.3_Nationaliteit <- plyr::count(df_Smultiple_imputation10.3, 'Nationaliteit')
S5MI10.3_Geboorteland <- plyr::count(df_Smultiple_imputation10.3, 'Geboorteland')
S5MI10.3_Onderwijsniveau <- plyr::count(df_Smultiple_imputation10.3, 'Onderwijsniveau')
S5MI10.3_Econ_status <- plyr::count(df_Smultiple_imputation10.3, 'Econ._status')
S5MI10.3_Beroep <- plyr::count(df_Smultiple_imputation10.3, 'Beroep')
S5MI10.3_SBI <- plyr::count(df_Smultiple_imputation10.3, 'SBI')
S5MI10.3_Burg_Staat <- plyr::count(df_Smultiple_imputation10.3, 'Burg._Staat')

rm(df_Smultiple_imputation10.1, df_Smultiple_imputation10.2, df_Smultiple_imputation10.3)


# 10.1 data set 
BS5MI10.1_Geslacht <- Bias(S5MI10.1_Geslacht, Geslacht, "Geslacht")
BOS5MI10.1_Geslacht <- sum(BS5MI10.1_Geslacht$percentageABS)/nrow(BS5MI10.1_Geslacht)

BS5MI10.1_Leeftijd <- Bias(S5MI10.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI10.1_Leeftijd <- sum(BS5MI10.1_Leeftijd$percentageABS)/nrow(BS5MI10.1_Leeftijd)

BS5MI10.1_HH_Pos <- Bias(S5MI10.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI10.1_HH_Pos <- sum(BS5MI10.1_HH_Pos$percentageABS)/nrow(BS5MI10.1_HH_Pos)

BS5MI10.1_HH_grootte <- Bias(S5MI10.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI10.1_HH_grootte <- sum(BS5MI10.1_HH_grootte$percentageABS)/nrow(BS5MI10.1_HH_grootte)

BS5MI10.1_Woonregio_vorig_jaar <- Bias(S5MI10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI10.1_Woonregio_vorig_jaar <- sum(BS5MI10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI10.1_Woonregio_vorig_jaar)

BS5MI10.1_Nationaliteit <- Bias(S5MI10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI10.1_Nationaliteit <- sum(BS5MI10.1_Nationaliteit$percentageABS)/nrow(BS5MI10.1_Nationaliteit)

BS5MI10.1_Geboorteland <- Bias(S5MI10.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI10.1_Geboorteland <- sum(BS5MI10.1_Geboorteland$percentageABS)/nrow(BS5MI10.1_Geboorteland)

BS5MI10.1_Onderwijsniveau <- Bias(S5MI10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI10.1_Onderwijsniveau <- sum(BS5MI10.1_Onderwijsniveau$percentageABS)/nrow(BS5MI10.1_Onderwijsniveau)

BS5MI10.1_Econ_status <- Bias2(S5MI10.1_Econ_status, Econ_status, "Econ._status")
BOS5MI10.1_Econ_status <- sum(BS5MI10.1_Econ_status$percentageABS)/nrow(BS5MI10.1_Econ_status)

BS5MI10.1_Beroep <- Bias(S5MI10.1_Beroep, Beroep, "Beroep")
BOS5MI10.1_Beroep <- sum(BS5MI10.1_Beroep$percentageABS)/nrow(BS5MI10.1_Beroep)

BS5MI10.1_SBI <- Bias(S5MI10.1_SBI, SBI, "SBI")
BOS5MI10.1_SBI <- sum(BS5MI10.1_SBI$percentageABS)/nrow(BS5MI10.1_SBI)

BS5MI10.1_Burg_Staat <- Bias2(S5MI10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI10.1_Burg_Staat <- sum(BS5MI10.1_Burg_Staat$percentageABS)/nrow(BS5MI10.1_Burg_Staat)

BiasOverallS5MI10.1 <- sum(BOS5MI10.1_Geslacht, BOS5MI10.1_Leeftijd, BOS5MI10.1_HH_Pos, BOS5MI10.1_HH_grootte, BOS5MI10.1_Woonregio_vorig_jaar, 
                            BOS5MI10.1_Nationaliteit, BOS5MI10.1_Geboorteland, BOS5MI10.1_Onderwijsniveau, BOS5MI10.1_Econ_status, 
                            BOS5MI10.1_Beroep, BOS5MI10.1_SBI, BOS5MI10.1_Burg_Staat) / 12

rm(S5MI10.1_Geslacht, S5MI10.1_Leeftijd, S5MI10.1_HH_Pos, S5MI10.1_HH_grootte, S5MI10.1_Woonregio_vorig_jaar, 
   S5MI10.1_Nationaliteit, S5MI10.1_Geboorteland, S5MI10.1_Onderwijsniveau, S5MI10.1_Econ_status, 
   S5MI10.1_Beroep, S5MI10.1_SBI, S5MI10.1_Burg_Staat)
rm(BS5MI10.1_Geslacht, BS5MI10.1_Leeftijd, BS5MI10.1_HH_Pos, BS5MI10.1_HH_grootte, BS5MI10.1_Woonregio_vorig_jaar, 
   BS5MI10.1_Nationaliteit, BS5MI10.1_Geboorteland, BS5MI10.1_Onderwijsniveau, BS5MI10.1_Econ_status, 
   BS5MI10.1_Beroep, BS5MI10.1_SBI, BS5MI10.1_Burg_Staat)
rm(BOS5MI10.1_Geslacht, BOS5MI10.1_Leeftijd, BOS5MI10.1_HH_Pos, BOS5MI10.1_HH_grootte, BOS5MI10.1_Woonregio_vorig_jaar, 
   BOS5MI10.1_Nationaliteit, BOS5MI10.1_Geboorteland, BOS5MI10.1_Onderwijsniveau, BOS5MI10.1_Econ_status, 
   BOS5MI10.1_Beroep, BOS5MI10.1_SBI, BOS5MI10.1_Burg_Staat)

# 10.2 data set 
BS5MI10.2_Geslacht <- Bias(S5MI10.2_Geslacht, Geslacht, "Geslacht")
BOS5MI10.2_Geslacht <- sum(BS5MI10.2_Geslacht$percentageABS)/nrow(BS5MI10.2_Geslacht)

BS5MI10.2_Leeftijd <- Bias(S5MI10.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI10.2_Leeftijd <- sum(BS5MI10.2_Leeftijd$percentageABS)/nrow(BS5MI10.2_Leeftijd)

BS5MI10.2_HH_Pos <- Bias(S5MI10.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI10.2_HH_Pos <- sum(BS5MI10.2_HH_Pos$percentageABS)/nrow(BS5MI10.2_HH_Pos)

BS5MI10.2_HH_grootte <- Bias(S5MI10.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI10.2_HH_grootte <- sum(BS5MI10.2_HH_grootte$percentageABS)/nrow(BS5MI10.2_HH_grootte)

BS5MI10.2_Woonregio_vorig_jaar <- Bias(S5MI10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI10.2_Woonregio_vorig_jaar <- sum(BS5MI10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI10.2_Woonregio_vorig_jaar)

BS5MI10.2_Nationaliteit <- Bias(S5MI10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI10.2_Nationaliteit <- sum(BS5MI10.2_Nationaliteit$percentageABS)/nrow(BS5MI10.2_Nationaliteit)

BS5MI10.2_Geboorteland <- Bias(S5MI10.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI10.2_Geboorteland <- sum(BS5MI10.2_Geboorteland$percentageABS)/nrow(BS5MI10.2_Geboorteland)

BS5MI10.2_Onderwijsniveau <- Bias(S5MI10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI10.2_Onderwijsniveau <- sum(BS5MI10.2_Onderwijsniveau$percentageABS)/nrow(BS5MI10.2_Onderwijsniveau)

BS5MI10.2_Econ_status <- Bias2(S5MI10.2_Econ_status, Econ_status, "Econ._status")
BOS5MI10.2_Econ_status <- sum(BS5MI10.2_Econ_status$percentageABS)/nrow(BS5MI10.2_Econ_status)

BS5MI10.2_Beroep <- Bias(S5MI10.2_Beroep, Beroep, "Beroep")
BOS5MI10.2_Beroep <- sum(BS5MI10.2_Beroep$percentageABS)/nrow(BS5MI10.2_Beroep)

BS5MI10.2_SBI <- Bias(S5MI10.2_SBI, SBI, "SBI")
BOS5MI10.2_SBI <- sum(BS5MI10.2_SBI$percentageABS)/nrow(BS5MI10.2_SBI)

BS5MI10.2_Burg_Staat <- Bias2(S5MI10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI10.2_Burg_Staat <- sum(BS5MI10.2_Burg_Staat$percentageABS)/nrow(BS5MI10.2_Burg_Staat)

BiasOverallS5MI10.2 <- sum(BOS5MI10.2_Geslacht, BOS5MI10.2_Leeftijd, BOS5MI10.2_HH_Pos, BOS5MI10.2_HH_grootte, BOS5MI10.2_Woonregio_vorig_jaar, 
                            BOS5MI10.2_Nationaliteit, BOS5MI10.2_Geboorteland, BOS5MI10.2_Onderwijsniveau, BOS5MI10.2_Econ_status, 
                            BOS5MI10.2_Beroep, BOS5MI10.2_SBI, BOS5MI10.2_Burg_Staat) / 12

rm(S5MI10.2_Geslacht, S5MI10.2_Leeftijd, S5MI10.2_HH_Pos, S5MI10.2_HH_grootte, S5MI10.2_Woonregio_vorig_jaar, 
   S5MI10.2_Nationaliteit, S5MI10.2_Geboorteland, S5MI10.2_Onderwijsniveau, S5MI10.2_Econ_status, 
   S5MI10.2_Beroep, S5MI10.2_SBI, S5MI10.2_Burg_Staat)
rm(BS5MI10.2_Geslacht, BS5MI10.2_Leeftijd, BS5MI10.2_HH_Pos, BS5MI10.2_HH_grootte, BS5MI10.2_Woonregio_vorig_jaar, 
   BS5MI10.2_Nationaliteit, BS5MI10.2_Geboorteland, BS5MI10.2_Onderwijsniveau, BS5MI10.2_Econ_status, 
   BS5MI10.2_Beroep, BS5MI10.2_SBI, BS5MI10.2_Burg_Staat)
rm(BOS5MI10.2_Geslacht, BOS5MI10.2_Leeftijd, BOS5MI10.2_HH_Pos, BOS5MI10.2_HH_grootte, BOS5MI10.2_Woonregio_vorig_jaar, 
   BOS5MI10.2_Nationaliteit, BOS5MI10.2_Geboorteland, BOS5MI10.2_Onderwijsniveau, BOS5MI10.2_Econ_status, 
   BOS5MI10.2_Beroep, BOS5MI10.2_SBI, BOS5MI10.2_Burg_Staat)

# 10.3 data set 
BS5MI10.3_Geslacht <- Bias(S5MI10.3_Geslacht, Geslacht, "Geslacht")
BOS5MI10.3_Geslacht <- sum(BS5MI10.3_Geslacht$percentageABS)/nrow(BS5MI10.3_Geslacht)

BS5MI10.3_Leeftijd <- Bias(S5MI10.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5MI10.3_Leeftijd <- sum(BS5MI10.3_Leeftijd$percentageABS)/nrow(BS5MI10.3_Leeftijd)

BS5MI10.3_HH_Pos <- Bias(S5MI10.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5MI10.3_HH_Pos <- sum(BS5MI10.3_HH_Pos$percentageABS)/nrow(BS5MI10.3_HH_Pos)

BS5MI10.3_HH_grootte <- Bias(S5MI10.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5MI10.3_HH_grootte <- sum(BS5MI10.3_HH_grootte$percentageABS)/nrow(BS5MI10.3_HH_grootte)

BS5MI10.3_Woonregio_vorig_jaar <- Bias(S5MI10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MI10.3_Woonregio_vorig_jaar <- sum(BS5MI10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MI10.3_Woonregio_vorig_jaar)

BS5MI10.3_Nationaliteit <- Bias(S5MI10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MI10.3_Nationaliteit <- sum(BS5MI10.3_Nationaliteit$percentageABS)/nrow(BS5MI10.3_Nationaliteit)

BS5MI10.3_Geboorteland <- Bias(S5MI10.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5MI10.3_Geboorteland <- sum(BS5MI10.3_Geboorteland$percentageABS)/nrow(BS5MI10.3_Geboorteland)

BS5MI10.3_Onderwijsniveau <- Bias(S5MI10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MI10.3_Onderwijsniveau <- sum(BS5MI10.3_Onderwijsniveau$percentageABS)/nrow(BS5MI10.3_Onderwijsniveau)

BS5MI10.3_Econ_status <- Bias2(S5MI10.3_Econ_status, Econ_status, "Econ._status")
BOS5MI10.3_Econ_status <- sum(BS5MI10.3_Econ_status$percentageABS)/nrow(BS5MI10.3_Econ_status)

BS5MI10.3_Beroep <- Bias(S5MI10.3_Beroep, Beroep, "Beroep")
BOS5MI10.3_Beroep <- sum(BS5MI10.3_Beroep$percentageABS)/nrow(BS5MI10.3_Beroep)

BS5MI10.3_SBI <- Bias(S5MI10.3_SBI, SBI, "SBI")
BOS5MI10.3_SBI <- sum(BS5MI10.3_SBI$percentageABS)/nrow(BS5MI10.3_SBI)

BS5MI10.3_Burg_Staat <- Bias2(S5MI10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MI10.3_Burg_Staat <- sum(BS5MI10.3_Burg_Staat$percentageABS)/nrow(BS5MI10.3_Burg_Staat)

BiasOverallS5MI10.3 <- sum(BOS5MI10.3_Geslacht, BOS5MI10.3_Leeftijd, BOS5MI10.3_HH_Pos, BOS5MI10.3_HH_grootte, BOS5MI10.3_Woonregio_vorig_jaar, 
                            BOS5MI10.3_Nationaliteit, BOS5MI10.3_Geboorteland, BOS5MI10.3_Onderwijsniveau, BOS5MI10.3_Econ_status, 
                            BOS5MI10.3_Beroep, BOS5MI10.3_SBI, BOS5MI10.3_Burg_Staat) / 12

rm(S5MI10.3_Geslacht, S5MI10.3_Leeftijd, S5MI10.3_HH_Pos, S5MI10.3_HH_grootte, S5MI10.3_Woonregio_vorig_jaar, 
   S5MI10.3_Nationaliteit, S5MI10.3_Geboorteland, S5MI10.3_Onderwijsniveau, S5MI10.3_Econ_status, 
   S5MI10.3_Beroep, S5MI10.3_SBI, S5MI10.3_Burg_Staat)
rm(BS5MI10.3_Geslacht, BS5MI10.3_Leeftijd, BS5MI10.3_HH_Pos, BS5MI10.3_HH_grootte, BS5MI10.3_Woonregio_vorig_jaar, 
   BS5MI10.3_Nationaliteit, BS5MI10.3_Geboorteland, BS5MI10.3_Onderwijsniveau, BS5MI10.3_Econ_status, 
   BS5MI10.3_Beroep, BS5MI10.3_SBI, BS5MI10.3_Burg_Staat)
rm(BOS5MI10.3_Geslacht, BOS5MI10.3_Leeftijd, BOS5MI10.3_HH_Pos, BOS5MI10.3_HH_grootte, BOS5MI10.3_Woonregio_vorig_jaar, 
   BOS5MI10.3_Nationaliteit, BOS5MI10.3_Geboorteland, BOS5MI10.3_Onderwijsniveau, BOS5MI10.3_Econ_status, 
   BOS5MI10.3_Beroep, BOS5MI10.3_SBI, BOS5MI10.3_Burg_Staat)


