

## Installing packages -----------------------------------------------------------------------------------------------------------------
#install.packages("missForest", dependencies = TRUE)
#install.packages("readxl")
#install.packages("devtools")
#install.packages("dplyr")
#install.packages("plyr")


## Loading packages --------------------------------------------------------------------------------------------------------------------
library("missForest")
library("readxl")
library("devtools")
install_github("jabiru/tictoc")
library("tictoc")
library("dplyr")
library("plyr")


## Loading MCAR data frames to use ----------------------------------------------------------------------------------------------------
sub_ipums5 <- get(load(file = "sub_ipums5.Rdata"))


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


# Setting correct and total to '0' 
S5RF2.1_correct <- S5RF2.2_correct <- S5RF2.3_correct <- S5RF5.1_correct <- S5RF5.2_correct <- S5RF5.3_correct <- S5RF10.1_correct <- S5RF10.2_correct <- S5RF10.3_correct <- 0
S5RF2.1_total <- S5RF2.2_total <- S5RF2.3_total <- S5RF5.1_total <- S5RF5.2_total <- S5RF5.3_total <- S5RF10.1_total <- S5RF10.2_total <- S5RF10.3_total <- 0


## Random Forest Imputation with 'missForest' ----------------------------------------------------------------------------------------
set.seed(8)


# 2% data sets

# 2.1 data set
sub5_MCAR2.1 <- get(load(file = "sub5_mcar21.Rdata"))
S5_RFMCAR2.1 <- data.frame(sub5_MCAR2.1)
names(S5_RFMCAR2.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR2.1)) {
  S5_RFMCAR2.1[, i] <- as.factor(S5_RFMCAR2.1[, i])
}

tic("Random Forest 2.1 processing time...")
S5random_forest2.1 <- missForest(S5_RFMCAR2.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest2.1 <- as.data.frame(S5random_forest2.1$ximp)
save(S5df_random_forest2.1, file = "S5random_forest21.Rdata")
S5RF2.1_correct <- S5RF2.1_correct + sum(sub_ipums5 == S5df_random_forest2.1)
S5RF2.1_total <- S5RF2.1_total + sum(!is.na(S5df_random_forest2.1))

# 2.2 data set
sub5_MCAR2.2 <- get(load(file = "sub5_mcar22.Rdata"))
S5_RFMCAR2.2 <- data.frame(sub5_MCAR2.2)
names(S5_RFMCAR2.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR2.2)) {
  S5_RFMCAR2.2[, i] <- as.factor(S5_RFMCAR2.2[, i])
}

tic("Random Forest 2.2 processing time...")
S5random_forest2.2 <- missForest(S5_RFMCAR2.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest2.2 <- as.data.frame(S5random_forest2.2$ximp)
save(S5df_random_forest2.2, file = "S5random_forest22.Rdata")
S5RF2.2_correct <- S5RF2.2_correct + sum(sub_ipums5 == S5df_random_forest2.2)
S5RF2.2_total <- S5RF2.2_total + sum(!is.na(S5df_random_forest2.2))

# 2.3 data set
sub5_MCAR2.3 <- get(load(file = "sub5_mcar23.Rdata"))
S5_RFMCAR2.3 <- data.frame(sub5_MCAR2.3)
names(S5_RFMCAR2.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR2.3)) {
  S5_RFMCAR2.3[, i] <- as.factor(S5_RFMCAR2.3[, i])
}

tic("Random Forest 2.3 processing time...")
S5random_forest2.3 <- missForest(S5_RFMCAR2.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest2.3 <- as.data.frame(S5random_forest2.3$ximp)
save(S5df_random_forest2.3, file = "S5random_forest23.Rdata")
S5RF2.3_correct <- S5RF2.3_correct + sum(sub_ipums5 == S5df_random_forest2.3)
S5RF2.3_total <- S5RF2.3_total + sum(!is.na(S5df_random_forest2.3))


# 5% data sets 

# 5.1 data set
sub5_MCAR5.1 <- get(load(file = "sub5_mcar51.Rdata"))
S5_RFMCAR5.1 <- data.frame(sub5_MCAR5.1)
names(S5_RFMCAR5.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR5.1)) {
  S5_RFMCAR5.1[, i] <- as.factor(S5_RFMCAR5.1[, i])
}

tic("Random Forest 5.1 processing time...")
S5random_forest5.1 <- missForest(S5_RFMCAR5.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest5.1 <- as.data.frame(S5random_forest5.1$ximp)
save(S5df_random_forest5.1, file = "S5random_forest51.Rdata")
S5RF5.1_correct <- S5RF5.1_correct + sum(sub_ipums5 == S5df_random_forest5.1)
S5RF5.1_total <- S5RF5.1_total + sum(!is.na(S5df_random_forest5.1))

# 5.2 data set
sub5_MCAR5.2 <- get(load(file = "sub5_mcar52.Rdata"))
S5_RFMCAR5.2 <- data.frame(sub5_MCAR5.2)
names(S5_RFMCAR5.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR5.2)) {
  S5_RFMCAR5.2[, i] <- as.factor(S5_RFMCAR5.2[, i])
}

tic("Random Forest 5.2 processing time...")
S5random_forest5.2 <- missForest(S5_RFMCAR5.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest5.2 <- as.data.frame(S5random_forest5.2$ximp)
save(S5df_random_forest5.2, file = "S5random_forest52.Rdata")
S5RF5.2_correct <- S5RF5.2_correct + sum(sub_ipums5 == S5df_random_forest5.2)
S5RF5.2_total <- S5RF5.2_total + sum(!is.na(S5df_random_forest5.2))

# 5.3 data set
sub5_MCAR5.3 <- get(load(file = "sub5_mcar53.Rdata"))
S5_RFMCAR5.3 <- data.frame(sub5_MCAR5.3)
names(S5_RFMCAR5.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR5.3)) {
  S5_RFMCAR5.3[, i] <- as.factor(S5_RFMCAR5.3[, i])
}

tic("Random Forest 5.3 processing time...")
S5random_forest5.3 <- missForest(S5_RFMCAR5.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest5.3 <- as.data.frame(S5random_forest5.3$ximp)
save(S5df_random_forest5.3, file = "S5random_forest53.Rdata")
S5RF5.3_correct <- S5RF5.3_correct + sum(sub_ipums5 == S5df_random_forest5.3)
S5RF5.3_total <- S5RF5.3_total + sum(!is.na(S5df_random_forest5.3))


# 10% data sets

# 10.1 data set
sub5_MCAR10.1 <- get(load(file = "sub5_mcar101.Rdata"))
S5_RFMCAR10.1 <- data.frame(sub5_MCAR10.1)
names(S5_RFMCAR10.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR10.1)) {
  S5_RFMCAR10.1[, i] <- as.factor(S5_RFMCAR10.1[, i])
}

tic("Random Forest 10.1 processing time...")
S5random_forest10.1 <- missForest(S5_RFMCAR10.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest10.1 <- as.data.frame(S5random_forest10.1$ximp)
save(S5df_random_forest10.1, file = "S5random_forest101.Rdata")
S5RF10.1_correct <- S5RF10.1_correct + sum(sub_ipums5 == S5df_random_forest10.1)
S5RF10.1_total <- S5RF10.1_total + sum(!is.na(S5df_random_forest10.1))

# 10.2 data set
sub5_MCAR10.2 <- get(load(file = "sub5_mcar102.Rdata"))
S5_RFMCAR10.2 <- data.frame(sub5_MCAR10.2)
names(S5_RFMCAR10.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR10.2)) {
  S5_RFMCAR10.2[, i] <- as.factor(S5_RFMCAR10.2[, i])
}

tic("Random Forest 10.2 processing time...")
S5random_forest10.2 <- missForest(S5_RFMCAR10.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest10.2 <- as.data.frame(S5random_forest10.2$ximp)
save(S5df_random_forest10.2, file = "S5random_forest102.Rdata")
S5RF10.2_correct <- S5RF10.2_correct + sum(sub_ipums5 == S5df_random_forest10.2)
S5RF10.2_total <- S5RF10.2_total + sum(!is.na(S5df_random_forest10.2))

# 10.3 data set
sub5_MCAR10.3 <- get(load(file = "sub5_mcar103.Rdata"))
S5_RFMCAR10.3 <- data.frame(sub5_MCAR10.3)
names(S5_RFMCAR10.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(S5_RFMCAR10.3)) {
  S5_RFMCAR10.3[, i] <- as.factor(S5_RFMCAR10.3[, i])
}

tic("Random Forest 10.3 processing time...")
S5random_forest10.3 <- missForest(S5_RFMCAR10.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
S5df_random_forest10.3 <- as.data.frame(S5random_forest10.3$ximp)
save(S5df_random_forest10.3, file = "S5random_forest103.Rdata")
S5RF10.3_correct <- S5RF10.3_correct + sum(sub_ipums5 == S5df_random_forest10.3)
S5RF10.3_total <- S5RF10.3_total + sum(!is.na(S5df_random_forest10.3))


# Check if all values are imputed 
#anyNA(c(df_random_forest2.1, df_random_forest2.2, df_random_forest2.3, df_random_forest5.1, df_random_forest5.2, 
#df_random_forest5.3, df_random_forest10.1, df_random_forest10.2, df_random_forest10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------


# Computing the accuracy of imputation 
S5RF2.1_accuracy <- S5RF2.1_correct / S5RF2.1_total
S5RF2.2_accuracy <- S5RF2.2_correct / S5RF2.2_total
S5RF2.3_accuracy <- S5RF2.3_correct / S5RF2.3_total

S5RF5.1_accuracy <- S5RF5.1_correct / S5RF5.1_total
S5RF5.2_accuracy <- S5RF5.2_correct / S5RF5.2_total
S5RF5.3_accuracy <- S5RF5.3_correct / S5RF5.3_total

S5RF10.1_accuracy <- S5RF10.1_correct / S5RF10.1_total
S5RF10.2_accuracy <- S5RF10.2_correct / S5RF10.2_total
S5RF10.3_accuracy <- S5RF10.3_correct / S5RF10.3_total




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
S5RF2.1_Geslacht <- plyr::count(S5df_random_forest2.1, 'Geslacht')
S5RF2.1_Leeftijd <- plyr::count(S5df_random_forest2.1, 'Leeftijd')
S5RF2.1_HH_Pos <- plyr::count(S5df_random_forest2.1, 'HH_Pos')
S5RF2.1_HH_grootte <- plyr::count(S5df_random_forest2.1, 'HH_grootte')
S5RF2.1_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest2.1, 'Woonregio_vorig_jaar')
S5RF2.1_Nationaliteit <- plyr::count(S5df_random_forest2.1, 'Nationaliteit')
S5RF2.1_Geboorteland <- plyr::count(S5df_random_forest2.1, 'Geboorteland')
S5RF2.1_Onderwijsniveau <- plyr::count(S5df_random_forest2.1, 'Onderwijsniveau')
S5RF2.1_Econ_status <- plyr::count(S5df_random_forest2.1, 'Econ._status')
S5RF2.1_Beroep <- plyr::count(S5df_random_forest2.1, 'Beroep')
S5RF2.1_SBI <- plyr::count(S5df_random_forest2.1, 'SBI')
S5RF2.1_Burg_Staat <- plyr::count(S5df_random_forest2.1, 'Burg._Staat')

S5RF2.2_Geslacht <- plyr::count(S5df_random_forest2.2, 'Geslacht')
S5RF2.2_Leeftijd <- plyr::count(S5df_random_forest2.2, 'Leeftijd')
S5RF2.2_HH_Pos <- plyr::count(S5df_random_forest2.2, 'HH_Pos')
S5RF2.2_HH_grootte <- plyr::count(S5df_random_forest2.2, 'HH_grootte')
S5RF2.2_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest2.2, 'Woonregio_vorig_jaar')
S5RF2.2_Nationaliteit <- plyr::count(S5df_random_forest2.2, 'Nationaliteit')
S5RF2.2_Geboorteland <- plyr::count(S5df_random_forest2.2, 'Geboorteland')
S5RF2.2_Onderwijsniveau <- plyr::count(S5df_random_forest2.2, 'Onderwijsniveau')
S5RF2.2_Econ_status <- plyr::count(S5df_random_forest2.2, 'Econ._status')
S5RF2.2_Beroep <- plyr::count(S5df_random_forest2.2, 'Beroep')
S5RF2.2_SBI <- plyr::count(S5df_random_forest2.2, 'SBI')
S5RF2.2_Burg_Staat <- plyr::count(S5df_random_forest2.2, 'Burg._Staat')

S5RF2.3_Geslacht <- plyr::count(S5df_random_forest2.3, 'Geslacht')
S5RF2.3_Leeftijd <- plyr::count(S5df_random_forest2.3, 'Leeftijd')
S5RF2.3_HH_Pos <- plyr::count(S5df_random_forest2.3, 'HH_Pos')
S5RF2.3_HH_grootte <- plyr::count(S5df_random_forest2.3, 'HH_grootte')
S5RF2.3_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest2.3, 'Woonregio_vorig_jaar')
S5RF2.3_Nationaliteit <- plyr::count(S5df_random_forest2.3, 'Nationaliteit')
S5RF2.3_Geboorteland <- plyr::count(S5df_random_forest2.3, 'Geboorteland')
S5RF2.3_Onderwijsniveau <- plyr::count(S5df_random_forest2.3, 'Onderwijsniveau')
S5RF2.3_Econ_status <- plyr::count(S5df_random_forest2.3, 'Econ._status')
S5RF2.3_Beroep <- plyr::count(S5df_random_forest2.3, 'Beroep')
S5RF2.3_SBI <- plyr::count(S5df_random_forest2.3, 'SBI')
S5RF2.3_Burg_Staat <- plyr::count(S5df_random_forest2.3, 'Burg._Staat')

rm(S5df_random_forest2.1, S5df_random_forest2.2, S5df_random_forest2.3)

# 2.1 data set 
BS5RF2.1_Geslacht <- Bias(S5RF2.1_Geslacht, Geslacht, "Geslacht")
BOS5RF2.1_Geslacht <- sum(BS5RF2.1_Geslacht$percentageABS)/nrow(BS5RF2.1_Geslacht)

BS5RF2.1_Leeftijd <- Bias(S5RF2.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF2.1_Leeftijd <- sum(BS5RF2.1_Leeftijd$percentageABS)/nrow(BS5RF2.1_Leeftijd)

BS5RF2.1_HH_Pos <- Bias(S5RF2.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF2.1_HH_Pos <- sum(BS5RF2.1_HH_Pos$percentageABS)/nrow(BS5RF2.1_HH_Pos)

BS5RF2.1_HH_grootte <- Bias(S5RF2.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF2.1_HH_grootte <- sum(BS5RF2.1_HH_grootte$percentageABS)/nrow(BS5RF2.1_HH_grootte)

BS5RF2.1_Woonregio_vorig_jaar <- Bias(S5RF2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF2.1_Woonregio_vorig_jaar <- sum(BS5RF2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF2.1_Woonregio_vorig_jaar)

BS5RF2.1_Nationaliteit <- Bias(S5RF2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF2.1_Nationaliteit <- sum(BS5RF2.1_Nationaliteit$percentageABS)/nrow(BS5RF2.1_Nationaliteit)

BS5RF2.1_Geboorteland <- Bias(S5RF2.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF2.1_Geboorteland <- sum(BS5RF2.1_Geboorteland$percentageABS)/nrow(BS5RF2.1_Geboorteland)

BS5RF2.1_Onderwijsniveau <- Bias(S5RF2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF2.1_Onderwijsniveau <- sum(BS5RF2.1_Onderwijsniveau$percentageABS)/nrow(BS5RF2.1_Onderwijsniveau)

BS5RF2.1_Econ_status <- Bias2(S5RF2.1_Econ_status, Econ_status, "Econ._status")
BOS5RF2.1_Econ_status <- sum(BS5RF2.1_Econ_status$percentageABS)/nrow(BS5RF2.1_Econ_status)

BS5RF2.1_Beroep <- Bias(S5RF2.1_Beroep, Beroep, "Beroep")
BOS5RF2.1_Beroep <- sum(BS5RF2.1_Beroep$percentageABS)/nrow(BS5RF2.1_Beroep)

BS5RF2.1_SBI <- Bias(S5RF2.1_SBI, SBI, "SBI")
BOS5RF2.1_SBI <- sum(BS5RF2.1_SBI$percentageABS)/nrow(BS5RF2.1_SBI)

BS5RF2.1_Burg_Staat <- Bias2(S5RF2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF2.1_Burg_Staat <- sum(BS5RF2.1_Burg_Staat$percentageABS)/nrow(BS5RF2.1_Burg_Staat)

BiasOverallS5RF2.1 <- sum(BOS5RF2.1_Geslacht, BOS5RF2.1_Leeftijd, BOS5RF2.1_HH_Pos, BOS5RF2.1_HH_grootte, BOS5RF2.1_Woonregio_vorig_jaar, 
                        BOS5RF2.1_Nationaliteit, BOS5RF2.1_Geboorteland, BOS5RF2.1_Onderwijsniveau, BOS5RF2.1_Econ_status, 
                        BOS5RF2.1_Beroep, BOS5RF2.1_SBI, BOS5RF2.1_Burg_Staat) / 12

rm(S5RF2.1_Geslacht, S5RF2.1_Leeftijd, S5RF2.1_HH_Pos, S5RF2.1_HH_grootte, S5RF2.1_Woonregio_vorig_jaar, 
   S5RF2.1_Nationaliteit, S5RF2.1_Geboorteland, S5RF2.1_Onderwijsniveau, S5RF2.1_Econ_status, 
   S5RF2.1_Beroep, S5RF2.1_SBI, S5RF2.1_Burg_Staat)
rm(BS5RF2.1_Geslacht, BS5RF2.1_Leeftijd, BS5RF2.1_HH_Pos, BS5RF2.1_HH_grootte, BS5RF2.1_Woonregio_vorig_jaar, 
   BS5RF2.1_Nationaliteit, BS5RF2.1_Geboorteland, BS5RF2.1_Onderwijsniveau, BS5RF2.1_Econ_status, 
   BS5RF2.1_Beroep, BS5RF2.1_SBI, BS5RF2.1_Burg_Staat)
rm(BOS5RF2.1_Geslacht, BOS5RF2.1_Leeftijd, BOS5RF2.1_HH_Pos, BOS5RF2.1_HH_grootte, BOS5RF2.1_Woonregio_vorig_jaar, 
   BOS5RF2.1_Nationaliteit, BOS5RF2.1_Geboorteland, BOS5RF2.1_Onderwijsniveau, BOS5RF2.1_Econ_status, 
   BOS5RF2.1_Beroep, BOS5RF2.1_SBI, BOS5RF2.1_Burg_Staat)

# 2.2 data set 
BS5RF2.2_Geslacht <- Bias(S5RF2.2_Geslacht, Geslacht, "Geslacht")
BOS5RF2.2_Geslacht <- sum(BS5RF2.2_Geslacht$percentageABS)/nrow(BS5RF2.2_Geslacht)

BS5RF2.2_Leeftijd <- Bias(S5RF2.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF2.2_Leeftijd <- sum(BS5RF2.2_Leeftijd$percentageABS)/nrow(BS5RF2.2_Leeftijd)

BS5RF2.2_HH_Pos <- Bias(S5RF2.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF2.2_HH_Pos <- sum(BS5RF2.2_HH_Pos$percentageABS)/nrow(BS5RF2.2_HH_Pos)

BS5RF2.2_HH_grootte <- Bias(S5RF2.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF2.2_HH_grootte <- sum(BS5RF2.2_HH_grootte$percentageABS)/nrow(BS5RF2.2_HH_grootte)

BS5RF2.2_Woonregio_vorig_jaar <- Bias(S5RF2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF2.2_Woonregio_vorig_jaar <- sum(BS5RF2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF2.2_Woonregio_vorig_jaar)

BS5RF2.2_Nationaliteit <- Bias(S5RF2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF2.2_Nationaliteit <- sum(BS5RF2.2_Nationaliteit$percentageABS)/nrow(BS5RF2.2_Nationaliteit)

BS5RF2.2_Geboorteland <- Bias(S5RF2.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF2.2_Geboorteland <- sum(BS5RF2.2_Geboorteland$percentageABS)/nrow(BS5RF2.2_Geboorteland)

BS5RF2.2_Onderwijsniveau <- Bias(S5RF2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF2.2_Onderwijsniveau <- sum(BS5RF2.2_Onderwijsniveau$percentageABS)/nrow(BS5RF2.2_Onderwijsniveau)

BS5RF2.2_Econ_status <- Bias(S5RF2.2_Econ_status, Econ_status, "Econ._status")
BOS5RF2.2_Econ_status <- sum(BS5RF2.2_Econ_status$percentageABS)/nrow(BS5RF2.2_Econ_status)

BS5RF2.2_Beroep <- Bias(S5RF2.2_Beroep, Beroep, "Beroep")
BOS5RF2.2_Beroep <- sum(BS5RF2.2_Beroep$percentageABS)/nrow(BS5RF2.2_Beroep)

BS5RF2.2_SBI <- Bias(S5RF2.2_SBI, SBI, "SBI")
BOS5RF2.2_SBI <- sum(BS5RF2.2_SBI$percentageABS)/nrow(BS5RF2.2_SBI)

BS5RF2.2_Burg_Staat <- Bias(S5RF2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF2.2_Burg_Staat <- sum(BS5RF2.2_Burg_Staat$percentageABS)/nrow(BS5RF2.2_Burg_Staat)

BiasOverallS5RF2.2 <- sum(BOS5RF2.2_Geslacht, BOS5RF2.2_Leeftijd, BOS5RF2.2_HH_Pos, BOS5RF2.2_HH_grootte, BOS5RF2.2_Woonregio_vorig_jaar, 
                        BOS5RF2.2_Nationaliteit, BOS5RF2.2_Geboorteland, BOS5RF2.2_Onderwijsniveau, BOS5RF2.2_Econ_status, 
                        BOS5RF2.2_Beroep, BOS5RF2.2_SBI, BOS5RF2.2_Burg_Staat) / 12

rm(S5RF2.2_Geslacht, S5RF2.2_Leeftijd, S5RF2.2_HH_Pos, S5RF2.2_HH_grootte, S5RF2.2_Woonregio_vorig_jaar, 
   S5RF2.2_Nationaliteit, S5RF2.2_Geboorteland, S5RF2.2_Onderwijsniveau, S5RF2.2_Econ_status, 
   S5RF2.2_Beroep, S5RF2.2_SBI, S5RF2.2_Burg_Staat)
rm(BS5RF2.2_Geslacht, BS5RF2.2_Leeftijd, BS5RF2.2_HH_Pos, BS5RF2.2_HH_grootte, BS5RF2.2_Woonregio_vorig_jaar, 
   BS5RF2.2_Nationaliteit, BS5RF2.2_Geboorteland, BS5RF2.2_Onderwijsniveau, BS5RF2.2_Econ_status, 
   BS5RF2.2_Beroep, BS5RF2.2_SBI, BS5RF2.2_Burg_Staat)
rm(BOS5RF2.2_Geslacht, BOS5RF2.2_Leeftijd, BOS5RF2.2_HH_Pos, BOS5RF2.2_HH_grootte, BOS5RF2.2_Woonregio_vorig_jaar, 
   BOS5RF2.2_Nationaliteit, BOS5RF2.2_Geboorteland, BOS5RF2.2_Onderwijsniveau, BOS5RF2.2_Econ_status, 
   BOS5RF2.2_Beroep, BOS5RF2.2_SBI, BOS5RF2.2_Burg_Staat)

# 2.3 data set 
BS5RF2.3_Geslacht <- Bias(S5RF2.3_Geslacht, Geslacht, "Geslacht")
BOS5RF2.3_Geslacht <- sum(BS5RF2.3_Geslacht$percentageABS)/nrow(BS5RF2.3_Geslacht)

BS5RF2.3_Leeftijd <- Bias(S5RF2.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF2.3_Leeftijd <- sum(BS5RF2.3_Leeftijd$percentageABS)/nrow(BS5RF2.3_Leeftijd)

BS5RF2.3_HH_Pos <- Bias(S5RF2.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF2.3_HH_Pos <- sum(BS5RF2.3_HH_Pos$percentageABS)/nrow(BS5RF2.3_HH_Pos)

BS5RF2.3_HH_grootte <- Bias(S5RF2.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF2.3_HH_grootte <- sum(BS5RF2.3_HH_grootte$percentageABS)/nrow(BS5RF2.3_HH_grootte)

BS5RF2.3_Woonregio_vorig_jaar <- Bias(S5RF2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF2.3_Woonregio_vorig_jaar <- sum(BS5RF2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF2.3_Woonregio_vorig_jaar)

BS5RF2.3_Nationaliteit <- Bias(S5RF2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF2.3_Nationaliteit <- sum(BS5RF2.3_Nationaliteit$percentageABS)/nrow(BS5RF2.3_Nationaliteit)

BS5RF2.3_Geboorteland <- Bias(S5RF2.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF2.3_Geboorteland <- sum(BS5RF2.3_Geboorteland$percentageABS)/nrow(BS5RF2.3_Geboorteland)

BS5RF2.3_Onderwijsniveau <- Bias(S5RF2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF2.3_Onderwijsniveau <- sum(BS5RF2.3_Onderwijsniveau$percentageABS)/nrow(BS5RF2.3_Onderwijsniveau)

BS5RF2.3_Econ_status <- Bias2(S5RF2.3_Econ_status, Econ_status, "Econ._status")
BOS5RF2.3_Econ_status <- sum(BS5RF2.3_Econ_status$percentageABS)/nrow(BS5RF2.3_Econ_status)

BS5RF2.3_Beroep <- Bias(S5RF2.3_Beroep, Beroep, "Beroep")
BOS5RF2.3_Beroep <- sum(BS5RF2.3_Beroep$percentageABS)/nrow(BS5RF2.3_Beroep)

BS5RF2.3_SBI <- Bias(S5RF2.3_SBI, SBI, "SBI")
BOS5RF2.3_SBI <- sum(BS5RF2.3_SBI$percentageABS)/nrow(BS5RF2.3_SBI)

BS5RF2.3_Burg_Staat <- Bias2(S5RF2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF2.3_Burg_Staat <- sum(BS5RF2.3_Burg_Staat$percentageABS)/nrow(BS5RF2.3_Burg_Staat)

BiasOverallS5RF2.3 <- sum(BOS5RF2.3_Geslacht, BOS5RF2.3_Leeftijd, BOS5RF2.3_HH_Pos, BOS5RF2.3_HH_grootte, BOS5RF2.3_Woonregio_vorig_jaar, 
                        BOS5RF2.3_Nationaliteit, BOS5RF2.3_Geboorteland, BOS5RF2.3_Onderwijsniveau, BOS5RF2.3_Econ_status, 
                        BOS5RF2.3_Beroep, BOS5RF2.3_SBI, BOS5RF2.3_Burg_Staat) / 12

rm(S5RF2.3_Geslacht, S5RF2.3_Leeftijd, S5RF2.3_HH_Pos, S5RF2.3_HH_grootte, S5RF2.3_Woonregio_vorig_jaar, 
   S5RF2.3_Nationaliteit, S5RF2.3_Geboorteland, S5RF2.3_Onderwijsniveau, S5RF2.3_Econ_status, 
   S5RF2.3_Beroep, S5RF2.3_SBI, S5RF2.3_Burg_Staat)
rm(BS5RF2.3_Geslacht, BS5RF2.3_Leeftijd, BS5RF2.3_HH_Pos, BS5RF2.3_HH_grootte, BS5RF2.3_Woonregio_vorig_jaar, 
   BS5RF2.3_Nationaliteit, BS5RF2.3_Geboorteland, BS5RF2.3_Onderwijsniveau, BS5RF2.3_Econ_status, 
   BS5RF2.3_Beroep, BS5RF2.3_SBI, BS5RF2.3_Burg_Staat)
rm(BOS5RF2.3_Geslacht, BOS5RF2.3_Leeftijd, BOS5RF2.3_HH_Pos, BOS5RF2.3_HH_grootte, BOS5RF2.3_Woonregio_vorig_jaar, 
   BOS5RF2.3_Nationaliteit, BOS5RF2.3_Geboorteland, BOS5RF2.3_Onderwijsniveau, BOS5RF2.3_Econ_status, 
   BOS5RF2.3_Beroep, BOS5RF2.3_SBI, BOS5RF2.3_Burg_Staat)


# 5% data sets

# Counting values
S5RF5.1_Geslacht <- plyr::count(S5df_random_forest5.1, 'Geslacht')
S5RF5.1_Leeftijd <- plyr::count(S5df_random_forest5.1, 'Leeftijd')
S5RF5.1_HH_Pos <- plyr::count(S5df_random_forest5.1, 'HH_Pos')
S5RF5.1_HH_grootte <- plyr::count(S5df_random_forest5.1, 'HH_grootte')
S5RF5.1_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest5.1, 'Woonregio_vorig_jaar')
S5RF5.1_Nationaliteit <- plyr::count(S5df_random_forest5.1, 'Nationaliteit')
S5RF5.1_Geboorteland <- plyr::count(S5df_random_forest5.1, 'Geboorteland')
S5RF5.1_Onderwijsniveau <- plyr::count(S5df_random_forest5.1, 'Onderwijsniveau')
S5RF5.1_Econ_status <- plyr::count(S5df_random_forest5.1, 'Econ._status')
S5RF5.1_Beroep <- plyr::count(S5df_random_forest5.1, 'Beroep')
S5RF5.1_SBI <- plyr::count(S5df_random_forest5.1, 'SBI')
S5RF5.1_Burg_Staat <- plyr::count(S5df_random_forest5.1, 'Burg._Staat')

S5RF5.2_Geslacht <- plyr::count(S5df_random_forest5.2, 'Geslacht')
S5RF5.2_Leeftijd <- plyr::count(S5df_random_forest5.2, 'Leeftijd')
S5RF5.2_HH_Pos <- plyr::count(S5df_random_forest5.2, 'HH_Pos')
S5RF5.2_HH_grootte <- plyr::count(S5df_random_forest5.2, 'HH_grootte')
S5RF5.2_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest5.2, 'Woonregio_vorig_jaar')
S5RF5.2_Nationaliteit <- plyr::count(S5df_random_forest5.2, 'Nationaliteit')
S5RF5.2_Geboorteland <- plyr::count(S5df_random_forest5.2, 'Geboorteland')
S5RF5.2_Onderwijsniveau <- plyr::count(S5df_random_forest5.2, 'Onderwijsniveau')
S5RF5.2_Econ_status <- plyr::count(S5df_random_forest5.2, 'Econ._status')
S5RF5.2_Beroep <- plyr::count(S5df_random_forest5.2, 'Beroep')
S5RF5.2_SBI <- plyr::count(S5df_random_forest5.2, 'SBI')
S5RF5.2_Burg_Staat <- plyr::count(S5df_random_forest5.2, 'Burg._Staat')

S5RF5.3_Geslacht <- plyr::count(S5df_random_forest5.3, 'Geslacht')
S5RF5.3_Leeftijd <- plyr::count(S5df_random_forest5.3, 'Leeftijd')
S5RF5.3_HH_Pos <- plyr::count(S5df_random_forest5.3, 'HH_Pos')
S5RF5.3_HH_grootte <- plyr::count(S5df_random_forest5.3, 'HH_grootte')
S5RF5.3_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest5.3, 'Woonregio_vorig_jaar')
S5RF5.3_Nationaliteit <- plyr::count(S5df_random_forest5.3, 'Nationaliteit')
S5RF5.3_Geboorteland <- plyr::count(S5df_random_forest5.3, 'Geboorteland')
S5RF5.3_Onderwijsniveau <- plyr::count(S5df_random_forest5.3, 'Onderwijsniveau')
S5RF5.3_Econ_status <- plyr::count(S5df_random_forest5.3, 'Econ._status')
S5RF5.3_Beroep <- plyr::count(S5df_random_forest5.3, 'Beroep')
S5RF5.3_SBI <- plyr::count(S5df_random_forest5.3, 'SBI')
S5RF5.3_Burg_Staat <- plyr::count(S5df_random_forest5.3, 'Burg._Staat')

rm(S5df_random_forest5.1, S5df_random_forest5.2, S5df_random_forest5.3)


# 5.1 data set 
BS5RF5.1_Geslacht <- Bias(S5RF5.1_Geslacht, Geslacht, "Geslacht")
BOS5RF5.1_Geslacht <- sum(BS5RF5.1_Geslacht$percentageABS)/nrow(BS5RF5.1_Geslacht)

BS5RF5.1_Leeftijd <- Bias(S5RF5.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF5.1_Leeftijd <- sum(BS5RF5.1_Leeftijd$percentageABS)/nrow(BS5RF5.1_Leeftijd)

BS5RF5.1_HH_Pos <- Bias(S5RF5.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF5.1_HH_Pos <- sum(BS5RF5.1_HH_Pos$percentageABS)/nrow(BS5RF5.1_HH_Pos)

BS5RF5.1_HH_grootte <- Bias(S5RF5.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF5.1_HH_grootte <- sum(BS5RF5.1_HH_grootte$percentageABS)/nrow(BS5RF5.1_HH_grootte)

BS5RF5.1_Woonregio_vorig_jaar <- Bias(S5RF5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF5.1_Woonregio_vorig_jaar <- sum(BS5RF5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF5.1_Woonregio_vorig_jaar)

BS5RF5.1_Nationaliteit <- Bias(S5RF5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF5.1_Nationaliteit <- sum(BS5RF5.1_Nationaliteit$percentageABS)/nrow(BS5RF5.1_Nationaliteit)

BS5RF5.1_Geboorteland <- Bias(S5RF5.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF5.1_Geboorteland <- sum(BS5RF5.1_Geboorteland$percentageABS)/nrow(BS5RF5.1_Geboorteland)

BS5RF5.1_Onderwijsniveau <- Bias(S5RF5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF5.1_Onderwijsniveau <- sum(BS5RF5.1_Onderwijsniveau$percentageABS)/nrow(BS5RF5.1_Onderwijsniveau)

BS5RF5.1_Econ_status <- Bias2(S5RF5.1_Econ_status, Econ_status, "Econ._status")
BOS5RF5.1_Econ_status <- sum(BS5RF5.1_Econ_status$percentageABS)/nrow(BS5RF5.1_Econ_status)

BS5RF5.1_Beroep <- Bias(S5RF5.1_Beroep, Beroep, "Beroep")
BOS5RF5.1_Beroep <- sum(BS5RF5.1_Beroep$percentageABS)/nrow(BS5RF5.1_Beroep)

BS5RF5.1_SBI <- Bias(S5RF5.1_SBI, SBI, "SBI")
BOS5RF5.1_SBI <- sum(BS5RF5.1_SBI$percentageABS)/nrow(BS5RF5.1_SBI)

BS5RF5.1_Burg_Staat <- Bias2(S5RF5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF5.1_Burg_Staat <- sum(BS5RF5.1_Burg_Staat$percentageABS)/nrow(BS5RF5.1_Burg_Staat)

BiasOverallS5RF5.1 <- sum(BOS5RF5.1_Geslacht, BOS5RF5.1_Leeftijd, BOS5RF5.1_HH_Pos, BOS5RF5.1_HH_grootte, BOS5RF5.1_Woonregio_vorig_jaar, 
                        BOS5RF5.1_Nationaliteit, BOS5RF5.1_Geboorteland, BOS5RF5.1_Onderwijsniveau, BOS5RF5.1_Econ_status, 
                        BOS5RF5.1_Beroep, BOS5RF5.1_SBI, BOS5RF5.1_Burg_Staat) / 12

rm(S5RF5.1_Geslacht, S5RF5.1_Leeftijd, S5RF5.1_HH_Pos, S5RF5.1_HH_grootte, S5RF5.1_Woonregio_vorig_jaar, 
   S5RF5.1_Nationaliteit, S5RF5.1_Geboorteland, S5RF5.1_Onderwijsniveau, S5RF5.1_Econ_status, 
   S5RF5.1_Beroep, S5RF5.1_SBI, S5RF5.1_Burg_Staat)
rm(BS5RF5.1_Geslacht, BS5RF5.1_Leeftijd, BS5RF5.1_HH_Pos, BS5RF5.1_HH_grootte, BS5RF5.1_Woonregio_vorig_jaar, 
   BS5RF5.1_Nationaliteit, BS5RF5.1_Geboorteland, BS5RF5.1_Onderwijsniveau, BS5RF5.1_Econ_status, 
   BS5RF5.1_Beroep, BS5RF5.1_SBI, BS5RF5.1_Burg_Staat)
rm(BOS5RF5.1_Geslacht, BOS5RF5.1_Leeftijd, BOS5RF5.1_HH_Pos, BOS5RF5.1_HH_grootte, BOS5RF5.1_Woonregio_vorig_jaar, 
   BOS5RF5.1_Nationaliteit, BOS5RF5.1_Geboorteland, BOS5RF5.1_Onderwijsniveau, BOS5RF5.1_Econ_status, 
   BOS5RF5.1_Beroep, BOS5RF5.1_SBI, BOS5RF5.1_Burg_Staat)

# 5.2 data set 
BS5RF5.2_Geslacht <- Bias(S5RF5.2_Geslacht, Geslacht, "Geslacht")
BOS5RF5.2_Geslacht <- sum(BS5RF5.2_Geslacht$percentageABS)/nrow(BS5RF5.2_Geslacht)

BS5RF5.2_Leeftijd <- Bias(S5RF5.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF5.2_Leeftijd <- sum(BS5RF5.2_Leeftijd$percentageABS)/nrow(BS5RF5.2_Leeftijd)

BS5RF5.2_HH_Pos <- Bias(S5RF5.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF5.2_HH_Pos <- sum(BS5RF5.2_HH_Pos$percentageABS)/nrow(BS5RF5.2_HH_Pos)

BS5RF5.2_HH_grootte <- Bias(S5RF5.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF5.2_HH_grootte <- sum(BS5RF5.2_HH_grootte$percentageABS)/nrow(BS5RF5.2_HH_grootte)

BS5RF5.2_Woonregio_vorig_jaar <- Bias(S5RF5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF5.2_Woonregio_vorig_jaar <- sum(BS5RF5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF5.2_Woonregio_vorig_jaar)

BS5RF5.2_Nationaliteit <- Bias(S5RF5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF5.2_Nationaliteit <- sum(BS5RF5.2_Nationaliteit$percentageABS)/nrow(BS5RF5.2_Nationaliteit)

BS5RF5.2_Geboorteland <- Bias(S5RF5.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF5.2_Geboorteland <- sum(BS5RF5.2_Geboorteland$percentageABS)/nrow(BS5RF5.2_Geboorteland)

BS5RF5.2_Onderwijsniveau <- Bias(S5RF5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF5.2_Onderwijsniveau <- sum(BS5RF5.2_Onderwijsniveau$percentageABS)/nrow(BS5RF5.2_Onderwijsniveau)

BS5RF5.2_Econ_status <- Bias2(S5RF5.2_Econ_status, Econ_status, "Econ._status")
BOS5RF5.2_Econ_status <- sum(BS5RF5.2_Econ_status$percentageABS)/nrow(BS5RF5.2_Econ_status)

BS5RF5.2_Beroep <- Bias(S5RF5.2_Beroep, Beroep, "Beroep")
BOS5RF5.2_Beroep <- sum(BS5RF5.2_Beroep$percentageABS)/nrow(BS5RF5.2_Beroep)

BS5RF5.2_SBI <- Bias(S5RF5.2_SBI, SBI, "SBI")
BOS5RF5.2_SBI <- sum(BS5RF5.2_SBI$percentageABS)/nrow(BS5RF5.2_SBI)

BS5RF5.2_Burg_Staat <- Bias2(S5RF5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF5.2_Burg_Staat <- sum(BS5RF5.2_Burg_Staat$percentageABS)/nrow(BS5RF5.2_Burg_Staat)

BiasOverallS5RF5.2 <- sum(BOS5RF5.2_Geslacht, BOS5RF5.2_Leeftijd, BOS5RF5.2_HH_Pos, BOS5RF5.2_HH_grootte, BOS5RF5.2_Woonregio_vorig_jaar, 
                        BOS5RF5.2_Nationaliteit, BOS5RF5.2_Geboorteland, BOS5RF5.2_Onderwijsniveau, BOS5RF5.2_Econ_status, 
                        BOS5RF5.2_Beroep, BOS5RF5.2_SBI, BOS5RF5.2_Burg_Staat) / 12

rm(S5RF5.2_Geslacht, S5RF5.2_Leeftijd, S5RF5.2_HH_Pos, S5RF5.2_HH_grootte, S5RF5.2_Woonregio_vorig_jaar, 
   S5RF5.2_Nationaliteit, S5RF5.2_Geboorteland, S5RF5.2_Onderwijsniveau, S5RF5.2_Econ_status, 
   S5RF5.2_Beroep, S5RF5.2_SBI, S5RF5.2_Burg_Staat)
rm(BS5RF5.2_Geslacht, BS5RF5.2_Leeftijd, BS5RF5.2_HH_Pos, BS5RF5.2_HH_grootte, BS5RF5.2_Woonregio_vorig_jaar, 
   BS5RF5.2_Nationaliteit, BS5RF5.2_Geboorteland, BS5RF5.2_Onderwijsniveau, BS5RF5.2_Econ_status, 
   BS5RF5.2_Beroep, BS5RF5.2_SBI, BS5RF5.2_Burg_Staat)
rm(BOS5RF5.2_Geslacht, BOS5RF5.2_Leeftijd, BOS5RF5.2_HH_Pos, BOS5RF5.2_HH_grootte, BOS5RF5.2_Woonregio_vorig_jaar, 
   BOS5RF5.2_Nationaliteit, BOS5RF5.2_Geboorteland, BOS5RF5.2_Onderwijsniveau, BOS5RF5.2_Econ_status, 
   BOS5RF5.2_Beroep, BOS5RF5.2_SBI, BOS5RF5.2_Burg_Staat)

# 5.3 data set 
BS5RF5.3_Geslacht <- Bias(S5RF5.3_Geslacht, Geslacht, "Geslacht")
BOS5RF5.3_Geslacht <- sum(BS5RF5.3_Geslacht$percentageABS)/nrow(BS5RF5.3_Geslacht)

BS5RF5.3_Leeftijd <- Bias(S5RF5.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF5.3_Leeftijd <- sum(BS5RF5.3_Leeftijd$percentageABS)/nrow(BS5RF5.3_Leeftijd)

BS5RF5.3_HH_Pos <- Bias(S5RF5.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF5.3_HH_Pos <- sum(BS5RF5.3_HH_Pos$percentageABS)/nrow(BS5RF5.3_HH_Pos)

BS5RF5.3_HH_grootte <- Bias(S5RF5.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF5.3_HH_grootte <- sum(BS5RF5.3_HH_grootte$percentageABS)/nrow(BS5RF5.3_HH_grootte)

BS5RF5.3_Woonregio_vorig_jaar <- Bias(S5RF5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF5.3_Woonregio_vorig_jaar <- sum(BS5RF5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF5.3_Woonregio_vorig_jaar)

BS5RF5.3_Nationaliteit <- Bias(S5RF5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF5.3_Nationaliteit <- sum(BS5RF5.3_Nationaliteit$percentageABS)/nrow(BS5RF5.3_Nationaliteit)

BS5RF5.3_Geboorteland <- Bias(S5RF5.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF5.3_Geboorteland <- sum(BS5RF5.3_Geboorteland$percentageABS)/nrow(BS5RF5.3_Geboorteland)

BS5RF5.3_Onderwijsniveau <- Bias(S5RF5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF5.3_Onderwijsniveau <- sum(BS5RF5.3_Onderwijsniveau$percentageABS)/nrow(BS5RF5.3_Onderwijsniveau)

BS5RF5.3_Econ_status <- Bias2(S5RF5.3_Econ_status, Econ_status, "Econ._status")
BOS5RF5.3_Econ_status <- sum(BS5RF5.3_Econ_status$percentageABS)/nrow(BS5RF5.3_Econ_status)

BS5RF5.3_Beroep <- Bias(S5RF5.3_Beroep, Beroep, "Beroep")
BOS5RF5.3_Beroep <- sum(BS5RF5.3_Beroep$percentageABS)/nrow(BS5RF5.3_Beroep)

BS5RF5.3_SBI <- Bias(S5RF5.3_SBI, SBI, "SBI")
BOS5RF5.3_SBI <- sum(BS5RF5.3_SBI$percentageABS)/nrow(BS5RF5.3_SBI)

BS5RF5.3_Burg_Staat <- Bias2(S5RF5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF5.3_Burg_Staat <- sum(BS5RF5.3_Burg_Staat$percentageABS)/nrow(BS5RF5.3_Burg_Staat)

BiasOverallS5RF5.3 <- sum(BOS5RF5.3_Geslacht, BOS5RF5.3_Leeftijd, BOS5RF5.3_HH_Pos, BOS5RF5.3_HH_grootte, BOS5RF5.3_Woonregio_vorig_jaar, 
                        BOS5RF5.3_Nationaliteit, BOS5RF5.3_Geboorteland, BOS5RF5.3_Onderwijsniveau, BOS5RF5.3_Econ_status, 
                        BOS5RF5.3_Beroep, BOS5RF5.3_SBI, BOS5RF5.3_Burg_Staat) / 12

rm(S5RF5.3_Geslacht, S5RF5.3_Leeftijd, S5RF5.3_HH_Pos, S5RF5.3_HH_grootte, S5RF5.3_Woonregio_vorig_jaar, 
   S5RF5.3_Nationaliteit, S5RF5.3_Geboorteland, S5RF5.3_Onderwijsniveau, S5RF5.3_Econ_status, 
   S5RF5.3_Beroep, S5RF5.3_SBI, S5RF5.3_Burg_Staat)
rm(BS5RF5.3_Geslacht, BS5RF5.3_Leeftijd, BS5RF5.3_HH_Pos, BS5RF5.3_HH_grootte, BS5RF5.3_Woonregio_vorig_jaar, 
   BS5RF5.3_Nationaliteit, BS5RF5.3_Geboorteland, BS5RF5.3_Onderwijsniveau, BS5RF5.3_Econ_status, 
   BS5RF5.3_Beroep, BS5RF5.3_SBI, BS5RF5.3_Burg_Staat)
rm(BOS5RF5.3_Geslacht, BOS5RF5.3_Leeftijd, BOS5RF5.3_HH_Pos, BOS5RF5.3_HH_grootte, BOS5RF5.3_Woonregio_vorig_jaar, 
   BOS5RF5.3_Nationaliteit, BOS5RF5.3_Geboorteland, BOS5RF5.3_Onderwijsniveau, BOS5RF5.3_Econ_status, 
   BOS5RF5.3_Beroep, BOS5RF5.3_SBI, BOS5RF5.3_Burg_Staat)


# 10% data sets

# Counting values
S5RF10.1_Geslacht <- plyr::count(S5df_random_forest10.1, 'Geslacht')
S5RF10.1_Leeftijd <- plyr::count(S5df_random_forest10.1, 'Leeftijd')
S5RF10.1_HH_Pos <- plyr::count(S5df_random_forest10.1, 'HH_Pos')
S5RF10.1_HH_grootte <- plyr::count(S5df_random_forest10.1, 'HH_grootte')
S5RF10.1_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest10.1, 'Woonregio_vorig_jaar')
S5RF10.1_Nationaliteit <- plyr::count(S5df_random_forest10.1, 'Nationaliteit')
S5RF10.1_Geboorteland <- plyr::count(S5df_random_forest10.1, 'Geboorteland')
S5RF10.1_Onderwijsniveau <- plyr::count(S5df_random_forest10.1, 'Onderwijsniveau')
S5RF10.1_Econ_status <- plyr::count(S5df_random_forest10.1, 'Econ._status')
S5RF10.1_Beroep <- plyr::count(S5df_random_forest10.1, 'Beroep')
S5RF10.1_SBI <- plyr::count(S5df_random_forest10.1, 'SBI')
S5RF10.1_Burg_Staat <- plyr::count(S5df_random_forest10.1, 'Burg._Staat')

S5RF10.2_Geslacht <- plyr::count(S5df_random_forest10.2, 'Geslacht')
S5RF10.2_Leeftijd <- plyr::count(S5df_random_forest10.2, 'Leeftijd')
S5RF10.2_HH_Pos <- plyr::count(S5df_random_forest10.2, 'HH_Pos')
S5RF10.2_HH_grootte <- plyr::count(S5df_random_forest10.2, 'HH_grootte')
S5RF10.2_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest10.2, 'Woonregio_vorig_jaar')
S5RF10.2_Nationaliteit <- plyr::count(S5df_random_forest10.2, 'Nationaliteit')
S5RF10.2_Geboorteland <- plyr::count(S5df_random_forest10.2, 'Geboorteland')
S5RF10.2_Onderwijsniveau <- plyr::count(S5df_random_forest10.2, 'Onderwijsniveau')
S5RF10.2_Econ_status <- plyr::count(S5df_random_forest10.2, 'Econ._status')
S5RF10.2_Beroep <- plyr::count(S5df_random_forest10.2, 'Beroep')
S5RF10.2_SBI <- plyr::count(S5df_random_forest10.2, 'SBI')
S5RF10.2_Burg_Staat <- plyr::count(S5df_random_forest10.2, 'Burg._Staat')

S5RF10.3_Geslacht <- plyr::count(S5df_random_forest10.3, 'Geslacht')
S5RF10.3_Leeftijd <- plyr::count(S5df_random_forest10.3, 'Leeftijd')
S5RF10.3_HH_Pos <- plyr::count(S5df_random_forest10.3, 'HH_Pos')
S5RF10.3_HH_grootte <- plyr::count(S5df_random_forest10.3, 'HH_grootte')
S5RF10.3_Woonregio_vorig_jaar <- plyr::count(S5df_random_forest10.3, 'Woonregio_vorig_jaar')
S5RF10.3_Nationaliteit <- plyr::count(S5df_random_forest10.3, 'Nationaliteit')
S5RF10.3_Geboorteland <- plyr::count(S5df_random_forest10.3, 'Geboorteland')
S5RF10.3_Onderwijsniveau <- plyr::count(S5df_random_forest10.3, 'Onderwijsniveau')
S5RF10.3_Econ_status <- plyr::count(S5df_random_forest10.3, 'Econ._status')
S5RF10.3_Beroep <- plyr::count(S5df_random_forest10.3, 'Beroep')
S5RF10.3_SBI <- plyr::count(S5df_random_forest10.3, 'SBI')
S5RF10.3_Burg_Staat <- plyr::count(S5df_random_forest10.3, 'Burg._Staat')

rm(S5df_random_forest10.1, S5df_random_forest10.2, S5df_random_forest10.3)


# 10.1 data set 
BS5RF10.1_Geslacht <- Bias(S5RF10.1_Geslacht, Geslacht, "Geslacht")
BOS5RF10.1_Geslacht <- sum(BS5RF10.1_Geslacht$percentageABS)/nrow(BS5RF10.1_Geslacht)

BS5RF10.1_Leeftijd <- Bias(S5RF10.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF10.1_Leeftijd <- sum(BS5RF10.1_Leeftijd$percentageABS)/nrow(BS5RF10.1_Leeftijd)

BS5RF10.1_HH_Pos <- Bias(S5RF10.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF10.1_HH_Pos <- sum(BS5RF10.1_HH_Pos$percentageABS)/nrow(BS5RF10.1_HH_Pos)

BS5RF10.1_HH_grootte <- Bias(S5RF10.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF10.1_HH_grootte <- sum(BS5RF10.1_HH_grootte$percentageABS)/nrow(BS5RF10.1_HH_grootte)

BS5RF10.1_Woonregio_vorig_jaar <- Bias(S5RF10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF10.1_Woonregio_vorig_jaar <- sum(BS5RF10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF10.1_Woonregio_vorig_jaar)

BS5RF10.1_Nationaliteit <- Bias(S5RF10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF10.1_Nationaliteit <- sum(BS5RF10.1_Nationaliteit$percentageABS)/nrow(BS5RF10.1_Nationaliteit)

BS5RF10.1_Geboorteland <- Bias(S5RF10.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF10.1_Geboorteland <- sum(BS5RF10.1_Geboorteland$percentageABS)/nrow(BS5RF10.1_Geboorteland)

BS5RF10.1_Onderwijsniveau <- Bias(S5RF10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF10.1_Onderwijsniveau <- sum(BS5RF10.1_Onderwijsniveau$percentageABS)/nrow(BS5RF10.1_Onderwijsniveau)

BS5RF10.1_Econ_status <- Bias2(S5RF10.1_Econ_status, Econ_status, "Econ._status")
BOS5RF10.1_Econ_status <- sum(BS5RF10.1_Econ_status$percentageABS)/nrow(BS5RF10.1_Econ_status)

BS5RF10.1_Beroep <- Bias(S5RF10.1_Beroep, Beroep, "Beroep")
BOS5RF10.1_Beroep <- sum(BS5RF10.1_Beroep$percentageABS)/nrow(BS5RF10.1_Beroep)

BS5RF10.1_SBI <- Bias(S5RF10.1_SBI, SBI, "SBI")
BOS5RF10.1_SBI <- sum(BS5RF10.1_SBI$percentageABS)/nrow(BS5RF10.1_SBI)

BS5RF10.1_Burg_Staat <- Bias2(S5RF10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF10.1_Burg_Staat <- sum(BS5RF10.1_Burg_Staat$percentageABS)/nrow(BS5RF10.1_Burg_Staat)

BiasOverallS5RF10.1 <- sum(BOS5RF10.1_Geslacht, BOS5RF10.1_Leeftijd, BOS5RF10.1_HH_Pos, BOS5RF10.1_HH_grootte, BOS5RF10.1_Woonregio_vorig_jaar, 
                         BOS5RF10.1_Nationaliteit, BOS5RF10.1_Geboorteland, BOS5RF10.1_Onderwijsniveau, BOS5RF10.1_Econ_status, 
                         BOS5RF10.1_Beroep, BOS5RF10.1_SBI, BOS5RF10.1_Burg_Staat) / 12

rm(S5RF10.1_Geslacht, S5RF10.1_Leeftijd, S5RF10.1_HH_Pos, S5RF10.1_HH_grootte, S5RF10.1_Woonregio_vorig_jaar, 
   S5RF10.1_Nationaliteit, S5RF10.1_Geboorteland, S5RF10.1_Onderwijsniveau, S5RF10.1_Econ_status, 
   S5RF10.1_Beroep, S5RF10.1_SBI, S5RF10.1_Burg_Staat)
rm(BS5RF10.1_Geslacht, BS5RF10.1_Leeftijd, BS5RF10.1_HH_Pos, BS5RF10.1_HH_grootte, BS5RF10.1_Woonregio_vorig_jaar, 
   BS5RF10.1_Nationaliteit, BS5RF10.1_Geboorteland, BS5RF10.1_Onderwijsniveau, BS5RF10.1_Econ_status, 
   BS5RF10.1_Beroep, BS5RF10.1_SBI, BS5RF10.1_Burg_Staat)
rm(BOS5RF10.1_Geslacht, BOS5RF10.1_Leeftijd, BOS5RF10.1_HH_Pos, BOS5RF10.1_HH_grootte, BOS5RF10.1_Woonregio_vorig_jaar, 
   BOS5RF10.1_Nationaliteit, BOS5RF10.1_Geboorteland, BOS5RF10.1_Onderwijsniveau, BOS5RF10.1_Econ_status, 
   BOS5RF10.1_Beroep, BOS5RF10.1_SBI, BOS5RF10.1_Burg_Staat)

# 10.2 data set 
BS5RF10.2_Geslacht <- Bias(S5RF10.2_Geslacht, Geslacht, "Geslacht")
BOS5RF10.2_Geslacht <- sum(BS5RF10.2_Geslacht$percentageABS)/nrow(BS5RF10.2_Geslacht)

BS5RF10.2_Leeftijd <- Bias(S5RF10.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF10.2_Leeftijd <- sum(BS5RF10.2_Leeftijd$percentageABS)/nrow(BS5RF10.2_Leeftijd)

BS5RF10.2_HH_Pos <- Bias(S5RF10.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF10.2_HH_Pos <- sum(BS5RF10.2_HH_Pos$percentageABS)/nrow(BS5RF10.2_HH_Pos)

BS5RF10.2_HH_grootte <- Bias(S5RF10.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF10.2_HH_grootte <- sum(BS5RF10.2_HH_grootte$percentageABS)/nrow(BS5RF10.2_HH_grootte)

BS5RF10.2_Woonregio_vorig_jaar <- Bias(S5RF10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF10.2_Woonregio_vorig_jaar <- sum(BS5RF10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF10.2_Woonregio_vorig_jaar)

BS5RF10.2_Nationaliteit <- Bias(S5RF10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF10.2_Nationaliteit <- sum(BS5RF10.2_Nationaliteit$percentageABS)/nrow(BS5RF10.2_Nationaliteit)

BS5RF10.2_Geboorteland <- Bias(S5RF10.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF10.2_Geboorteland <- sum(BS5RF10.2_Geboorteland$percentageABS)/nrow(BS5RF10.2_Geboorteland)

BS5RF10.2_Onderwijsniveau <- Bias(S5RF10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF10.2_Onderwijsniveau <- sum(BS5RF10.2_Onderwijsniveau$percentageABS)/nrow(BS5RF10.2_Onderwijsniveau)

BS5RF10.2_Econ_status <- Bias2(S5RF10.2_Econ_status, Econ_status, "Econ._status")
BOS5RF10.2_Econ_status <- sum(BS5RF10.2_Econ_status$percentageABS)/nrow(BS5RF10.2_Econ_status)

BS5RF10.2_Beroep <- Bias(S5RF10.2_Beroep, Beroep, "Beroep")
BOS5RF10.2_Beroep <- sum(BS5RF10.2_Beroep$percentageABS)/nrow(BS5RF10.2_Beroep)

BS5RF10.2_SBI <- Bias(S5RF10.2_SBI, SBI, "SBI")
BOS5RF10.2_SBI <- sum(BS5RF10.2_SBI$percentageABS)/nrow(BS5RF10.2_SBI)

BS5RF10.2_Burg_Staat <- Bias2(S5RF10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF10.2_Burg_Staat <- sum(BS5RF10.2_Burg_Staat$percentageABS)/nrow(BS5RF10.2_Burg_Staat)

BiasOverallS5RF10.2 <- sum(BOS5RF10.2_Geslacht, BOS5RF10.2_Leeftijd, BOS5RF10.2_HH_Pos, BOS5RF10.2_HH_grootte, BOS5RF10.2_Woonregio_vorig_jaar, 
                         BOS5RF10.2_Nationaliteit, BOS5RF10.2_Geboorteland, BOS5RF10.2_Onderwijsniveau, BOS5RF10.2_Econ_status, 
                         BOS5RF10.2_Beroep, BOS5RF10.2_SBI, BOS5RF10.2_Burg_Staat) / 12

rm(S5RF10.2_Geslacht, S5RF10.2_Leeftijd, S5RF10.2_HH_Pos, S5RF10.2_HH_grootte, S5RF10.2_Woonregio_vorig_jaar, 
   S5RF10.2_Nationaliteit, S5RF10.2_Geboorteland, S5RF10.2_Onderwijsniveau, S5RF10.2_Econ_status, 
   S5RF10.2_Beroep, S5RF10.2_SBI, S5RF10.2_Burg_Staat)
rm(BS5RF10.2_Geslacht, BS5RF10.2_Leeftijd, BS5RF10.2_HH_Pos, BS5RF10.2_HH_grootte, BS5RF10.2_Woonregio_vorig_jaar, 
   BS5RF10.2_Nationaliteit, BS5RF10.2_Geboorteland, BS5RF10.2_Onderwijsniveau, BS5RF10.2_Econ_status, 
   BS5RF10.2_Beroep, BS5RF10.2_SBI, BS5RF10.2_Burg_Staat)
rm(BOS5RF10.2_Geslacht, BOS5RF10.2_Leeftijd, BOS5RF10.2_HH_Pos, BOS5RF10.2_HH_grootte, BOS5RF10.2_Woonregio_vorig_jaar, 
   BOS5RF10.2_Nationaliteit, BOS5RF10.2_Geboorteland, BOS5RF10.2_Onderwijsniveau, BOS5RF10.2_Econ_status, 
   BOS5RF10.2_Beroep, BOS5RF10.2_SBI, BOS5RF10.2_Burg_Staat)

# 10.3 data set 
BS5RF10.3_Geslacht <- Bias(S5RF10.3_Geslacht, Geslacht, "Geslacht")
BOS5RF10.3_Geslacht <- sum(BS5RF10.3_Geslacht$percentageABS)/nrow(BS5RF10.3_Geslacht)

BS5RF10.3_Leeftijd <- Bias(S5RF10.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5RF10.3_Leeftijd <- sum(BS5RF10.3_Leeftijd$percentageABS)/nrow(BS5RF10.3_Leeftijd)

BS5RF10.3_HH_Pos <- Bias(S5RF10.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5RF10.3_HH_Pos <- sum(BS5RF10.3_HH_Pos$percentageABS)/nrow(BS5RF10.3_HH_Pos)

BS5RF10.3_HH_grootte <- Bias(S5RF10.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5RF10.3_HH_grootte <- sum(BS5RF10.3_HH_grootte$percentageABS)/nrow(BS5RF10.3_HH_grootte)

BS5RF10.3_Woonregio_vorig_jaar <- Bias(S5RF10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5RF10.3_Woonregio_vorig_jaar <- sum(BS5RF10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5RF10.3_Woonregio_vorig_jaar)

BS5RF10.3_Nationaliteit <- Bias(S5RF10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5RF10.3_Nationaliteit <- sum(BS5RF10.3_Nationaliteit$percentageABS)/nrow(BS5RF10.3_Nationaliteit)

BS5RF10.3_Geboorteland <- Bias(S5RF10.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5RF10.3_Geboorteland <- sum(BS5RF10.3_Geboorteland$percentageABS)/nrow(BS5RF10.3_Geboorteland)

BS5RF10.3_Onderwijsniveau <- Bias(S5RF10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5RF10.3_Onderwijsniveau <- sum(BS5RF10.3_Onderwijsniveau$percentageABS)/nrow(BS5RF10.3_Onderwijsniveau)

BS5RF10.3_Econ_status <- Bias2(S5RF10.3_Econ_status, Econ_status, "Econ._status")
BOS5RF10.3_Econ_status <- sum(BS5RF10.3_Econ_status$percentageABS)/nrow(BS5RF10.3_Econ_status)

BS5RF10.3_Beroep <- Bias(S5RF10.3_Beroep, Beroep, "Beroep")
BOS5RF10.3_Beroep <- sum(BS5RF10.3_Beroep$percentageABS)/nrow(BS5RF10.3_Beroep)

BS5RF10.3_SBI <- Bias(S5RF10.3_SBI, SBI, "SBI")
BOS5RF10.3_SBI <- sum(BS5RF10.3_SBI$percentageABS)/nrow(BS5RF10.3_SBI)

BS5RF10.3_Burg_Staat <- Bias2(S5RF10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5RF10.3_Burg_Staat <- sum(BS5RF10.3_Burg_Staat$percentageABS)/nrow(BS5RF10.3_Burg_Staat)

BiasOverallS5RF10.3 <- sum(BOS5RF10.3_Geslacht, BOS5RF10.3_Leeftijd, BOS5RF10.3_HH_Pos, BOS5RF10.3_HH_grootte, BOS5RF10.3_Woonregio_vorig_jaar, 
                         BOS5RF10.3_Nationaliteit, BOS5RF10.3_Geboorteland, BOS5RF10.3_Onderwijsniveau, BOS5RF10.3_Econ_status, 
                         BOS5RF10.3_Beroep, BOS5RF10.3_SBI, BOS5RF10.3_Burg_Staat) / 12

rm(S5RF10.3_Geslacht, S5RF10.3_Leeftijd, S5RF10.3_HH_Pos, S5RF10.3_HH_grootte, S5RF10.3_Woonregio_vorig_jaar, 
   S5RF10.3_Nationaliteit, S5RF10.3_Geboorteland, S5RF10.3_Onderwijsniveau, S5RF10.3_Econ_status, 
   S5RF10.3_Beroep, S5RF10.3_SBI, S5RF10.3_Burg_Staat)
rm(BS5RF10.3_Geslacht, BS5RF10.3_Leeftijd, BS5RF10.3_HH_Pos, BS5RF10.3_HH_grootte, BS5RF10.3_Woonregio_vorig_jaar, 
   BS5RF10.3_Nationaliteit, BS5RF10.3_Geboorteland, BS5RF10.3_Onderwijsniveau, BS5RF10.3_Econ_status, 
   BS5RF10.3_Beroep, BS5RF10.3_SBI, BS5RF10.3_Burg_Staat)
rm(BOS5RF10.3_Geslacht, BOS5RF10.3_Leeftijd, BOS5RF10.3_HH_Pos, BOS5RF10.3_HH_grootte, BOS5RF10.3_Woonregio_vorig_jaar, 
   BOS5RF10.3_Nationaliteit, BOS5RF10.3_Geboorteland, BOS5RF10.3_Onderwijsniveau, BOS5RF10.3_Econ_status, 
   BOS5RF10.3_Beroep, BOS5RF10.3_SBI, BOS5RF10.3_Burg_Staat)

