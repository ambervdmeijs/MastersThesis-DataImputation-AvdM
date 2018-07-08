## Installing packages -----------------------------------------------------------------------------------------------------------------
#install.packages("missForest", dependencies = TRUE)
#install.packages("readxl")
#install.packages("devtools")
#install.packages("plyr")


## Loading packages --------------------------------------------------------------------------------------------------------------------
library("missForest")
library("readxl")
library("devtools")
install_github("jabiru/tictoc")
library("tictoc")
library("plyr")


## Loading ipums data set ---------------------------------------------------------------------------------------------------------
ipums <- get(load(file = "IPUMS/ipums.Rdata"))


## Loading MCAR data frames to use ----------------------------------------------------------------------------------------------------
MCAR2.1 <- get(load(file = "MCAR2_1.Rdata"))
MCAR2.2 <- get(load(file = "MCAR2_2.Rdata"))
MCAR2.3 <- get(load(file = "MCAR2_3.Rdata"))
MCAR5.1 <- get(load(file = "MCAR5_1.Rdata"))
MCAR5.2 <- get(load(file = "MCAR5_2.Rdata"))
MCAR5.3 <- get(load(file = "MCAR5_3.Rdata"))
MCAR10.1 <- get(load(file = "MCAR10_1.Rdata"))
MCAR10.2 <- get(load(file = "MCAR10_2.Rdata"))
MCAR10.3 <- get(load(file = "MCAR10_3.Rdata"))


## Turning data set into dataframe -----------------------------------------------------------------------------------------------------
RF_MCAR2.1 <- data.frame(MCAR2.1)
RF_MCAR2.2 <- data.frame(MCAR2.2)
RF_MCAR2.3 <- data.frame(MCAR2.3)
RF_MCAR5.1 <- data.frame(MCAR5.1)
RF_MCAR5.2 <- data.frame(MCAR5.2)
RF_MCAR5.3 <- data.frame(MCAR5.3)
RF_MCAR10.1 <- data.frame(MCAR10.1)
RF_MCAR10.2 <- data.frame(MCAR10.2)
RF_MCAR10.3 <- data.frame(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(RF_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(RF_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)


## Converting to factors for categorical prediction ------------------------------------------------------------------------------------
for (i in 1:ncol(RF_MCAR2.1)) {
  RF_MCAR2.1[, i] <- as.factor(RF_MCAR2.1[, i])
}

for (i in 1:ncol(RF_MCAR2.2)) {
  RF_MCAR2.2[, i] <- as.factor(RF_MCAR2.2[, i])
}

for (i in 1:ncol(RF_MCAR2.3)) {
  RF_MCAR2.3[, i] <- as.factor(RF_MCAR2.3[, i])
}

for (i in 1:ncol(RF_MCAR5.1)) {
  RF_MCAR5.1[, i] <- as.factor(RF_MCAR5.1[, i])
}

for (i in 1:ncol(RF_MCAR5.2)) {
  RF_MCAR5.2[, i] <- as.factor(RF_MCAR5.2[, i])
}

for (i in 1:ncol(RF_MCAR5.3)) {
  RF_MCAR5.3[, i] <- as.factor(RF_MCAR5.3[, i])
}

for (i in 1:ncol(RF_MCAR10.1)) {
  RF_MCAR10.1[, i] <- as.factor(RF_MCAR10.1[, i])
}

for (i in 1:ncol(RF_MCAR10.2)) {
  RF_MCAR10.2[, i] <- as.factor(RF_MCAR10.2[, i])
}

for (i in 1:ncol(RF_MCAR10.3)) {
  RF_MCAR10.3[, i] <- as.factor(RF_MCAR10.3[, i])
}



# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------



## Random Forest Imputation with 'missForest' ----------------------------------------------------------------------------------------
set.seed(8)


# 2% data sets
tic("Random Forest 2.1 processing time...")
random_forest2.1 <- missForest(RF_MCAR2.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest2.1 <- as.data.frame(random_forest2.1$ximp)
save(df_random_forest2.1, file = "random_forest21.Rdata")

tic("Random Forest 2.2 processing time...")
random_forest2.2 <- missForest(RF_MCAR2.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest2.2 <- as.data.frame(random_forest2.2$ximp)
save(df_random_forest2.2, file = "random_forest22.Rdata")

tic("Random Forest 2.3 processing time...")
random_forest2.3 <- missForest(RF_MCAR2.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest2.3 <- as.data.frame(random_forest2.3$ximp)
save(df_random_forest2.3, file = "random_forest23.Rdata")


# 5% data sets 
tic("Random Forest 5.1 processing time...")
random_forest5.1 <- missForest(RF_MCAR5.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest5.1 <- as.data.frame(random_forest5.1$ximp)
save(df_random_forest5.1, file = "random_forest51.Rdata")

tic("Random Forest 5.2 processing time...")
random_forest5.2 <- missForest(RF_MCAR5.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest5.2 <- as.data.frame(random_forest5.2$ximp)
save(df_random_forest5.2, file = "random_forest52.Rdata")

tic("Random Forest 5.3 processing time...")
random_forest5.3 <- missForest(RF_MCAR5.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest5.3 <- as.data.frame(random_forest5.3$ximp)
save(df_random_forest5.3, file = "random_forest53.Rdata")


# 10% data sets
tic("Random Forest 10.1 processing time...")
random_forest10.1 <- missForest(RF_MCAR10.1, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest10.1 <- as.data.frame(random_forest10.1$ximp)
save(df_random_forest10.1, file = "random_forest101.Rdata")

tic("Random Forest 10.2 processing time...")
random_forest10.2 <- missForest(RF_MCAR10.2, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest10.2 <- as.data.frame(random_forest10.2$ximp)
save(df_random_forest10.2, file = "random_forest102.Rdata")

tic("Random Forest 10.3 processing time...")
random_forest10.3 <- missForest(RF_MCAR10.3, ntree = 30, maxiter = 5, replace = TRUE)
toc(log = TRUE)
df_random_forest10.3 <- as.data.frame(random_forest10.3$ximp)
save(df_random_forest10.3, file = "random_forest103.Rdata")


# Check if all values are imputed 
anyNA(c(df_random_forest2.1, df_random_forest2.2, df_random_forest2.3, df_random_forest5.1, df_random_forest5.2, 
        df_random_forest5.3, df_random_forest10.1, df_random_forest10.2, df_random_forest10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Setting correct and total to '0' 
RF2.1_correct <- RF2.2_correct <- RF2.3_correct <- RF5.1_correct <- RF5.2_correct <- RF5.3_correct <- RF10.1_correct <- RF10.2_correct <- RF10.3_correct <- 0
RF2.1_total <- RF2.2_total <- RF2.3_total <- RF5.1_total <- RF5.2_total <- RF5.3_total <- RF10.1_total <- RF10.2_total <- RF10.3_total <- 0


# Computing the correct imputed values 
RF2.1_correct <- RF2.1_correct + sum(ipums == df_random_forest2.1)
RF2.2_correct <- RF2.2_correct + sum(ipums == df_random_forest2.2)
RF2.3_correct <- RF2.3_correct + sum(ipums == df_random_forest2.3)

RF5.1_correct <- RF5.1_correct + sum(ipums == df_random_forest5.1)
RF5.2_correct <- RF5.2_correct + sum(ipums == df_random_forest5.2)
RF5.3_correct <- RF5.3_correct + sum(ipums == df_random_forest5.3)

RF10.1_correct <- RF10.1_correct + sum(ipums == df_random_forest10.1)
RF10.2_correct <- RF10.2_correct + sum(ipums == df_random_forest10.2)
RF10.3_correct <- RF10.3_correct + sum(ipums == df_random_forest10.3)



# Computing the total values in data set
RF2.1_total <- RF2.1_total + sum(!is.na(df_random_forest2.1))
RF2.2_total <- RF2.2_total + sum(!is.na(df_random_forest2.2))
RF2.3_total <- RF2.3_total + sum(!is.na(df_random_forest2.3))

RF5.1_total <- RF5.1_total + sum(!is.na(df_random_forest5.1))
RF5.2_total <- RF5.2_total + sum(!is.na(df_random_forest5.2))
RF5.3_total <- RF5.3_total + sum(!is.na(df_random_forest5.3))

RF10.1_total <- RF10.1_total + sum(!is.na(df_random_forest10.1))
RF10.2_total <- RF10.2_total + sum(!is.na(df_random_forest10.2))
RF10.3_total <- RF10.3_total + sum(!is.na(df_random_forest10.3))


# Computing the accuracy of imputation 
RF2.1_accuracy <- RF2.1_correct / RF2.1_total
RF2.2_accuracy <- RF2.2_correct / RF2.2_total
RF2.3_accuracy <- RF2.3_correct / RF2.3_total

RF5.1_accuracy <- RF5.1_correct / RF5.1_total
RF5.2_accuracy <- RF5.2_correct / RF5.2_total
RF5.3_accuracy <- RF5.3_correct / RF5.3_total

RF10.1_accuracy <- RF10.1_correct / RF10.1_total
RF10.2_accuracy <- RF10.2_correct / RF10.2_total
RF10.3_accuracy <- RF10.3_correct / RF10.3_total



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
Beroep <- get(load(file = "Freq_Beroep.Rdata"))
SBI <- get(load(file = "Freq_SBI.Rdata"))
Burg_Staat <- get(load(file = "Freq_Burgstaat.Rdata"))


# Computing Bias 
Bias <- function(df1, df2, column){
  Bdf <- merge(df1, df2, by = column, all = TRUE)
  Bdf$result <- Bdf["freq.x"] - Bdf["freq.y"]
  Bdf$percentage <- (Bdf["result"]/Bdf["freq.y"])*100
  Bdf$percentageABS <- abs(Bdf$percentage)
  return(Bdf)
}


# 2% data set

# Counting values
RF2.1_Geslacht <- plyr::count(df_random_forest2.1, 'Geslacht')
RF2.1_Leeftijd <- plyr::count(df_random_forest2.1, 'Leeftijd')
RF2.1_HH_Pos <- plyr::count(df_random_forest2.1, 'HH_Pos')
RF2.1_HH_grootte <- plyr::count(df_random_forest2.1, 'HH_grootte')
RF2.1_Woonregio_vorig_jaar <- plyr::count(df_random_forest2.1, 'Woonregio_vorig_jaar')
RF2.1_Nationaliteit <- plyr::count(df_random_forest2.1, 'Nationaliteit')
RF2.1_Geboorteland <- plyr::count(df_random_forest2.1, 'Geboorteland')
RF2.1_Onderwijsniveau <- plyr::count(df_random_forest2.1, 'Onderwijsniveau')
RF2.1_Econ_status <- plyr::count(df_random_forest2.1, 'Econ._status')
RF2.1_Beroep <- plyr::count(df_random_forest2.1, 'Beroep')
RF2.1_SBI <- plyr::count(df_random_forest2.1, 'SBI')
RF2.1_Burg_Staat <- plyr::count(df_random_forest2.1, 'Burg._Staat')

RF2.2_Geslacht <- plyr::count(df_random_forest2.2, 'Geslacht')
RF2.2_Leeftijd <- plyr::count(df_random_forest2.2, 'Leeftijd')
RF2.2_HH_Pos <- plyr::count(df_random_forest2.2, 'HH_Pos')
RF2.2_HH_grootte <- plyr::count(df_random_forest2.2, 'HH_grootte')
RF2.2_Woonregio_vorig_jaar <- plyr::count(df_random_forest2.2, 'Woonregio_vorig_jaar')
RF2.2_Nationaliteit <- plyr::count(df_random_forest2.2, 'Nationaliteit')
RF2.2_Geboorteland <- plyr::count(df_random_forest2.2, 'Geboorteland')
RF2.2_Onderwijsniveau <- plyr::count(df_random_forest2.2, 'Onderwijsniveau')
RF2.2_Econ_status <- plyr::count(df_random_forest2.2, 'Econ._status')
RF2.2_Beroep <- plyr::count(df_random_forest2.2, 'Beroep')
RF2.2_SBI <- plyr::count(df_random_forest2.2, 'SBI')
RF2.2_Burg_Staat <- plyr::count(df_random_forest2.2, 'Burg._Staat')

RF2.3_Geslacht <- plyr::count(df_random_forest2.3, 'Geslacht')
RF2.3_Leeftijd <- plyr::count(df_random_forest2.3, 'Leeftijd')
RF2.3_HH_Pos <- plyr::count(df_random_forest2.3, 'HH_Pos')
RF2.3_HH_grootte <- plyr::count(df_random_forest2.3, 'HH_grootte')
RF2.3_Woonregio_vorig_jaar <- plyr::count(df_random_forest2.3, 'Woonregio_vorig_jaar')
RF2.3_Nationaliteit <- plyr::count(df_random_forest2.3, 'Nationaliteit')
RF2.3_Geboorteland <- plyr::count(df_random_forest2.3, 'Geboorteland')
RF2.3_Onderwijsniveau <- plyr::count(df_random_forest2.3, 'Onderwijsniveau')
RF2.3_Econ_status <- plyr::count(df_random_forest2.3, 'Econ._status')
RF2.3_Beroep <- plyr::count(df_random_forest2.3, 'Beroep')
RF2.3_SBI <- plyr::count(df_random_forest2.3, 'SBI')
RF2.3_Burg_Staat <- plyr::count(df_random_forest2.3, 'Burg._Staat')

rm(df_random_forest2.1, df_random_forest2.2, df_random_forest2.3)

# 2.1 data set 
BRF2.1_Geslacht <- Bias(RF2.1_Geslacht, Geslacht, "Geslacht")
BORF2.1_Geslacht <- sum(BRF2.1_Geslacht$percentageABS)/nrow(BRF2.1_Geslacht)

BRF2.1_Leeftijd <- Bias(RF2.1_Leeftijd, Leeftijd, "Leeftijd")
BORF2.1_Leeftijd <- sum(BRF2.1_Leeftijd$percentageABS)/nrow(BRF2.1_Leeftijd)

BRF2.1_HH_Pos <- Bias(RF2.1_HH_Pos, HH_Pos, "HH_Pos")
BORF2.1_HH_Pos <- sum(BRF2.1_HH_Pos$percentageABS)/nrow(BRF2.1_HH_Pos)

BRF2.1_HH_grootte <- Bias(RF2.1_HH_grootte, HH_grootte, "HH_grootte")
BORF2.1_HH_grootte <- sum(BRF2.1_HH_grootte$percentageABS)/nrow(BRF2.1_HH_grootte)

BRF2.1_Woonregio_vorig_jaar <- Bias(RF2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF2.1_Woonregio_vorig_jaar <- sum(BRF2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BRF2.1_Woonregio_vorig_jaar)

BRF2.1_Nationaliteit <- Bias(RF2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF2.1_Nationaliteit <- sum(BRF2.1_Nationaliteit$percentageABS)/nrow(BRF2.1_Nationaliteit)

BRF2.1_Geboorteland <- Bias(RF2.1_Geboorteland, Geboorteland, "Geboorteland")
BORF2.1_Geboorteland <- sum(BRF2.1_Geboorteland$percentageABS)/nrow(BRF2.1_Geboorteland)

BRF2.1_Onderwijsniveau <- Bias(RF2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF2.1_Onderwijsniveau <- sum(BRF2.1_Onderwijsniveau$percentageABS)/nrow(BRF2.1_Onderwijsniveau)

BRF2.1_Econ_status <- Bias(RF2.1_Econ_status, Econ_status, "Econ._status")
BORF2.1_Econ_status <- sum(BRF2.1_Econ_status$percentageABS)/nrow(BRF2.1_Econ_status)

BRF2.1_Beroep <- Bias(RF2.1_Beroep, Beroep, "Beroep")
BORF2.1_Beroep <- sum(BRF2.1_Beroep$percentageABS)/nrow(BRF2.1_Beroep)

BRF2.1_SBI <- Bias(RF2.1_SBI, SBI, "SBI")
BORF2.1_SBI <- sum(BRF2.1_SBI$percentageABS)/nrow(BRF2.1_SBI)

BRF2.1_Burg_Staat <- Bias(RF2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF2.1_Burg_Staat <- sum(BRF2.1_Burg_Staat$percentageABS)/nrow(BRF2.1_Burg_Staat)

BiasOverallRF2.1 <- sum(BORF2.1_Geslacht, BORF2.1_Leeftijd, BORF2.1_HH_Pos, BORF2.1_HH_grootte, BORF2.1_Woonregio_vorig_jaar, 
                        BORF2.1_Nationaliteit, BORF2.1_Geboorteland, BORF2.1_Onderwijsniveau, BORF2.1_Econ_status, 
                        BORF2.1_Beroep, BORF2.1_SBI, BORF2.1_Burg_Staat) / 12

rm(RF2.1_Geslacht, RF2.1_Leeftijd, RF2.1_HH_Pos, RF2.1_HH_grootte, RF2.1_Woonregio_vorig_jaar, 
   RF2.1_Nationaliteit, RF2.1_Geboorteland, RF2.1_Onderwijsniveau, RF2.1_Econ_status, 
   RF2.1_Beroep, RF2.1_SBI, RF2.1_Burg_Staat)
rm(BRF2.1_Geslacht, BRF2.1_Leeftijd, BRF2.1_HH_Pos, BRF2.1_HH_grootte, BRF2.1_Woonregio_vorig_jaar, 
   BRF2.1_Nationaliteit, BRF2.1_Geboorteland, BRF2.1_Onderwijsniveau, BRF2.1_Econ_status, 
   BRF2.1_Beroep, BRF2.1_SBI, BRF2.1_Burg_Staat)
rm(BORF2.1_Geslacht, BORF2.1_Leeftijd, BORF2.1_HH_Pos, BORF2.1_HH_grootte, BORF2.1_Woonregio_vorig_jaar, 
   BORF2.1_Nationaliteit, BORF2.1_Geboorteland, BORF2.1_Onderwijsniveau, BORF2.1_Econ_status, 
   BORF2.1_Beroep, BORF2.1_SBI, BORF2.1_Burg_Staat)

# 2.2 data set 
BRF2.2_Geslacht <- Bias(RF2.2_Geslacht, Geslacht, "Geslacht")
BORF2.2_Geslacht <- sum(BRF2.2_Geslacht$percentageABS)/nrow(BRF2.2_Geslacht)

BRF2.2_Leeftijd <- Bias(RF2.2_Leeftijd, Leeftijd, "Leeftijd")
BORF2.2_Leeftijd <- sum(BRF2.2_Leeftijd$percentageABS)/nrow(BRF2.2_Leeftijd)

BRF2.2_HH_Pos <- Bias(RF2.2_HH_Pos, HH_Pos, "HH_Pos")
BORF2.2_HH_Pos <- sum(BRF2.2_HH_Pos$percentageABS)/nrow(BRF2.2_HH_Pos)

BRF2.2_HH_grootte <- Bias(RF2.2_HH_grootte, HH_grootte, "HH_grootte")
BORF2.2_HH_grootte <- sum(BRF2.2_HH_grootte$percentageABS)/nrow(BRF2.2_HH_grootte)

BRF2.2_Woonregio_vorig_jaar <- Bias(RF2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF2.2_Woonregio_vorig_jaar <- sum(BRF2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BRF2.2_Woonregio_vorig_jaar)

BRF2.2_Nationaliteit <- Bias(RF2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF2.2_Nationaliteit <- sum(BRF2.2_Nationaliteit$percentageABS)/nrow(BRF2.2_Nationaliteit)

BRF2.2_Geboorteland <- Bias(RF2.2_Geboorteland, Geboorteland, "Geboorteland")
BORF2.2_Geboorteland <- sum(BRF2.2_Geboorteland$percentageABS)/nrow(BRF2.2_Geboorteland)

BRF2.2_Onderwijsniveau <- Bias(RF2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF2.2_Onderwijsniveau <- sum(BRF2.2_Onderwijsniveau$percentageABS)/nrow(BRF2.2_Onderwijsniveau)

BRF2.2_Econ_status <- Bias(RF2.2_Econ_status, Econ_status, "Econ._status")
BORF2.2_Econ_status <- sum(BRF2.2_Econ_status$percentageABS)/nrow(BRF2.2_Econ_status)

BRF2.2_Beroep <- Bias(RF2.2_Beroep, Beroep, "Beroep")
BORF2.2_Beroep <- sum(BRF2.2_Beroep$percentageABS)/nrow(BRF2.2_Beroep)

BRF2.2_SBI <- Bias(RF2.2_SBI, SBI, "SBI")
BORF2.2_SBI <- sum(BRF2.2_SBI$percentageABS)/nrow(BRF2.2_SBI)

BRF2.2_Burg_Staat <- Bias(RF2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF2.2_Burg_Staat <- sum(BRF2.2_Burg_Staat$percentageABS)/nrow(BRF2.2_Burg_Staat)

BiasOverallRF2.2 <- sum(BORF2.2_Geslacht, BORF2.2_Leeftijd, BORF2.2_HH_Pos, BORF2.2_HH_grootte, BORF2.2_Woonregio_vorig_jaar, 
                        BORF2.2_Nationaliteit, BORF2.2_Geboorteland, BORF2.2_Onderwijsniveau, BORF2.2_Econ_status, 
                        BORF2.2_Beroep, BORF2.2_SBI, BORF2.2_Burg_Staat) / 12

rm(RF2.2_Geslacht, RF2.2_Leeftijd, RF2.2_HH_Pos, RF2.2_HH_grootte, RF2.2_Woonregio_vorig_jaar, 
   RF2.2_Nationaliteit, RF2.2_Geboorteland, RF2.2_Onderwijsniveau, RF2.2_Econ_status, 
   RF2.2_Beroep, RF2.2_SBI, RF2.2_Burg_Staat)
rm(BRF2.2_Geslacht, BRF2.2_Leeftijd, BRF2.2_HH_Pos, BRF2.2_HH_grootte, BRF2.2_Woonregio_vorig_jaar, 
   BRF2.2_Nationaliteit, BRF2.2_Geboorteland, BRF2.2_Onderwijsniveau, BRF2.2_Econ_status, 
   BRF2.2_Beroep, BRF2.2_SBI, BRF2.2_Burg_Staat)
rm(BORF2.2_Geslacht, BORF2.2_Leeftijd, BORF2.2_HH_Pos, BORF2.2_HH_grootte, BORF2.2_Woonregio_vorig_jaar, 
   BORF2.2_Nationaliteit, BORF2.2_Geboorteland, BORF2.2_Onderwijsniveau, BORF2.2_Econ_status, 
   BORF2.2_Beroep, BORF2.2_SBI, BORF2.2_Burg_Staat)

# 2.3 data set 
BRF2.3_Geslacht <- Bias(RF2.3_Geslacht, Geslacht, "Geslacht")
BORF2.3_Geslacht <- sum(BRF2.3_Geslacht$percentageABS)/nrow(BRF2.3_Geslacht)

BRF2.3_Leeftijd <- Bias(RF2.3_Leeftijd, Leeftijd, "Leeftijd")
BORF2.3_Leeftijd <- sum(BRF2.3_Leeftijd$percentageABS)/nrow(BRF2.3_Leeftijd)

BRF2.3_HH_Pos <- Bias(RF2.3_HH_Pos, HH_Pos, "HH_Pos")
BORF2.3_HH_Pos <- sum(BRF2.3_HH_Pos$percentageABS)/nrow(BRF2.3_HH_Pos)

BRF2.3_HH_grootte <- Bias(RF2.3_HH_grootte, HH_grootte, "HH_grootte")
BORF2.3_HH_grootte <- sum(BRF2.3_HH_grootte$percentageABS)/nrow(BRF2.3_HH_grootte)

BRF2.3_Woonregio_vorig_jaar <- Bias(RF2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF2.3_Woonregio_vorig_jaar <- sum(BRF2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BRF2.3_Woonregio_vorig_jaar)

BRF2.3_Nationaliteit <- Bias(RF2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF2.3_Nationaliteit <- sum(BRF2.3_Nationaliteit$percentageABS)/nrow(BRF2.3_Nationaliteit)

BRF2.3_Geboorteland <- Bias(RF2.3_Geboorteland, Geboorteland, "Geboorteland")
BORF2.3_Geboorteland <- sum(BRF2.3_Geboorteland$percentageABS)/nrow(BRF2.3_Geboorteland)

BRF2.3_Onderwijsniveau <- Bias(RF2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF2.3_Onderwijsniveau <- sum(BRF2.3_Onderwijsniveau$percentageABS)/nrow(BRF2.3_Onderwijsniveau)

BRF2.3_Econ_status <- Bias(RF2.3_Econ_status, Econ_status, "Econ._status")
BORF2.3_Econ_status <- sum(BRF2.3_Econ_status$percentageABS)/nrow(BRF2.3_Econ_status)

BRF2.3_Beroep <- Bias(RF2.3_Beroep, Beroep, "Beroep")
BORF2.3_Beroep <- sum(BRF2.3_Beroep$percentageABS)/nrow(BRF2.3_Beroep)

BRF2.3_SBI <- Bias(RF2.3_SBI, SBI, "SBI")
BORF2.3_SBI <- sum(BRF2.3_SBI$percentageABS)/nrow(BRF2.3_SBI)

BRF2.3_Burg_Staat <- Bias(RF2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF2.3_Burg_Staat <- sum(BRF2.3_Burg_Staat$percentageABS)/nrow(BRF2.3_Burg_Staat)

BiasOverallRF2.3 <- sum(BORF2.3_Geslacht, BORF2.3_Leeftijd, BORF2.3_HH_Pos, BORF2.3_HH_grootte, BORF2.3_Woonregio_vorig_jaar, 
                        BORF2.3_Nationaliteit, BORF2.3_Geboorteland, BORF2.3_Onderwijsniveau, BORF2.3_Econ_status, 
                        BORF2.3_Beroep, BORF2.3_SBI, BORF2.3_Burg_Staat) / 12

rm(RF2.3_Geslacht, RF2.3_Leeftijd, RF2.3_HH_Pos, RF2.3_HH_grootte, RF2.3_Woonregio_vorig_jaar, 
   RF2.3_Nationaliteit, RF2.3_Geboorteland, RF2.3_Onderwijsniveau, RF2.3_Econ_status, 
   RF2.3_Beroep, RF2.3_SBI, RF2.3_Burg_Staat)
rm(BRF2.3_Geslacht, BRF2.3_Leeftijd, BRF2.3_HH_Pos, BRF2.3_HH_grootte, BRF2.3_Woonregio_vorig_jaar, 
   BRF2.3_Nationaliteit, BRF2.3_Geboorteland, BRF2.3_Onderwijsniveau, BRF2.3_Econ_status, 
   BRF2.3_Beroep, BRF2.3_SBI, BRF2.3_Burg_Staat)
rm(BORF2.3_Geslacht, BORF2.3_Leeftijd, BORF2.3_HH_Pos, BORF2.3_HH_grootte, BORF2.3_Woonregio_vorig_jaar, 
   BORF2.3_Nationaliteit, BORF2.3_Geboorteland, BORF2.3_Onderwijsniveau, BORF2.3_Econ_status, 
   BORF2.3_Beroep, BORF2.3_SBI, BORF2.3_Burg_Staat)


# 5% data sets

# Counting values
RF5.1_Geslacht <- plyr::count(df_random_forest5.1, 'Geslacht')
RF5.1_Leeftijd <- plyr::count(df_random_forest5.1, 'Leeftijd')
RF5.1_HH_Pos <- plyr::count(df_random_forest5.1, 'HH_Pos')
RF5.1_HH_grootte <- plyr::count(df_random_forest5.1, 'HH_grootte')
RF5.1_Woonregio_vorig_jaar <- plyr::count(df_random_forest5.1, 'Woonregio_vorig_jaar')
RF5.1_Nationaliteit <- plyr::count(df_random_forest5.1, 'Nationaliteit')
RF5.1_Geboorteland <- plyr::count(df_random_forest5.1, 'Geboorteland')
RF5.1_Onderwijsniveau <- plyr::count(df_random_forest5.1, 'Onderwijsniveau')
RF5.1_Econ_status <- plyr::count(df_random_forest5.1, 'Econ._status')
RF5.1_Beroep <- plyr::count(df_random_forest5.1, 'Beroep')
RF5.1_SBI <- plyr::count(df_random_forest5.1, 'SBI')
RF5.1_Burg_Staat <- plyr::count(df_random_forest5.1, 'Burg._Staat')

RF5.2_Geslacht <- plyr::count(df_random_forest5.2, 'Geslacht')
RF5.2_Leeftijd <- plyr::count(df_random_forest5.2, 'Leeftijd')
RF5.2_HH_Pos <- plyr::count(df_random_forest5.2, 'HH_Pos')
RF5.2_HH_grootte <- plyr::count(df_random_forest5.2, 'HH_grootte')
RF5.2_Woonregio_vorig_jaar <- plyr::count(df_random_forest5.2, 'Woonregio_vorig_jaar')
RF5.2_Nationaliteit <- plyr::count(df_random_forest5.2, 'Nationaliteit')
RF5.2_Geboorteland <- plyr::count(df_random_forest5.2, 'Geboorteland')
RF5.2_Onderwijsniveau <- plyr::count(df_random_forest5.2, 'Onderwijsniveau')
RF5.2_Econ_status <- plyr::count(df_random_forest5.2, 'Econ._status')
RF5.2_Beroep <- plyr::count(df_random_forest5.2, 'Beroep')
RF5.2_SBI <- plyr::count(df_random_forest5.2, 'SBI')
RF5.2_Burg_Staat <- plyr::count(df_random_forest5.2, 'Burg._Staat')

RF5.3_Geslacht <- plyr::count(df_random_forest5.3, 'Geslacht')
RF5.3_Leeftijd <- plyr::count(df_random_forest5.3, 'Leeftijd')
RF5.3_HH_Pos <- plyr::count(df_random_forest5.3, 'HH_Pos')
RF5.3_HH_grootte <- plyr::count(df_random_forest5.3, 'HH_grootte')
RF5.3_Woonregio_vorig_jaar <- plyr::count(df_random_forest5.3, 'Woonregio_vorig_jaar')
RF5.3_Nationaliteit <- plyr::count(df_random_forest5.3, 'Nationaliteit')
RF5.3_Geboorteland <- plyr::count(df_random_forest5.3, 'Geboorteland')
RF5.3_Onderwijsniveau <- plyr::count(df_random_forest5.3, 'Onderwijsniveau')
RF5.3_Econ_status <- plyr::count(df_random_forest5.3, 'Econ._status')
RF5.3_Beroep <- plyr::count(df_random_forest5.3, 'Beroep')
RF5.3_SBI <- plyr::count(df_random_forest5.3, 'SBI')
RF5.3_Burg_Staat <- plyr::count(df_random_forest5.3, 'Burg._Staat')

rm(df_random_forest5.1, df_random_forest5.2, df_random_forest5.3)


# 5.1 data set 
BRF5.1_Geslacht <- Bias(RF5.1_Geslacht, Geslacht, "Geslacht")
BORF5.1_Geslacht <- sum(BRF5.1_Geslacht$percentageABS)/nrow(BRF5.1_Geslacht)

BRF5.1_Leeftijd <- Bias(RF5.1_Leeftijd, Leeftijd, "Leeftijd")
BORF5.1_Leeftijd <- sum(BRF5.1_Leeftijd$percentageABS)/nrow(BRF5.1_Leeftijd)

BRF5.1_HH_Pos <- Bias(RF5.1_HH_Pos, HH_Pos, "HH_Pos")
BORF5.1_HH_Pos <- sum(BRF5.1_HH_Pos$percentageABS)/nrow(BRF5.1_HH_Pos)

BRF5.1_HH_grootte <- Bias(RF5.1_HH_grootte, HH_grootte, "HH_grootte")
BORF5.1_HH_grootte <- sum(BRF5.1_HH_grootte$percentageABS)/nrow(BRF5.1_HH_grootte)

BRF5.1_Woonregio_vorig_jaar <- Bias(RF5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF5.1_Woonregio_vorig_jaar <- sum(BRF5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BRF5.1_Woonregio_vorig_jaar)

BRF5.1_Nationaliteit <- Bias(RF5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF5.1_Nationaliteit <- sum(BRF5.1_Nationaliteit$percentageABS)/nrow(BRF5.1_Nationaliteit)

BRF5.1_Geboorteland <- Bias(RF5.1_Geboorteland, Geboorteland, "Geboorteland")
BORF5.1_Geboorteland <- sum(BRF5.1_Geboorteland$percentageABS)/nrow(BRF5.1_Geboorteland)

BRF5.1_Onderwijsniveau <- Bias(RF5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF5.1_Onderwijsniveau <- sum(BRF5.1_Onderwijsniveau$percentageABS)/nrow(BRF5.1_Onderwijsniveau)

BRF5.1_Econ_status <- Bias(RF5.1_Econ_status, Econ_status, "Econ._status")
BORF5.1_Econ_status <- sum(BRF5.1_Econ_status$percentageABS)/nrow(BRF5.1_Econ_status)

BRF5.1_Beroep <- Bias(RF5.1_Beroep, Beroep, "Beroep")
BORF5.1_Beroep <- sum(BRF5.1_Beroep$percentageABS)/nrow(BRF5.1_Beroep)

BRF5.1_SBI <- Bias(RF5.1_SBI, SBI, "SBI")
BORF5.1_SBI <- sum(BRF5.1_SBI$percentageABS)/nrow(BRF5.1_SBI)

BRF5.1_Burg_Staat <- Bias(RF5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF5.1_Burg_Staat <- sum(BRF5.1_Burg_Staat$percentageABS)/nrow(BRF5.1_Burg_Staat)

BiasOverallRF5.1 <- sum(BORF5.1_Geslacht, BORF5.1_Leeftijd, BORF5.1_HH_Pos, BORF5.1_HH_grootte, BORF5.1_Woonregio_vorig_jaar, 
                        BORF5.1_Nationaliteit, BORF5.1_Geboorteland, BORF5.1_Onderwijsniveau, BORF5.1_Econ_status, 
                        BORF5.1_Beroep, BORF5.1_SBI, BORF5.1_Burg_Staat) / 12

rm(RF5.1_Geslacht, RF5.1_Leeftijd, RF5.1_HH_Pos, RF5.1_HH_grootte, RF5.1_Woonregio_vorig_jaar, 
   RF5.1_Nationaliteit, RF5.1_Geboorteland, RF5.1_Onderwijsniveau, RF5.1_Econ_status, 
   RF5.1_Beroep, RF5.1_SBI, RF5.1_Burg_Staat)
rm(BRF5.1_Geslacht, BRF5.1_Leeftijd, BRF5.1_HH_Pos, BRF5.1_HH_grootte, BRF5.1_Woonregio_vorig_jaar, 
   BRF5.1_Nationaliteit, BRF5.1_Geboorteland, BRF5.1_Onderwijsniveau, BRF5.1_Econ_status, 
   BRF5.1_Beroep, BRF5.1_SBI, BRF5.1_Burg_Staat)
rm(BORF5.1_Geslacht, BORF5.1_Leeftijd, BORF5.1_HH_Pos, BORF5.1_HH_grootte, BORF5.1_Woonregio_vorig_jaar, 
   BORF5.1_Nationaliteit, BORF5.1_Geboorteland, BORF5.1_Onderwijsniveau, BORF5.1_Econ_status, 
   BORF5.1_Beroep, BORF5.1_SBI, BORF5.1_Burg_Staat)

# 5.2 data set 
BRF5.2_Geslacht <- Bias(RF5.2_Geslacht, Geslacht, "Geslacht")
BORF5.2_Geslacht <- sum(BRF5.2_Geslacht$percentageABS)/nrow(BRF5.2_Geslacht)

BRF5.2_Leeftijd <- Bias(RF5.2_Leeftijd, Leeftijd, "Leeftijd")
BORF5.2_Leeftijd <- sum(BRF5.2_Leeftijd$percentageABS)/nrow(BRF5.2_Leeftijd)

BRF5.2_HH_Pos <- Bias(RF5.2_HH_Pos, HH_Pos, "HH_Pos")
BORF5.2_HH_Pos <- sum(BRF5.2_HH_Pos$percentageABS)/nrow(BRF5.2_HH_Pos)

BRF5.2_HH_grootte <- Bias(RF5.2_HH_grootte, HH_grootte, "HH_grootte")
BORF5.2_HH_grootte <- sum(BRF5.2_HH_grootte$percentageABS)/nrow(BRF5.2_HH_grootte)

BRF5.2_Woonregio_vorig_jaar <- Bias(RF5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF5.2_Woonregio_vorig_jaar <- sum(BRF5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BRF5.2_Woonregio_vorig_jaar)

BRF5.2_Nationaliteit <- Bias(RF5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF5.2_Nationaliteit <- sum(BRF5.2_Nationaliteit$percentageABS)/nrow(BRF5.2_Nationaliteit)

BRF5.2_Geboorteland <- Bias(RF5.2_Geboorteland, Geboorteland, "Geboorteland")
BORF5.2_Geboorteland <- sum(BRF5.2_Geboorteland$percentageABS)/nrow(BRF5.2_Geboorteland)

BRF5.2_Onderwijsniveau <- Bias(RF5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF5.2_Onderwijsniveau <- sum(BRF5.2_Onderwijsniveau$percentageABS)/nrow(BRF5.2_Onderwijsniveau)

BRF5.2_Econ_status <- Bias(RF5.2_Econ_status, Econ_status, "Econ._status")
BORF5.2_Econ_status <- sum(BRF5.2_Econ_status$percentageABS)/nrow(BRF5.2_Econ_status)

BRF5.2_Beroep <- Bias(RF5.2_Beroep, Beroep, "Beroep")
BORF5.2_Beroep <- sum(BRF5.2_Beroep$percentageABS)/nrow(BRF5.2_Beroep)

BRF5.2_SBI <- Bias(RF5.2_SBI, SBI, "SBI")
BORF5.2_SBI <- sum(BRF5.2_SBI$percentageABS)/nrow(BRF5.2_SBI)

BRF5.2_Burg_Staat <- Bias(RF5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF5.2_Burg_Staat <- sum(BRF5.2_Burg_Staat$percentageABS)/nrow(BRF5.2_Burg_Staat)

BiasOverallRF5.2 <- sum(BORF5.2_Geslacht, BORF5.2_Leeftijd, BORF5.2_HH_Pos, BORF5.2_HH_grootte, BORF5.2_Woonregio_vorig_jaar, 
                        BORF5.2_Nationaliteit, BORF5.2_Geboorteland, BORF5.2_Onderwijsniveau, BORF5.2_Econ_status, 
                        BORF5.2_Beroep, BORF5.2_SBI, BORF5.2_Burg_Staat) / 12

rm(RF5.2_Geslacht, RF5.2_Leeftijd, RF5.2_HH_Pos, RF5.2_HH_grootte, RF5.2_Woonregio_vorig_jaar, 
   RF5.2_Nationaliteit, RF5.2_Geboorteland, RF5.2_Onderwijsniveau, RF5.2_Econ_status, 
   RF5.2_Beroep, RF5.2_SBI, RF5.2_Burg_Staat)
rm(BRF5.2_Geslacht, BRF5.2_Leeftijd, BRF5.2_HH_Pos, BRF5.2_HH_grootte, BRF5.2_Woonregio_vorig_jaar, 
   BRF5.2_Nationaliteit, BRF5.2_Geboorteland, BRF5.2_Onderwijsniveau, BRF5.2_Econ_status, 
   BRF5.2_Beroep, BRF5.2_SBI, BRF5.2_Burg_Staat)
rm(BORF5.2_Geslacht, BORF5.2_Leeftijd, BORF5.2_HH_Pos, BORF5.2_HH_grootte, BORF5.2_Woonregio_vorig_jaar, 
   BORF5.2_Nationaliteit, BORF5.2_Geboorteland, BORF5.2_Onderwijsniveau, BORF5.2_Econ_status, 
   BORF5.2_Beroep, BORF5.2_SBI, BORF5.2_Burg_Staat)

# 5.3 data set 
BRF5.3_Geslacht <- Bias(RF5.3_Geslacht, Geslacht, "Geslacht")
BORF5.3_Geslacht <- sum(BRF5.3_Geslacht$percentageABS)/nrow(BRF5.3_Geslacht)

BRF5.3_Leeftijd <- Bias(RF5.3_Leeftijd, Leeftijd, "Leeftijd")
BORF5.3_Leeftijd <- sum(BRF5.3_Leeftijd$percentageABS)/nrow(BRF5.3_Leeftijd)

BRF5.3_HH_Pos <- Bias(RF5.3_HH_Pos, HH_Pos, "HH_Pos")
BORF5.3_HH_Pos <- sum(BRF5.3_HH_Pos$percentageABS)/nrow(BRF5.3_HH_Pos)

BRF5.3_HH_grootte <- Bias(RF5.3_HH_grootte, HH_grootte, "HH_grootte")
BORF5.3_HH_grootte <- sum(BRF5.3_HH_grootte$percentageABS)/nrow(BRF5.3_HH_grootte)

BRF5.3_Woonregio_vorig_jaar <- Bias(RF5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF5.3_Woonregio_vorig_jaar <- sum(BRF5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BRF5.3_Woonregio_vorig_jaar)

BRF5.3_Nationaliteit <- Bias(RF5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF5.3_Nationaliteit <- sum(BRF5.3_Nationaliteit$percentageABS)/nrow(BRF5.3_Nationaliteit)

BRF5.3_Geboorteland <- Bias(RF5.3_Geboorteland, Geboorteland, "Geboorteland")
BORF5.3_Geboorteland <- sum(BRF5.3_Geboorteland$percentageABS)/nrow(BRF5.3_Geboorteland)

BRF5.3_Onderwijsniveau <- Bias(RF5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF5.3_Onderwijsniveau <- sum(BRF5.3_Onderwijsniveau$percentageABS)/nrow(BRF5.3_Onderwijsniveau)

BRF5.3_Econ_status <- Bias(RF5.3_Econ_status, Econ_status, "Econ._status")
BORF5.3_Econ_status <- sum(BRF5.3_Econ_status$percentageABS)/nrow(BRF5.3_Econ_status)

BRF5.3_Beroep <- Bias(RF5.3_Beroep, Beroep, "Beroep")
BORF5.3_Beroep <- sum(BRF5.3_Beroep$percentageABS)/nrow(BRF5.3_Beroep)

BRF5.3_SBI <- Bias(RF5.3_SBI, SBI, "SBI")
BORF5.3_SBI <- sum(BRF5.3_SBI$percentageABS)/nrow(BRF5.3_SBI)

BRF5.3_Burg_Staat <- Bias(RF5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF5.3_Burg_Staat <- sum(BRF5.3_Burg_Staat$percentageABS)/nrow(BRF5.3_Burg_Staat)

BiasOverallRF5.3 <- sum(BORF5.3_Geslacht, BORF5.3_Leeftijd, BORF5.3_HH_Pos, BORF5.3_HH_grootte, BORF5.3_Woonregio_vorig_jaar, 
                        BORF5.3_Nationaliteit, BORF5.3_Geboorteland, BORF5.3_Onderwijsniveau, BORF5.3_Econ_status, 
                        BORF5.3_Beroep, BORF5.3_SBI, BORF5.3_Burg_Staat) / 12

rm(RF5.3_Geslacht, RF5.3_Leeftijd, RF5.3_HH_Pos, RF5.3_HH_grootte, RF5.3_Woonregio_vorig_jaar, 
   RF5.3_Nationaliteit, RF5.3_Geboorteland, RF5.3_Onderwijsniveau, RF5.3_Econ_status, 
   RF5.3_Beroep, RF5.3_SBI, RF5.3_Burg_Staat)
rm(BRF5.3_Geslacht, BRF5.3_Leeftijd, BRF5.3_HH_Pos, BRF5.3_HH_grootte, BRF5.3_Woonregio_vorig_jaar, 
   BRF5.3_Nationaliteit, BRF5.3_Geboorteland, BRF5.3_Onderwijsniveau, BRF5.3_Econ_status, 
   BRF5.3_Beroep, BRF5.3_SBI, BRF5.3_Burg_Staat)
rm(BORF5.3_Geslacht, BORF5.3_Leeftijd, BORF5.3_HH_Pos, BORF5.3_HH_grootte, BORF5.3_Woonregio_vorig_jaar, 
   BORF5.3_Nationaliteit, BORF5.3_Geboorteland, BORF5.3_Onderwijsniveau, BORF5.3_Econ_status, 
   BORF5.3_Beroep, BORF5.3_SBI, BORF5.3_Burg_Staat)


# 10% data sets

# Counting values
RF10.1_Geslacht <- plyr::count(df_random_forest10.1, 'Geslacht')
RF10.1_Leeftijd <- plyr::count(df_random_forest10.1, 'Leeftijd')
RF10.1_HH_Pos <- plyr::count(df_random_forest10.1, 'HH_Pos')
RF10.1_HH_grootte <- plyr::count(df_random_forest10.1, 'HH_grootte')
RF10.1_Woonregio_vorig_jaar <- plyr::count(df_random_forest10.1, 'Woonregio_vorig_jaar')
RF10.1_Nationaliteit <- plyr::count(df_random_forest10.1, 'Nationaliteit')
RF10.1_Geboorteland <- plyr::count(df_random_forest10.1, 'Geboorteland')
RF10.1_Onderwijsniveau <- plyr::count(df_random_forest10.1, 'Onderwijsniveau')
RF10.1_Econ_status <- plyr::count(df_random_forest10.1, 'Econ._status')
RF10.1_Beroep <- plyr::count(df_random_forest10.1, 'Beroep')
RF10.1_SBI <- plyr::count(df_random_forest10.1, 'SBI')
RF10.1_Burg_Staat <- plyr::count(df_random_forest10.1, 'Burg._Staat')

RF10.2_Geslacht <- plyr::count(df_random_forest10.2, 'Geslacht')
RF10.2_Leeftijd <- plyr::count(df_random_forest10.2, 'Leeftijd')
RF10.2_HH_Pos <- plyr::count(df_random_forest10.2, 'HH_Pos')
RF10.2_HH_grootte <- plyr::count(df_random_forest10.2, 'HH_grootte')
RF10.2_Woonregio_vorig_jaar <- plyr::count(df_random_forest10.2, 'Woonregio_vorig_jaar')
RF10.2_Nationaliteit <- plyr::count(df_random_forest10.2, 'Nationaliteit')
RF10.2_Geboorteland <- plyr::count(df_random_forest10.2, 'Geboorteland')
RF10.2_Onderwijsniveau <- plyr::count(df_random_forest10.2, 'Onderwijsniveau')
RF10.2_Econ_status <- plyr::count(df_random_forest10.2, 'Econ._status')
RF10.2_Beroep <- plyr::count(df_random_forest10.2, 'Beroep')
RF10.2_SBI <- plyr::count(df_random_forest10.2, 'SBI')
RF10.2_Burg_Staat <- plyr::count(df_random_forest10.2, 'Burg._Staat')

RF10.3_Geslacht <- plyr::count(df_random_forest10.3, 'Geslacht')
RF10.3_Leeftijd <- plyr::count(df_random_forest10.3, 'Leeftijd')
RF10.3_HH_Pos <- plyr::count(df_random_forest10.3, 'HH_Pos')
RF10.3_HH_grootte <- plyr::count(df_random_forest10.3, 'HH_grootte')
RF10.3_Woonregio_vorig_jaar <- plyr::count(df_random_forest10.3, 'Woonregio_vorig_jaar')
RF10.3_Nationaliteit <- plyr::count(df_random_forest10.3, 'Nationaliteit')
RF10.3_Geboorteland <- plyr::count(df_random_forest10.3, 'Geboorteland')
RF10.3_Onderwijsniveau <- plyr::count(df_random_forest10.3, 'Onderwijsniveau')
RF10.3_Econ_status <- plyr::count(df_random_forest10.3, 'Econ._status')
RF10.3_Beroep <- plyr::count(df_random_forest10.3, 'Beroep')
RF10.3_SBI <- plyr::count(df_random_forest10.3, 'SBI')
RF10.3_Burg_Staat <- plyr::count(df_random_forest10.3, 'Burg._Staat')

rm(df_random_forest10.1, df_random_forest10.2, df_random_forest10.3)


# 10.1 data set 
BRF10.1_Geslacht <- Bias(RF10.1_Geslacht, Geslacht, "Geslacht")
BORF10.1_Geslacht <- sum(BRF10.1_Geslacht$percentageABS)/nrow(BRF10.1_Geslacht)

BRF10.1_Leeftijd <- Bias(RF10.1_Leeftijd, Leeftijd, "Leeftijd")
BORF10.1_Leeftijd <- sum(BRF10.1_Leeftijd$percentageABS)/nrow(BRF10.1_Leeftijd)

BRF10.1_HH_Pos <- Bias(RF10.1_HH_Pos, HH_Pos, "HH_Pos")
BORF10.1_HH_Pos <- sum(BRF10.1_HH_Pos$percentageABS)/nrow(BRF10.1_HH_Pos)

BRF10.1_HH_grootte <- Bias(RF10.1_HH_grootte, HH_grootte, "HH_grootte")
BORF10.1_HH_grootte <- sum(BRF10.1_HH_grootte$percentageABS)/nrow(BRF10.1_HH_grootte)

BRF10.1_Woonregio_vorig_jaar <- Bias(RF10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF10.1_Woonregio_vorig_jaar <- sum(BRF10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BRF10.1_Woonregio_vorig_jaar)

BRF10.1_Nationaliteit <- Bias(RF10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF10.1_Nationaliteit <- sum(BRF10.1_Nationaliteit$percentageABS)/nrow(BRF10.1_Nationaliteit)

BRF10.1_Geboorteland <- Bias(RF10.1_Geboorteland, Geboorteland, "Geboorteland")
BORF10.1_Geboorteland <- sum(BRF10.1_Geboorteland$percentageABS)/nrow(BRF10.1_Geboorteland)

BRF10.1_Onderwijsniveau <- Bias(RF10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF10.1_Onderwijsniveau <- sum(BRF10.1_Onderwijsniveau$percentageABS)/nrow(BRF10.1_Onderwijsniveau)

BRF10.1_Econ_status <- Bias(RF10.1_Econ_status, Econ_status, "Econ._status")
BORF10.1_Econ_status <- sum(BRF10.1_Econ_status$percentageABS)/nrow(BRF10.1_Econ_status)

BRF10.1_Beroep <- Bias(RF10.1_Beroep, Beroep, "Beroep")
BORF10.1_Beroep <- sum(BRF10.1_Beroep$percentageABS)/nrow(BRF10.1_Beroep)

BRF10.1_SBI <- Bias(RF10.1_SBI, SBI, "SBI")
BORF10.1_SBI <- sum(BRF10.1_SBI$percentageABS)/nrow(BRF10.1_SBI)

BRF10.1_Burg_Staat <- Bias(RF10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF10.1_Burg_Staat <- sum(BRF10.1_Burg_Staat$percentageABS)/nrow(BRF10.1_Burg_Staat)

BiasOverallRF10.1 <- sum(BORF10.1_Geslacht, BORF10.1_Leeftijd, BORF10.1_HH_Pos, BORF10.1_HH_grootte, BORF10.1_Woonregio_vorig_jaar, 
                         BORF10.1_Nationaliteit, BORF10.1_Geboorteland, BORF10.1_Onderwijsniveau, BORF10.1_Econ_status, 
                         BORF10.1_Beroep, BORF10.1_SBI, BORF10.1_Burg_Staat) / 12

rm(RF10.1_Geslacht, RF10.1_Leeftijd, RF10.1_HH_Pos, RF10.1_HH_grootte, RF10.1_Woonregio_vorig_jaar, 
   RF10.1_Nationaliteit, RF10.1_Geboorteland, RF10.1_Onderwijsniveau, RF10.1_Econ_status, 
   RF10.1_Beroep, RF10.1_SBI, RF10.1_Burg_Staat)
rm(BRF10.1_Geslacht, BRF10.1_Leeftijd, BRF10.1_HH_Pos, BRF10.1_HH_grootte, BRF10.1_Woonregio_vorig_jaar, 
   BRF10.1_Nationaliteit, BRF10.1_Geboorteland, BRF10.1_Onderwijsniveau, BRF10.1_Econ_status, 
   BRF10.1_Beroep, BRF10.1_SBI, BRF10.1_Burg_Staat)
rm(BORF10.1_Geslacht, BORF10.1_Leeftijd, BORF10.1_HH_Pos, BORF10.1_HH_grootte, BORF10.1_Woonregio_vorig_jaar, 
   BORF10.1_Nationaliteit, BORF10.1_Geboorteland, BORF10.1_Onderwijsniveau, BORF10.1_Econ_status, 
   BORF10.1_Beroep, BORF10.1_SBI, BORF10.1_Burg_Staat)

# 10.2 data set 
BRF10.2_Geslacht <- Bias(RF10.2_Geslacht, Geslacht, "Geslacht")
BORF10.2_Geslacht <- sum(BRF10.2_Geslacht$percentageABS)/nrow(BRF10.2_Geslacht)

BRF10.2_Leeftijd <- Bias(RF10.2_Leeftijd, Leeftijd, "Leeftijd")
BORF10.2_Leeftijd <- sum(BRF10.2_Leeftijd$percentageABS)/nrow(BRF10.2_Leeftijd)

BRF10.2_HH_Pos <- Bias(RF10.2_HH_Pos, HH_Pos, "HH_Pos")
BORF10.2_HH_Pos <- sum(BRF10.2_HH_Pos$percentageABS)/nrow(BRF10.2_HH_Pos)

BRF10.2_HH_grootte <- Bias(RF10.2_HH_grootte, HH_grootte, "HH_grootte")
BORF10.2_HH_grootte <- sum(BRF10.2_HH_grootte$percentageABS)/nrow(BRF10.2_HH_grootte)

BRF10.2_Woonregio_vorig_jaar <- Bias(RF10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF10.2_Woonregio_vorig_jaar <- sum(BRF10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BRF10.2_Woonregio_vorig_jaar)

BRF10.2_Nationaliteit <- Bias(RF10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF10.2_Nationaliteit <- sum(BRF10.2_Nationaliteit$percentageABS)/nrow(BRF10.2_Nationaliteit)

BRF10.2_Geboorteland <- Bias(RF10.2_Geboorteland, Geboorteland, "Geboorteland")
BORF10.2_Geboorteland <- sum(BRF10.2_Geboorteland$percentageABS)/nrow(BRF10.2_Geboorteland)

BRF10.2_Onderwijsniveau <- Bias(RF10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF10.2_Onderwijsniveau <- sum(BRF10.2_Onderwijsniveau$percentageABS)/nrow(BRF10.2_Onderwijsniveau)

BRF10.2_Econ_status <- Bias(RF10.2_Econ_status, Econ_status, "Econ._status")
BORF10.2_Econ_status <- sum(BRF10.2_Econ_status$percentageABS)/nrow(BRF10.2_Econ_status)

BRF10.2_Beroep <- Bias(RF10.2_Beroep, Beroep, "Beroep")
BORF10.2_Beroep <- sum(BRF10.2_Beroep$percentageABS)/nrow(BRF10.2_Beroep)

BRF10.2_SBI <- Bias(RF10.2_SBI, SBI, "SBI")
BORF10.2_SBI <- sum(BRF10.2_SBI$percentageABS)/nrow(BRF10.2_SBI)

BRF10.2_Burg_Staat <- Bias(RF10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF10.2_Burg_Staat <- sum(BRF10.2_Burg_Staat$percentageABS)/nrow(BRF10.2_Burg_Staat)

BiasOverallRF10.2 <- sum(BORF10.2_Geslacht, BORF10.2_Leeftijd, BORF10.2_HH_Pos, BORF10.2_HH_grootte, BORF10.2_Woonregio_vorig_jaar, 
                         BORF10.2_Nationaliteit, BORF10.2_Geboorteland, BORF10.2_Onderwijsniveau, BORF10.2_Econ_status, 
                         BORF10.2_Beroep, BORF10.2_SBI, BORF10.2_Burg_Staat) / 12

rm(RF10.2_Geslacht, RF10.2_Leeftijd, RF10.2_HH_Pos, RF10.2_HH_grootte, RF10.2_Woonregio_vorig_jaar, 
   RF10.2_Nationaliteit, RF10.2_Geboorteland, RF10.2_Onderwijsniveau, RF10.2_Econ_status, 
   RF10.2_Beroep, RF10.2_SBI, RF10.2_Burg_Staat)
rm(BRF10.2_Geslacht, BRF10.2_Leeftijd, BRF10.2_HH_Pos, BRF10.2_HH_grootte, BRF10.2_Woonregio_vorig_jaar, 
   BRF10.2_Nationaliteit, BRF10.2_Geboorteland, BRF10.2_Onderwijsniveau, BRF10.2_Econ_status, 
   BRF10.2_Beroep, BRF10.2_SBI, BRF10.2_Burg_Staat)
rm(BORF10.2_Geslacht, BORF10.2_Leeftijd, BORF10.2_HH_Pos, BORF10.2_HH_grootte, BORF10.2_Woonregio_vorig_jaar, 
   BORF10.2_Nationaliteit, BORF10.2_Geboorteland, BORF10.2_Onderwijsniveau, BORF10.2_Econ_status, 
   BORF10.2_Beroep, BORF10.2_SBI, BORF10.2_Burg_Staat)

# 10.3 data set 
BRF10.3_Geslacht <- Bias(RF10.3_Geslacht, Geslacht, "Geslacht")
BORF10.3_Geslacht <- sum(BRF10.3_Geslacht$percentageABS)/nrow(BRF10.3_Geslacht)

BRF10.3_Leeftijd <- Bias(RF10.3_Leeftijd, Leeftijd, "Leeftijd")
BORF10.3_Leeftijd <- sum(BRF10.3_Leeftijd$percentageABS)/nrow(BRF10.3_Leeftijd)

BRF10.3_HH_Pos <- Bias(RF10.3_HH_Pos, HH_Pos, "HH_Pos")
BORF10.3_HH_Pos <- sum(BRF10.3_HH_Pos$percentageABS)/nrow(BRF10.3_HH_Pos)

BRF10.3_HH_grootte <- Bias(RF10.3_HH_grootte, HH_grootte, "HH_grootte")
BORF10.3_HH_grootte <- sum(BRF10.3_HH_grootte$percentageABS)/nrow(BRF10.3_HH_grootte)

BRF10.3_Woonregio_vorig_jaar <- Bias(RF10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BORF10.3_Woonregio_vorig_jaar <- sum(BRF10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BRF10.3_Woonregio_vorig_jaar)

BRF10.3_Nationaliteit <- Bias(RF10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BORF10.3_Nationaliteit <- sum(BRF10.3_Nationaliteit$percentageABS)/nrow(BRF10.3_Nationaliteit)

BRF10.3_Geboorteland <- Bias(RF10.3_Geboorteland, Geboorteland, "Geboorteland")
BORF10.3_Geboorteland <- sum(BRF10.3_Geboorteland$percentageABS)/nrow(BRF10.3_Geboorteland)

BRF10.3_Onderwijsniveau <- Bias(RF10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BORF10.3_Onderwijsniveau <- sum(BRF10.3_Onderwijsniveau$percentageABS)/nrow(BRF10.3_Onderwijsniveau)

BRF10.3_Econ_status <- Bias(RF10.3_Econ_status, Econ_status, "Econ._status")
BORF10.3_Econ_status <- sum(BRF10.3_Econ_status$percentageABS)/nrow(BRF10.3_Econ_status)

BRF10.3_Beroep <- Bias(RF10.3_Beroep, Beroep, "Beroep")
BORF10.3_Beroep <- sum(BRF10.3_Beroep$percentageABS)/nrow(BRF10.3_Beroep)

BRF10.3_SBI <- Bias(RF10.3_SBI, SBI, "SBI")
BORF10.3_SBI <- sum(BRF10.3_SBI$percentageABS)/nrow(BRF10.3_SBI)

BRF10.3_Burg_Staat <- Bias(RF10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BORF10.3_Burg_Staat <- sum(BRF10.3_Burg_Staat$percentageABS)/nrow(BRF10.3_Burg_Staat)

BiasOverallRF10.3 <- sum(BORF10.3_Geslacht, BORF10.3_Leeftijd, BORF10.3_HH_Pos, BORF10.3_HH_grootte, BORF10.3_Woonregio_vorig_jaar, 
                         BORF10.3_Nationaliteit, BORF10.3_Geboorteland, BORF10.3_Onderwijsniveau, BORF10.3_Econ_status, 
                         BORF10.3_Beroep, BORF10.3_SBI, BORF10.3_Burg_Staat) / 12

rm(RF10.3_Geslacht, RF10.3_Leeftijd, RF10.3_HH_Pos, RF10.3_HH_grootte, RF10.3_Woonregio_vorig_jaar, 
   RF10.3_Nationaliteit, RF10.3_Geboorteland, RF10.3_Onderwijsniveau, RF10.3_Econ_status, 
   RF10.3_Beroep, RF10.3_SBI, RF10.3_Burg_Staat)
rm(BRF10.3_Geslacht, BRF10.3_Leeftijd, BRF10.3_HH_Pos, BRF10.3_HH_grootte, BRF10.3_Woonregio_vorig_jaar, 
   BRF10.3_Nationaliteit, BRF10.3_Geboorteland, BRF10.3_Onderwijsniveau, BRF10.3_Econ_status, 
   BRF10.3_Beroep, BRF10.3_SBI, BRF10.3_Burg_Staat)
rm(BORF10.3_Geslacht, BORF10.3_Leeftijd, BORF10.3_HH_Pos, BORF10.3_HH_grootte, BORF10.3_Woonregio_vorig_jaar, 
   BORF10.3_Nationaliteit, BORF10.3_Geboorteland, BORF10.3_Onderwijsniveau, BORF10.3_Econ_status, 
   BORF10.3_Beroep, BORF10.3_SBI, BORF10.3_Burg_Staat)
