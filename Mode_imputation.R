
# Installing packages -----------------------------------------------------------------------------------------------------------------
#install.packages("ForImp")
#install.packages("devtools")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("plyr")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("ForImp")
library("readxl")
library("devtools")
install_github("jabiru/tictoc")
library("tictoc")
library("dplyr")
library("plyr")


## Loading ipums data set ---------------------------------------------------------------------------------------------------------
ipums <- get(load(file = "IPUMS/ipums.Rdata"))


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------


## Mode Imputation with 'ForImp' --------------------------------------------------------------------------------------------------!!!

# 2% data sets

# 2.1 data set 
MCAR2.1 <- get(load(file = "MCAR2_1.Rdata"))
MO_MCAR2.1 <- data.frame(MCAR2.1)
names(MO_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR2.1)
tic("Mode Imputation 2.1 processing time...")
mode_imputation2.1 <- modeimp(MO_MCAR2.1)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR2.1)
df_mode_imputation2.1 <- as.data.frame(mode_imputation2.1)
save(df_mode_imputation2.1, file = "mode_imputation21.Rdata")
rm(mode_imputation2.1)
MO2.1_correct <- MO2.1_correct + sum(ipums == df_mode_imputation2.1)
MO2.1_total <- MO2.1_total + sum(!is.na(df_mode_imputation2.1))
rm(df_mode_imputation2.1)

# 2.2 data set 
MCAR2.2 <- get(load(file = "MCAR2_2.Rdata"))
MO_MCAR2.2 <- data.frame(MCAR2.2)
names(MO_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR2.2)
tic("Mode Imputation 2.2 processing time...")
mode_imputation2.2 <- modeimp(MO_MCAR2.2)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR2.2)
df_mode_imputation2.2 <- as.data.frame(mode_imputation2.2)
save(df_mode_imputation2.2, file = "mode_imputation22.Rdata")
rm(mode_imputation2.2)
MO2.2_correct <- MO2.2_correct + sum(ipums == df_mode_imputation2.2)
MO2.2_total <- MO2.2_total + sum(!is.na(df_mode_imputation2.2))
rm(df_mode_imputation2.2)

# 2.3 data set 
MCAR2.3 <- get(load(file = "MCAR2_3.Rdata"))
MO_MCAR2.3 <- data.frame(MCAR2.3)
names(MO_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR2.3)
tic("Mode Imputation 2.3 processing time...")
mode_imputation2.3 <- modeimp(MO_MCAR2.3)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR2.3)
df_mode_imputation2.3 <- as.data.frame(mode_imputation2.3)
save(df_mode_imputation2.3, file = "mode_imputation23.Rdata")
rm(mode_imputation2.3)
MO2.3_correct <- MO2.3_correct + sum(ipums == df_mode_imputation2.3)
MO2.3_total <- MO2.3_total + sum(!is.na(df_mode_imputation2.3))
rm(df_mode_imputation2.3)


# 5% data sets

# 5.1 data set 
MCAR5.1 <- get(load(file = "MCAR5_1.Rdata"))
MO_MCAR5.1 <- data.frame(MCAR5.1)
names(MO_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR5.1)
tic("Mode Imputation 5.1 processing time...")
mode_imputation5.1 <- modeimp(MO_MCAR5.1)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR5.1)
df_mode_imputation5.1 <- as.data.frame(mode_imputation5.1)
save(df_mode_imputation5.1, file = "mode_imputation51.Rdata")
rm(mode_imputation5.1)
MO5.1_correct <- MO5.1_correct + sum(ipums == df_mode_imputation5.1)
MO5.1_total <- MO5.1_total + sum(!is.na(df_mode_imputation5.1))
rm(df_mode_imputation5.1)

# 5.2 data set 
MCAR5.2 <- get(load(file = "MCAR5_2.Rdata"))
MO_MCAR5.2 <- data.frame(MCAR5.2)
names(MO_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR5.2)
tic("Mode Imputation 5.2 processing time...")
mode_imputation5.2 <- modeimp(MO_MCAR5.2)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR5.2)
df_mode_imputation5.2 <- as.data.frame(mode_imputation5.2)
save(df_mode_imputation5.2, file = "mode_imputation52.Rdata")
rm(mode_imputation5.2)
MO5.2_correct <- MO5.2_correct + sum(ipums == df_mode_imputation5.2)
MO5.2_total <- MO5.2_total + sum(!is.na(df_mode_imputation5.2))
rm(df_mode_imputation5.2)

# 5.3 data set 
MCAR5.3 <- get(load(file = "MCAR5_3.Rdata"))
MO_MCAR5.3 <- data.frame(MCAR5.3)
names(MO_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR5.3)
tic("Mode Imputation 5.3 processing time...")
mode_imputation5.3 <- modeimp(MO_MCAR5.3)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR5.3)
df_mode_imputation5.3 <- as.data.frame(mode_imputation5.3)
save(df_mode_imputation5.3, file = "mode_imputation53.Rdata")
rm(mode_imputation5.3)
MO5.3_correct <- MO5.3_correct + sum(ipums == df_mode_imputation5.3)
MO5.3_total <- MO5.3_total + sum(!is.na(df_mode_imputation5.3))
rm(df_mode_imputation5.3)


# 10% data sets 

# 10.1 data set 
MCAR10.1 <- get(load(file = "MCAR10_1.Rdata"))
MO_MCAR10.1 <- data.frame(MCAR10.1)
names(MO_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR10.1)
tic("Mode Imputation 10.1 processing time...")
mode_imputation10.1 <- modeimp(MO_MCAR10.1)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR10.1)
df_mode_imputation10.1 <- as.data.frame(mode_imputation10.1)
save(df_mode_imputation10.1, file = "mode_imputation101.Rdata")
rm(mode_imputation10.1)
MO10.1_correct <- MO10.1_correct + sum(ipums == df_mode_imputation10.1)
MO10.1_total <- MO10.1_total + sum(!is.na(df_mode_imputation10.1))
rm(df_mode_imputation10.1)

# 10.2 data set 
MCAR10.2 <- get(load(file = "MCAR10_2.Rdata"))
MO_MCAR10.2 <- data.frame(MCAR10.2)
names(MO_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR10.2)
tic("Mode Imputation 10.2 processing time...")
mode_imputation10.2 <- modeimp(MO_MCAR10.2)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR10.2)
df_mode_imputation10.2 <- as.data.frame(mode_imputation10.2)
save(df_mode_imputation10.2, file = "mode_imputation102.Rdata")
rm(mode_imputation10.2)
MO10.2_correct <- MO10.2_correct + sum(ipums == df_mode_imputation10.2)
MO10.2_total <- MO10.2_total + sum(!is.na(df_mode_imputation10.2))
rm(df_mode_imputation10.2)

# 10.3 data set 
MCAR10.3 <- get(load(file = "MCAR10_3.Rdata"))
MO_MCAR10.3 <- data.frame(MCAR10.3)
names(MO_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
rm(MCAR10.3)
tic("Mode Imputation 10.3 processing time...")
mode_imputation10.3 <- modeimp(MO_MCAR10.3)
process_time <- toc(log = TRUE)
process_time
rm(MO_MCAR10.3)
df_mode_imputation10.3 <- as.data.frame(mode_imputation10.3)
save(df_mode_imputation10.3, file = "mode_imputation103.Rdata")
rm(mode_imputation10.3)
MO10.3_correct <- MO10.3_correct + sum(ipums == df_mode_imputation10.3)
MO10.3_total <- MO10.3_total + sum(!is.na(df_mode_imputation10.3))
rm(df_mode_imputation10.3)


# Check if all values are imputed 
# anyNA(c(df_mode_imputation2.1, df_mode_imputation2.2, df_mode_imputation2.3, df_mode_imputation5.1, df_mode_imputation5.2, 
       # df_mode_imputation5.3, df_mode_imputation10.1, df_mode_imputation10.2, df_mode_imputation10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------


MO2.1_accuracy <- MO2.1_correct / MO2.1_total
MO2.2_accuracy <- MO2.2_correct / MO2.2_total
MO2.3_accuracy <- MO2.3_correct / MO2.3_total

MO5.1_accuracy <- MO5.1_correct / MO5.1_total
MO5.2_accuracy <- MO5.2_correct / MO5.2_total
MO5.3_accuracy <- MO5.3_correct / MO5.3_total

MO10.1_accuracy <- MO10.1_correct / MO10.1_total
MO10.2_accuracy <- MO10.2_correct / MO10.2_total
MO10.3_accuracy <- MO10.3_correct / MO10.3_total


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
MO2.1_Geslacht <- plyr::count(df_mode_imputation2.1, 'Geslacht')
MO2.1_Leeftijd <- plyr::count(df_mode_imputation2.1, 'Leeftijd')
MO2.1_HH_Pos <- plyr::count(df_mode_imputation2.1, 'HH_Pos')
MO2.1_HH_grootte <- plyr::count(df_mode_imputation2.1, 'HH_grootte')
MO2.1_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation2.1, 'Woonregio_vorig_jaar')
MO2.1_Nationaliteit <- plyr::count(df_mode_imputation2.1, 'Nationaliteit')
MO2.1_Geboorteland <- plyr::count(df_mode_imputation2.1, 'Geboorteland')
MO2.1_Onderwijsniveau <- plyr::count(df_mode_imputation2.1, 'Onderwijsniveau')
MO2.1_Econ_status <- plyr::count(df_mode_imputation2.1, 'Econ._status')
MO2.1_Beroep <- plyr::count(df_mode_imputation2.1, 'Beroep')
MO2.1_SBI <- plyr::count(df_mode_imputation2.1, 'SBI')
MO2.1_Burg_Staat <- plyr::count(df_mode_imputation2.1, 'Burg._Staat')

MO2.2_Geslacht <- plyr::count(df_mode_imputation2.2, 'Geslacht')
MO2.2_Leeftijd <- plyr::count(df_mode_imputation2.2, 'Leeftijd')
MO2.2_HH_Pos <- plyr::count(df_mode_imputation2.2, 'HH_Pos')
MO2.2_HH_grootte <- plyr::count(df_mode_imputation2.2, 'HH_grootte')
MO2.2_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation2.2, 'Woonregio_vorig_jaar')
MO2.2_Nationaliteit <- plyr::count(df_mode_imputation2.2, 'Nationaliteit')
MO2.2_Geboorteland <- plyr::count(df_mode_imputation2.2, 'Geboorteland')
MO2.2_Onderwijsniveau <- plyr::count(df_mode_imputation2.2, 'Onderwijsniveau')
MO2.2_Econ_status <- plyr::count(df_mode_imputation2.2, 'Econ._status')
MO2.2_Beroep <- plyr::count(df_mode_imputation2.2, 'Beroep')
MO2.2_SBI <- plyr::count(df_mode_imputation2.2, 'SBI')
MO2.2_Burg_Staat <- plyr::count(df_mode_imputation2.2, 'Burg._Staat')

MO2.3_Geslacht <- plyr::count(df_mode_imputation2.3, 'Geslacht')
MO2.3_Leeftijd <- plyr::count(df_mode_imputation2.3, 'Leeftijd')
MO2.3_HH_Pos <- plyr::count(df_mode_imputation2.3, 'HH_Pos')
MO2.3_HH_grootte <- plyr::count(df_mode_imputation2.3, 'HH_grootte')
MO2.3_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation2.3, 'Woonregio_vorig_jaar')
MO2.3_Nationaliteit <- plyr::count(df_mode_imputation2.3, 'Nationaliteit')
MO2.3_Geboorteland <- plyr::count(df_mode_imputation2.3, 'Geboorteland')
MO2.3_Onderwijsniveau <- plyr::count(df_mode_imputation2.3, 'Onderwijsniveau')
MO2.3_Econ_status <- plyr::count(df_mode_imputation2.3, 'Econ._status')
MO2.3_Beroep <- plyr::count(df_mode_imputation2.3, 'Beroep')
MO2.3_SBI <- plyr::count(df_mode_imputation2.3, 'SBI')
MO2.3_Burg_Staat <- plyr::count(df_mode_imputation2.3, 'Burg._Staat')

rm(df_mode_imputation2.1, df_mode_imputation2.2, df_mode_imputation2.3)

# 2.1 data set 
BMO2.1_Geslacht <- Bias(MO2.1_Geslacht, Geslacht, "Geslacht")
BOMO2.1_Geslacht <- sum(BMO2.1_Geslacht$percentageABS)/nrow(BMO2.1_Geslacht)

BMO2.1_Leeftijd <- Bias(MO2.1_Leeftijd, Leeftijd, "Leeftijd")
BOMO2.1_Leeftijd <- sum(BMO2.1_Leeftijd$percentageABS)/nrow(BMO2.1_Leeftijd)

BMO2.1_HH_Pos <- Bias(MO2.1_HH_Pos, HH_Pos, "HH_Pos")
BOMO2.1_HH_Pos <- sum(BMO2.1_HH_Pos$percentageABS)/nrow(BMO2.1_HH_Pos)

BMO2.1_HH_grootte <- Bias(MO2.1_HH_grootte, HH_grootte, "HH_grootte")
BOMO2.1_HH_grootte <- sum(BMO2.1_HH_grootte$percentageABS)/nrow(BMO2.1_HH_grootte)

BMO2.1_Woonregio_vorig_jaar <- Bias(MO2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO2.1_Woonregio_vorig_jaar <- sum(BMO2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BMO2.1_Woonregio_vorig_jaar)

BMO2.1_Nationaliteit <- Bias(MO2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO2.1_Nationaliteit <- sum(BMO2.1_Nationaliteit$percentageABS)/nrow(BMO2.1_Nationaliteit)

BMO2.1_Geboorteland <- Bias(MO2.1_Geboorteland, Geboorteland, "Geboorteland")
BOMO2.1_Geboorteland <- sum(BMO2.1_Geboorteland$percentageABS)/nrow(BMO2.1_Geboorteland)

BMO2.1_Onderwijsniveau <- Bias(MO2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO2.1_Onderwijsniveau <- sum(BMO2.1_Onderwijsniveau$percentageABS)/nrow(BMO2.1_Onderwijsniveau)

BMO2.1_Econ_status <- Bias(MO2.1_Econ_status, Econ_status, "Econ._status")
BOMO2.1_Econ_status <- sum(BMO2.1_Econ_status$percentageABS)/nrow(BMO2.1_Econ_status)

BMO2.1_Beroep <- Bias(MO2.1_Beroep, Beroep, "Beroep")
BOMO2.1_Beroep <- sum(BMO2.1_Beroep$percentageABS)/nrow(BMO2.1_Beroep)

BMO2.1_SBI <- Bias(MO2.1_SBI, SBI, "SBI")
BOMO2.1_SBI <- sum(BMO2.1_SBI$percentageABS)/nrow(BMO2.1_SBI)

BMO2.1_Burg_Staat <- Bias(MO2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO2.1_Burg_Staat <- sum(BMO2.1_Burg_Staat$percentageABS)/nrow(BMO2.1_Burg_Staat)

BiasOverallMO2.1 <- sum(BOMO2.1_Geslacht, BOMO2.1_Leeftijd, BOMO2.1_HH_Pos, BOMO2.1_HH_grootte, BOMO2.1_Woonregio_vorig_jaar, 
                        BOMO2.1_Nationaliteit, BOMO2.1_Geboorteland, BOMO2.1_Onderwijsniveau, BOMO2.1_Econ_status, 
                        BOMO2.1_Beroep, BOMO2.1_SBI, BOMO2.1_Burg_Staat) / 12

rm(MO2.1_Geslacht, MO2.1_Leeftijd, MO2.1_HH_Pos, MO2.1_HH_grootte, MO2.1_Woonregio_vorig_jaar, 
   MO2.1_Nationaliteit, MO2.1_Geboorteland, MO2.1_Onderwijsniveau, MO2.1_Econ_status, 
   MO2.1_Beroep, MO2.1_SBI, MO2.1_Burg_Staat)
rm(BMO2.1_Geslacht, BMO2.1_Leeftijd, BMO2.1_HH_Pos, BMO2.1_HH_grootte, BMO2.1_Woonregio_vorig_jaar, 
   BMO2.1_Nationaliteit, BMO2.1_Geboorteland, BMO2.1_Onderwijsniveau, BMO2.1_Econ_status, 
   BMO2.1_Beroep, BMO2.1_SBI, BMO2.1_Burg_Staat)
rm(BOMO2.1_Geslacht, BOMO2.1_Leeftijd, BOMO2.1_HH_Pos, BOMO2.1_HH_grootte, BOMO2.1_Woonregio_vorig_jaar, 
   BOMO2.1_Nationaliteit, BOMO2.1_Geboorteland, BOMO2.1_Onderwijsniveau, BOMO2.1_Econ_status, 
   BOMO2.1_Beroep, BOMO2.1_SBI, BOMO2.1_Burg_Staat)

# 2.2 data set 
BMO2.2_Geslacht <- Bias(MO2.2_Geslacht, Geslacht, "Geslacht")
BOMO2.2_Geslacht <- sum(BMO2.2_Geslacht$percentageABS)/nrow(BMO2.2_Geslacht)

BMO2.2_Leeftijd <- Bias(MO2.2_Leeftijd, Leeftijd, "Leeftijd")
BOMO2.2_Leeftijd <- sum(BMO2.2_Leeftijd$percentageABS)/nrow(BMO2.2_Leeftijd)

BMO2.2_HH_Pos <- Bias(MO2.2_HH_Pos, HH_Pos, "HH_Pos")
BOMO2.2_HH_Pos <- sum(BMO2.2_HH_Pos$percentageABS)/nrow(BMO2.2_HH_Pos)

BMO2.2_HH_grootte <- Bias(MO2.2_HH_grootte, HH_grootte, "HH_grootte")
BOMO2.2_HH_grootte <- sum(BMO2.2_HH_grootte$percentageABS)/nrow(BMO2.2_HH_grootte)

BMO2.2_Woonregio_vorig_jaar <- Bias(MO2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO2.2_Woonregio_vorig_jaar <- sum(BMO2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BMO2.2_Woonregio_vorig_jaar)

BMO2.2_Nationaliteit <- Bias(MO2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO2.2_Nationaliteit <- sum(BMO2.2_Nationaliteit$percentageABS)/nrow(BMO2.2_Nationaliteit)

BMO2.2_Geboorteland <- Bias(MO2.2_Geboorteland, Geboorteland, "Geboorteland")
BOMO2.2_Geboorteland <- sum(BMO2.2_Geboorteland$percentageABS)/nrow(BMO2.2_Geboorteland)

BMO2.2_Onderwijsniveau <- Bias(MO2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO2.2_Onderwijsniveau <- sum(BMO2.2_Onderwijsniveau$percentageABS)/nrow(BMO2.2_Onderwijsniveau)

BMO2.2_Econ_status <- Bias(MO2.2_Econ_status, Econ_status, "Econ._status")
BOMO2.2_Econ_status <- sum(BMO2.2_Econ_status$percentageABS)/nrow(BMO2.2_Econ_status)

BMO2.2_Beroep <- Bias(MO2.2_Beroep, Beroep, "Beroep")
BOMO2.2_Beroep <- sum(BMO2.2_Beroep$percentageABS)/nrow(BMO2.2_Beroep)

BMO2.2_SBI <- Bias(MO2.2_SBI, SBI, "SBI")
BOMO2.2_SBI <- sum(BMO2.2_SBI$percentageABS)/nrow(BMO2.2_SBI)

BMO2.2_Burg_Staat <- Bias(MO2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO2.2_Burg_Staat <- sum(BMO2.2_Burg_Staat$percentageABS)/nrow(BMO2.2_Burg_Staat)

BiasOverallMO2.2 <- sum(BOMO2.2_Geslacht, BOMO2.2_Leeftijd, BOMO2.2_HH_Pos, BOMO2.2_HH_grootte, BOMO2.2_Woonregio_vorig_jaar, 
                        BOMO2.2_Nationaliteit, BOMO2.2_Geboorteland, BOMO2.2_Onderwijsniveau, BOMO2.2_Econ_status, 
                        BOMO2.2_Beroep, BOMO2.2_SBI, BOMO2.2_Burg_Staat) / 12

rm(MO2.2_Geslacht, MO2.2_Leeftijd, MO2.2_HH_Pos, MO2.2_HH_grootte, MO2.2_Woonregio_vorig_jaar, 
   MO2.2_Nationaliteit, MO2.2_Geboorteland, MO2.2_Onderwijsniveau, MO2.2_Econ_status, 
   MO2.2_Beroep, MO2.2_SBI, MO2.2_Burg_Staat)
rm(BMO2.2_Geslacht, BMO2.2_Leeftijd, BMO2.2_HH_Pos, BMO2.2_HH_grootte, BMO2.2_Woonregio_vorig_jaar, 
   BMO2.2_Nationaliteit, BMO2.2_Geboorteland, BMO2.2_Onderwijsniveau, BMO2.2_Econ_status, 
   BMO2.2_Beroep, BMO2.2_SBI, BMO2.2_Burg_Staat)
rm(BOMO2.2_Geslacht, BOMO2.2_Leeftijd, BOMO2.2_HH_Pos, BOMO2.2_HH_grootte, BOMO2.2_Woonregio_vorig_jaar, 
   BOMO2.2_Nationaliteit, BOMO2.2_Geboorteland, BOMO2.2_Onderwijsniveau, BOMO2.2_Econ_status, 
   BOMO2.2_Beroep, BOMO2.2_SBI, BOMO2.2_Burg_Staat)

# 2.3 data set 
BMO2.3_Geslacht <- Bias(MO2.3_Geslacht, Geslacht, "Geslacht")
BOMO2.3_Geslacht <- sum(BMO2.3_Geslacht$percentageABS)/nrow(BMO2.3_Geslacht)

BMO2.3_Leeftijd <- Bias(MO2.3_Leeftijd, Leeftijd, "Leeftijd")
BOMO2.3_Leeftijd <- sum(BMO2.3_Leeftijd$percentageABS)/nrow(BMO2.3_Leeftijd)

BMO2.3_HH_Pos <- Bias(MO2.3_HH_Pos, HH_Pos, "HH_Pos")
BOMO2.3_HH_Pos <- sum(BMO2.3_HH_Pos$percentageABS)/nrow(BMO2.3_HH_Pos)

BMO2.3_HH_grootte <- Bias(MO2.3_HH_grootte, HH_grootte, "HH_grootte")
BOMO2.3_HH_grootte <- sum(BMO2.3_HH_grootte$percentageABS)/nrow(BMO2.3_HH_grootte)

BMO2.3_Woonregio_vorig_jaar <- Bias(MO2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO2.3_Woonregio_vorig_jaar <- sum(BMO2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BMO2.3_Woonregio_vorig_jaar)

BMO2.3_Nationaliteit <- Bias(MO2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO2.3_Nationaliteit <- sum(BMO2.3_Nationaliteit$percentageABS)/nrow(BMO2.3_Nationaliteit)

BMO2.3_Geboorteland <- Bias(MO2.3_Geboorteland, Geboorteland, "Geboorteland")
BOMO2.3_Geboorteland <- sum(BMO2.3_Geboorteland$percentageABS)/nrow(BMO2.3_Geboorteland)

BMO2.3_Onderwijsniveau <- Bias(MO2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO2.3_Onderwijsniveau <- sum(BMO2.3_Onderwijsniveau$percentageABS)/nrow(BMO2.3_Onderwijsniveau)

BMO2.3_Econ_status <- Bias(MO2.3_Econ_status, Econ_status, "Econ._status")
BOMO2.3_Econ_status <- sum(BMO2.3_Econ_status$percentageABS)/nrow(BMO2.3_Econ_status)

BMO2.3_Beroep <- Bias(MO2.3_Beroep, Beroep, "Beroep")
BOMO2.3_Beroep <- sum(BMO2.3_Beroep$percentageABS)/nrow(BMO2.3_Beroep)

BMO2.3_SBI <- Bias(MO2.3_SBI, SBI, "SBI")
BOMO2.3_SBI <- sum(BMO2.3_SBI$percentageABS)/nrow(BMO2.3_SBI)

BMO2.3_Burg_Staat <- Bias(MO2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO2.3_Burg_Staat <- sum(BMO2.3_Burg_Staat$percentageABS)/nrow(BMO2.3_Burg_Staat)

BiasOverallMO2.3 <- sum(BOMO2.3_Geslacht, BOMO2.3_Leeftijd, BOMO2.3_HH_Pos, BOMO2.3_HH_grootte, BOMO2.3_Woonregio_vorig_jaar, 
                        BOMO2.3_Nationaliteit, BOMO2.3_Geboorteland, BOMO2.3_Onderwijsniveau, BOMO2.3_Econ_status, 
                        BOMO2.3_Beroep, BOMO2.3_SBI, BOMO2.3_Burg_Staat) / 12

rm(MO2.3_Geslacht, MO2.3_Leeftijd, MO2.3_HH_Pos, MO2.3_HH_grootte, MO2.3_Woonregio_vorig_jaar, 
   MO2.3_Nationaliteit, MO2.3_Geboorteland, MO2.3_Onderwijsniveau, MO2.3_Econ_status, 
   MO2.3_Beroep, MO2.3_SBI, MO2.3_Burg_Staat)
rm(BMO2.3_Geslacht, BMO2.3_Leeftijd, BMO2.3_HH_Pos, BMO2.3_HH_grootte, BMO2.3_Woonregio_vorig_jaar, 
   BMO2.3_Nationaliteit, BMO2.3_Geboorteland, BMO2.3_Onderwijsniveau, BMO2.3_Econ_status, 
   BMO2.3_Beroep, BMO2.3_SBI, BMO2.3_Burg_Staat)
rm(BOMO2.3_Geslacht, BOMO2.3_Leeftijd, BOMO2.3_HH_Pos, BOMO2.3_HH_grootte, BOMO2.3_Woonregio_vorig_jaar, 
   BOMO2.3_Nationaliteit, BOMO2.3_Geboorteland, BOMO2.3_Onderwijsniveau, BOMO2.3_Econ_status, 
   BOMO2.3_Beroep, BOMO2.3_SBI, BOMO2.3_Burg_Staat)


# 5% data sets

# Counting values
MO5.1_Geslacht <- plyr::count(df_mode_imputation5.1, 'Geslacht')
MO5.1_Leeftijd <- plyr::count(df_mode_imputation5.1, 'Leeftijd')
MO5.1_HH_Pos <- plyr::count(df_mode_imputation5.1, 'HH_Pos')
MO5.1_HH_grootte <- plyr::count(df_mode_imputation5.1, 'HH_grootte')
MO5.1_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation5.1, 'Woonregio_vorig_jaar')
MO5.1_Nationaliteit <- plyr::count(df_mode_imputation5.1, 'Nationaliteit')
MO5.1_Geboorteland <- plyr::count(df_mode_imputation5.1, 'Geboorteland')
MO5.1_Onderwijsniveau <- plyr::count(df_mode_imputation5.1, 'Onderwijsniveau')
MO5.1_Econ_status <- plyr::count(df_mode_imputation5.1, 'Econ._status')
MO5.1_Beroep <- plyr::count(df_mode_imputation5.1, 'Beroep')
MO5.1_SBI <- plyr::count(df_mode_imputation5.1, 'SBI')
MO5.1_Burg_Staat <- plyr::count(df_mode_imputation5.1, 'Burg._Staat')

MO5.2_Geslacht <- plyr::count(df_mode_imputation5.2, 'Geslacht')
MO5.2_Leeftijd <- plyr::count(df_mode_imputation5.2, 'Leeftijd')
MO5.2_HH_Pos <- plyr::count(df_mode_imputation5.2, 'HH_Pos')
MO5.2_HH_grootte <- plyr::count(df_mode_imputation5.2, 'HH_grootte')
MO5.2_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation5.2, 'Woonregio_vorig_jaar')
MO5.2_Nationaliteit <- plyr::count(df_mode_imputation5.2, 'Nationaliteit')
MO5.2_Geboorteland <- plyr::count(df_mode_imputation5.2, 'Geboorteland')
MO5.2_Onderwijsniveau <- plyr::count(df_mode_imputation5.2, 'Onderwijsniveau')
MO5.2_Econ_status <- plyr::count(df_mode_imputation5.2, 'Econ._status')
MO5.2_Beroep <- plyr::count(df_mode_imputation5.2, 'Beroep')
MO5.2_SBI <- plyr::count(df_mode_imputation5.2, 'SBI')
MO5.2_Burg_Staat <- plyr::count(df_mode_imputation5.2, 'Burg._Staat')

MO5.3_Geslacht <- plyr::count(df_mode_imputation5.3, 'Geslacht')
MO5.3_Leeftijd <- plyr::count(df_mode_imputation5.3, 'Leeftijd')
MO5.3_HH_Pos <- plyr::count(df_mode_imputation5.3, 'HH_Pos')
MO5.3_HH_grootte <- plyr::count(df_mode_imputation5.3, 'HH_grootte')
MO5.3_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation5.3, 'Woonregio_vorig_jaar')
MO5.3_Nationaliteit <- plyr::count(df_mode_imputation5.3, 'Nationaliteit')
MO5.3_Geboorteland <- plyr::count(df_mode_imputation5.3, 'Geboorteland')
MO5.3_Onderwijsniveau <- plyr::count(df_mode_imputation5.3, 'Onderwijsniveau')
MO5.3_Econ_status <- plyr::count(df_mode_imputation5.3, 'Econ._status')
MO5.3_Beroep <- plyr::count(df_mode_imputation5.3, 'Beroep')
MO5.3_SBI <- plyr::count(df_mode_imputation5.3, 'SBI')
MO5.3_Burg_Staat <- plyr::count(df_mode_imputation5.3, 'Burg._Staat')

rm(df_mode_imputation5.1, df_mode_imputation5.2, df_mode_imputation5.3)


# 5.1 data set 
BMO5.1_Geslacht <- Bias(MO5.1_Geslacht, Geslacht, "Geslacht")
BOMO5.1_Geslacht <- sum(BMO5.1_Geslacht$percentageABS)/nrow(BMO5.1_Geslacht)

BMO5.1_Leeftijd <- Bias(MO5.1_Leeftijd, Leeftijd, "Leeftijd")
BOMO5.1_Leeftijd <- sum(BMO5.1_Leeftijd$percentageABS)/nrow(BMO5.1_Leeftijd)

BMO5.1_HH_Pos <- Bias(MO5.1_HH_Pos, HH_Pos, "HH_Pos")
BOMO5.1_HH_Pos <- sum(BMO5.1_HH_Pos$percentageABS)/nrow(BMO5.1_HH_Pos)

BMO5.1_HH_grootte <- Bias(MO5.1_HH_grootte, HH_grootte, "HH_grootte")
BOMO5.1_HH_grootte <- sum(BMO5.1_HH_grootte$percentageABS)/nrow(BMO5.1_HH_grootte)

BMO5.1_Woonregio_vorig_jaar <- Bias(MO5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO5.1_Woonregio_vorig_jaar <- sum(BMO5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BMO5.1_Woonregio_vorig_jaar)

BMO5.1_Nationaliteit <- Bias(MO5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO5.1_Nationaliteit <- sum(BMO5.1_Nationaliteit$percentageABS)/nrow(BMO5.1_Nationaliteit)

BMO5.1_Geboorteland <- Bias(MO5.1_Geboorteland, Geboorteland, "Geboorteland")
BOMO5.1_Geboorteland <- sum(BMO5.1_Geboorteland$percentageABS)/nrow(BMO5.1_Geboorteland)

BMO5.1_Onderwijsniveau <- Bias(MO5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO5.1_Onderwijsniveau <- sum(BMO5.1_Onderwijsniveau$percentageABS)/nrow(BMO5.1_Onderwijsniveau)

BMO5.1_Econ_status <- Bias(MO5.1_Econ_status, Econ_status, "Econ._status")
BOMO5.1_Econ_status <- sum(BMO5.1_Econ_status$percentageABS)/nrow(BMO5.1_Econ_status)

BMO5.1_Beroep <- Bias(MO5.1_Beroep, Beroep, "Beroep")
BOMO5.1_Beroep <- sum(BMO5.1_Beroep$percentageABS)/nrow(BMO5.1_Beroep)

BMO5.1_SBI <- Bias(MO5.1_SBI, SBI, "SBI")
BOMO5.1_SBI <- sum(BMO5.1_SBI$percentageABS)/nrow(BMO5.1_SBI)

BMO5.1_Burg_Staat <- Bias(MO5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO5.1_Burg_Staat <- sum(BMO5.1_Burg_Staat$percentageABS)/nrow(BMO5.1_Burg_Staat)

BiasOverallMO5.1 <- sum(BOMO5.1_Geslacht, BOMO5.1_Leeftijd, BOMO5.1_HH_Pos, BOMO5.1_HH_grootte, BOMO5.1_Woonregio_vorig_jaar, 
                        BOMO5.1_Nationaliteit, BOMO5.1_Geboorteland, BOMO5.1_Onderwijsniveau, BOMO5.1_Econ_status, 
                        BOMO5.1_Beroep, BOMO5.1_SBI, BOMO5.1_Burg_Staat) / 12

rm(MO5.1_Geslacht, MO5.1_Leeftijd, MO5.1_HH_Pos, MO5.1_HH_grootte, MO5.1_Woonregio_vorig_jaar, 
   MO5.1_Nationaliteit, MO5.1_Geboorteland, MO5.1_Onderwijsniveau, MO5.1_Econ_status, 
   MO5.1_Beroep, MO5.1_SBI, MO5.1_Burg_Staat)
rm(BMO5.1_Geslacht, BMO5.1_Leeftijd, BMO5.1_HH_Pos, BMO5.1_HH_grootte, BMO5.1_Woonregio_vorig_jaar, 
   BMO5.1_Nationaliteit, BMO5.1_Geboorteland, BMO5.1_Onderwijsniveau, BMO5.1_Econ_status, 
   BMO5.1_Beroep, BMO5.1_SBI, BMO5.1_Burg_Staat)
rm(BOMO5.1_Geslacht, BOMO5.1_Leeftijd, BOMO5.1_HH_Pos, BOMO5.1_HH_grootte, BOMO5.1_Woonregio_vorig_jaar, 
   BOMO5.1_Nationaliteit, BOMO5.1_Geboorteland, BOMO5.1_Onderwijsniveau, BOMO5.1_Econ_status, 
   BOMO5.1_Beroep, BOMO5.1_SBI, BOMO5.1_Burg_Staat)

# 5.2 data set 
BMO5.2_Geslacht <- Bias(MO5.2_Geslacht, Geslacht, "Geslacht")
BOMO5.2_Geslacht <- sum(BMO5.2_Geslacht$percentageABS)/nrow(BMO5.2_Geslacht)

BMO5.2_Leeftijd <- Bias(MO5.2_Leeftijd, Leeftijd, "Leeftijd")
BOMO5.2_Leeftijd <- sum(BMO5.2_Leeftijd$percentageABS)/nrow(BMO5.2_Leeftijd)

BMO5.2_HH_Pos <- Bias(MO5.2_HH_Pos, HH_Pos, "HH_Pos")
BOMO5.2_HH_Pos <- sum(BMO5.2_HH_Pos$percentageABS)/nrow(BMO5.2_HH_Pos)

BMO5.2_HH_grootte <- Bias(MO5.2_HH_grootte, HH_grootte, "HH_grootte")
BOMO5.2_HH_grootte <- sum(BMO5.2_HH_grootte$percentageABS)/nrow(BMO5.2_HH_grootte)

BMO5.2_Woonregio_vorig_jaar <- Bias(MO5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO5.2_Woonregio_vorig_jaar <- sum(BMO5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BMO5.2_Woonregio_vorig_jaar)

BMO5.2_Nationaliteit <- Bias(MO5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO5.2_Nationaliteit <- sum(BMO5.2_Nationaliteit$percentageABS)/nrow(BMO5.2_Nationaliteit)

BMO5.2_Geboorteland <- Bias(MO5.2_Geboorteland, Geboorteland, "Geboorteland")
BOMO5.2_Geboorteland <- sum(BMO5.2_Geboorteland$percentageABS)/nrow(BMO5.2_Geboorteland)

BMO5.2_Onderwijsniveau <- Bias(MO5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO5.2_Onderwijsniveau <- sum(BMO5.2_Onderwijsniveau$percentageABS)/nrow(BMO5.2_Onderwijsniveau)

BMO5.2_Econ_status <- Bias(MO5.2_Econ_status, Econ_status, "Econ._status")
BOMO5.2_Econ_status <- sum(BMO5.2_Econ_status$percentageABS)/nrow(BMO5.2_Econ_status)

BMO5.2_Beroep <- Bias(MO5.2_Beroep, Beroep, "Beroep")
BOMO5.2_Beroep <- sum(BMO5.2_Beroep$percentageABS)/nrow(BMO5.2_Beroep)

BMO5.2_SBI <- Bias(MO5.2_SBI, SBI, "SBI")
BOMO5.2_SBI <- sum(BMO5.2_SBI$percentageABS)/nrow(BMO5.2_SBI)

BMO5.2_Burg_Staat <- Bias(MO5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO5.2_Burg_Staat <- sum(BMO5.2_Burg_Staat$percentageABS)/nrow(BMO5.2_Burg_Staat)

BiasOverallMO5.2 <- sum(BOMO5.2_Geslacht, BOMO5.2_Leeftijd, BOMO5.2_HH_Pos, BOMO5.2_HH_grootte, BOMO5.2_Woonregio_vorig_jaar, 
                        BOMO5.2_Nationaliteit, BOMO5.2_Geboorteland, BOMO5.2_Onderwijsniveau, BOMO5.2_Econ_status, 
                        BOMO5.2_Beroep, BOMO5.2_SBI, BOMO5.2_Burg_Staat) / 12

rm(MO5.2_Geslacht, MO5.2_Leeftijd, MO5.2_HH_Pos, MO5.2_HH_grootte, MO5.2_Woonregio_vorig_jaar, 
   MO5.2_Nationaliteit, MO5.2_Geboorteland, MO5.2_Onderwijsniveau, MO5.2_Econ_status, 
   MO5.2_Beroep, MO5.2_SBI, MO5.2_Burg_Staat)
rm(BMO5.2_Geslacht, BMO5.2_Leeftijd, BMO5.2_HH_Pos, BMO5.2_HH_grootte, BMO5.2_Woonregio_vorig_jaar, 
   BMO5.2_Nationaliteit, BMO5.2_Geboorteland, BMO5.2_Onderwijsniveau, BMO5.2_Econ_status, 
   BMO5.2_Beroep, BMO5.2_SBI, BMO5.2_Burg_Staat)
rm(BOMO5.2_Geslacht, BOMO5.2_Leeftijd, BOMO5.2_HH_Pos, BOMO5.2_HH_grootte, BOMO5.2_Woonregio_vorig_jaar, 
   BOMO5.2_Nationaliteit, BOMO5.2_Geboorteland, BOMO5.2_Onderwijsniveau, BOMO5.2_Econ_status, 
   BOMO5.2_Beroep, BOMO5.2_SBI, BOMO5.2_Burg_Staat)

# 5.3 data set 
BMO5.3_Geslacht <- Bias(MO5.3_Geslacht, Geslacht, "Geslacht")
BOMO5.3_Geslacht <- sum(BMO5.3_Geslacht$percentageABS)/nrow(BMO5.3_Geslacht)

BMO5.3_Leeftijd <- Bias(MO5.3_Leeftijd, Leeftijd, "Leeftijd")
BOMO5.3_Leeftijd <- sum(BMO5.3_Leeftijd$percentageABS)/nrow(BMO5.3_Leeftijd)

BMO5.3_HH_Pos <- Bias(MO5.3_HH_Pos, HH_Pos, "HH_Pos")
BOMO5.3_HH_Pos <- sum(BMO5.3_HH_Pos$percentageABS)/nrow(BMO5.3_HH_Pos)

BMO5.3_HH_grootte <- Bias(MO5.3_HH_grootte, HH_grootte, "HH_grootte")
BOMO5.3_HH_grootte <- sum(BMO5.3_HH_grootte$percentageABS)/nrow(BMO5.3_HH_grootte)

BMO5.3_Woonregio_vorig_jaar <- Bias(MO5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO5.3_Woonregio_vorig_jaar <- sum(BMO5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BMO5.3_Woonregio_vorig_jaar)

BMO5.3_Nationaliteit <- Bias(MO5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO5.3_Nationaliteit <- sum(BMO5.3_Nationaliteit$percentageABS)/nrow(BMO5.3_Nationaliteit)

BMO5.3_Geboorteland <- Bias(MO5.3_Geboorteland, Geboorteland, "Geboorteland")
BOMO5.3_Geboorteland <- sum(BMO5.3_Geboorteland$percentageABS)/nrow(BMO5.3_Geboorteland)

BMO5.3_Onderwijsniveau <- Bias(MO5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO5.3_Onderwijsniveau <- sum(BMO5.3_Onderwijsniveau$percentageABS)/nrow(BMO5.3_Onderwijsniveau)

BMO5.3_Econ_status <- Bias(MO5.3_Econ_status, Econ_status, "Econ._status")
BOMO5.3_Econ_status <- sum(BMO5.3_Econ_status$percentageABS)/nrow(BMO5.3_Econ_status)

BMO5.3_Beroep <- Bias(MO5.3_Beroep, Beroep, "Beroep")
BOMO5.3_Beroep <- sum(BMO5.3_Beroep$percentageABS)/nrow(BMO5.3_Beroep)

BMO5.3_SBI <- Bias(MO5.3_SBI, SBI, "SBI")
BOMO5.3_SBI <- sum(BMO5.3_SBI$percentageABS)/nrow(BMO5.3_SBI)

BMO5.3_Burg_Staat <- Bias(MO5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO5.3_Burg_Staat <- sum(BMO5.3_Burg_Staat$percentageABS)/nrow(BMO5.3_Burg_Staat)

BiasOverallMO5.3 <- sum(BOMO5.3_Geslacht, BOMO5.3_Leeftijd, BOMO5.3_HH_Pos, BOMO5.3_HH_grootte, BOMO5.3_Woonregio_vorig_jaar, 
                        BOMO5.3_Nationaliteit, BOMO5.3_Geboorteland, BOMO5.3_Onderwijsniveau, BOMO5.3_Econ_status, 
                        BOMO5.3_Beroep, BOMO5.3_SBI, BOMO5.3_Burg_Staat) / 12

rm(MO5.3_Geslacht, MO5.3_Leeftijd, MO5.3_HH_Pos, MO5.3_HH_grootte, MO5.3_Woonregio_vorig_jaar, 
   MO5.3_Nationaliteit, MO5.3_Geboorteland, MO5.3_Onderwijsniveau, MO5.3_Econ_status, 
   MO5.3_Beroep, MO5.3_SBI, MO5.3_Burg_Staat)
rm(BMO5.3_Geslacht, BMO5.3_Leeftijd, BMO5.3_HH_Pos, BMO5.3_HH_grootte, BMO5.3_Woonregio_vorig_jaar, 
   BMO5.3_Nationaliteit, BMO5.3_Geboorteland, BMO5.3_Onderwijsniveau, BMO5.3_Econ_status, 
   BMO5.3_Beroep, BMO5.3_SBI, BMO5.3_Burg_Staat)
rm(BOMO5.3_Geslacht, BOMO5.3_Leeftijd, BOMO5.3_HH_Pos, BOMO5.3_HH_grootte, BOMO5.3_Woonregio_vorig_jaar, 
   BOMO5.3_Nationaliteit, BOMO5.3_Geboorteland, BOMO5.3_Onderwijsniveau, BOMO5.3_Econ_status, 
   BOMO5.3_Beroep, BOMO5.3_SBI, BOMO5.3_Burg_Staat)


# 10% data sets

# Counting values
MO10.1_Geslacht <- plyr::count(df_mode_imputation10.1, 'Geslacht')
MO10.1_Leeftijd <- plyr::count(df_mode_imputation10.1, 'Leeftijd')
MO10.1_HH_Pos <- plyr::count(df_mode_imputation10.1, 'HH_Pos')
MO10.1_HH_grootte <- plyr::count(df_mode_imputation10.1, 'HH_grootte')
MO10.1_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation10.1, 'Woonregio_vorig_jaar')
MO10.1_Nationaliteit <- plyr::count(df_mode_imputation10.1, 'Nationaliteit')
MO10.1_Geboorteland <- plyr::count(df_mode_imputation10.1, 'Geboorteland')
MO10.1_Onderwijsniveau <- plyr::count(df_mode_imputation10.1, 'Onderwijsniveau')
MO10.1_Econ_status <- plyr::count(df_mode_imputation10.1, 'Econ._status')
MO10.1_Beroep <- plyr::count(df_mode_imputation10.1, 'Beroep')
MO10.1_SBI <- plyr::count(df_mode_imputation10.1, 'SBI')
MO10.1_Burg_Staat <- plyr::count(df_mode_imputation10.1, 'Burg._Staat')

MO10.2_Geslacht <- plyr::count(df_mode_imputation10.2, 'Geslacht')
MO10.2_Leeftijd <- plyr::count(df_mode_imputation10.2, 'Leeftijd')
MO10.2_HH_Pos <- plyr::count(df_mode_imputation10.2, 'HH_Pos')
MO10.2_HH_grootte <- plyr::count(df_mode_imputation10.2, 'HH_grootte')
MO10.2_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation10.2, 'Woonregio_vorig_jaar')
MO10.2_Nationaliteit <- plyr::count(df_mode_imputation10.2, 'Nationaliteit')
MO10.2_Geboorteland <- plyr::count(df_mode_imputation10.2, 'Geboorteland')
MO10.2_Onderwijsniveau <- plyr::count(df_mode_imputation10.2, 'Onderwijsniveau')
MO10.2_Econ_status <- plyr::count(df_mode_imputation10.2, 'Econ._status')
MO10.2_Beroep <- plyr::count(df_mode_imputation10.2, 'Beroep')
MO10.2_SBI <- plyr::count(df_mode_imputation10.2, 'SBI')
MO10.2_Burg_Staat <- plyr::count(df_mode_imputation10.2, 'Burg._Staat')

MO10.3_Geslacht <- plyr::count(df_mode_imputation10.3, 'Geslacht')
MO10.3_Leeftijd <- plyr::count(df_mode_imputation10.3, 'Leeftijd')
MO10.3_HH_Pos <- plyr::count(df_mode_imputation10.3, 'HH_Pos')
MO10.3_HH_grootte <- plyr::count(df_mode_imputation10.3, 'HH_grootte')
MO10.3_Woonregio_vorig_jaar <- plyr::count(df_mode_imputation10.3, 'Woonregio_vorig_jaar')
MO10.3_Nationaliteit <- plyr::count(df_mode_imputation10.3, 'Nationaliteit')
MO10.3_Geboorteland <- plyr::count(df_mode_imputation10.3, 'Geboorteland')
MO10.3_Onderwijsniveau <- plyr::count(df_mode_imputation10.3, 'Onderwijsniveau')
MO10.3_Econ_status <- plyr::count(df_mode_imputation10.3, 'Econ._status')
MO10.3_Beroep <- plyr::count(df_mode_imputation10.3, 'Beroep')
MO10.3_SBI <- plyr::count(df_mode_imputation10.3, 'SBI')
MO10.3_Burg_Staat <- plyr::count(df_mode_imputation10.3, 'Burg._Staat')

rm(df_mode_imputation10.1, df_mode_imputation10.2, df_mode_imputation10.3)


# 10.1 data set 
BMO10.1_Geslacht <- Bias(MO10.1_Geslacht, Geslacht, "Geslacht")
BOMO10.1_Geslacht <- sum(BMO10.1_Geslacht$percentageABS)/nrow(BMO10.1_Geslacht)

BMO10.1_Leeftijd <- Bias(MO10.1_Leeftijd, Leeftijd, "Leeftijd")
BOMO10.1_Leeftijd <- sum(BMO10.1_Leeftijd$percentageABS)/nrow(BMO10.1_Leeftijd)

BMO10.1_HH_Pos <- Bias(MO10.1_HH_Pos, HH_Pos, "HH_Pos")
BOMO10.1_HH_Pos <- sum(BMO10.1_HH_Pos$percentageABS)/nrow(BMO10.1_HH_Pos)

BMO10.1_HH_grootte <- Bias(MO10.1_HH_grootte, HH_grootte, "HH_grootte")
BOMO10.1_HH_grootte <- sum(BMO10.1_HH_grootte$percentageABS)/nrow(BMO10.1_HH_grootte)

BMO10.1_Woonregio_vorig_jaar <- Bias(MO10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO10.1_Woonregio_vorig_jaar <- sum(BMO10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BMO10.1_Woonregio_vorig_jaar)

BMO10.1_Nationaliteit <- Bias(MO10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO10.1_Nationaliteit <- sum(BMO10.1_Nationaliteit$percentageABS)/nrow(BMO10.1_Nationaliteit)

BMO10.1_Geboorteland <- Bias(MO10.1_Geboorteland, Geboorteland, "Geboorteland")
BOMO10.1_Geboorteland <- sum(BMO10.1_Geboorteland$percentageABS)/nrow(BMO10.1_Geboorteland)

BMO10.1_Onderwijsniveau <- Bias(MO10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO10.1_Onderwijsniveau <- sum(BMO10.1_Onderwijsniveau$percentageABS)/nrow(BMO10.1_Onderwijsniveau)

BMO10.1_Econ_status <- Bias(MO10.1_Econ_status, Econ_status, "Econ._status")
BOMO10.1_Econ_status <- sum(BMO10.1_Econ_status$percentageABS)/nrow(BMO10.1_Econ_status)

BMO10.1_Beroep <- Bias(MO10.1_Beroep, Beroep, "Beroep")
BOMO10.1_Beroep <- sum(BMO10.1_Beroep$percentageABS)/nrow(BMO10.1_Beroep)

BMO10.1_SBI <- Bias(MO10.1_SBI, SBI, "SBI")
BOMO10.1_SBI <- sum(BMO10.1_SBI$percentageABS)/nrow(BMO10.1_SBI)

BMO10.1_Burg_Staat <- Bias(MO10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO10.1_Burg_Staat <- sum(BMO10.1_Burg_Staat$percentageABS)/nrow(BMO10.1_Burg_Staat)

BiasOverallMO10.1 <- sum(BOMO10.1_Geslacht, BOMO10.1_Leeftijd, BOMO10.1_HH_Pos, BOMO10.1_HH_grootte, BOMO10.1_Woonregio_vorig_jaar, 
                         BOMO10.1_Nationaliteit, BOMO10.1_Geboorteland, BOMO10.1_Onderwijsniveau, BOMO10.1_Econ_status, 
                         BOMO10.1_Beroep, BOMO10.1_SBI, BOMO10.1_Burg_Staat) / 12

rm(MO10.1_Geslacht, MO10.1_Leeftijd, MO10.1_HH_Pos, MO10.1_HH_grootte, MO10.1_Woonregio_vorig_jaar, 
   MO10.1_Nationaliteit, MO10.1_Geboorteland, MO10.1_Onderwijsniveau, MO10.1_Econ_status, 
   MO10.1_Beroep, MO10.1_SBI, MO10.1_Burg_Staat)
rm(BMO10.1_Geslacht, BMO10.1_Leeftijd, BMO10.1_HH_Pos, BMO10.1_HH_grootte, BMO10.1_Woonregio_vorig_jaar, 
   BMO10.1_Nationaliteit, BMO10.1_Geboorteland, BMO10.1_Onderwijsniveau, BMO10.1_Econ_status, 
   BMO10.1_Beroep, BMO10.1_SBI, BMO10.1_Burg_Staat)
rm(BOMO10.1_Geslacht, BOMO10.1_Leeftijd, BOMO10.1_HH_Pos, BOMO10.1_HH_grootte, BOMO10.1_Woonregio_vorig_jaar, 
   BOMO10.1_Nationaliteit, BOMO10.1_Geboorteland, BOMO10.1_Onderwijsniveau, BOMO10.1_Econ_status, 
   BOMO10.1_Beroep, BOMO10.1_SBI, BOMO10.1_Burg_Staat)

# 10.2 data set 
BMO10.2_Geslacht <- Bias(MO10.2_Geslacht, Geslacht, "Geslacht")
BOMO10.2_Geslacht <- sum(BMO10.2_Geslacht$percentageABS)/nrow(BMO10.2_Geslacht)

BMO10.2_Leeftijd <- Bias(MO10.2_Leeftijd, Leeftijd, "Leeftijd")
BOMO10.2_Leeftijd <- sum(BMO10.2_Leeftijd$percentageABS)/nrow(BMO10.2_Leeftijd)

BMO10.2_HH_Pos <- Bias(MO10.2_HH_Pos, HH_Pos, "HH_Pos")
BOMO10.2_HH_Pos <- sum(BMO10.2_HH_Pos$percentageABS)/nrow(BMO10.2_HH_Pos)

BMO10.2_HH_grootte <- Bias(MO10.2_HH_grootte, HH_grootte, "HH_grootte")
BOMO10.2_HH_grootte <- sum(BMO10.2_HH_grootte$percentageABS)/nrow(BMO10.2_HH_grootte)

BMO10.2_Woonregio_vorig_jaar <- Bias(MO10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO10.2_Woonregio_vorig_jaar <- sum(BMO10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BMO10.2_Woonregio_vorig_jaar)

BMO10.2_Nationaliteit <- Bias(MO10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO10.2_Nationaliteit <- sum(BMO10.2_Nationaliteit$percentageABS)/nrow(BMO10.2_Nationaliteit)

BMO10.2_Geboorteland <- Bias(MO10.2_Geboorteland, Geboorteland, "Geboorteland")
BOMO10.2_Geboorteland <- sum(BMO10.2_Geboorteland$percentageABS)/nrow(BMO10.2_Geboorteland)

BMO10.2_Onderwijsniveau <- Bias(MO10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO10.2_Onderwijsniveau <- sum(BMO10.2_Onderwijsniveau$percentageABS)/nrow(BMO10.2_Onderwijsniveau)

BMO10.2_Econ_status <- Bias(MO10.2_Econ_status, Econ_status, "Econ._status")
BOMO10.2_Econ_status <- sum(BMO10.2_Econ_status$percentageABS)/nrow(BMO10.2_Econ_status)

BMO10.2_Beroep <- Bias(MO10.2_Beroep, Beroep, "Beroep")
BOMO10.2_Beroep <- sum(BMO10.2_Beroep$percentageABS)/nrow(BMO10.2_Beroep)

BMO10.2_SBI <- Bias(MO10.2_SBI, SBI, "SBI")
BOMO10.2_SBI <- sum(BMO10.2_SBI$percentageABS)/nrow(BMO10.2_SBI)

BMO10.2_Burg_Staat <- Bias(MO10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO10.2_Burg_Staat <- sum(BMO10.2_Burg_Staat$percentageABS)/nrow(BMO10.2_Burg_Staat)

BiasOverallMO10.2 <- sum(BOMO10.2_Geslacht, BOMO10.2_Leeftijd, BOMO10.2_HH_Pos, BOMO10.2_HH_grootte, BOMO10.2_Woonregio_vorig_jaar, 
                         BOMO10.2_Nationaliteit, BOMO10.2_Geboorteland, BOMO10.2_Onderwijsniveau, BOMO10.2_Econ_status, 
                         BOMO10.2_Beroep, BOMO10.2_SBI, BOMO10.2_Burg_Staat) / 12

rm(MO10.2_Geslacht, MO10.2_Leeftijd, MO10.2_HH_Pos, MO10.2_HH_grootte, MO10.2_Woonregio_vorig_jaar, 
   MO10.2_Nationaliteit, MO10.2_Geboorteland, MO10.2_Onderwijsniveau, MO10.2_Econ_status, 
   MO10.2_Beroep, MO10.2_SBI, MO10.2_Burg_Staat)
rm(BMO10.2_Geslacht, BMO10.2_Leeftijd, BMO10.2_HH_Pos, BMO10.2_HH_grootte, BMO10.2_Woonregio_vorig_jaar, 
   BMO10.2_Nationaliteit, BMO10.2_Geboorteland, BMO10.2_Onderwijsniveau, BMO10.2_Econ_status, 
   BMO10.2_Beroep, BMO10.2_SBI, BMO10.2_Burg_Staat)
rm(BOMO10.2_Geslacht, BOMO10.2_Leeftijd, BOMO10.2_HH_Pos, BOMO10.2_HH_grootte, BOMO10.2_Woonregio_vorig_jaar, 
   BOMO10.2_Nationaliteit, BOMO10.2_Geboorteland, BOMO10.2_Onderwijsniveau, BOMO10.2_Econ_status, 
   BOMO10.2_Beroep, BOMO10.2_SBI, BOMO10.2_Burg_Staat)

# 10.3 data set 
BMO10.3_Geslacht <- Bias(MO10.3_Geslacht, Geslacht, "Geslacht")
BOMO10.3_Geslacht <- sum(BMO10.3_Geslacht$percentageABS)/nrow(BMO10.3_Geslacht)

BMO10.3_Leeftijd <- Bias(MO10.3_Leeftijd, Leeftijd, "Leeftijd")
BOMO10.3_Leeftijd <- sum(BMO10.3_Leeftijd$percentageABS)/nrow(BMO10.3_Leeftijd)

BMO10.3_HH_Pos <- Bias(MO10.3_HH_Pos, HH_Pos, "HH_Pos")
BOMO10.3_HH_Pos <- sum(BMO10.3_HH_Pos$percentageABS)/nrow(BMO10.3_HH_Pos)

BMO10.3_HH_grootte <- Bias(MO10.3_HH_grootte, HH_grootte, "HH_grootte")
BOMO10.3_HH_grootte <- sum(BMO10.3_HH_grootte$percentageABS)/nrow(BMO10.3_HH_grootte)

BMO10.3_Woonregio_vorig_jaar <- Bias(MO10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOMO10.3_Woonregio_vorig_jaar <- sum(BMO10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BMO10.3_Woonregio_vorig_jaar)

BMO10.3_Nationaliteit <- Bias(MO10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOMO10.3_Nationaliteit <- sum(BMO10.3_Nationaliteit$percentageABS)/nrow(BMO10.3_Nationaliteit)

BMO10.3_Geboorteland <- Bias(MO10.3_Geboorteland, Geboorteland, "Geboorteland")
BOMO10.3_Geboorteland <- sum(BMO10.3_Geboorteland$percentageABS)/nrow(BMO10.3_Geboorteland)

BMO10.3_Onderwijsniveau <- Bias(MO10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOMO10.3_Onderwijsniveau <- sum(BMO10.3_Onderwijsniveau$percentageABS)/nrow(BMO10.3_Onderwijsniveau)

BMO10.3_Econ_status <- Bias(MO10.3_Econ_status, Econ_status, "Econ._status")
BOMO10.3_Econ_status <- sum(BMO10.3_Econ_status$percentageABS)/nrow(BMO10.3_Econ_status)

BMO10.3_Beroep <- Bias(MO10.3_Beroep, Beroep, "Beroep")
BOMO10.3_Beroep <- sum(BMO10.3_Beroep$percentageABS)/nrow(BMO10.3_Beroep)

BMO10.3_SBI <- Bias(MO10.3_SBI, SBI, "SBI")
BOMO10.3_SBI <- sum(BMO10.3_SBI$percentageABS)/nrow(BMO10.3_SBI)

BMO10.3_Burg_Staat <- Bias(MO10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOMO10.3_Burg_Staat <- sum(BMO10.3_Burg_Staat$percentageABS)/nrow(BMO10.3_Burg_Staat)

BiasOverallMO10.3 <- sum(BOMO10.3_Geslacht, BOMO10.3_Leeftijd, BOMO10.3_HH_Pos, BOMO10.3_HH_grootte, BOMO10.3_Woonregio_vorig_jaar, 
                         BOMO10.3_Nationaliteit, BOMO10.3_Geboorteland, BOMO10.3_Onderwijsniveau, BOMO10.3_Econ_status, 
                         BOMO10.3_Beroep, BOMO10.3_SBI, BOMO10.3_Burg_Staat) / 12

rm(MO10.3_Geslacht, MO10.3_Leeftijd, MO10.3_HH_Pos, MO10.3_HH_grootte, MO10.3_Woonregio_vorig_jaar, 
   MO10.3_Nationaliteit, MO10.3_Geboorteland, MO10.3_Onderwijsniveau, MO10.3_Econ_status, 
   MO10.3_Beroep, MO10.3_SBI, MO10.3_Burg_Staat)
rm(BMO10.3_Geslacht, BMO10.3_Leeftijd, BMO10.3_HH_Pos, BMO10.3_HH_grootte, BMO10.3_Woonregio_vorig_jaar, 
   BMO10.3_Nationaliteit, BMO10.3_Geboorteland, BMO10.3_Onderwijsniveau, BMO10.3_Econ_status, 
   BMO10.3_Beroep, BMO10.3_SBI, BMO10.3_Burg_Staat)
rm(BOMO10.3_Geslacht, BOMO10.3_Leeftijd, BOMO10.3_HH_Pos, BOMO10.3_HH_grootte, BOMO10.3_Woonregio_vorig_jaar, 
   BOMO10.3_Nationaliteit, BOMO10.3_Geboorteland, BOMO10.3_Onderwijsniveau, BOMO10.3_Econ_status, 
   BOMO10.3_Beroep, BOMO10.3_SBI, BOMO10.3_Burg_Staat)