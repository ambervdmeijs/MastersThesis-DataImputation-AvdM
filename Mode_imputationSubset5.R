
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


## Loading subset to use ----------------------------------------------------------------------------------------------------

sub_ipums5 <- get(load(file = "sub_ipums5.Rdata"))

# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

# Setting correct and total to '0' 
S5MO2.1_correct <- S5MO2.2_correct <- S5MO2.3_correct <- S5MO5.1_correct <- S5MO5.2_correct <- S5MO5.3_correct <- S5MO10.1_correct <- S5MO10.2_correct <- S5MO10.3_correct <- 0
S5MO2.1_total <- S5MO2.2_total <- S5MO2.3_total <- S5MO5.1_total <- S5MO5.2_total <- S5MO5.3_total <- S5MO10.1_total <- S5MO10.2_total <- S5MO10.3_total <- 0


## Mode Imputation with 'ForImp' --------------------------------------------------------------------------------------------------!!!


# 2% data sets

# 2.1 data set
sub5_MCAR2.1 <- get(load(file = "sub5_MCAR21.Rdata"))
S5MO_MCAR2.1 <- data.frame(sub5_MCAR2.1)
names(S5MO_MCAR2.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 2.1 processing time...")
S5mode_imputation2.1 <- modeimp(S5MO_MCAR2.1)
toc(log = TRUE)
rm(S5MO_MCAR2.1)

S5df_mode_imputation2.1 <- as.data.frame(S5mode_imputation2.1)
save(S5df_mode_imputation2.1, file = "S5mode_imputation21.Rdata")
rm(S5mode_imputation2.1)
S5MO2.1_correct <- S5MO2.1_correct + sum(sub_ipums5 == S5df_mode_imputation2.1)
S5MO2.1_total <- S5MO2.1_total + sum(!is.na(S5df_mode_imputation2.1))

# 2.2 data set
sub5_MCAR2.2 <- get(load(file = "sub5_MCAR22.Rdata"))
S5MO_MCAR2.2 <- data.frame(sub5_MCAR2.2)
names(S5MO_MCAR2.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 2.2 processing time...")
S5mode_imputation2.2 <- modeimp(S5MO_MCAR2.2)
toc(log = TRUE)
rm(S5MO_MCAR2.2)

S5df_mode_imputation2.2 <- as.data.frame(S5mode_imputation2.2)
save(S5df_mode_imputation2.2, file = "S5mode_imputation22.Rdata")
rm(S5mode_imputation2.2)
S5MO2.2_correct <- S5MO2.2_correct + sum(sub_ipums5 == S5df_mode_imputation2.2)
S5MO2.2_total <- S5MO2.2_total + sum(!is.na(S5df_mode_imputation2.2))

# 2.3 data set
sub5_MCAR2.3 <- get(load(file = "sub5_MCAR23.Rdata"))
S5MO_MCAR2.3 <- data.frame(sub5_MCAR2.3)
names(S5MO_MCAR2.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 2.3 processing time...")
S5mode_imputation2.3 <- modeimp(S5MO_MCAR2.3)
toc(log = TRUE)
rm(S5MO_MCAR2.3)

S5df_mode_imputation2.3 <- as.data.frame(S5mode_imputation2.3)
save(S5df_mode_imputation2.3, file = "S5mode_imputation23.Rdata")
rm(S5mode_imputation2.3)
S5MO2.3_correct <- S5MO2.3_correct + sum(sub_ipums5 == S5df_mode_imputation2.3)
S5MO2.3_total <- S5MO2.3_total + sum(!is.na(S5df_mode_imputation2.3))


# 5% data sets

# 5.1 data set
sub5_MCAR5.1 <- get(load(file = "sub5_MCAR51.Rdata"))
S5MO_MCAR5.1 <- data.frame(sub5_MCAR5.1)
names(S5MO_MCAR5.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 5.1 processing time...")
S5mode_imputation5.1 <- modeimp(S5MO_MCAR5.1)
toc(log = TRUE)
rm(S5MO_MCAR5.1)

S5df_mode_imputation5.1 <- as.data.frame(S5mode_imputation5.1)
save(S5df_mode_imputation5.1, file = "S5mode_imputation51.Rdata")
rm(S5mode_imputation5.1)
S5MO5.1_correct <- S5MO5.1_correct + sum(sub_ipums5 == S5df_mode_imputation5.1)
S5MO5.1_total <- S5MO5.1_total + sum(!is.na(S5df_mode_imputation5.1))

# 5.2 data set
sub5_MCAR5.2 <- get(load(file = "sub5_MCAR52.Rdata"))
S5MO_MCAR5.2 <- data.frame(sub5_MCAR5.2)
names(S5MO_MCAR5.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 5.2 processing time...")
S5mode_imputation5.2 <- modeimp(S5MO_MCAR5.2)
toc(log = TRUE)
rm(S5MO_MCAR5.2)

S5df_mode_imputation5.2 <- as.data.frame(S5mode_imputation5.2)
save(S5df_mode_imputation5.2, file = "S5mode_imputation52.Rdata")
rm(S5mode_imputation5.2)
S5MO5.2_correct <- S5MO5.2_correct + sum(sub_ipums5 == S5df_mode_imputation5.2)
S5MO5.2_total <- S5MO5.2_total + sum(!is.na(S5df_mode_imputation5.2))

# 5.3 data set
sub5_MCAR5.3 <- get(load(file = "sub5_MCAR53.Rdata"))
S5MO_MCAR5.3 <- data.frame(sub5_MCAR5.3)
names(S5MO_MCAR5.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 5.3 processing time...")
S5mode_imputation5.3 <- modeimp(S5MO_MCAR5.3)
toc(log = TRUE)
rm(S5MO_MCAR5.3)

S5df_mode_imputation5.3 <- as.data.frame(S5mode_imputation5.3)
save(S5df_mode_imputation5.3, file = "S5mode_imputation53.Rdata")
rm(S5mode_imputation5.3)
S5MO5.3_correct <- S5MO5.3_correct + sum(sub_ipums5 == S5df_mode_imputation5.3)
S5MO5.3_total <- S5MO5.3_total + sum(!is.na(S5df_mode_imputation5.3))


# 10% data sets 

# 10.1 data set
sub5_MCAR10.1 <- get(load(file = "sub5_MCAR101.Rdata"))
S5MO_MCAR10.1 <- data.frame(sub5_MCAR10.1)
names(S5MO_MCAR10.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 10.1 processing time...")
S5mode_imputation10.1 <- modeimp(S5MO_MCAR10.1)
toc(log = TRUE)
rm(S5MO_MCAR10.1)

S5df_mode_imputation10.1 <- as.data.frame(S5mode_imputation10.1)
save(S5df_mode_imputation10.1, file = "S5mode_imputation101.Rdata")
rm(S5mode_imputation10.1)
S5MO10.1_correct <- S5MO10.1_correct + sum(sub_ipums5 == S5df_mode_imputation10.1)
S5MO10.1_total <- S5MO10.1_total + sum(!is.na(S5df_mode_imputation10.1))

# 10.2 data set
sub5_MCAR10.2 <- get(load(file = "sub5_MCAR102.Rdata"))
S5MO_MCAR10.2 <- data.frame(sub5_MCAR10.2)
names(S5MO_MCAR10.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 10.2 processing time...")
S5mode_imputation10.2 <- modeimp(S5MO_MCAR10.2)
toc(log = TRUE)
rm(S5MO_MCAR10.2)

S5df_mode_imputation10.2 <- as.data.frame(S5mode_imputation10.2)
save(S5df_mode_imputation10.2, file = "S5mode_imputation102.Rdata")
rm(S5mode_imputation10.2)
S5MO10.2_correct <- S5MO10.2_correct + sum(sub_ipums5 == S5df_mode_imputation10.2)
S5MO10.2_total <- S5MO10.2_total + sum(!is.na(S5df_mode_imputation10.2))

# 10.3 data set
sub5_MCAR10.3 <- get(load(file = "sub5_MCAR103.Rdata"))
S5MO_MCAR10.3 <- data.frame(sub5_MCAR10.3)
names(S5MO_MCAR10.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("Mode Imputation 10.3 processing time...")
S5mode_imputation10.3 <- modeimp(S5MO_MCAR10.3)
toc(log = TRUE)
rm(S5MO_MCAR10.3)

S5df_mode_imputation10.3 <- as.data.frame(S5mode_imputation10.3)
save(S5df_mode_imputation10.3, file = "S5mode_imputation103.Rdata")
rm(S5mode_imputation10.3)
S5MO10.3_correct <- S5MO10.3_correct + sum(sub_ipums5 == S5df_mode_imputation10.3)
S5MO10.3_total <- S5MO10.3_total + sum(!is.na(S5df_mode_imputation10.3))



# Check if all values are imputed 
#anyNA(c(df_mode_imputation2.1, df_mode_imputation2.2, df_mode_imputation2.3, df_mode_imputation5.1, df_mode_imputation5.2, 
       # df_mode_imputation5.3, df_mode_imputation10.1, df_mode_imputation10.2, df_mode_imputation10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Computing the accuracy of imputation 
S5MO2.1_accuracy <- S5MO2.1_correct / S5MO2.1_total
S5MO2.2_accuracy <- S5MO2.2_correct / S5MO2.2_total
S5MO2.3_accuracy <- S5MO2.3_correct / S5MO2.3_total

S5MO5.1_accuracy <- S5MO5.1_correct / S5MO5.1_total
S5MO5.2_accuracy <- S5MO5.2_correct / S5MO5.2_total
S5MO5.3_accuracy <- S5MO5.3_correct / S5MO5.3_total

S5MO10.1_accuracy <- S5MO10.1_correct / S5MO10.1_total
S5MO10.2_accuracy <- S5MO10.2_correct / S5MO10.2_total
S5MO10.3_accuracy <- S5MO10.3_correct / S5MO10.3_total




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
  Bdf$freq.x[is.na(Bdf$freq.x)] <- 0 
  Bdf$result <- Bdf["freq.x"] - Bdf["freq.y"]
  Bdf$percentage <- (Bdf["result"]/Bdf["freq.y"])*100
  Bdf$percentageABS <- abs(Bdf$percentage)
  return(Bdf)
}


# 2% data set

# Counting values
S5MO2.1_Geslacht <- plyr::count(S5df_mode_imputation2.1, 'Geslacht')
S5MO2.1_Leeftijd <- plyr::count(S5df_mode_imputation2.1, 'Leeftijd')
S5MO2.1_HH_Pos <- plyr::count(S5df_mode_imputation2.1, 'HH_Pos')
S5MO2.1_HH_grootte <- plyr::count(S5df_mode_imputation2.1, 'HH_grootte')
S5MO2.1_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation2.1, 'Woonregio_vorig_jaar')
S5MO2.1_Nationaliteit <- plyr::count(S5df_mode_imputation2.1, 'Nationaliteit')
S5MO2.1_Geboorteland <- plyr::count(S5df_mode_imputation2.1, 'Geboorteland')
S5MO2.1_Onderwijsniveau <- plyr::count(S5df_mode_imputation2.1, 'Onderwijsniveau')
S5MO2.1_Econ_status <- plyr::count(S5df_mode_imputation2.1, 'Econ._status')
S5MO2.1_Beroep <- plyr::count(S5df_mode_imputation2.1, 'Beroep')
S5MO2.1_SBI <- plyr::count(S5df_mode_imputation2.1, 'SBI')
S5MO2.1_Burg_Staat <- plyr::count(S5df_mode_imputation2.1, 'Burg._Staat')

S5MO2.2_Geslacht <- plyr::count(S5df_mode_imputation2.2, 'Geslacht')
S5MO2.2_Leeftijd <- plyr::count(S5df_mode_imputation2.2, 'Leeftijd')
S5MO2.2_HH_Pos <- plyr::count(S5df_mode_imputation2.2, 'HH_Pos')
S5MO2.2_HH_grootte <- plyr::count(S5df_mode_imputation2.2, 'HH_grootte')
S5MO2.2_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation2.2, 'Woonregio_vorig_jaar')
S5MO2.2_Nationaliteit <- plyr::count(S5df_mode_imputation2.2, 'Nationaliteit')
S5MO2.2_Geboorteland <- plyr::count(S5df_mode_imputation2.2, 'Geboorteland')
S5MO2.2_Onderwijsniveau <- plyr::count(S5df_mode_imputation2.2, 'Onderwijsniveau')
S5MO2.2_Econ_status <- plyr::count(S5df_mode_imputation2.2, 'Econ._status')
S5MO2.2_Beroep <- plyr::count(S5df_mode_imputation2.2, 'Beroep')
S5MO2.2_SBI <- plyr::count(S5df_mode_imputation2.2, 'SBI')
S5MO2.2_Burg_Staat <- plyr::count(S5df_mode_imputation2.2, 'Burg._Staat')

S5MO2.3_Geslacht <- plyr::count(S5df_mode_imputation2.3, 'Geslacht')
S5MO2.3_Leeftijd <- plyr::count(S5df_mode_imputation2.3, 'Leeftijd')
S5MO2.3_HH_Pos <- plyr::count(S5df_mode_imputation2.3, 'HH_Pos')
S5MO2.3_HH_grootte <- plyr::count(S5df_mode_imputation2.3, 'HH_grootte')
S5MO2.3_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation2.3, 'Woonregio_vorig_jaar')
S5MO2.3_Nationaliteit <- plyr::count(S5df_mode_imputation2.3, 'Nationaliteit')
S5MO2.3_Geboorteland <- plyr::count(S5df_mode_imputation2.3, 'Geboorteland')
S5MO2.3_Onderwijsniveau <- plyr::count(S5df_mode_imputation2.3, 'Onderwijsniveau')
S5MO2.3_Econ_status <- plyr::count(S5df_mode_imputation2.3, 'Econ._status')
S5MO2.3_Beroep <- plyr::count(S5df_mode_imputation2.3, 'Beroep')
S5MO2.3_SBI <- plyr::count(S5df_mode_imputation2.3, 'SBI')
S5MO2.3_Burg_Staat <- plyr::count(S5df_mode_imputation2.3, 'Burg._Staat')

rm(S5df_mode_imputation2.1, S5df_mode_imputation2.2, S5df_mode_imputation2.3)

# 2.1 data set 
BS5MO2.1_Geslacht <- Bias(S5MO2.1_Geslacht, Geslacht, "Geslacht")
BOS5MO2.1_Geslacht <- sum(BS5MO2.1_Geslacht$percentageABS)/nrow(BS5MO2.1_Geslacht)

BS5MO2.1_Leeftijd <- Bias(S5MO2.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO2.1_Leeftijd <- sum(BS5MO2.1_Leeftijd$percentageABS)/nrow(BS5MO2.1_Leeftijd)

BS5MO2.1_HH_Pos <- Bias(S5MO2.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO2.1_HH_Pos <- sum(BS5MO2.1_HH_Pos$percentageABS)/nrow(BS5MO2.1_HH_Pos)

BS5MO2.1_HH_grootte <- Bias(S5MO2.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO2.1_HH_grootte <- sum(BS5MO2.1_HH_grootte$percentageABS)/nrow(BS5MO2.1_HH_grootte)

BS5MO2.1_Woonregio_vorig_jaar <- Bias(S5MO2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO2.1_Woonregio_vorig_jaar <- sum(BS5MO2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO2.1_Woonregio_vorig_jaar)

BS5MO2.1_Nationaliteit <- Bias(S5MO2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO2.1_Nationaliteit <- sum(BS5MO2.1_Nationaliteit$percentageABS)/nrow(BS5MO2.1_Nationaliteit)

BS5MO2.1_Geboorteland <- Bias(S5MO2.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO2.1_Geboorteland <- sum(BS5MO2.1_Geboorteland$percentageABS)/nrow(BS5MO2.1_Geboorteland)

BS5MO2.1_Onderwijsniveau <- Bias(S5MO2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO2.1_Onderwijsniveau <- sum(BS5MO2.1_Onderwijsniveau$percentageABS)/nrow(BS5MO2.1_Onderwijsniveau)

BS5MO2.1_Econ_status <- Bias(S5MO2.1_Econ_status, Econ_status, "Econ._status")
BOS5MO2.1_Econ_status <- sum(BS5MO2.1_Econ_status$percentageABS)/nrow(BS5MO2.1_Econ_status)

BS5MO2.1_Beroep <- Bias(S5MO2.1_Beroep, Beroep, "Beroep")
BOS5MO2.1_Beroep <- sum(BS5MO2.1_Beroep$percentageABS)/nrow(BS5MO2.1_Beroep)

BS5MO2.1_SBI <- Bias(S5MO2.1_SBI, SBI, "SBI")
BOS5MO2.1_SBI <- sum(BS5MO2.1_SBI$percentageABS)/nrow(BS5MO2.1_SBI)

BS5MO2.1_Burg_Staat <- Bias(S5MO2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO2.1_Burg_Staat <- sum(BS5MO2.1_Burg_Staat$percentageABS)/nrow(BS5MO2.1_Burg_Staat)

BiasOverallS5MO2.1 <- sum(BOS5MO2.1_Geslacht, BOS5MO2.1_Leeftijd, BOS5MO2.1_HH_Pos, BOS5MO2.1_HH_grootte, BOS5MO2.1_Woonregio_vorig_jaar, 
                        BOS5MO2.1_Nationaliteit, BOS5MO2.1_Geboorteland, BOS5MO2.1_Onderwijsniveau, BOS5MO2.1_Econ_status, 
                        BOS5MO2.1_Beroep, BOS5MO2.1_SBI, BOS5MO2.1_Burg_Staat) / 12

rm(S5MO2.1_Geslacht, S5MO2.1_Leeftijd, S5MO2.1_HH_Pos, S5MO2.1_HH_grootte, S5MO2.1_Woonregio_vorig_jaar, 
   S5MO2.1_Nationaliteit, S5MO2.1_Geboorteland, S5MO2.1_Onderwijsniveau, S5MO2.1_Econ_status, 
   S5MO2.1_Beroep, S5MO2.1_SBI, S5MO2.1_Burg_Staat)
rm(BS5MO2.1_Geslacht, BS5MO2.1_Leeftijd, BS5MO2.1_HH_Pos, BS5MO2.1_HH_grootte, BS5MO2.1_Woonregio_vorig_jaar, 
   BS5MO2.1_Nationaliteit, BS5MO2.1_Geboorteland, BS5MO2.1_Onderwijsniveau, BS5MO2.1_Econ_status, 
   BS5MO2.1_Beroep, BS5MO2.1_SBI, BS5MO2.1_Burg_Staat)
rm(BOS5MO2.1_Geslacht, BOS5MO2.1_Leeftijd, BOS5MO2.1_HH_Pos, BOS5MO2.1_HH_grootte, BOS5MO2.1_Woonregio_vorig_jaar, 
   BOS5MO2.1_Nationaliteit, BOS5MO2.1_Geboorteland, BOS5MO2.1_Onderwijsniveau, BOS5MO2.1_Econ_status, 
   BOS5MO2.1_Beroep, BOS5MO2.1_SBI, BOS5MO2.1_Burg_Staat)

# 2.2 data set 
BS5MO2.2_Geslacht <- Bias(S5MO2.2_Geslacht, Geslacht, "Geslacht")
BOS5MO2.2_Geslacht <- sum(BS5MO2.2_Geslacht$percentageABS)/nrow(BS5MO2.2_Geslacht)

BS5MO2.2_Leeftijd <- Bias(S5MO2.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO2.2_Leeftijd <- sum(BS5MO2.2_Leeftijd$percentageABS)/nrow(BS5MO2.2_Leeftijd)

BS5MO2.2_HH_Pos <- Bias(S5MO2.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO2.2_HH_Pos <- sum(BS5MO2.2_HH_Pos$percentageABS)/nrow(BS5MO2.2_HH_Pos)

BS5MO2.2_HH_grootte <- Bias(S5MO2.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO2.2_HH_grootte <- sum(BS5MO2.2_HH_grootte$percentageABS)/nrow(BS5MO2.2_HH_grootte)

BS5MO2.2_Woonregio_vorig_jaar <- Bias(S5MO2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO2.2_Woonregio_vorig_jaar <- sum(BS5MO2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO2.2_Woonregio_vorig_jaar)

BS5MO2.2_Nationaliteit <- Bias(S5MO2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO2.2_Nationaliteit <- sum(BS5MO2.2_Nationaliteit$percentageABS)/nrow(BS5MO2.2_Nationaliteit)

BS5MO2.2_Geboorteland <- Bias(S5MO2.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO2.2_Geboorteland <- sum(BS5MO2.2_Geboorteland$percentageABS)/nrow(BS5MO2.2_Geboorteland)

BS5MO2.2_Onderwijsniveau <- Bias(S5MO2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO2.2_Onderwijsniveau <- sum(BS5MO2.2_Onderwijsniveau$percentageABS)/nrow(BS5MO2.2_Onderwijsniveau)

BS5MO2.2_Econ_status <- Bias(S5MO2.2_Econ_status, Econ_status, "Econ._status")
BOS5MO2.2_Econ_status <- sum(BS5MO2.2_Econ_status$percentageABS)/nrow(BS5MO2.2_Econ_status)

BS5MO2.2_Beroep <- Bias(S5MO2.2_Beroep, Beroep, "Beroep")
BOS5MO2.2_Beroep <- sum(BS5MO2.2_Beroep$percentageABS)/nrow(BS5MO2.2_Beroep)

BS5MO2.2_SBI <- Bias(S5MO2.2_SBI, SBI, "SBI")
BOS5MO2.2_SBI <- sum(BS5MO2.2_SBI$percentageABS)/nrow(BS5MO2.2_SBI)

BS5MO2.2_Burg_Staat <- Bias(S5MO2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO2.2_Burg_Staat <- sum(BS5MO2.2_Burg_Staat$percentageABS)/nrow(BS5MO2.2_Burg_Staat)

BiasOverallS5MO2.2 <- sum(BOS5MO2.2_Geslacht, BOS5MO2.2_Leeftijd, BOS5MO2.2_HH_Pos, BOS5MO2.2_HH_grootte, BOS5MO2.2_Woonregio_vorig_jaar, 
                        BOS5MO2.2_Nationaliteit, BOS5MO2.2_Geboorteland, BOS5MO2.2_Onderwijsniveau, BOS5MO2.2_Econ_status, 
                        BOS5MO2.2_Beroep, BOS5MO2.2_SBI, BOS5MO2.2_Burg_Staat) / 12

rm(S5MO2.2_Geslacht, S5MO2.2_Leeftijd, S5MO2.2_HH_Pos, S5MO2.2_HH_grootte, S5MO2.2_Woonregio_vorig_jaar, 
   S5MO2.2_Nationaliteit, S5MO2.2_Geboorteland, S5MO2.2_Onderwijsniveau, S5MO2.2_Econ_status, 
   S5MO2.2_Beroep, S5MO2.2_SBI, S5MO2.2_Burg_Staat)
rm(BS5MO2.2_Geslacht, BS5MO2.2_Leeftijd, BS5MO2.2_HH_Pos, BS5MO2.2_HH_grootte, BS5MO2.2_Woonregio_vorig_jaar, 
   BS5MO2.2_Nationaliteit, BS5MO2.2_Geboorteland, BS5MO2.2_Onderwijsniveau, BS5MO2.2_Econ_status, 
   BS5MO2.2_Beroep, BS5MO2.2_SBI, BS5MO2.2_Burg_Staat)
rm(BOS5MO2.2_Geslacht, BOS5MO2.2_Leeftijd, BOS5MO2.2_HH_Pos, BOS5MO2.2_HH_grootte, BOS5MO2.2_Woonregio_vorig_jaar, 
   BOS5MO2.2_Nationaliteit, BOS5MO2.2_Geboorteland, BOS5MO2.2_Onderwijsniveau, BOS5MO2.2_Econ_status, 
   BOS5MO2.2_Beroep, BOS5MO2.2_SBI, BOS5MO2.2_Burg_Staat)

# 2.3 data set 
BS5MO2.3_Geslacht <- Bias(S5MO2.3_Geslacht, Geslacht, "Geslacht")
BOS5MO2.3_Geslacht <- sum(BS5MO2.3_Geslacht$percentageABS)/nrow(BS5MO2.3_Geslacht)

BS5MO2.3_Leeftijd <- Bias(S5MO2.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO2.3_Leeftijd <- sum(BS5MO2.3_Leeftijd$percentageABS)/nrow(BS5MO2.3_Leeftijd)

BS5MO2.3_HH_Pos <- Bias(S5MO2.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO2.3_HH_Pos <- sum(BS5MO2.3_HH_Pos$percentageABS)/nrow(BS5MO2.3_HH_Pos)

BS5MO2.3_HH_grootte <- Bias(S5MO2.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO2.3_HH_grootte <- sum(BS5MO2.3_HH_grootte$percentageABS)/nrow(BS5MO2.3_HH_grootte)

BS5MO2.3_Woonregio_vorig_jaar <- Bias(S5MO2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO2.3_Woonregio_vorig_jaar <- sum(BS5MO2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO2.3_Woonregio_vorig_jaar)

BS5MO2.3_Nationaliteit <- Bias(S5MO2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO2.3_Nationaliteit <- sum(BS5MO2.3_Nationaliteit$percentageABS)/nrow(BS5MO2.3_Nationaliteit)

BS5MO2.3_Geboorteland <- Bias(S5MO2.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO2.3_Geboorteland <- sum(BS5MO2.3_Geboorteland$percentageABS)/nrow(BS5MO2.3_Geboorteland)

BS5MO2.3_Onderwijsniveau <- Bias(S5MO2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO2.3_Onderwijsniveau <- sum(BS5MO2.3_Onderwijsniveau$percentageABS)/nrow(BS5MO2.3_Onderwijsniveau)

BS5MO2.3_Econ_status <- Bias(S5MO2.3_Econ_status, Econ_status, "Econ._status")
BOS5MO2.3_Econ_status <- sum(BS5MO2.3_Econ_status$percentageABS)/nrow(BS5MO2.3_Econ_status)

BS5MO2.3_Beroep <- Bias(S5MO2.3_Beroep, Beroep, "Beroep")
BOS5MO2.3_Beroep <- sum(BS5MO2.3_Beroep$percentageABS)/nrow(BS5MO2.3_Beroep)

BS5MO2.3_SBI <- Bias(S5MO2.3_SBI, SBI, "SBI")
BOS5MO2.3_SBI <- sum(BS5MO2.3_SBI$percentageABS)/nrow(BS5MO2.3_SBI)

BS5MO2.3_Burg_Staat <- Bias(S5MO2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO2.3_Burg_Staat <- sum(BS5MO2.3_Burg_Staat$percentageABS)/nrow(BS5MO2.3_Burg_Staat)

BiasOverallS5MO2.3 <- sum(BOS5MO2.3_Geslacht, BOS5MO2.3_Leeftijd, BOS5MO2.3_HH_Pos, BOS5MO2.3_HH_grootte, BOS5MO2.3_Woonregio_vorig_jaar, 
                        BOS5MO2.3_Nationaliteit, BOS5MO2.3_Geboorteland, BOS5MO2.3_Onderwijsniveau, BOS5MO2.3_Econ_status, 
                        BOS5MO2.3_Beroep, BOS5MO2.3_SBI, BOS5MO2.3_Burg_Staat) / 12

rm(S5MO2.3_Geslacht, S5MO2.3_Leeftijd, S5MO2.3_HH_Pos, S5MO2.3_HH_grootte, S5MO2.3_Woonregio_vorig_jaar, 
   S5MO2.3_Nationaliteit, S5MO2.3_Geboorteland, S5MO2.3_Onderwijsniveau, S5MO2.3_Econ_status, 
   S5MO2.3_Beroep, S5MO2.3_SBI, S5MO2.3_Burg_Staat)
rm(BS5MO2.3_Geslacht, BS5MO2.3_Leeftijd, BS5MO2.3_HH_Pos, BS5MO2.3_HH_grootte, BS5MO2.3_Woonregio_vorig_jaar, 
   BS5MO2.3_Nationaliteit, BS5MO2.3_Geboorteland, BS5MO2.3_Onderwijsniveau, BS5MO2.3_Econ_status, 
   BS5MO2.3_Beroep, BS5MO2.3_SBI, BS5MO2.3_Burg_Staat)
rm(BOS5MO2.3_Geslacht, BOS5MO2.3_Leeftijd, BOS5MO2.3_HH_Pos, BOS5MO2.3_HH_grootte, BOS5MO2.3_Woonregio_vorig_jaar, 
   BOS5MO2.3_Nationaliteit, BOS5MO2.3_Geboorteland, BOS5MO2.3_Onderwijsniveau, BOS5MO2.3_Econ_status, 
   BOS5MO2.3_Beroep, BOS5MO2.3_SBI, BOS5MO2.3_Burg_Staat)


# 5% data sets

# Counting values
S5MO5.1_Geslacht <- plyr::count(S5df_mode_imputation5.1, 'Geslacht')
S5MO5.1_Leeftijd <- plyr::count(S5df_mode_imputation5.1, 'Leeftijd')
S5MO5.1_HH_Pos <- plyr::count(S5df_mode_imputation5.1, 'HH_Pos')
S5MO5.1_HH_grootte <- plyr::count(S5df_mode_imputation5.1, 'HH_grootte')
S5MO5.1_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation5.1, 'Woonregio_vorig_jaar')
S5MO5.1_Nationaliteit <- plyr::count(S5df_mode_imputation5.1, 'Nationaliteit')
S5MO5.1_Geboorteland <- plyr::count(S5df_mode_imputation5.1, 'Geboorteland')
S5MO5.1_Onderwijsniveau <- plyr::count(S5df_mode_imputation5.1, 'Onderwijsniveau')
S5MO5.1_Econ_status <- plyr::count(S5df_mode_imputation5.1, 'Econ._status')
S5MO5.1_Beroep <- plyr::count(S5df_mode_imputation5.1, 'Beroep')
S5MO5.1_SBI <- plyr::count(S5df_mode_imputation5.1, 'SBI')
S5MO5.1_Burg_Staat <- plyr::count(S5df_mode_imputation5.1, 'Burg._Staat')

S5MO5.2_Geslacht <- plyr::count(S5df_mode_imputation5.2, 'Geslacht')
S5MO5.2_Leeftijd <- plyr::count(S5df_mode_imputation5.2, 'Leeftijd')
S5MO5.2_HH_Pos <- plyr::count(S5df_mode_imputation5.2, 'HH_Pos')
S5MO5.2_HH_grootte <- plyr::count(S5df_mode_imputation5.2, 'HH_grootte')
S5MO5.2_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation5.2, 'Woonregio_vorig_jaar')
S5MO5.2_Nationaliteit <- plyr::count(S5df_mode_imputation5.2, 'Nationaliteit')
S5MO5.2_Geboorteland <- plyr::count(S5df_mode_imputation5.2, 'Geboorteland')
S5MO5.2_Onderwijsniveau <- plyr::count(S5df_mode_imputation5.2, 'Onderwijsniveau')
S5MO5.2_Econ_status <- plyr::count(S5df_mode_imputation5.2, 'Econ._status')
S5MO5.2_Beroep <- plyr::count(S5df_mode_imputation5.2, 'Beroep')
S5MO5.2_SBI <- plyr::count(S5df_mode_imputation5.2, 'SBI')
S5MO5.2_Burg_Staat <- plyr::count(S5df_mode_imputation5.2, 'Burg._Staat')

S5MO5.3_Geslacht <- plyr::count(S5df_mode_imputation5.3, 'Geslacht')
S5MO5.3_Leeftijd <- plyr::count(S5df_mode_imputation5.3, 'Leeftijd')
S5MO5.3_HH_Pos <- plyr::count(S5df_mode_imputation5.3, 'HH_Pos')
S5MO5.3_HH_grootte <- plyr::count(S5df_mode_imputation5.3, 'HH_grootte')
S5MO5.3_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation5.3, 'Woonregio_vorig_jaar')
S5MO5.3_Nationaliteit <- plyr::count(S5df_mode_imputation5.3, 'Nationaliteit')
S5MO5.3_Geboorteland <- plyr::count(S5df_mode_imputation5.3, 'Geboorteland')
S5MO5.3_Onderwijsniveau <- plyr::count(S5df_mode_imputation5.3, 'Onderwijsniveau')
S5MO5.3_Econ_status <- plyr::count(S5df_mode_imputation5.3, 'Econ._status')
S5MO5.3_Beroep <- plyr::count(S5df_mode_imputation5.3, 'Beroep')
S5MO5.3_SBI <- plyr::count(S5df_mode_imputation5.3, 'SBI')
S5MO5.3_Burg_Staat <- plyr::count(S5df_mode_imputation5.3, 'Burg._Staat')

rm(S5df_mode_imputation5.1, S5df_mode_imputation5.2, S5df_mode_imputation5.3)


# 5.1 data set 
BS5MO5.1_Geslacht <- Bias(S5MO5.1_Geslacht, Geslacht, "Geslacht")
BOS5MO5.1_Geslacht <- sum(BS5MO5.1_Geslacht$percentageABS)/nrow(BS5MO5.1_Geslacht)

BS5MO5.1_Leeftijd <- Bias(S5MO5.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO5.1_Leeftijd <- sum(BS5MO5.1_Leeftijd$percentageABS)/nrow(BS5MO5.1_Leeftijd)

BS5MO5.1_HH_Pos <- Bias(S5MO5.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO5.1_HH_Pos <- sum(BS5MO5.1_HH_Pos$percentageABS)/nrow(BS5MO5.1_HH_Pos)

BS5MO5.1_HH_grootte <- Bias(S5MO5.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO5.1_HH_grootte <- sum(BS5MO5.1_HH_grootte$percentageABS)/nrow(BS5MO5.1_HH_grootte)

BS5MO5.1_Woonregio_vorig_jaar <- Bias(S5MO5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO5.1_Woonregio_vorig_jaar <- sum(BS5MO5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO5.1_Woonregio_vorig_jaar)

BS5MO5.1_Nationaliteit <- Bias(S5MO5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO5.1_Nationaliteit <- sum(BS5MO5.1_Nationaliteit$percentageABS)/nrow(BS5MO5.1_Nationaliteit)

BS5MO5.1_Geboorteland <- Bias(S5MO5.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO5.1_Geboorteland <- sum(BS5MO5.1_Geboorteland$percentageABS)/nrow(BS5MO5.1_Geboorteland)

BS5MO5.1_Onderwijsniveau <- Bias(S5MO5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO5.1_Onderwijsniveau <- sum(BS5MO5.1_Onderwijsniveau$percentageABS)/nrow(BS5MO5.1_Onderwijsniveau)

BS5MO5.1_Econ_status <- Bias(S5MO5.1_Econ_status, Econ_status, "Econ._status")
BOS5MO5.1_Econ_status <- sum(BS5MO5.1_Econ_status$percentageABS)/nrow(BS5MO5.1_Econ_status)

BS5MO5.1_Beroep <- Bias(S5MO5.1_Beroep, Beroep, "Beroep")
BOS5MO5.1_Beroep <- sum(BS5MO5.1_Beroep$percentageABS)/nrow(BS5MO5.1_Beroep)

BS5MO5.1_SBI <- Bias(S5MO5.1_SBI, SBI, "SBI")
BOS5MO5.1_SBI <- sum(BS5MO5.1_SBI$percentageABS)/nrow(BS5MO5.1_SBI)

BS5MO5.1_Burg_Staat <- Bias(S5MO5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO5.1_Burg_Staat <- sum(BS5MO5.1_Burg_Staat$percentageABS)/nrow(BS5MO5.1_Burg_Staat)

BiasOverallS5MO5.1 <- sum(BOS5MO5.1_Geslacht, BOS5MO5.1_Leeftijd, BOS5MO5.1_HH_Pos, BOS5MO5.1_HH_grootte, BOS5MO5.1_Woonregio_vorig_jaar, 
                        BOS5MO5.1_Nationaliteit, BOS5MO5.1_Geboorteland, BOS5MO5.1_Onderwijsniveau, BOS5MO5.1_Econ_status, 
                        BOS5MO5.1_Beroep, BOS5MO5.1_SBI, BOS5MO5.1_Burg_Staat) / 12

rm(S5MO5.1_Geslacht, S5MO5.1_Leeftijd, S5MO5.1_HH_Pos, S5MO5.1_HH_grootte, S5MO5.1_Woonregio_vorig_jaar, 
   S5MO5.1_Nationaliteit, S5MO5.1_Geboorteland, S5MO5.1_Onderwijsniveau, S5MO5.1_Econ_status, 
   S5MO5.1_Beroep, S5MO5.1_SBI, S5MO5.1_Burg_Staat)
rm(BS5MO5.1_Geslacht, BS5MO5.1_Leeftijd, BS5MO5.1_HH_Pos, BS5MO5.1_HH_grootte, BS5MO5.1_Woonregio_vorig_jaar, 
   BS5MO5.1_Nationaliteit, BS5MO5.1_Geboorteland, BS5MO5.1_Onderwijsniveau, BS5MO5.1_Econ_status, 
   BS5MO5.1_Beroep, BS5MO5.1_SBI, BS5MO5.1_Burg_Staat)
rm(BOS5MO5.1_Geslacht, BOS5MO5.1_Leeftijd, BOS5MO5.1_HH_Pos, BOS5MO5.1_HH_grootte, BOS5MO5.1_Woonregio_vorig_jaar, 
   BOS5MO5.1_Nationaliteit, BOS5MO5.1_Geboorteland, BOS5MO5.1_Onderwijsniveau, BOS5MO5.1_Econ_status, 
   BOS5MO5.1_Beroep, BOS5MO5.1_SBI, BOS5MO5.1_Burg_Staat)

# 5.2 data set 
BS5MO5.2_Geslacht <- Bias(S5MO5.2_Geslacht, Geslacht, "Geslacht")
BOS5MO5.2_Geslacht <- sum(BS5MO5.2_Geslacht$percentageABS)/nrow(BS5MO5.2_Geslacht)

BS5MO5.2_Leeftijd <- Bias(S5MO5.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO5.2_Leeftijd <- sum(BS5MO5.2_Leeftijd$percentageABS)/nrow(BS5MO5.2_Leeftijd)

BS5MO5.2_HH_Pos <- Bias(S5MO5.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO5.2_HH_Pos <- sum(BS5MO5.2_HH_Pos$percentageABS)/nrow(BS5MO5.2_HH_Pos)

BS5MO5.2_HH_grootte <- Bias(S5MO5.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO5.2_HH_grootte <- sum(BS5MO5.2_HH_grootte$percentageABS)/nrow(BS5MO5.2_HH_grootte)

BS5MO5.2_Woonregio_vorig_jaar <- Bias(S5MO5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO5.2_Woonregio_vorig_jaar <- sum(BS5MO5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO5.2_Woonregio_vorig_jaar)

BS5MO5.2_Nationaliteit <- Bias(S5MO5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO5.2_Nationaliteit <- sum(BS5MO5.2_Nationaliteit$percentageABS)/nrow(BS5MO5.2_Nationaliteit)

BS5MO5.2_Geboorteland <- Bias(S5MO5.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO5.2_Geboorteland <- sum(BS5MO5.2_Geboorteland$percentageABS)/nrow(BS5MO5.2_Geboorteland)

BS5MO5.2_Onderwijsniveau <- Bias(S5MO5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO5.2_Onderwijsniveau <- sum(BS5MO5.2_Onderwijsniveau$percentageABS)/nrow(BS5MO5.2_Onderwijsniveau)

BS5MO5.2_Econ_status <- Bias(S5MO5.2_Econ_status, Econ_status, "Econ._status")
BOS5MO5.2_Econ_status <- sum(BS5MO5.2_Econ_status$percentageABS)/nrow(BS5MO5.2_Econ_status)

BS5MO5.2_Beroep <- Bias(S5MO5.2_Beroep, Beroep, "Beroep")
BOS5MO5.2_Beroep <- sum(BS5MO5.2_Beroep$percentageABS)/nrow(BS5MO5.2_Beroep)

BS5MO5.2_SBI <- Bias(S5MO5.2_SBI, SBI, "SBI")
BOS5MO5.2_SBI <- sum(BS5MO5.2_SBI$percentageABS)/nrow(BS5MO5.2_SBI)

BS5MO5.2_Burg_Staat <- Bias(S5MO5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO5.2_Burg_Staat <- sum(BS5MO5.2_Burg_Staat$percentageABS)/nrow(BS5MO5.2_Burg_Staat)

BiasOverallS5MO5.2 <- sum(BOS5MO5.2_Geslacht, BOS5MO5.2_Leeftijd, BOS5MO5.2_HH_Pos, BOS5MO5.2_HH_grootte, BOS5MO5.2_Woonregio_vorig_jaar, 
                        BOS5MO5.2_Nationaliteit, BOS5MO5.2_Geboorteland, BOS5MO5.2_Onderwijsniveau, BOS5MO5.2_Econ_status, 
                        BOS5MO5.2_Beroep, BOS5MO5.2_SBI, BOS5MO5.2_Burg_Staat) / 12

rm(S5MO5.2_Geslacht, S5MO5.2_Leeftijd, S5MO5.2_HH_Pos, S5MO5.2_HH_grootte, S5MO5.2_Woonregio_vorig_jaar, 
   S5MO5.2_Nationaliteit, S5MO5.2_Geboorteland, S5MO5.2_Onderwijsniveau, S5MO5.2_Econ_status, 
   S5MO5.2_Beroep, S5MO5.2_SBI, S5MO5.2_Burg_Staat)
rm(BS5MO5.2_Geslacht, BS5MO5.2_Leeftijd, BS5MO5.2_HH_Pos, BS5MO5.2_HH_grootte, BS5MO5.2_Woonregio_vorig_jaar, 
   BS5MO5.2_Nationaliteit, BS5MO5.2_Geboorteland, BS5MO5.2_Onderwijsniveau, BS5MO5.2_Econ_status, 
   BS5MO5.2_Beroep, BS5MO5.2_SBI, BS5MO5.2_Burg_Staat)
rm(BOS5MO5.2_Geslacht, BOS5MO5.2_Leeftijd, BOS5MO5.2_HH_Pos, BOS5MO5.2_HH_grootte, BOS5MO5.2_Woonregio_vorig_jaar, 
   BOS5MO5.2_Nationaliteit, BOS5MO5.2_Geboorteland, BOS5MO5.2_Onderwijsniveau, BOS5MO5.2_Econ_status, 
   BOS5MO5.2_Beroep, BOS5MO5.2_SBI, BOS5MO5.2_Burg_Staat)

# 5.3 data set 
BS5MO5.3_Geslacht <- Bias(S5MO5.3_Geslacht, Geslacht, "Geslacht")
BOS5MO5.3_Geslacht <- sum(BS5MO5.3_Geslacht$percentageABS)/nrow(BS5MO5.3_Geslacht)

BS5MO5.3_Leeftijd <- Bias(S5MO5.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO5.3_Leeftijd <- sum(BS5MO5.3_Leeftijd$percentageABS)/nrow(BS5MO5.3_Leeftijd)

BS5MO5.3_HH_Pos <- Bias(S5MO5.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO5.3_HH_Pos <- sum(BS5MO5.3_HH_Pos$percentageABS)/nrow(BS5MO5.3_HH_Pos)

BS5MO5.3_HH_grootte <- Bias(S5MO5.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO5.3_HH_grootte <- sum(BS5MO5.3_HH_grootte$percentageABS)/nrow(BS5MO5.3_HH_grootte)

BS5MO5.3_Woonregio_vorig_jaar <- Bias(S5MO5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO5.3_Woonregio_vorig_jaar <- sum(BS5MO5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO5.3_Woonregio_vorig_jaar)

BS5MO5.3_Nationaliteit <- Bias(S5MO5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO5.3_Nationaliteit <- sum(BS5MO5.3_Nationaliteit$percentageABS)/nrow(BS5MO5.3_Nationaliteit)

BS5MO5.3_Geboorteland <- Bias(S5MO5.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO5.3_Geboorteland <- sum(BS5MO5.3_Geboorteland$percentageABS)/nrow(BS5MO5.3_Geboorteland)

BS5MO5.3_Onderwijsniveau <- Bias(S5MO5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO5.3_Onderwijsniveau <- sum(BS5MO5.3_Onderwijsniveau$percentageABS)/nrow(BS5MO5.3_Onderwijsniveau)

BS5MO5.3_Econ_status <- Bias(S5MO5.3_Econ_status, Econ_status, "Econ._status")
BOS5MO5.3_Econ_status <- sum(BS5MO5.3_Econ_status$percentageABS)/nrow(BS5MO5.3_Econ_status)

BS5MO5.3_Beroep <- Bias(S5MO5.3_Beroep, Beroep, "Beroep")
BOS5MO5.3_Beroep <- sum(BS5MO5.3_Beroep$percentageABS)/nrow(BS5MO5.3_Beroep)

BS5MO5.3_SBI <- Bias(S5MO5.3_SBI, SBI, "SBI")
BOS5MO5.3_SBI <- sum(BS5MO5.3_SBI$percentageABS)/nrow(BS5MO5.3_SBI)

BS5MO5.3_Burg_Staat <- Bias(S5MO5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO5.3_Burg_Staat <- sum(BS5MO5.3_Burg_Staat$percentageABS)/nrow(BS5MO5.3_Burg_Staat)

BiasOverallS5MO5.3 <- sum(BOS5MO5.3_Geslacht, BOS5MO5.3_Leeftijd, BOS5MO5.3_HH_Pos, BOS5MO5.3_HH_grootte, BOS5MO5.3_Woonregio_vorig_jaar, 
                        BOS5MO5.3_Nationaliteit, BOS5MO5.3_Geboorteland, BOS5MO5.3_Onderwijsniveau, BOS5MO5.3_Econ_status, 
                        BOS5MO5.3_Beroep, BOS5MO5.3_SBI, BOS5MO5.3_Burg_Staat) / 12

rm(S5MO5.3_Geslacht, S5MO5.3_Leeftijd, S5MO5.3_HH_Pos, S5MO5.3_HH_grootte, S5MO5.3_Woonregio_vorig_jaar, 
   S5MO5.3_Nationaliteit, S5MO5.3_Geboorteland, S5MO5.3_Onderwijsniveau, S5MO5.3_Econ_status, 
   S5MO5.3_Beroep, S5MO5.3_SBI, S5MO5.3_Burg_Staat)
rm(BS5MO5.3_Geslacht, BS5MO5.3_Leeftijd, BS5MO5.3_HH_Pos, BS5MO5.3_HH_grootte, BS5MO5.3_Woonregio_vorig_jaar, 
   BS5MO5.3_Nationaliteit, BS5MO5.3_Geboorteland, BS5MO5.3_Onderwijsniveau, BS5MO5.3_Econ_status, 
   BS5MO5.3_Beroep, BS5MO5.3_SBI, BS5MO5.3_Burg_Staat)
rm(BOS5MO5.3_Geslacht, BOS5MO5.3_Leeftijd, BOS5MO5.3_HH_Pos, BOS5MO5.3_HH_grootte, BOS5MO5.3_Woonregio_vorig_jaar, 
   BOS5MO5.3_Nationaliteit, BOS5MO5.3_Geboorteland, BOS5MO5.3_Onderwijsniveau, BOS5MO5.3_Econ_status, 
   BOS5MO5.3_Beroep, BOS5MO5.3_SBI, BOS5MO5.3_Burg_Staat)


# 10% data sets

# Counting values
S5MO10.1_Geslacht <- plyr::count(S5df_mode_imputation10.1, 'Geslacht')
S5MO10.1_Leeftijd <- plyr::count(S5df_mode_imputation10.1, 'Leeftijd')
S5MO10.1_HH_Pos <- plyr::count(S5df_mode_imputation10.1, 'HH_Pos')
S5MO10.1_HH_grootte <- plyr::count(S5df_mode_imputation10.1, 'HH_grootte')
S5MO10.1_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation10.1, 'Woonregio_vorig_jaar')
S5MO10.1_Nationaliteit <- plyr::count(S5df_mode_imputation10.1, 'Nationaliteit')
S5MO10.1_Geboorteland <- plyr::count(S5df_mode_imputation10.1, 'Geboorteland')
S5MO10.1_Onderwijsniveau <- plyr::count(S5df_mode_imputation10.1, 'Onderwijsniveau')
S5MO10.1_Econ_status <- plyr::count(S5df_mode_imputation10.1, 'Econ._status')
S5MO10.1_Beroep <- plyr::count(S5df_mode_imputation10.1, 'Beroep')
S5MO10.1_SBI <- plyr::count(S5df_mode_imputation10.1, 'SBI')
S5MO10.1_Burg_Staat <- plyr::count(S5df_mode_imputation10.1, 'Burg._Staat')

S5MO10.2_Geslacht <- plyr::count(S5df_mode_imputation10.2, 'Geslacht')
S5MO10.2_Leeftijd <- plyr::count(S5df_mode_imputation10.2, 'Leeftijd')
S5MO10.2_HH_Pos <- plyr::count(S5df_mode_imputation10.2, 'HH_Pos')
S5MO10.2_HH_grootte <- plyr::count(S5df_mode_imputation10.2, 'HH_grootte')
S5MO10.2_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation10.2, 'Woonregio_vorig_jaar')
S5MO10.2_Nationaliteit <- plyr::count(S5df_mode_imputation10.2, 'Nationaliteit')
S5MO10.2_Geboorteland <- plyr::count(S5df_mode_imputation10.2, 'Geboorteland')
S5MO10.2_Onderwijsniveau <- plyr::count(S5df_mode_imputation10.2, 'Onderwijsniveau')
S5MO10.2_Econ_status <- plyr::count(S5df_mode_imputation10.2, 'Econ._status')
S5MO10.2_Beroep <- plyr::count(S5df_mode_imputation10.2, 'Beroep')
S5MO10.2_SBI <- plyr::count(S5df_mode_imputation10.2, 'SBI')
S5MO10.2_Burg_Staat <- plyr::count(S5df_mode_imputation10.2, 'Burg._Staat')

S5MO10.3_Geslacht <- plyr::count(S5df_mode_imputation10.3, 'Geslacht')
S5MO10.3_Leeftijd <- plyr::count(S5df_mode_imputation10.3, 'Leeftijd')
S5MO10.3_HH_Pos <- plyr::count(S5df_mode_imputation10.3, 'HH_Pos')
S5MO10.3_HH_grootte <- plyr::count(S5df_mode_imputation10.3, 'HH_grootte')
S5MO10.3_Woonregio_vorig_jaar <- plyr::count(S5df_mode_imputation10.3, 'Woonregio_vorig_jaar')
S5MO10.3_Nationaliteit <- plyr::count(S5df_mode_imputation10.3, 'Nationaliteit')
S5MO10.3_Geboorteland <- plyr::count(S5df_mode_imputation10.3, 'Geboorteland')
S5MO10.3_Onderwijsniveau <- plyr::count(S5df_mode_imputation10.3, 'Onderwijsniveau')
S5MO10.3_Econ_status <- plyr::count(S5df_mode_imputation10.3, 'Econ._status')
S5MO10.3_Beroep <- plyr::count(S5df_mode_imputation10.3, 'Beroep')
S5MO10.3_SBI <- plyr::count(S5df_mode_imputation10.3, 'SBI')
S5MO10.3_Burg_Staat <- plyr::count(S5df_mode_imputation10.3, 'Burg._Staat')

rm(S5df_mode_imputation10.1, S5df_mode_imputation10.2, S5df_mode_imputation10.3)


# 10.1 data set 
BS5MO10.1_Geslacht <- Bias(S5MO10.1_Geslacht, Geslacht, "Geslacht")
BOS5MO10.1_Geslacht <- sum(BS5MO10.1_Geslacht$percentageABS)/nrow(BS5MO10.1_Geslacht)

BS5MO10.1_Leeftijd <- Bias(S5MO10.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO10.1_Leeftijd <- sum(BS5MO10.1_Leeftijd$percentageABS)/nrow(BS5MO10.1_Leeftijd)

BS5MO10.1_HH_Pos <- Bias(S5MO10.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO10.1_HH_Pos <- sum(BS5MO10.1_HH_Pos$percentageABS)/nrow(BS5MO10.1_HH_Pos)

BS5MO10.1_HH_grootte <- Bias(S5MO10.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO10.1_HH_grootte <- sum(BS5MO10.1_HH_grootte$percentageABS)/nrow(BS5MO10.1_HH_grootte)

BS5MO10.1_Woonregio_vorig_jaar <- Bias(S5MO10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO10.1_Woonregio_vorig_jaar <- sum(BS5MO10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO10.1_Woonregio_vorig_jaar)

BS5MO10.1_Nationaliteit <- Bias(S5MO10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO10.1_Nationaliteit <- sum(BS5MO10.1_Nationaliteit$percentageABS)/nrow(BS5MO10.1_Nationaliteit)

BS5MO10.1_Geboorteland <- Bias(S5MO10.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO10.1_Geboorteland <- sum(BS5MO10.1_Geboorteland$percentageABS)/nrow(BS5MO10.1_Geboorteland)

BS5MO10.1_Onderwijsniveau <- Bias(S5MO10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO10.1_Onderwijsniveau <- sum(BS5MO10.1_Onderwijsniveau$percentageABS)/nrow(BS5MO10.1_Onderwijsniveau)

BS5MO10.1_Econ_status <- Bias(S5MO10.1_Econ_status, Econ_status, "Econ._status")
BOS5MO10.1_Econ_status <- sum(BS5MO10.1_Econ_status$percentageABS)/nrow(BS5MO10.1_Econ_status)

BS5MO10.1_Beroep <- Bias(S5MO10.1_Beroep, Beroep, "Beroep")
BOS5MO10.1_Beroep <- sum(BS5MO10.1_Beroep$percentageABS)/nrow(BS5MO10.1_Beroep)

BS5MO10.1_SBI <- Bias(S5MO10.1_SBI, SBI, "SBI")
BOS5MO10.1_SBI <- sum(BS5MO10.1_SBI$percentageABS)/nrow(BS5MO10.1_SBI)

BS5MO10.1_Burg_Staat <- Bias(S5MO10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO10.1_Burg_Staat <- sum(BS5MO10.1_Burg_Staat$percentageABS)/nrow(BS5MO10.1_Burg_Staat)

BiasOverallS5MO10.1 <- sum(BOS5MO10.1_Geslacht, BOS5MO10.1_Leeftijd, BOS5MO10.1_HH_Pos, BOS5MO10.1_HH_grootte, BOS5MO10.1_Woonregio_vorig_jaar, 
                         BOS5MO10.1_Nationaliteit, BOS5MO10.1_Geboorteland, BOS5MO10.1_Onderwijsniveau, BOS5MO10.1_Econ_status, 
                         BOS5MO10.1_Beroep, BOS5MO10.1_SBI, BOS5MO10.1_Burg_Staat) / 12

rm(S5MO10.1_Geslacht, S5MO10.1_Leeftijd, S5MO10.1_HH_Pos, S5MO10.1_HH_grootte, S5MO10.1_Woonregio_vorig_jaar, 
   S5MO10.1_Nationaliteit, S5MO10.1_Geboorteland, S5MO10.1_Onderwijsniveau, S5MO10.1_Econ_status, 
   S5MO10.1_Beroep, S5MO10.1_SBI, S5MO10.1_Burg_Staat)
rm(BS5MO10.1_Geslacht, BS5MO10.1_Leeftijd, BS5MO10.1_HH_Pos, BS5MO10.1_HH_grootte, BS5MO10.1_Woonregio_vorig_jaar, 
   BS5MO10.1_Nationaliteit, BS5MO10.1_Geboorteland, BS5MO10.1_Onderwijsniveau, BS5MO10.1_Econ_status, 
   BS5MO10.1_Beroep, BS5MO10.1_SBI, BS5MO10.1_Burg_Staat)
rm(BOS5MO10.1_Geslacht, BOS5MO10.1_Leeftijd, BOS5MO10.1_HH_Pos, BOS5MO10.1_HH_grootte, BOS5MO10.1_Woonregio_vorig_jaar, 
   BOS5MO10.1_Nationaliteit, BOS5MO10.1_Geboorteland, BOS5MO10.1_Onderwijsniveau, BOS5MO10.1_Econ_status, 
   BOS5MO10.1_Beroep, BOS5MO10.1_SBI, BOS5MO10.1_Burg_Staat)

# 10.2 data set 
BS5MO10.2_Geslacht <- Bias(S5MO10.2_Geslacht, Geslacht, "Geslacht")
BOS5MO10.2_Geslacht <- sum(BS5MO10.2_Geslacht$percentageABS)/nrow(BS5MO10.2_Geslacht)

BS5MO10.2_Leeftijd <- Bias(S5MO10.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO10.2_Leeftijd <- sum(BS5MO10.2_Leeftijd$percentageABS)/nrow(BS5MO10.2_Leeftijd)

BS5MO10.2_HH_Pos <- Bias(S5MO10.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO10.2_HH_Pos <- sum(BS5MO10.2_HH_Pos$percentageABS)/nrow(BS5MO10.2_HH_Pos)

BS5MO10.2_HH_grootte <- Bias(S5MO10.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO10.2_HH_grootte <- sum(BS5MO10.2_HH_grootte$percentageABS)/nrow(BS5MO10.2_HH_grootte)

BS5MO10.2_Woonregio_vorig_jaar <- Bias(S5MO10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO10.2_Woonregio_vorig_jaar <- sum(BS5MO10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO10.2_Woonregio_vorig_jaar)

BS5MO10.2_Nationaliteit <- Bias(S5MO10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO10.2_Nationaliteit <- sum(BS5MO10.2_Nationaliteit$percentageABS)/nrow(BS5MO10.2_Nationaliteit)

BS5MO10.2_Geboorteland <- Bias(S5MO10.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO10.2_Geboorteland <- sum(BS5MO10.2_Geboorteland$percentageABS)/nrow(BS5MO10.2_Geboorteland)

BS5MO10.2_Onderwijsniveau <- Bias(S5MO10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO10.2_Onderwijsniveau <- sum(BS5MO10.2_Onderwijsniveau$percentageABS)/nrow(BS5MO10.2_Onderwijsniveau)

BS5MO10.2_Econ_status <- Bias(S5MO10.2_Econ_status, Econ_status, "Econ._status")
BOS5MO10.2_Econ_status <- sum(BS5MO10.2_Econ_status$percentageABS)/nrow(BS5MO10.2_Econ_status)

BS5MO10.2_Beroep <- Bias(S5MO10.2_Beroep, Beroep, "Beroep")
BOS5MO10.2_Beroep <- sum(BS5MO10.2_Beroep$percentageABS)/nrow(BS5MO10.2_Beroep)

BS5MO10.2_SBI <- Bias(S5MO10.2_SBI, SBI, "SBI")
BOS5MO10.2_SBI <- sum(BS5MO10.2_SBI$percentageABS)/nrow(BS5MO10.2_SBI)

BS5MO10.2_Burg_Staat <- Bias(S5MO10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO10.2_Burg_Staat <- sum(BS5MO10.2_Burg_Staat$percentageABS)/nrow(BS5MO10.2_Burg_Staat)

BiasOverallS5MO10.2 <- sum(BOS5MO10.2_Geslacht, BOS5MO10.2_Leeftijd, BOS5MO10.2_HH_Pos, BOS5MO10.2_HH_grootte, BOS5MO10.2_Woonregio_vorig_jaar, 
                         BOS5MO10.2_Nationaliteit, BOS5MO10.2_Geboorteland, BOS5MO10.2_Onderwijsniveau, BOS5MO10.2_Econ_status, 
                         BOS5MO10.2_Beroep, BOS5MO10.2_SBI, BOS5MO10.2_Burg_Staat) / 12

rm(S5MO10.2_Geslacht, S5MO10.2_Leeftijd, S5MO10.2_HH_Pos, S5MO10.2_HH_grootte, S5MO10.2_Woonregio_vorig_jaar, 
   S5MO10.2_Nationaliteit, S5MO10.2_Geboorteland, S5MO10.2_Onderwijsniveau, S5MO10.2_Econ_status, 
   S5MO10.2_Beroep, S5MO10.2_SBI, S5MO10.2_Burg_Staat)
rm(BS5MO10.2_Geslacht, BS5MO10.2_Leeftijd, BS5MO10.2_HH_Pos, BS5MO10.2_HH_grootte, BS5MO10.2_Woonregio_vorig_jaar, 
   BS5MO10.2_Nationaliteit, BS5MO10.2_Geboorteland, BS5MO10.2_Onderwijsniveau, BS5MO10.2_Econ_status, 
   BS5MO10.2_Beroep, BS5MO10.2_SBI, BS5MO10.2_Burg_Staat)
rm(BOS5MO10.2_Geslacht, BOS5MO10.2_Leeftijd, BOS5MO10.2_HH_Pos, BOS5MO10.2_HH_grootte, BOS5MO10.2_Woonregio_vorig_jaar, 
   BOS5MO10.2_Nationaliteit, BOS5MO10.2_Geboorteland, BOS5MO10.2_Onderwijsniveau, BOS5MO10.2_Econ_status, 
   BOS5MO10.2_Beroep, BOS5MO10.2_SBI, BOS5MO10.2_Burg_Staat)

# 10.3 data set 
BS5MO10.3_Geslacht <- Bias(S5MO10.3_Geslacht, Geslacht, "Geslacht")
BOS5MO10.3_Geslacht <- sum(BS5MO10.3_Geslacht$percentageABS)/nrow(BS5MO10.3_Geslacht)

BS5MO10.3_Leeftijd <- Bias(S5MO10.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5MO10.3_Leeftijd <- sum(BS5MO10.3_Leeftijd$percentageABS)/nrow(BS5MO10.3_Leeftijd)

BS5MO10.3_HH_Pos <- Bias(S5MO10.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5MO10.3_HH_Pos <- sum(BS5MO10.3_HH_Pos$percentageABS)/nrow(BS5MO10.3_HH_Pos)

BS5MO10.3_HH_grootte <- Bias(S5MO10.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5MO10.3_HH_grootte <- sum(BS5MO10.3_HH_grootte$percentageABS)/nrow(BS5MO10.3_HH_grootte)

BS5MO10.3_Woonregio_vorig_jaar <- Bias(S5MO10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5MO10.3_Woonregio_vorig_jaar <- sum(BS5MO10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5MO10.3_Woonregio_vorig_jaar)

BS5MO10.3_Nationaliteit <- Bias(S5MO10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5MO10.3_Nationaliteit <- sum(BS5MO10.3_Nationaliteit$percentageABS)/nrow(BS5MO10.3_Nationaliteit)

BS5MO10.3_Geboorteland <- Bias(S5MO10.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5MO10.3_Geboorteland <- sum(BS5MO10.3_Geboorteland$percentageABS)/nrow(BS5MO10.3_Geboorteland)

BS5MO10.3_Onderwijsniveau <- Bias(S5MO10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5MO10.3_Onderwijsniveau <- sum(BS5MO10.3_Onderwijsniveau$percentageABS)/nrow(BS5MO10.3_Onderwijsniveau)

BS5MO10.3_Econ_status <- Bias(S5MO10.3_Econ_status, Econ_status, "Econ._status")
BOS5MO10.3_Econ_status <- sum(BS5MO10.3_Econ_status$percentageABS)/nrow(BS5MO10.3_Econ_status)

BS5MO10.3_Beroep <- Bias(S5MO10.3_Beroep, Beroep, "Beroep")
BOS5MO10.3_Beroep <- sum(BS5MO10.3_Beroep$percentageABS)/nrow(BS5MO10.3_Beroep)

BS5MO10.3_SBI <- Bias(S5MO10.3_SBI, SBI, "SBI")
BOS5MO10.3_SBI <- sum(BS5MO10.3_SBI$percentageABS)/nrow(BS5MO10.3_SBI)

BS5MO10.3_Burg_Staat <- Bias(S5MO10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5MO10.3_Burg_Staat <- sum(BS5MO10.3_Burg_Staat$percentageABS)/nrow(BS5MO10.3_Burg_Staat)

BiasOverallS5MO10.3 <- sum(BOS5MO10.3_Geslacht, BOS5MO10.3_Leeftijd, BOS5MO10.3_HH_Pos, BOS5MO10.3_HH_grootte, BOS5MO10.3_Woonregio_vorig_jaar, 
                         BOS5MO10.3_Nationaliteit, BOS5MO10.3_Geboorteland, BOS5MO10.3_Onderwijsniveau, BOS5MO10.3_Econ_status, 
                         BOS5MO10.3_Beroep, BOS5MO10.3_SBI, BOS5MO10.3_Burg_Staat) / 12

rm(S5MO10.3_Geslacht, S5MO10.3_Leeftijd, S5MO10.3_HH_Pos, S5MO10.3_HH_grootte, S5MO10.3_Woonregio_vorig_jaar, 
   S5MO10.3_Nationaliteit, S5MO10.3_Geboorteland, S5MO10.3_Onderwijsniveau, S5MO10.3_Econ_status, 
   S5MO10.3_Beroep, S5MO10.3_SBI, S5MO10.3_Burg_Staat)
rm(BS5MO10.3_Geslacht, BS5MO10.3_Leeftijd, BS5MO10.3_HH_Pos, BS5MO10.3_HH_grootte, BS5MO10.3_Woonregio_vorig_jaar, 
   BS5MO10.3_Nationaliteit, BS5MO10.3_Geboorteland, BS5MO10.3_Onderwijsniveau, BS5MO10.3_Econ_status, 
   BS5MO10.3_Beroep, BS5MO10.3_SBI, BS5MO10.3_Burg_Staat)
rm(BOS5MO10.3_Geslacht, BOS5MO10.3_Leeftijd, BOS5MO10.3_HH_Pos, BOS5MO10.3_HH_grootte, BOS5MO10.3_Woonregio_vorig_jaar, 
   BOS5MO10.3_Nationaliteit, BOS5MO10.3_Geboorteland, BOS5MO10.3_Onderwijsniveau, BOS5MO10.3_Econ_status, 
   BOS5MO10.3_Beroep, BOS5MO10.3_SBI, BOS5MO10.3_Burg_Staat)




