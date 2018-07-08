
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

sub_ipums10 <- get(load(file = "sub_ipums10.Rdata"))

# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

# Setting correct and total to '0' 
S10MO2.1_correct <- S10MO2.2_correct <- S10MO2.3_correct <- S10MO5.1_correct <- S10MO5.2_correct <- S10MO5.3_correct <- S10MO10.1_correct <- S10MO10.2_correct <- S10MO10.3_correct <- 0
S10MO2.1_total <- S10MO2.2_total <- S10MO2.3_total <- S10MO5.1_total <- S10MO5.2_total <- S10MO5.3_total <- S10MO10.1_total <- S10MO10.2_total <- S10MO10.3_total <- 0


## Mode Imputation with 'ForImp' --------------------------------------------------------------------------------------------------!!!


# 2% data sets

# 2.1 data set
sub10_MCAR2.1 <- get(load(file = "sub10_MCAR21.Rdata"))
S10MO_MCAR2.1 <- data.frame(sub10_MCAR2.1)
names(S10MO_MCAR2.1) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 2.1 processing time...")
S10mode_imputation2.1 <- modeimp(S10MO_MCAR2.1)
toc(log = TRUE)
rm(S10MO_MCAR2.1)

S10df_mode_imputation2.1 <- as.data.frame(S10mode_imputation2.1)
save(S10df_mode_imputation2.1, file = "S10mode_imputation21.Rdata")
rm(S10mode_imputation2.1)
S10MO2.1_correct <- S10MO2.1_correct + sum(sub_ipums10 == S10df_mode_imputation2.1)
S10MO2.1_total <- S10MO2.1_total + sum(!is.na(S10df_mode_imputation2.1))

# 2.2 data set
sub10_MCAR2.2 <- get(load(file = "sub10_MCAR22.Rdata"))
S10MO_MCAR2.2 <- data.frame(sub10_MCAR2.2)
names(S10MO_MCAR2.2) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 2.2 processing time...")
S10mode_imputation2.2 <- modeimp(S10MO_MCAR2.2)
toc(log = TRUE)
rm(S10MO_MCAR2.2)

S10df_mode_imputation2.2 <- as.data.frame(S10mode_imputation2.2)
save(S10df_mode_imputation2.2, file = "S10mode_imputation22.Rdata")
rm(S10mode_imputation2.2)
S10MO2.2_correct <- S10MO2.2_correct + sum(sub_ipums10 == S10df_mode_imputation2.2)
S10MO2.2_total <- S10MO2.2_total + sum(!is.na(S10df_mode_imputation2.2))

# 2.3 data set
sub10_MCAR2.3 <- get(load(file = "sub10_MCAR23.Rdata"))
S10MO_MCAR2.3 <- data.frame(sub10_MCAR2.3)
names(S10MO_MCAR2.3) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 2.3 processing time...")
S10mode_imputation2.3 <- modeimp(S10MO_MCAR2.3)
toc(log = TRUE)
rm(S10MO_MCAR2.3)

S10df_mode_imputation2.3 <- as.data.frame(S10mode_imputation2.3)
save(S10df_mode_imputation2.3, file = "S10mode_imputation23.Rdata")
rm(S10mode_imputation2.3)
S10MO2.3_correct <- S10MO2.3_correct + sum(sub_ipums10 == S10df_mode_imputation2.3)
S10MO2.3_total <- S10MO2.3_total + sum(!is.na(S10df_mode_imputation2.3))


# 5% data sets

# 5.1 data set
sub10_MCAR5.1 <- get(load(file = "sub10_MCAR51.Rdata"))
S10MO_MCAR5.1 <- data.frame(sub10_MCAR5.1)
names(S10MO_MCAR5.1) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 5.1 processing time...")
S10mode_imputation5.1 <- modeimp(S10MO_MCAR5.1)
toc(log = TRUE)
rm(S10MO_MCAR5.1)

S10df_mode_imputation5.1 <- as.data.frame(S10mode_imputation5.1)
save(S10df_mode_imputation5.1, file = "S10mode_imputation51.Rdata")
rm(S10mode_imputation5.1)
S10MO5.1_correct <- S10MO5.1_correct + sum(sub_ipums10 == S10df_mode_imputation5.1)
S10MO5.1_total <- S10MO5.1_total + sum(!is.na(S10df_mode_imputation5.1))

# 5.2 data set
sub10_MCAR5.2 <- get(load(file = "sub10_MCAR52.Rdata"))
S10MO_MCAR5.2 <- data.frame(sub10_MCAR5.2)
names(S10MO_MCAR5.2) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 5.2 processing time...")
S10mode_imputation5.2 <- modeimp(S10MO_MCAR5.2)
toc(log = TRUE)
rm(S10MO_MCAR5.2)

S10df_mode_imputation5.2 <- as.data.frame(S10mode_imputation5.2)
save(S10df_mode_imputation5.2, file = "S10mode_imputation52.Rdata")
rm(S10mode_imputation5.2)
S10MO5.2_correct <- S10MO5.2_correct + sum(sub_ipums10 == S10df_mode_imputation5.2)
S10MO5.2_total <- S10MO5.2_total + sum(!is.na(S10df_mode_imputation5.2))

# 5.3 data set
sub10_MCAR5.3 <- get(load(file = "sub10_MCAR53.Rdata"))
S10MO_MCAR5.3 <- data.frame(sub10_MCAR5.3)
names(S10MO_MCAR5.3) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 5.3 processing time...")
S10mode_imputation5.3 <- modeimp(S10MO_MCAR5.3)
toc(log = TRUE)
rm(S10MO_MCAR5.3)

S10df_mode_imputation5.3 <- as.data.frame(S10mode_imputation5.3)
save(S10df_mode_imputation5.3, file = "S10mode_imputation53.Rdata")
rm(S10mode_imputation5.3)
S10MO5.3_correct <- S10MO5.3_correct + sum(sub_ipums10 == S10df_mode_imputation5.3)
S10MO5.3_total <- S10MO5.3_total + sum(!is.na(S10df_mode_imputation5.3))


# 10% data sets 

# 10.1 data set
sub10_MCAR10.1 <- get(load(file = "sub10_MCAR101.Rdata"))
S10MO_MCAR10.1 <- data.frame(sub10_MCAR10.1)
names(S10MO_MCAR10.1) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 10.1 processing time...")
S10mode_imputation10.1 <- modeimp(S10MO_MCAR10.1)
toc(log = TRUE)
rm(S10MO_MCAR10.1)

S10df_mode_imputation10.1 <- as.data.frame(S10mode_imputation10.1)
save(S10df_mode_imputation10.1, file = "S10mode_imputation101.Rdata")
rm(S10mode_imputation10.1)
S10MO10.1_correct <- S10MO10.1_correct + sum(sub_ipums10 == S10df_mode_imputation10.1)
S10MO10.1_total <- S10MO10.1_total + sum(!is.na(S10df_mode_imputation10.1))

# 10.2 data set
sub10_MCAR10.2 <- get(load(file = "sub10_MCAR102.Rdata"))
S10MO_MCAR10.2 <- data.frame(sub10_MCAR10.2)
names(S10MO_MCAR10.2) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 10.2 processing time...")
S10mode_imputation10.2 <- modeimp(S10MO_MCAR10.2)
toc(log = TRUE)
rm(S10MO_MCAR10.2)

S10df_mode_imputation10.2 <- as.data.frame(S10mode_imputation10.2)
save(S10df_mode_imputation10.2, file = "S10mode_imputation102.Rdata")
rm(S10mode_imputation10.2)
S10MO10.2_correct <- S10MO10.2_correct + sum(sub_ipums10 == S10df_mode_imputation10.2)
S10MO10.2_total <- S10MO10.2_total + sum(!is.na(S10df_mode_imputation10.2))

# 10.3 data set
sub10_MCAR10.3 <- get(load(file = "sub10_MCAR103.Rdata"))
S10MO_MCAR10.3 <- data.frame(sub10_MCAR10.3)
names(S10MO_MCAR10.3) <- gsub(" ", "_", names(sub_ipums10), fixed=TRUE)
tic("Mode Imputation 10.3 processing time...")
S10mode_imputation10.3 <- modeimp(S10MO_MCAR10.3)
toc(log = TRUE)
rm(S10MO_MCAR10.3)

S10df_mode_imputation10.3 <- as.data.frame(S10mode_imputation10.3)
save(S10df_mode_imputation10.3, file = "S10mode_imputation103.Rdata")
rm(S10mode_imputation10.3)
S10MO10.3_correct <- S10MO10.3_correct + sum(sub_ipums10 == S10df_mode_imputation10.3)
S10MO10.3_total <- S10MO10.3_total + sum(!is.na(S10df_mode_imputation10.3))


# Check if all values are imputed 
#anyNA(c(df_mode_imputation2.1, df_mode_imputation2.2, df_mode_imputation2.3, df_mode_imputation5.1, df_mode_imputation5.2, 
# df_mode_imputation5.3, df_mode_imputation10.1, df_mode_imputation10.2, df_mode_imputation10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------

# Computing the accuracy of imputation 
S10MO2.1_accuracy <- S10MO2.1_correct / S10MO2.1_total
S10MO2.2_accuracy <- S10MO2.2_correct / S10MO2.2_total
S10MO2.3_accuracy <- S10MO2.3_correct / S10MO2.3_total

S10MO5.1_accuracy <- S10MO5.1_correct / S10MO5.1_total
S10MO5.2_accuracy <- S10MO5.2_correct / S10MO5.2_total
S10MO5.3_accuracy <- S10MO5.3_correct / S10MO5.3_total

S10MO10.1_accuracy <- S10MO10.1_correct / S10MO10.1_total
S10MO10.2_accuracy <- S10MO10.2_correct / S10MO10.2_total
S10MO10.3_accuracy <- S10MO10.3_correct / S10MO10.3_total




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
  Bdf[is.na(Bdf)] <- 0 
  return(Bdf)
}

Bias2 <- function(df1, df2, column){
  Bdf <- merge(df1, df2, by = column, all = TRUE)
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
S10MO2.1_Geslacht <- plyr::count(S10df_mode_imputation2.1, 'Geslacht')
S10MO2.1_Leeftijd <- plyr::count(S10df_mode_imputation2.1, 'Leeftijd')
S10MO2.1_HH_Pos <- plyr::count(S10df_mode_imputation2.1, 'HH_Pos')
S10MO2.1_HH_grootte <- plyr::count(S10df_mode_imputation2.1, 'HH_grootte')
S10MO2.1_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation2.1, 'Woonregio_vorig_jaar')
S10MO2.1_Nationaliteit <- plyr::count(S10df_mode_imputation2.1, 'Nationaliteit')
S10MO2.1_Geboorteland <- plyr::count(S10df_mode_imputation2.1, 'Geboorteland')
S10MO2.1_Onderwijsniveau <- plyr::count(S10df_mode_imputation2.1, 'Onderwijsniveau')
S10MO2.1_Econ_status <- plyr::count(S10df_mode_imputation2.1, 'Econ._status')
S10MO2.1_Beroep <- plyr::count(S10df_mode_imputation2.1, 'Beroep')
S10MO2.1_SBI <- plyr::count(S10df_mode_imputation2.1, 'SBI')
S10MO2.1_Burg_Staat <- plyr::count(S10df_mode_imputation2.1, 'Burg._Staat')

S10MO2.2_Geslacht <- plyr::count(S10df_mode_imputation2.2, 'Geslacht')
S10MO2.2_Leeftijd <- plyr::count(S10df_mode_imputation2.2, 'Leeftijd')
S10MO2.2_HH_Pos <- plyr::count(S10df_mode_imputation2.2, 'HH_Pos')
S10MO2.2_HH_grootte <- plyr::count(S10df_mode_imputation2.2, 'HH_grootte')
S10MO2.2_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation2.2, 'Woonregio_vorig_jaar')
S10MO2.2_Nationaliteit <- plyr::count(S10df_mode_imputation2.2, 'Nationaliteit')
S10MO2.2_Geboorteland <- plyr::count(S10df_mode_imputation2.2, 'Geboorteland')
S10MO2.2_Onderwijsniveau <- plyr::count(S10df_mode_imputation2.2, 'Onderwijsniveau')
S10MO2.2_Econ_status <- plyr::count(S10df_mode_imputation2.2, 'Econ._status')
S10MO2.2_Beroep <- plyr::count(S10df_mode_imputation2.2, 'Beroep')
S10MO2.2_SBI <- plyr::count(S10df_mode_imputation2.2, 'SBI')
S10MO2.2_Burg_Staat <- plyr::count(S10df_mode_imputation2.2, 'Burg._Staat')

S10MO2.3_Geslacht <- plyr::count(S10df_mode_imputation2.3, 'Geslacht')
S10MO2.3_Leeftijd <- plyr::count(S10df_mode_imputation2.3, 'Leeftijd')
S10MO2.3_HH_Pos <- plyr::count(S10df_mode_imputation2.3, 'HH_Pos')
S10MO2.3_HH_grootte <- plyr::count(S10df_mode_imputation2.3, 'HH_grootte')
S10MO2.3_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation2.3, 'Woonregio_vorig_jaar')
S10MO2.3_Nationaliteit <- plyr::count(S10df_mode_imputation2.3, 'Nationaliteit')
S10MO2.3_Geboorteland <- plyr::count(S10df_mode_imputation2.3, 'Geboorteland')
S10MO2.3_Onderwijsniveau <- plyr::count(S10df_mode_imputation2.3, 'Onderwijsniveau')
S10MO2.3_Econ_status <- plyr::count(S10df_mode_imputation2.3, 'Econ._status')
S10MO2.3_Beroep <- plyr::count(S10df_mode_imputation2.3, 'Beroep')
S10MO2.3_SBI <- plyr::count(S10df_mode_imputation2.3, 'SBI')
S10MO2.3_Burg_Staat <- plyr::count(S10df_mode_imputation2.3, 'Burg._Staat')

#rm(S10df_mode_imputation2.1, S10df_mode_imputation2.2, S10df_mode_imputation2.3)

# 2.1 data set 
BS10MO2.1_Geslacht <- Bias(S10MO2.1_Geslacht, Geslacht, "Geslacht")
BOS10MO2.1_Geslacht <- sum(BS10MO2.1_Geslacht$percentageABS)/nrow(BS10MO2.1_Geslacht)

BS10MO2.1_Leeftijd <- Bias(S10MO2.1_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO2.1_Leeftijd <- sum(BS10MO2.1_Leeftijd$percentageABS)/nrow(BS10MO2.1_Leeftijd)

BS10MO2.1_HH_Pos <- Bias(S10MO2.1_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO2.1_HH_Pos <- sum(BS10MO2.1_HH_Pos$percentageABS)/nrow(BS10MO2.1_HH_Pos)

BS10MO2.1_HH_grootte <- Bias(S10MO2.1_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO2.1_HH_grootte <- sum(BS10MO2.1_HH_grootte$percentageABS)/nrow(BS10MO2.1_HH_grootte)

BS10MO2.1_Woonregio_vorig_jaar <- Bias(S10MO2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO2.1_Woonregio_vorig_jaar <- sum(BS10MO2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO2.1_Woonregio_vorig_jaar)

BS10MO2.1_Nationaliteit <- Bias(S10MO2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO2.1_Nationaliteit <- sum(BS10MO2.1_Nationaliteit$percentageABS)/nrow(BS10MO2.1_Nationaliteit)

BS10MO2.1_Geboorteland <- Bias(S10MO2.1_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO2.1_Geboorteland <- sum(BS10MO2.1_Geboorteland$percentageABS)/nrow(BS10MO2.1_Geboorteland)

BS10MO2.1_Onderwijsniveau <- Bias(S10MO2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO2.1_Onderwijsniveau <- sum(BS10MO2.1_Onderwijsniveau$percentageABS)/nrow(BS10MO2.1_Onderwijsniveau)

BS10MO2.1_Econ_status <- Bias(S10MO2.1_Econ_status, Econ_status, "Econ._status")
BOS10MO2.1_Econ_status <- sum(BS10MO2.1_Econ_status$percentageABS)/nrow(BS10MO2.1_Econ_status)

BS10MO2.1_Beroep <- Bias(S10MO2.1_Beroep, Beroep, "Beroep")
BOS10MO2.1_Beroep <- sum(BS10MO2.1_Beroep$percentageABS)/nrow(BS10MO2.1_Beroep)

BS10MO2.1_SBI <- Bias(S10MO2.1_SBI, SBI, "SBI")
BOS10MO2.1_SBI <- sum(BS10MO2.1_SBI$percentageABS)/nrow(BS10MO2.1_SBI)

BS10MO2.1_Burg_Staat <- Bias(S10MO2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO2.1_Burg_Staat <- sum(BS10MO2.1_Burg_Staat$percentageABS)/nrow(BS10MO2.1_Burg_Staat)

BiasOverallS10MO2.1 <- sum(BOS10MO2.1_Geslacht, BOS10MO2.1_Leeftijd, BOS10MO2.1_HH_Pos, BOS10MO2.1_HH_grootte, BOS10MO2.1_Woonregio_vorig_jaar, 
                          BOS10MO2.1_Nationaliteit, BOS10MO2.1_Geboorteland, BOS10MO2.1_Onderwijsniveau, BOS10MO2.1_Econ_status, 
                          BOS10MO2.1_Beroep, BOS10MO2.1_SBI, BOS10MO2.1_Burg_Staat) / 12

#rm(S10MO2.1_Geslacht, S10MO2.1_Leeftijd, S10MO2.1_HH_Pos, S10MO2.1_HH_grootte, S10MO2.1_Woonregio_vorig_jaar, 
   #S10MO2.1_Nationaliteit, S10MO2.1_Geboorteland, S10MO2.1_Onderwijsniveau, S10MO2.1_Econ_status, 
   #S10MO2.1_Beroep, S10MO2.1_SBI, S10MO2.1_Burg_Staat)
#rm(BS10MO2.1_Geslacht, BS10MO2.1_Leeftijd, BS10MO2.1_HH_Pos, BS10MO2.1_HH_grootte, BS10MO2.1_Woonregio_vorig_jaar, 
   #BS10MO2.1_Nationaliteit, BS10MO2.1_Geboorteland, BS10MO2.1_Onderwijsniveau, BS10MO2.1_Econ_status, 
   #BS10MO2.1_Beroep, BS10MO2.1_SBI, BS10MO2.1_Burg_Staat)
#rm(BOS10MO2.1_Geslacht, BOS10MO2.1_Leeftijd, BOS10MO2.1_HH_Pos, BOS10MO2.1_HH_grootte, BOS10MO2.1_Woonregio_vorig_jaar, 
   #BOS10MO2.1_Nationaliteit, BOS10MO2.1_Geboorteland, BOS10MO2.1_Onderwijsniveau, BOS10MO2.1_Econ_status, 
   #BOS10MO2.1_Beroep, BOS10MO2.1_SBI, BOS10MO2.1_Burg_Staat)

# 2.2 data set 
BS10MO2.2_Geslacht <- Bias(S10MO2.2_Geslacht, Geslacht, "Geslacht")
BOS10MO2.2_Geslacht <- sum(BS10MO2.2_Geslacht$percentageABS)/nrow(BS10MO2.2_Geslacht)

BS10MO2.2_Leeftijd <- Bias(S10MO2.2_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO2.2_Leeftijd <- sum(BS10MO2.2_Leeftijd$percentageABS)/nrow(BS10MO2.2_Leeftijd)

BS10MO2.2_HH_Pos <- Bias(S10MO2.2_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO2.2_HH_Pos <- sum(BS10MO2.2_HH_Pos$percentageABS)/nrow(BS10MO2.2_HH_Pos)

BS10MO2.2_HH_grootte <- Bias(S10MO2.2_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO2.2_HH_grootte <- sum(BS10MO2.2_HH_grootte$percentageABS)/nrow(BS10MO2.2_HH_grootte)

BS10MO2.2_Woonregio_vorig_jaar <- Bias(S10MO2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO2.2_Woonregio_vorig_jaar <- sum(BS10MO2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO2.2_Woonregio_vorig_jaar)

BS10MO2.2_Nationaliteit <- Bias(S10MO2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO2.2_Nationaliteit <- sum(BS10MO2.2_Nationaliteit$percentageABS)/nrow(BS10MO2.2_Nationaliteit)

BS10MO2.2_Geboorteland <- Bias(S10MO2.2_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO2.2_Geboorteland <- sum(BS10MO2.2_Geboorteland$percentageABS)/nrow(BS10MO2.2_Geboorteland)

BS10MO2.2_Onderwijsniveau <- Bias(S10MO2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO2.2_Onderwijsniveau <- sum(BS10MO2.2_Onderwijsniveau$percentageABS)/nrow(BS10MO2.2_Onderwijsniveau)

BS10MO2.2_Econ_status <- Bias(S10MO2.2_Econ_status, Econ_status, "Econ._status")
BOS10MO2.2_Econ_status <- sum(BS10MO2.2_Econ_status$percentageABS)/nrow(BS10MO2.2_Econ_status)

BS10MO2.2_Beroep <- Bias(S10MO2.2_Beroep, Beroep, "Beroep")
BOS10MO2.2_Beroep <- sum(BS10MO2.2_Beroep$percentageABS)/nrow(BS10MO2.2_Beroep)

BS10MO2.2_SBI <- Bias(S10MO2.2_SBI, SBI, "SBI")
BOS10MO2.2_SBI <- sum(BS10MO2.2_SBI$percentageABS)/nrow(BS10MO2.2_SBI)

BS10MO2.2_Burg_Staat <- Bias(S10MO2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO2.2_Burg_Staat <- sum(BS10MO2.2_Burg_Staat$percentageABS)/nrow(BS10MO2.2_Burg_Staat)

BiasOverallS10MO2.2 <- sum(BOS10MO2.2_Geslacht, BOS10MO2.2_Leeftijd, BOS10MO2.2_HH_Pos, BOS10MO2.2_HH_grootte, BOS10MO2.2_Woonregio_vorig_jaar, 
                          BOS10MO2.2_Nationaliteit, BOS10MO2.2_Geboorteland, BOS10MO2.2_Onderwijsniveau, BOS10MO2.2_Econ_status, 
                          BOS10MO2.2_Beroep, BOS10MO2.2_SBI, BOS10MO2.2_Burg_Staat) / 12

#rm(S10MO2.2_Geslacht, S10MO2.2_Leeftijd, S10MO2.2_HH_Pos, S10MO2.2_HH_grootte, S10MO2.2_Woonregio_vorig_jaar, 
   #S10MO2.2_Nationaliteit, S10MO2.2_Geboorteland, S10MO2.2_Onderwijsniveau, S10MO2.2_Econ_status, 
   #S10MO2.2_Beroep, S10MO2.2_SBI, S10MO2.2_Burg_Staat)
#rm(BS10MO2.2_Geslacht, BS10MO2.2_Leeftijd, BS10MO2.2_HH_Pos, BS10MO2.2_HH_grootte, BS10MO2.2_Woonregio_vorig_jaar, 
   #BS10MO2.2_Nationaliteit, BS10MO2.2_Geboorteland, BS10MO2.2_Onderwijsniveau, BS10MO2.2_Econ_status, 
   #BS10MO2.2_Beroep, BS10MO2.2_SBI, BS10MO2.2_Burg_Staat)
#rm(BOS10MO2.2_Geslacht, BOS10MO2.2_Leeftijd, BOS10MO2.2_HH_Pos, BOS10MO2.2_HH_grootte, BOS10MO2.2_Woonregio_vorig_jaar, 
   #BOS10MO2.2_Nationaliteit, BOS10MO2.2_Geboorteland, BOS10MO2.2_Onderwijsniveau, BOS10MO2.2_Econ_status, 
   #BOS10MO2.2_Beroep, BOS10MO2.2_SBI, BOS10MO2.2_Burg_Staat)

# 2.3 data set 
BS10MO2.3_Geslacht <- Bias(S10MO2.3_Geslacht, Geslacht, "Geslacht")
BOS10MO2.3_Geslacht <- sum(BS10MO2.3_Geslacht$percentageABS)/nrow(BS10MO2.3_Geslacht)

BS10MO2.3_Leeftijd <- Bias(S10MO2.3_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO2.3_Leeftijd <- sum(BS10MO2.3_Leeftijd$percentageABS)/nrow(BS10MO2.3_Leeftijd)

BS10MO2.3_HH_Pos <- Bias(S10MO2.3_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO2.3_HH_Pos <- sum(BS10MO2.3_HH_Pos$percentageABS)/nrow(BS10MO2.3_HH_Pos)

BS10MO2.3_HH_grootte <- Bias(S10MO2.3_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO2.3_HH_grootte <- sum(BS10MO2.3_HH_grootte$percentageABS)/nrow(BS10MO2.3_HH_grootte)

BS10MO2.3_Woonregio_vorig_jaar <- Bias(S10MO2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO2.3_Woonregio_vorig_jaar <- sum(BS10MO2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO2.3_Woonregio_vorig_jaar)

BS10MO2.3_Nationaliteit <- Bias(S10MO2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO2.3_Nationaliteit <- sum(BS10MO2.3_Nationaliteit$percentageABS)/nrow(BS10MO2.3_Nationaliteit)

BS10MO2.3_Geboorteland <- Bias(S10MO2.3_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO2.3_Geboorteland <- sum(BS10MO2.3_Geboorteland$percentageABS)/nrow(BS10MO2.3_Geboorteland)

BS10MO2.3_Onderwijsniveau <- Bias(S10MO2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO2.3_Onderwijsniveau <- sum(BS10MO2.3_Onderwijsniveau$percentageABS)/nrow(BS10MO2.3_Onderwijsniveau)

BS10MO2.3_Econ_status <- Bias(S10MO2.3_Econ_status, Econ_status, "Econ._status")
BOS10MO2.3_Econ_status <- sum(BS10MO2.3_Econ_status$percentageABS)/nrow(BS10MO2.3_Econ_status)

BS10MO2.3_Beroep <- Bias(S10MO2.3_Beroep, Beroep, "Beroep")
BOS10MO2.3_Beroep <- sum(BS10MO2.3_Beroep$percentageABS)/nrow(BS10MO2.3_Beroep)

BS10MO2.3_SBI <- Bias(S10MO2.3_SBI, SBI, "SBI")
BOS10MO2.3_SBI <- sum(BS10MO2.3_SBI$percentageABS)/nrow(BS10MO2.3_SBI)

BS10MO2.3_Burg_Staat <- Bias(S10MO2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO2.3_Burg_Staat <- sum(BS10MO2.3_Burg_Staat$percentageABS)/nrow(BS10MO2.3_Burg_Staat)

BiasOverallS10MO2.3 <- sum(BOS10MO2.3_Geslacht, BOS10MO2.3_Leeftijd, BOS10MO2.3_HH_Pos, BOS10MO2.3_HH_grootte, BOS10MO2.3_Woonregio_vorig_jaar, 
                          BOS10MO2.3_Nationaliteit, BOS10MO2.3_Geboorteland, BOS10MO2.3_Onderwijsniveau, BOS10MO2.3_Econ_status, 
                          BOS10MO2.3_Beroep, BOS10MO2.3_SBI, BOS10MO2.3_Burg_Staat) / 12

#rm(S10MO2.3_Geslacht, S10MO2.3_Leeftijd, S10MO2.3_HH_Pos, S10MO2.3_HH_grootte, S10MO2.3_Woonregio_vorig_jaar, 
   #S10MO2.3_Nationaliteit, S10MO2.3_Geboorteland, S10MO2.3_Onderwijsniveau, S10MO2.3_Econ_status, 
   #S10MO2.3_Beroep, S10MO2.3_SBI, S10MO2.3_Burg_Staat)
#rm(BS10MO2.3_Geslacht, BS10MO2.3_Leeftijd, BS10MO2.3_HH_Pos, BS10MO2.3_HH_grootte, BS10MO2.3_Woonregio_vorig_jaar, 
   #BS10MO2.3_Nationaliteit, BS10MO2.3_Geboorteland, BS10MO2.3_Onderwijsniveau, BS10MO2.3_Econ_status, 
   #BS10MO2.3_Beroep, BS10MO2.3_SBI, BS10MO2.3_Burg_Staat)
#rm(BOS10MO2.3_Geslacht, BOS10MO2.3_Leeftijd, BOS10MO2.3_HH_Pos, BOS10MO2.3_HH_grootte, BOS10MO2.3_Woonregio_vorig_jaar, 
   #BOS10MO2.3_Nationaliteit, BOS10MO2.3_Geboorteland, BOS10MO2.3_Onderwijsniveau, BOS10MO2.3_Econ_status, 
   #BOS10MO2.3_Beroep, BOS10MO2.3_SBI, BOS10MO2.3_Burg_Staat)


# 5% data sets

# Counting values
S10MO5.1_Geslacht <- plyr::count(S10df_mode_imputation5.1, 'Geslacht')
S10MO5.1_Leeftijd <- plyr::count(S10df_mode_imputation5.1, 'Leeftijd')
S10MO5.1_HH_Pos <- plyr::count(S10df_mode_imputation5.1, 'HH_Pos')
S10MO5.1_HH_grootte <- plyr::count(S10df_mode_imputation5.1, 'HH_grootte')
S10MO5.1_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation5.1, 'Woonregio_vorig_jaar')
S10MO5.1_Nationaliteit <- plyr::count(S10df_mode_imputation5.1, 'Nationaliteit')
S10MO5.1_Geboorteland <- plyr::count(S10df_mode_imputation5.1, 'Geboorteland')
S10MO5.1_Onderwijsniveau <- plyr::count(S10df_mode_imputation5.1, 'Onderwijsniveau')
S10MO5.1_Econ_status <- plyr::count(S10df_mode_imputation5.1, 'Econ._status')
S10MO5.1_Beroep <- plyr::count(S10df_mode_imputation5.1, 'Beroep')
S10MO5.1_SBI <- plyr::count(S10df_mode_imputation5.1, 'SBI')
S10MO5.1_Burg_Staat <- plyr::count(S10df_mode_imputation5.1, 'Burg._Staat')

S10MO5.2_Geslacht <- plyr::count(S10df_mode_imputation5.2, 'Geslacht')
S10MO5.2_Leeftijd <- plyr::count(S10df_mode_imputation5.2, 'Leeftijd')
S10MO5.2_HH_Pos <- plyr::count(S10df_mode_imputation5.2, 'HH_Pos')
S10MO5.2_HH_grootte <- plyr::count(S10df_mode_imputation5.2, 'HH_grootte')
S10MO5.2_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation5.2, 'Woonregio_vorig_jaar')
S10MO5.2_Nationaliteit <- plyr::count(S10df_mode_imputation5.2, 'Nationaliteit')
S10MO5.2_Geboorteland <- plyr::count(S10df_mode_imputation5.2, 'Geboorteland')
S10MO5.2_Onderwijsniveau <- plyr::count(S10df_mode_imputation5.2, 'Onderwijsniveau')
S10MO5.2_Econ_status <- plyr::count(S10df_mode_imputation5.2, 'Econ._status')
S10MO5.2_Beroep <- plyr::count(S10df_mode_imputation5.2, 'Beroep')
S10MO5.2_SBI <- plyr::count(S10df_mode_imputation5.2, 'SBI')
S10MO5.2_Burg_Staat <- plyr::count(S10df_mode_imputation5.2, 'Burg._Staat')

S10MO5.3_Geslacht <- plyr::count(S10df_mode_imputation5.3, 'Geslacht')
S10MO5.3_Leeftijd <- plyr::count(S10df_mode_imputation5.3, 'Leeftijd')
S10MO5.3_HH_Pos <- plyr::count(S10df_mode_imputation5.3, 'HH_Pos')
S10MO5.3_HH_grootte <- plyr::count(S10df_mode_imputation5.3, 'HH_grootte')
S10MO5.3_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation5.3, 'Woonregio_vorig_jaar')
S10MO5.3_Nationaliteit <- plyr::count(S10df_mode_imputation5.3, 'Nationaliteit')
S10MO5.3_Geboorteland <- plyr::count(S10df_mode_imputation5.3, 'Geboorteland')
S10MO5.3_Onderwijsniveau <- plyr::count(S10df_mode_imputation5.3, 'Onderwijsniveau')
S10MO5.3_Econ_status <- plyr::count(S10df_mode_imputation5.3, 'Econ._status')
S10MO5.3_Beroep <- plyr::count(S10df_mode_imputation5.3, 'Beroep')
S10MO5.3_SBI <- plyr::count(S10df_mode_imputation5.3, 'SBI')
S10MO5.3_Burg_Staat <- plyr::count(S10df_mode_imputation5.3, 'Burg._Staat')

#rm(S10df_mode_imputation5.1, S10df_mode_imputation5.2, S10df_mode_imputation5.3)


# 5.1 data set 
BS10MO5.1_Geslacht <- Bias(S10MO5.1_Geslacht, Geslacht, "Geslacht")
BOS10MO5.1_Geslacht <- sum(BS10MO5.1_Geslacht$percentageABS)/nrow(BS10MO5.1_Geslacht)

BS10MO5.1_Leeftijd <- Bias(S10MO5.1_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO5.1_Leeftijd <- sum(BS10MO5.1_Leeftijd$percentageABS)/nrow(BS10MO5.1_Leeftijd)

BS10MO5.1_HH_Pos <- Bias(S10MO5.1_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO5.1_HH_Pos <- sum(BS10MO5.1_HH_Pos$percentageABS)/nrow(BS10MO5.1_HH_Pos)

BS10MO5.1_HH_grootte <- Bias(S10MO5.1_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO5.1_HH_grootte <- sum(BS10MO5.1_HH_grootte$percentageABS)/nrow(BS10MO5.1_HH_grootte)

BS10MO5.1_Woonregio_vorig_jaar <- Bias(S10MO5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO5.1_Woonregio_vorig_jaar <- sum(BS10MO5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO5.1_Woonregio_vorig_jaar)

BS10MO5.1_Nationaliteit <- Bias(S10MO5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO5.1_Nationaliteit <- sum(BS10MO5.1_Nationaliteit$percentageABS)/nrow(BS10MO5.1_Nationaliteit)

BS10MO5.1_Geboorteland <- Bias(S10MO5.1_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO5.1_Geboorteland <- sum(BS10MO5.1_Geboorteland$percentageABS)/nrow(BS10MO5.1_Geboorteland)

BS10MO5.1_Onderwijsniveau <- Bias(S10MO5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO5.1_Onderwijsniveau <- sum(BS10MO5.1_Onderwijsniveau$percentageABS)/nrow(BS10MO5.1_Onderwijsniveau)

BS10MO5.1_Econ_status <- Bias2(S10MO5.1_Econ_status, Econ_status, "Econ._status")
BOS10MO5.1_Econ_status <- sum(BS10MO5.1_Econ_status$percentageABS)/nrow(BS10MO5.1_Econ_status)

BS10MO5.1_Beroep <- Bias(S10MO5.1_Beroep, Beroep, "Beroep")
BOS10MO5.1_Beroep <- sum(BS10MO5.1_Beroep$percentageABS)/nrow(BS10MO5.1_Beroep)

BS10MO5.1_SBI <- Bias(S10MO5.1_SBI, SBI, "SBI")
BOS10MO5.1_SBI <- sum(BS10MO5.1_SBI$percentageABS)/nrow(BS10MO5.1_SBI)

BS10MO5.1_Burg_Staat <- Bias(S10MO5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO5.1_Burg_Staat <- sum(BS10MO5.1_Burg_Staat$percentageABS)/nrow(BS10MO5.1_Burg_Staat)

BiasOverallS10MO5.1 <- sum(BOS10MO5.1_Geslacht, BOS10MO5.1_Leeftijd, BOS10MO5.1_HH_Pos, BOS10MO5.1_HH_grootte, BOS10MO5.1_Woonregio_vorig_jaar, 
                          BOS10MO5.1_Nationaliteit, BOS10MO5.1_Geboorteland, BOS10MO5.1_Onderwijsniveau, BOS10MO5.1_Econ_status, 
                          BOS10MO5.1_Beroep, BOS10MO5.1_SBI, BOS10MO5.1_Burg_Staat) / 12

#rm(S10MO5.1_Geslacht, S10MO5.1_Leeftijd, S10MO5.1_HH_Pos, S10MO5.1_HH_grootte, S10MO5.1_Woonregio_vorig_jaar, 
   #S10MO5.1_Nationaliteit, S10MO5.1_Geboorteland, S10MO5.1_Onderwijsniveau, S10MO5.1_Econ_status, 
   #S10MO5.1_Beroep, S10MO5.1_SBI, S10MO5.1_Burg_Staat)
#rm(BS10MO5.1_Geslacht, BS10MO5.1_Leeftijd, BS10MO5.1_HH_Pos, BS10MO5.1_HH_grootte, BS10MO5.1_Woonregio_vorig_jaar, 
   #BS10MO5.1_Nationaliteit, BS10MO5.1_Geboorteland, BS10MO5.1_Onderwijsniveau, BS10MO5.1_Econ_status, 
   #BS10MO5.1_Beroep, BS10MO5.1_SBI, BS10MO5.1_Burg_Staat)
#rm(BOS10MO5.1_Geslacht, BOS10MO5.1_Leeftijd, BOS10MO5.1_HH_Pos, BOS10MO5.1_HH_grootte, BOS10MO5.1_Woonregio_vorig_jaar, 
   #BOS10MO5.1_Nationaliteit, BOS10MO5.1_Geboorteland, BOS10MO5.1_Onderwijsniveau, BOS10MO5.1_Econ_status, 
   #BOS10MO5.1_Beroep, BOS10MO5.1_SBI, BOS10MO5.1_Burg_Staat)

# 5.2 data set 
BS10MO5.2_Geslacht <- Bias(S10MO5.2_Geslacht, Geslacht, "Geslacht")
BOS10MO5.2_Geslacht <- sum(BS10MO5.2_Geslacht$percentageABS)/nrow(BS10MO5.2_Geslacht)

BS10MO5.2_Leeftijd <- Bias(S10MO5.2_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO5.2_Leeftijd <- sum(BS10MO5.2_Leeftijd$percentageABS)/nrow(BS10MO5.2_Leeftijd)

BS10MO5.2_HH_Pos <- Bias(S10MO5.2_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO5.2_HH_Pos <- sum(BS10MO5.2_HH_Pos$percentageABS)/nrow(BS10MO5.2_HH_Pos)

BS10MO5.2_HH_grootte <- Bias(S10MO5.2_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO5.2_HH_grootte <- sum(BS10MO5.2_HH_grootte$percentageABS)/nrow(BS10MO5.2_HH_grootte)

BS10MO5.2_Woonregio_vorig_jaar <- Bias(S10MO5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO5.2_Woonregio_vorig_jaar <- sum(BS10MO5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO5.2_Woonregio_vorig_jaar)

BS10MO5.2_Nationaliteit <- Bias(S10MO5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO5.2_Nationaliteit <- sum(BS10MO5.2_Nationaliteit$percentageABS)/nrow(BS10MO5.2_Nationaliteit)

BS10MO5.2_Geboorteland <- Bias(S10MO5.2_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO5.2_Geboorteland <- sum(BS10MO5.2_Geboorteland$percentageABS)/nrow(BS10MO5.2_Geboorteland)

BS10MO5.2_Onderwijsniveau <- Bias(S10MO5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO5.2_Onderwijsniveau <- sum(BS10MO5.2_Onderwijsniveau$percentageABS)/nrow(BS10MO5.2_Onderwijsniveau)

BS10MO5.2_Econ_status <- Bias(S10MO5.2_Econ_status, Econ_status, "Econ._status")
BOS10MO5.2_Econ_status <- sum(BS10MO5.2_Econ_status$percentageABS)/nrow(BS10MO5.2_Econ_status)

BS10MO5.2_Beroep <- Bias(S10MO5.2_Beroep, Beroep, "Beroep")
BOS10MO5.2_Beroep <- sum(BS10MO5.2_Beroep$percentageABS)/nrow(BS10MO5.2_Beroep)

BS10MO5.2_SBI <- Bias(S10MO5.2_SBI, SBI, "SBI")
BOS10MO5.2_SBI <- sum(BS10MO5.2_SBI$percentageABS)/nrow(BS10MO5.2_SBI)

BS10MO5.2_Burg_Staat <- Bias(S10MO5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO5.2_Burg_Staat <- sum(BS10MO5.2_Burg_Staat$percentageABS)/nrow(BS10MO5.2_Burg_Staat)

BiasOverallS10MO5.2 <- sum(BOS10MO5.2_Geslacht, BOS10MO5.2_Leeftijd, BOS10MO5.2_HH_Pos, BOS10MO5.2_HH_grootte, BOS10MO5.2_Woonregio_vorig_jaar, 
                          BOS10MO5.2_Nationaliteit, BOS10MO5.2_Geboorteland, BOS10MO5.2_Onderwijsniveau, BOS10MO5.2_Econ_status, 
                          BOS10MO5.2_Beroep, BOS10MO5.2_SBI, BOS10MO5.2_Burg_Staat) / 12

#rm(S10MO5.2_Geslacht, S10MO5.2_Leeftijd, S10MO5.2_HH_Pos, S10MO5.2_HH_grootte, S10MO5.2_Woonregio_vorig_jaar, 
   #S10MO5.2_Nationaliteit, S10MO5.2_Geboorteland, S10MO5.2_Onderwijsniveau, S10MO5.2_Econ_status, 
   #S10MO5.2_Beroep, S10MO5.2_SBI, S10MO5.2_Burg_Staat)
#rm(BS10MO5.2_Geslacht, BS10MO5.2_Leeftijd, BS10MO5.2_HH_Pos, BS10MO5.2_HH_grootte, BS10MO5.2_Woonregio_vorig_jaar, 
   #BS10MO5.2_Nationaliteit, BS10MO5.2_Geboorteland, BS10MO5.2_Onderwijsniveau, BS10MO5.2_Econ_status, 
   #BS10MO5.2_Beroep, BS10MO5.2_SBI, BS10MO5.2_Burg_Staat)
#rm(BOS10MO5.2_Geslacht, BOS10MO5.2_Leeftijd, BOS10MO5.2_HH_Pos, BOS10MO5.2_HH_grootte, BOS10MO5.2_Woonregio_vorig_jaar, 
   #BOS10MO5.2_Nationaliteit, BOS10MO5.2_Geboorteland, BOS10MO5.2_Onderwijsniveau, BOS10MO5.2_Econ_status, 
   #BOS10MO5.2_Beroep, BOS10MO5.2_SBI, BOS10MO5.2_Burg_Staat)

# 5.3 data set 
BS10MO5.3_Geslacht <- Bias(S10MO5.3_Geslacht, Geslacht, "Geslacht")
BOS10MO5.3_Geslacht <- sum(BS10MO5.3_Geslacht$percentageABS)/nrow(BS10MO5.3_Geslacht)

BS10MO5.3_Leeftijd <- Bias(S10MO5.3_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO5.3_Leeftijd <- sum(BS10MO5.3_Leeftijd$percentageABS)/nrow(BS10MO5.3_Leeftijd)

BS10MO5.3_HH_Pos <- Bias(S10MO5.3_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO5.3_HH_Pos <- sum(BS10MO5.3_HH_Pos$percentageABS)/nrow(BS10MO5.3_HH_Pos)

BS10MO5.3_HH_grootte <- Bias(S10MO5.3_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO5.3_HH_grootte <- sum(BS10MO5.3_HH_grootte$percentageABS)/nrow(BS10MO5.3_HH_grootte)

BS10MO5.3_Woonregio_vorig_jaar <- Bias(S10MO5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO5.3_Woonregio_vorig_jaar <- sum(BS10MO5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO5.3_Woonregio_vorig_jaar)

BS10MO5.3_Nationaliteit <- Bias(S10MO5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO5.3_Nationaliteit <- sum(BS10MO5.3_Nationaliteit$percentageABS)/nrow(BS10MO5.3_Nationaliteit)

BS10MO5.3_Geboorteland <- Bias(S10MO5.3_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO5.3_Geboorteland <- sum(BS10MO5.3_Geboorteland$percentageABS)/nrow(BS10MO5.3_Geboorteland)

BS10MO5.3_Onderwijsniveau <- Bias(S10MO5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO5.3_Onderwijsniveau <- sum(BS10MO5.3_Onderwijsniveau$percentageABS)/nrow(BS10MO5.3_Onderwijsniveau)

BS10MO5.3_Econ_status <- Bias(S10MO5.3_Econ_status, Econ_status, "Econ._status")
BOS10MO5.3_Econ_status <- sum(BS10MO5.3_Econ_status$percentageABS)/nrow(BS10MO5.3_Econ_status)

BS10MO5.3_Beroep <- Bias(S10MO5.3_Beroep, Beroep, "Beroep")
BOS10MO5.3_Beroep <- sum(BS10MO5.3_Beroep$percentageABS)/nrow(BS10MO5.3_Beroep)

BS10MO5.3_SBI <- Bias(S10MO5.3_SBI, SBI, "SBI")
BOS10MO5.3_SBI <- sum(BS10MO5.3_SBI$percentageABS)/nrow(BS10MO5.3_SBI)

BS10MO5.3_Burg_Staat <- Bias(S10MO5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO5.3_Burg_Staat <- sum(BS10MO5.3_Burg_Staat$percentageABS)/nrow(BS10MO5.3_Burg_Staat)

BiasOverallS10MO5.3 <- sum(BOS10MO5.3_Geslacht, BOS10MO5.3_Leeftijd, BOS10MO5.3_HH_Pos, BOS10MO5.3_HH_grootte, BOS10MO5.3_Woonregio_vorig_jaar, 
                          BOS10MO5.3_Nationaliteit, BOS10MO5.3_Geboorteland, BOS10MO5.3_Onderwijsniveau, BOS10MO5.3_Econ_status, 
                          BOS10MO5.3_Beroep, BOS10MO5.3_SBI, BOS10MO5.3_Burg_Staat) / 12

#rm(S10MO5.3_Geslacht, S10MO5.3_Leeftijd, S10MO5.3_HH_Pos, S10MO5.3_HH_grootte, S10MO5.3_Woonregio_vorig_jaar, 
   #S10MO5.3_Nationaliteit, S10MO5.3_Geboorteland, S10MO5.3_Onderwijsniveau, S10MO5.3_Econ_status, 
   #S10MO5.3_Beroep, S10MO5.3_SBI, S10MO5.3_Burg_Staat)
#rm(BS10MO5.3_Geslacht, BS10MO5.3_Leeftijd, BS10MO5.3_HH_Pos, BS10MO5.3_HH_grootte, BS10MO5.3_Woonregio_vorig_jaar, 
   #BS10MO5.3_Nationaliteit, BS10MO5.3_Geboorteland, BS10MO5.3_Onderwijsniveau, BS10MO5.3_Econ_status, 
   #BS10MO5.3_Beroep, BS10MO5.3_SBI, BS10MO5.3_Burg_Staat)
#rm(BOS10MO5.3_Geslacht, BOS10MO5.3_Leeftijd, BOS10MO5.3_HH_Pos, BOS10MO5.3_HH_grootte, BOS10MO5.3_Woonregio_vorig_jaar, 
   #BOS10MO5.3_Nationaliteit, BOS10MO5.3_Geboorteland, BOS10MO5.3_Onderwijsniveau, BOS10MO5.3_Econ_status, 
   #BOS10MO5.3_Beroep, BOS10MO5.3_SBI, BOS10MO5.3_Burg_Staat)


# 10% data sets

# Counting values
S10MO10.1_Geslacht <- plyr::count(S10df_mode_imputation10.1, 'Geslacht')
S10MO10.1_Leeftijd <- plyr::count(S10df_mode_imputation10.1, 'Leeftijd')
S10MO10.1_HH_Pos <- plyr::count(S10df_mode_imputation10.1, 'HH_Pos')
S10MO10.1_HH_grootte <- plyr::count(S10df_mode_imputation10.1, 'HH_grootte')
S10MO10.1_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation10.1, 'Woonregio_vorig_jaar')
S10MO10.1_Nationaliteit <- plyr::count(S10df_mode_imputation10.1, 'Nationaliteit')
S10MO10.1_Geboorteland <- plyr::count(S10df_mode_imputation10.1, 'Geboorteland')
S10MO10.1_Onderwijsniveau <- plyr::count(S10df_mode_imputation10.1, 'Onderwijsniveau')
S10MO10.1_Econ_status <- plyr::count(S10df_mode_imputation10.1, 'Econ._status')
S10MO10.1_Beroep <- plyr::count(S10df_mode_imputation10.1, 'Beroep')
S10MO10.1_SBI <- plyr::count(S10df_mode_imputation10.1, 'SBI')
S10MO10.1_Burg_Staat <- plyr::count(S10df_mode_imputation10.1, 'Burg._Staat')

S10MO10.2_Geslacht <- plyr::count(S10df_mode_imputation10.2, 'Geslacht')
S10MO10.2_Leeftijd <- plyr::count(S10df_mode_imputation10.2, 'Leeftijd')
S10MO10.2_HH_Pos <- plyr::count(S10df_mode_imputation10.2, 'HH_Pos')
S10MO10.2_HH_grootte <- plyr::count(S10df_mode_imputation10.2, 'HH_grootte')
S10MO10.2_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation10.2, 'Woonregio_vorig_jaar')
S10MO10.2_Nationaliteit <- plyr::count(S10df_mode_imputation10.2, 'Nationaliteit')
S10MO10.2_Geboorteland <- plyr::count(S10df_mode_imputation10.2, 'Geboorteland')
S10MO10.2_Onderwijsniveau <- plyr::count(S10df_mode_imputation10.2, 'Onderwijsniveau')
S10MO10.2_Econ_status <- plyr::count(S10df_mode_imputation10.2, 'Econ._status')
S10MO10.2_Beroep <- plyr::count(S10df_mode_imputation10.2, 'Beroep')
S10MO10.2_SBI <- plyr::count(S10df_mode_imputation10.2, 'SBI')
S10MO10.2_Burg_Staat <- plyr::count(S10df_mode_imputation10.2, 'Burg._Staat')

S10MO10.3_Geslacht <- plyr::count(S10df_mode_imputation10.3, 'Geslacht')
S10MO10.3_Leeftijd <- plyr::count(S10df_mode_imputation10.3, 'Leeftijd')
S10MO10.3_HH_Pos <- plyr::count(S10df_mode_imputation10.3, 'HH_Pos')
S10MO10.3_HH_grootte <- plyr::count(S10df_mode_imputation10.3, 'HH_grootte')
S10MO10.3_Woonregio_vorig_jaar <- plyr::count(S10df_mode_imputation10.3, 'Woonregio_vorig_jaar')
S10MO10.3_Nationaliteit <- plyr::count(S10df_mode_imputation10.3, 'Nationaliteit')
S10MO10.3_Geboorteland <- plyr::count(S10df_mode_imputation10.3, 'Geboorteland')
S10MO10.3_Onderwijsniveau <- plyr::count(S10df_mode_imputation10.3, 'Onderwijsniveau')
S10MO10.3_Econ_status <- plyr::count(S10df_mode_imputation10.3, 'Econ._status')
S10MO10.3_Beroep <- plyr::count(S10df_mode_imputation10.3, 'Beroep')
S10MO10.3_SBI <- plyr::count(S10df_mode_imputation10.3, 'SBI')
S10MO10.3_Burg_Staat <- plyr::count(S10df_mode_imputation10.3, 'Burg._Staat')

#rm(S10df_mode_imputation10.1, S10df_mode_imputation10.2, S10df_mode_imputation10.3)


# 10.1 data set 
BS10MO10.1_Geslacht <- Bias(S10MO10.1_Geslacht, Geslacht, "Geslacht")
BOS10MO10.1_Geslacht <- sum(BS10MO10.1_Geslacht$percentageABS)/nrow(BS10MO10.1_Geslacht)

BS10MO10.1_Leeftijd <- Bias(S10MO10.1_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO10.1_Leeftijd <- sum(BS10MO10.1_Leeftijd$percentageABS)/nrow(BS10MO10.1_Leeftijd)

BS10MO10.1_HH_Pos <- Bias(S10MO10.1_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO10.1_HH_Pos <- sum(BS10MO10.1_HH_Pos$percentageABS)/nrow(BS10MO10.1_HH_Pos)

BS10MO10.1_HH_grootte <- Bias(S10MO10.1_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO10.1_HH_grootte <- sum(BS10MO10.1_HH_grootte$percentageABS)/nrow(BS10MO10.1_HH_grootte)

BS10MO10.1_Woonregio_vorig_jaar <- Bias(S10MO10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO10.1_Woonregio_vorig_jaar <- sum(BS10MO10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO10.1_Woonregio_vorig_jaar)

BS10MO10.1_Nationaliteit <- Bias(S10MO10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO10.1_Nationaliteit <- sum(BS10MO10.1_Nationaliteit$percentageABS)/nrow(BS10MO10.1_Nationaliteit)

BS10MO10.1_Geboorteland <- Bias(S10MO10.1_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO10.1_Geboorteland <- sum(BS10MO10.1_Geboorteland$percentageABS)/nrow(BS10MO10.1_Geboorteland)

BS10MO10.1_Onderwijsniveau <- Bias(S10MO10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO10.1_Onderwijsniveau <- sum(BS10MO10.1_Onderwijsniveau$percentageABS)/nrow(BS10MO10.1_Onderwijsniveau)

BS10MO10.1_Econ_status <- Bias2(S10MO10.1_Econ_status, Econ_status, "Econ._status")
BOS10MO10.1_Econ_status <- sum(BS10MO10.1_Econ_status$percentageABS)/nrow(BS10MO10.1_Econ_status)

BS10MO10.1_Beroep <- Bias(S10MO10.1_Beroep, Beroep, "Beroep")
BOS10MO10.1_Beroep <- sum(BS10MO10.1_Beroep$percentageABS)/nrow(BS10MO10.1_Beroep)

BS10MO10.1_SBI <- Bias(S10MO10.1_SBI, SBI, "SBI")
BOS10MO10.1_SBI <- sum(BS10MO10.1_SBI$percentageABS)/nrow(BS10MO10.1_SBI)

BS10MO10.1_Burg_Staat <- Bias(S10MO10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO10.1_Burg_Staat <- sum(BS10MO10.1_Burg_Staat$percentageABS)/nrow(BS10MO10.1_Burg_Staat)

BiasOverallS10MO10.1 <- sum(BOS10MO10.1_Geslacht, BOS10MO10.1_Leeftijd, BOS10MO10.1_HH_Pos, BOS10MO10.1_HH_grootte, BOS10MO10.1_Woonregio_vorig_jaar, 
                           BOS10MO10.1_Nationaliteit, BOS10MO10.1_Geboorteland, BOS10MO10.1_Onderwijsniveau, BOS10MO10.1_Econ_status, 
                           BOS10MO10.1_Beroep, BOS10MO10.1_SBI, BOS10MO10.1_Burg_Staat) / 12

#rm(S10MO10.1_Geslacht, S10MO10.1_Leeftijd, S10MO10.1_HH_Pos, S10MO10.1_HH_grootte, S10MO10.1_Woonregio_vorig_jaar, 
   #S10MO10.1_Nationaliteit, S10MO10.1_Geboorteland, S10MO10.1_Onderwijsniveau, S10MO10.1_Econ_status, 
   #S10MO10.1_Beroep, S10MO10.1_SBI, S10MO10.1_Burg_Staat)
#rm(BS10MO10.1_Geslacht, BS10MO10.1_Leeftijd, BS10MO10.1_HH_Pos, BS10MO10.1_HH_grootte, BS10MO10.1_Woonregio_vorig_jaar, 
   #BS10MO10.1_Nationaliteit, BS10MO10.1_Geboorteland, BS10MO10.1_Onderwijsniveau, BS10MO10.1_Econ_status, 
   #BS10MO10.1_Beroep, BS10MO10.1_SBI, BS10MO10.1_Burg_Staat)
#rm(BOS10MO10.1_Geslacht, BOS10MO10.1_Leeftijd, BOS10MO10.1_HH_Pos, BOS10MO10.1_HH_grootte, BOS10MO10.1_Woonregio_vorig_jaar, 
   #BOS10MO10.1_Nationaliteit, BOS10MO10.1_Geboorteland, BOS10MO10.1_Onderwijsniveau, BOS10MO10.1_Econ_status, 
   #BOS10MO10.1_Beroep, BOS10MO10.1_SBI, BOS10MO10.1_Burg_Staat)

# 10.2 data set 
BS10MO10.2_Geslacht <- Bias(S10MO10.2_Geslacht, Geslacht, "Geslacht")
BOS10MO10.2_Geslacht <- sum(BS10MO10.2_Geslacht$percentageABS)/nrow(BS10MO10.2_Geslacht)

BS10MO10.2_Leeftijd <- Bias(S10MO10.2_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO10.2_Leeftijd <- sum(BS10MO10.2_Leeftijd$percentageABS)/nrow(BS10MO10.2_Leeftijd)

BS10MO10.2_HH_Pos <- Bias(S10MO10.2_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO10.2_HH_Pos <- sum(BS10MO10.2_HH_Pos$percentageABS)/nrow(BS10MO10.2_HH_Pos)

BS10MO10.2_HH_grootte <- Bias(S10MO10.2_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO10.2_HH_grootte <- sum(BS10MO10.2_HH_grootte$percentageABS)/nrow(BS10MO10.2_HH_grootte)

BS10MO10.2_Woonregio_vorig_jaar <- Bias(S10MO10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO10.2_Woonregio_vorig_jaar <- sum(BS10MO10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO10.2_Woonregio_vorig_jaar)

BS10MO10.2_Nationaliteit <- Bias(S10MO10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO10.2_Nationaliteit <- sum(BS10MO10.2_Nationaliteit$percentageABS)/nrow(BS10MO10.2_Nationaliteit)

BS10MO10.2_Geboorteland <- Bias(S10MO10.2_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO10.2_Geboorteland <- sum(BS10MO10.2_Geboorteland$percentageABS)/nrow(BS10MO10.2_Geboorteland)

BS10MO10.2_Onderwijsniveau <- Bias(S10MO10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO10.2_Onderwijsniveau <- sum(BS10MO10.2_Onderwijsniveau$percentageABS)/nrow(BS10MO10.2_Onderwijsniveau)

BS10MO10.2_Econ_status <- Bias(S10MO10.2_Econ_status, Econ_status, "Econ._status")
BOS10MO10.2_Econ_status <- sum(BS10MO10.2_Econ_status$percentageABS)/nrow(BS10MO10.2_Econ_status)

BS10MO10.2_Beroep <- Bias(S10MO10.2_Beroep, Beroep, "Beroep")
BOS10MO10.2_Beroep <- sum(BS10MO10.2_Beroep$percentageABS)/nrow(BS10MO10.2_Beroep)

BS10MO10.2_SBI <- Bias(S10MO10.2_SBI, SBI, "SBI")
BOS10MO10.2_SBI <- sum(BS10MO10.2_SBI$percentageABS)/nrow(BS10MO10.2_SBI)

BS10MO10.2_Burg_Staat <- Bias(S10MO10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO10.2_Burg_Staat <- sum(BS10MO10.2_Burg_Staat$percentageABS)/nrow(BS10MO10.2_Burg_Staat)

BiasOverallS10MO10.2 <- sum(BOS10MO10.2_Geslacht, BOS10MO10.2_Leeftijd, BOS10MO10.2_HH_Pos, BOS10MO10.2_HH_grootte, BOS10MO10.2_Woonregio_vorig_jaar, 
                           BOS10MO10.2_Nationaliteit, BOS10MO10.2_Geboorteland, BOS10MO10.2_Onderwijsniveau, BOS10MO10.2_Econ_status, 
                           BOS10MO10.2_Beroep, BOS10MO10.2_SBI, BOS10MO10.2_Burg_Staat) / 12

#rm(S10MO10.2_Geslacht, S10MO10.2_Leeftijd, S10MO10.2_HH_Pos, S10MO10.2_HH_grootte, S10MO10.2_Woonregio_vorig_jaar, 
   #S10MO10.2_Nationaliteit, S10MO10.2_Geboorteland, S10MO10.2_Onderwijsniveau, S10MO10.2_Econ_status, 
   #S10MO10.2_Beroep, S10MO10.2_SBI, S10MO10.2_Burg_Staat)
#rm(BS10MO10.2_Geslacht, BS10MO10.2_Leeftijd, BS10MO10.2_HH_Pos, BS10MO10.2_HH_grootte, BS10MO10.2_Woonregio_vorig_jaar, 
   #BS10MO10.2_Nationaliteit, BS10MO10.2_Geboorteland, BS10MO10.2_Onderwijsniveau, BS10MO10.2_Econ_status, 
   #BS10MO10.2_Beroep, BS10MO10.2_SBI, BS10MO10.2_Burg_Staat)
#rm(BOS10MO10.2_Geslacht, BOS10MO10.2_Leeftijd, BOS10MO10.2_HH_Pos, BOS10MO10.2_HH_grootte, BOS10MO10.2_Woonregio_vorig_jaar, 
   #BOS10MO10.2_Nationaliteit, BOS10MO10.2_Geboorteland, BOS10MO10.2_Onderwijsniveau, BOS10MO10.2_Econ_status, 
   #BOS10MO10.2_Beroep, BOS10MO10.2_SBI, BOS10MO10.2_Burg_Staat)

# 10.3 data set 
BS10MO10.3_Geslacht <- Bias(S10MO10.3_Geslacht, Geslacht, "Geslacht")
BOS10MO10.3_Geslacht <- sum(BS10MO10.3_Geslacht$percentageABS)/nrow(BS10MO10.3_Geslacht)

BS10MO10.3_Leeftijd <- Bias(S10MO10.3_Leeftijd, Leeftijd, "Leeftijd")
BOS10MO10.3_Leeftijd <- sum(BS10MO10.3_Leeftijd$percentageABS)/nrow(BS10MO10.3_Leeftijd)

BS10MO10.3_HH_Pos <- Bias(S10MO10.3_HH_Pos, HH_Pos, "HH_Pos")
BOS10MO10.3_HH_Pos <- sum(BS10MO10.3_HH_Pos$percentageABS)/nrow(BS10MO10.3_HH_Pos)

BS10MO10.3_HH_grootte <- Bias(S10MO10.3_HH_grootte, HH_grootte, "HH_grootte")
BOS10MO10.3_HH_grootte <- sum(BS10MO10.3_HH_grootte$percentageABS)/nrow(BS10MO10.3_HH_grootte)

BS10MO10.3_Woonregio_vorig_jaar <- Bias(S10MO10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS10MO10.3_Woonregio_vorig_jaar <- sum(BS10MO10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS10MO10.3_Woonregio_vorig_jaar)

BS10MO10.3_Nationaliteit <- Bias(S10MO10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS10MO10.3_Nationaliteit <- sum(BS10MO10.3_Nationaliteit$percentageABS)/nrow(BS10MO10.3_Nationaliteit)

BS10MO10.3_Geboorteland <- Bias(S10MO10.3_Geboorteland, Geboorteland, "Geboorteland")
BOS10MO10.3_Geboorteland <- sum(BS10MO10.3_Geboorteland$percentageABS)/nrow(BS10MO10.3_Geboorteland)

BS10MO10.3_Onderwijsniveau <- Bias(S10MO10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS10MO10.3_Onderwijsniveau <- sum(BS10MO10.3_Onderwijsniveau$percentageABS)/nrow(BS10MO10.3_Onderwijsniveau)

BS10MO10.3_Econ_status <- Bias2(S10MO10.3_Econ_status, Econ_status, "Econ._status")
BOS10MO10.3_Econ_status <- sum(BS10MO10.3_Econ_status$percentageABS)/nrow(BS10MO10.3_Econ_status)

BS10MO10.3_Beroep <- Bias(S10MO10.3_Beroep, Beroep, "Beroep")
BOS10MO10.3_Beroep <- sum(BS10MO10.3_Beroep$percentageABS)/nrow(BS10MO10.3_Beroep)

BS10MO10.3_SBI <- Bias(S10MO10.3_SBI, SBI, "SBI")
BOS10MO10.3_SBI <- sum(BS10MO10.3_SBI$percentageABS)/nrow(BS10MO10.3_SBI)

BS10MO10.3_Burg_Staat <- Bias(S10MO10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS10MO10.3_Burg_Staat <- sum(BS10MO10.3_Burg_Staat$percentageABS)/nrow(BS10MO10.3_Burg_Staat)

BiasOverallS10MO10.3 <- sum(BOS10MO10.3_Geslacht, BOS10MO10.3_Leeftijd, BOS10MO10.3_HH_Pos, BOS10MO10.3_HH_grootte, BOS10MO10.3_Woonregio_vorig_jaar, 
                           BOS10MO10.3_Nationaliteit, BOS10MO10.3_Geboorteland, BOS10MO10.3_Onderwijsniveau, BOS10MO10.3_Econ_status, 
                           BOS10MO10.3_Beroep, BOS10MO10.3_SBI, BOS10MO10.3_Burg_Staat) / 12

#rm(S10MO10.3_Geslacht, S10MO10.3_Leeftijd, S10MO10.3_HH_Pos, S10MO10.3_HH_grootte, S10MO10.3_Woonregio_vorig_jaar, 
   #S10MO10.3_Nationaliteit, S10MO10.3_Geboorteland, S10MO10.3_Onderwijsniveau, S10MO10.3_Econ_status, 
   #S10MO10.3_Beroep, S10MO10.3_SBI, S10MO10.3_Burg_Staat)
#rm(BS10MO10.3_Geslacht, BS10MO10.3_Leeftijd, BS10MO10.3_HH_Pos, BS10MO10.3_HH_grootte, BS10MO10.3_Woonregio_vorig_jaar, 
   #BS10MO10.3_Nationaliteit, BS10MO10.3_Geboorteland, BS10MO10.3_Onderwijsniveau, BS10MO10.3_Econ_status, 
   #BS10MO10.3_Beroep, BS10MO10.3_SBI, BS10MO10.3_Burg_Staat)
#rm(BOS10MO10.3_Geslacht, BOS10MO10.3_Leeftijd, BOS10MO10.3_HH_Pos, BOS10MO10.3_HH_grootte, BOS10MO10.3_Woonregio_vorig_jaar, 
   #BOS10MO10.3_Nationaliteit, BOS10MO10.3_Geboorteland, BOS10MO10.3_Onderwijsniveau, BOS10MO10.3_Econ_status, 
   #BOS10MO10.3_Beroep, BOS10MO10.3_SBI, BOS10MO10.3_Burg_Staat)




