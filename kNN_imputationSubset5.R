
# Installing packages -----------------------------------------------------------------------------------------------------------------
install.packages("bnstruct")
install.packages("readxl")
install.packages("devtools")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("bnstruct")
library("readxl")
library("devtools")
install_github("jabiru/tictoc")
library("tictoc")


## Loading ipums subset ------------------------------------------------------------------------------------------------------------
sub_ipums5 <- get(load(file = "sub_ipums5.Rdata"))

## Turning data set into dataframe -----------------------------------------------------------------------------------------------------
kNN_MCAR2.1 <- as.matrix(MCAR2.1)
kNN_MCAR2.2 <- as.matrix(MCAR2.2)
kNN_MCAR2.3 <- as.matrix(MCAR2.3)
kNN_MCAR5.1 <- as.matrix(MCAR5.1)
kNN_MCAR5.2 <- as.matrix(MCAR5.2)
kNN_MCAR5.3 <- as.matrix(MCAR5.3)
kNN_MCAR10.1 <- as.matrix(MCAR10.1)
kNN_MCAR10.2 <- as.matrix(MCAR10.2)
kNN_MCAR10.3 <- as.matrix(MCAR10.3)


## Giving variables original names and making variables readable -----------------------------------------------------------------------
names(kNN_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
names(kNN_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)



# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

# Setting correct and total to '0' 
S5kNN2.1_correct <- S5kNN2.2_correct <- S5kNN2.3_correct <- S5kNN5.1_correct <- S5kNN5.2_correct <- S5kNN5.3_correct <- S5kNN10.1_correct <- S5kNN10.2_correct <- S5kNN10.3_correct <- 0
S5kNN2.1_total <- S5kNN2.2_total <- S5kNN2.3_total <- S5kNN5.1_total <- S5kNN5.2_total <- S5kNN5.3_total <- S5kNN10.1_total <- S5kNN10.2_total <- S5kNN10.3_total <- 0


## k-Nearest Neighbor Imputation with 'bnstruct' --------------------------------------------------------------------------------------
set.seed(35)

# 2% data sets

# 2.1 data set
sub5_MCAR2.1 <- get(load(file = "sub5_mcar21.Rdata"))
S5_kNNMCAR2.1 <- as.matrix(sub5_MCAR2.1)
names(S5_kNNMCAR2.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 2.1 processing time...")
S5_kNN2.1 <- knn.impute(S5_kNNMCAR2.1, k = 10) 
toc(log = TRUE)
S5df_kNN2.1 <- as.data.frame(S5_kNN2.1)
save(S5df_kNN2.1, file = "S5_kNN21.Rdata")
S5kNN2.1_correct <- S5kNN2.1_correct + sum(sub_ipums5 == S5df_kNN2.1)
S5kNN2.1_total <- S5kNN2.1_total + sum(!is.na(S5df_kNN2.1))

#2.2 data set
sub5_MCAR2.2 <- get(load(file = "sub5_mcar22.Rdata"))
S5_kNNMCAR2.2 <- as.matrix(sub5_MCAR2.2)
names(S5_kNNMCAR2.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 2.2 processing time...")
S5_kNN2.2 <- knn.impute(S5_kNNMCAR2.2, k = 10) 
toc(log = TRUE)
S5df_kNN2.2 <- as.data.frame(S5_kNN2.2)
save(S5df_kNN2.2, file = "S5_kNN22.Rdata")
S5kNN2.2_correct <- S5kNN2.2_correct + sum(sub_ipums5 == S5df_kNN2.2)
S5kNN2.2_total <- S5kNN2.2_total + sum(!is.na(S5df_kNN2.2))

# 2.3 data set
sub5_MCAR2.3 <- get(load(file = "sub5_mcar23.Rdata"))
S5_kNNMCAR2.3 <- as.matrix(sub5_MCAR2.3)
names(S5_kNNMCAR2.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 2.3 processing time...")
S5_kNN2.3 <- knn.impute(S5_kNNMCAR2.3, k = 10) 
toc(log = TRUE)
S5df_kNN2.3 <- as.data.frame(S5_kNN2.3)
save(S5df_kNN2.3, file = "S5_kNN23.Rdata")
S5kNN2.3_correct <- S5kNN2.3_correct + sum(sub_ipums5 == S5df_kNN2.3)
S5kNN2.3_total <- S5kNN2.3_total + sum(!is.na(S5df_kNN2.3))


# 5% data sets

#5.1 data set
sub5_MCAR5.1 <- get(load(file = "sub5_mcar51.Rdata"))
S5_kNNMCAR5.1 <- as.matrix(sub5_MCAR5.1)
names(S5_kNNMCAR5.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 5.1 processing time...")
S5_kNN5.1 <- knn.impute(S5_kNNMCAR5.1, k = 10) 
toc(log = TRUE)
S5df_kNN5.1 <- as.data.frame(S5_kNN5.1)
save(S5df_kNN5.1, file = "S5_kNN51.Rdata")
S5kNN5.1_correct <- S5kNN5.1_correct + sum(sub_ipums5 == S5df_kNN5.1)
S5kNN5.1_total <- S5kNN5.1_total + sum(!is.na(S5df_kNN5.1))

# 5.2 data set
sub5_MCAR5.2 <- get(load(file = "sub5_mcar52.Rdata"))
S5_kNNMCAR5.2 <- as.matrix(sub5_MCAR5.2)
names(S5_kNNMCAR5.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 5.2 processing time...")
S5_kNN5.2 <- knn.impute(S5_kNNMCAR5.2, k = 10) 
toc(log = TRUE)
S5df_kNN5.2 <- as.data.frame(S5_kNN5.2)
save(S5df_kNN5.2, file = "S5_kNN52.Rdata")
S5kNN5.2_correct <- S5kNN5.2_correct + sum(sub_ipums5 == S5df_kNN5.2)
S5kNN5.2_total <- S5kNN5.2_total + sum(!is.na(S5df_kNN5.2))

# 5.3 data set
sub5_MCAR5.3 <- get(load(file = "sub5_mcar53.Rdata"))
S5_kNNMCAR5.3 <- as.matrix(sub5_MCAR5.3)
names(S5_kNNMCAR5.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 5.3 processing time...")
S5_kNN5.3 <- knn.impute(S5_kNNMCAR5.3, k = 10) 
toc(log = TRUE)
S5df_kNN5.3 <- as.data.frame(S5_kNN5.3)
save(S5df_kNN5.3, file = "S5_kNN53.Rdata")
S5kNN5.3_correct <- S5kNN5.3_correct + sum(sub_ipums5 == S5df_kNN5.3)
S5kNN5.3_total <- S5kNN5.3_total + sum(!is.na(S5df_kNN5.3))


# 10% data sets 

# 10.1 data set
sub5_MCAR10.1 <- get(load(file = "sub5_mcar101.Rdata"))
S5_kNNMCAR10.1 <- as.matrix(sub5_MCAR10.1)
names(S5_kNNMCAR10.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 10.1 processing time...")
S5_kNN10.1 <- knn.impute(S5_kNNMCAR10.1, k = 10) 
toc(log = TRUE)
S5df_kNN10.1 <- as.data.frame(S5_kNN10.1)
save(S5df_kNN10.1, file = "S5_kNN101.Rdata")
S5kNN10.1_correct <- S5kNN10.1_correct + sum(sub_ipums5 == S5df_kNN10.1)
S5kNN10.1_total <- S5kNN10.1_total + sum(!is.na(S5df_kNN10.1))

#10.2 data set
sub5_MCAR10.2 <- get(load(file = "sub5_mcar102.Rdata"))
S5_kNNMCAR10.2 <- as.matrix(sub5_MCAR10.2)
names(S5_kNNMCAR10.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 10.2 processing time...")
S5_kNN10.2 <- knn.impute(S5_kNNMCAR10.2, k = 10) 
toc(log = TRUE)
S5df_kNN10.2 <- as.data.frame(S5_kNN10.2)
save(S5df_kNN10.2, file = "S5_kNN102.Rdata")
S5kNN10.2_correct <- S5kNN10.2_correct + sum(sub_ipums5 == S5df_kNN10.2)
S5kNN10.2_total <- S5kNN10.2_total + sum(!is.na(S5df_kNN10.2))

#10.3 data set
sub5_MCAR10.3 <- get(load(file = "sub5_mcar103.Rdata"))
S5_kNNMCAR10.3 <- as.matrix(sub5_MCAR10.3)
names(S5_kNNMCAR10.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
tic("kNN 10.3 processing time...")
S5_kNN10.3 <- knn.impute(S5_kNNMCAR10.3, k = 10) 
toc(log = TRUE)
S5df_kNN10.3 <- as.data.frame(S5_kNN10.3)
save(S5df_kNN10.3, file = "S5_kNN103.Rdata")
S5kNN10.3_correct <- S5kNN10.3_correct + sum(sub_ipums5 == S5df_kNN10.3)
S5kNN10.3_total <- S5kNN10.3_total + sum(!is.na(S5df_kNN10.3))


# Check if all values are imputed 
#anyNA(c(df_kNN2.1, df_kNN2.2, df_kNN2.3, df_kNN5.1, df_kNN5.2, 
        #df_kNN5.3, df_kNN10.1, df_kNN10.2, df_kNN10.3))


## Computing accuracy ---------------------------------------------------------------------------------------------------------------------------------


# Computing the accuracy of imputation 
S5kNN2.1_accuracy <- S5kNN2.1_correct / S5kNN2.1_total
S5kNN2.2_accuracy <- S5kNN2.2_correct / S5kNN2.2_total
S5kNN2.3_accuracy <- S5kNN2.3_correct / S5kNN2.3_total

S5kNN5.1_accuracy <- S5kNN5.1_correct / S5kNN5.1_total
S5kNN5.2_accuracy <- S5kNN5.2_correct / S5kNN5.2_total
S5kNN5.3_accuracy <- S5kNN5.3_correct / S5kNN5.3_total

S5kNN10.1_accuracy <- S5kNN10.1_correct / S5kNN10.1_total
S5kNN10.2_accuracy <- S5kNN10.2_correct / S5kNN10.2_total
S5kNN10.3_accuracy <- S5kNN10.3_correct / S5kNN10.3_total


## Computing Bias -------------------------------------------------------------------------------------------------------


## Change names kNN imputation data frame
names(S5df_kNN2.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN2.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN2.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN5.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN5.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN5.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN10.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN10.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
names(S5df_kNN10.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)


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
kNN2.1_Geslacht <- plyr::count(S5df_kNN2.1, 'Geslacht')
kNN2.1_Leeftijd <- plyr::count(S5df_kNN2.1, 'Leeftijd')
kNN2.1_HH_Pos <- plyr::count(S5df_kNN2.1, 'HH_Pos')
kNN2.1_HH_grootte <- plyr::count(S5df_kNN2.1, 'HH_grootte')
kNN2.1_Woonregio_vorig_jaar <- plyr::count(S5df_kNN2.1, 'Woonregio_vorig_jaar')
kNN2.1_Nationaliteit <- plyr::count(S5df_kNN2.1, 'Nationaliteit')
kNN2.1_Geboorteland <- plyr::count(S5df_kNN2.1, 'Geboorteland')
kNN2.1_Onderwijsniveau <- plyr::count(S5df_kNN2.1, 'Onderwijsniveau')
kNN2.1_Econ_status <- plyr::count(S5df_kNN2.1, 'Econ._status')
kNN2.1_Beroep <- plyr::count(S5df_kNN2.1, 'Beroep')
kNN2.1_SBI <- plyr::count(S5df_kNN2.1, 'SBI')
kNN2.1_Burg_Staat <- plyr::count(S5df_kNN2.1, 'Burg._Staat')

kNN2.2_Geslacht <- plyr::count(S5df_kNN2.2, 'Geslacht')
kNN2.2_Leeftijd <- plyr::count(S5df_kNN2.2, 'Leeftijd')
kNN2.2_HH_Pos <- plyr::count(S5df_kNN2.2, 'HH_Pos')
kNN2.2_HH_grootte <- plyr::count(S5df_kNN2.2, 'HH_grootte')
kNN2.2_Woonregio_vorig_jaar <- plyr::count(S5df_kNN2.2, 'Woonregio_vorig_jaar')
kNN2.2_Nationaliteit <- plyr::count(S5df_kNN2.2, 'Nationaliteit')
kNN2.2_Geboorteland <- plyr::count(S5df_kNN2.2, 'Geboorteland')
kNN2.2_Onderwijsniveau <- plyr::count(S5df_kNN2.2, 'Onderwijsniveau')
kNN2.2_Econ_status <- plyr::count(S5df_kNN2.2, 'Econ._status')
kNN2.2_Beroep <- plyr::count(S5df_kNN2.2, 'Beroep')
kNN2.2_SBI <- plyr::count(S5df_kNN2.2, 'SBI')
kNN2.2_Burg_Staat <- plyr::count(S5df_kNN2.2, 'Burg._Staat')

kNN2.3_Geslacht <- plyr::count(S5df_kNN2.3, 'Geslacht')
kNN2.3_Leeftijd <- plyr::count(S5df_kNN2.3, 'Leeftijd')
kNN2.3_HH_Pos <- plyr::count(S5df_kNN2.3, 'HH_Pos')
kNN2.3_HH_grootte <- plyr::count(S5df_kNN2.3, 'HH_grootte')
kNN2.3_Woonregio_vorig_jaar <- plyr::count(S5df_kNN2.3, 'Woonregio_vorig_jaar')
kNN2.3_Nationaliteit <- plyr::count(S5df_kNN2.3, 'Nationaliteit')
kNN2.3_Geboorteland <- plyr::count(S5df_kNN2.3, 'Geboorteland')
kNN2.3_Onderwijsniveau <- plyr::count(S5df_kNN2.3, 'Onderwijsniveau')
kNN2.3_Econ_status <- plyr::count(S5df_kNN2.3, 'Econ._status')
kNN2.3_Beroep <- plyr::count(S5df_kNN2.3, 'Beroep')
kNN2.3_SBI <- plyr::count(S5df_kNN2.3, 'SBI')
kNN2.3_Burg_Staat <- plyr::count(S5df_kNN2.3, 'Burg._Staat')

rm(S5df_kNN2.1, S5df_kNN2.2, S5df_kNN2.3)

# 2.1 data set 
BkNN2.1_Geslacht <- Bias(kNN2.1_Geslacht, Geslacht, "Geslacht")
BOkNN2.1_Geslacht <- sum(BkNN2.1_Geslacht$percentageABS)/nrow(BkNN2.1_Geslacht)

BkNN2.1_Leeftijd <- Bias(kNN2.1_Leeftijd, Leeftijd, "Leeftijd")
BOkNN2.1_Leeftijd <- sum(BkNN2.1_Leeftijd$percentageABS)/nrow(BkNN2.1_Leeftijd)

BkNN2.1_HH_Pos <- Bias(kNN2.1_HH_Pos, HH_Pos, "HH_Pos")
BOkNN2.1_HH_Pos <- sum(BkNN2.1_HH_Pos$percentageABS)/nrow(BkNN2.1_HH_Pos)

BkNN2.1_HH_grootte <- Bias(kNN2.1_HH_grootte, HH_grootte, "HH_grootte")
BOkNN2.1_HH_grootte <- sum(BkNN2.1_HH_grootte$percentageABS)/nrow(BkNN2.1_HH_grootte)

BkNN2.1_Woonregio_vorig_jaar <- Bias(kNN2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN2.1_Woonregio_vorig_jaar <- sum(BkNN2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN2.1_Woonregio_vorig_jaar)

BkNN2.1_Nationaliteit <- Bias(kNN2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN2.1_Nationaliteit <- sum(BkNN2.1_Nationaliteit$percentageABS)/nrow(BkNN2.1_Nationaliteit)

BkNN2.1_Geboorteland <- Bias(kNN2.1_Geboorteland, Geboorteland, "Geboorteland")
BOkNN2.1_Geboorteland <- sum(BkNN2.1_Geboorteland$percentageABS)/nrow(BkNN2.1_Geboorteland)

BkNN2.1_Onderwijsniveau <- Bias(kNN2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN2.1_Onderwijsniveau <- sum(BkNN2.1_Onderwijsniveau$percentageABS)/nrow(BkNN2.1_Onderwijsniveau)

BkNN2.1_Econ_status <- Bias(kNN2.1_Econ_status, Econ_status, "Econ._status")
BOkNN2.1_Econ_status <- sum(BkNN2.1_Econ_status$percentageABS)/nrow(BkNN2.1_Econ_status)

BkNN2.1_Beroep <- Bias(kNN2.1_Beroep, Beroep, "Beroep")
BOkNN2.1_Beroep <- sum(BkNN2.1_Beroep$percentageABS)/nrow(BkNN2.1_Beroep)

BkNN2.1_SBI <- Bias(kNN2.1_SBI, SBI, "SBI")
BOkNN2.1_SBI <- sum(BkNN2.1_SBI$percentageABS)/nrow(BkNN2.1_SBI)

BkNN2.1_Burg_Staat <- Bias(kNN2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN2.1_Burg_Staat <- sum(BkNN2.1_Burg_Staat$percentageABS)/nrow(BkNN2.1_Burg_Staat)

BiasOverallkNN2.1 <- sum(BOkNN2.1_Geslacht, BOkNN2.1_Leeftijd, BOkNN2.1_HH_Pos, BOkNN2.1_HH_grootte, BOkNN2.1_Woonregio_vorig_jaar, 
                         BOkNN2.1_Nationaliteit, BOkNN2.1_Geboorteland, BOkNN2.1_Onderwijsniveau, BOkNN2.1_Econ_status, 
                         BOkNN2.1_Beroep, BOkNN2.1_SBI, BOkNN2.1_Burg_Staat) / 12

rm(kNN2.1_Geslacht, kNN2.1_Leeftijd, kNN2.1_HH_Pos, kNN2.1_HH_grootte, kNN2.1_Woonregio_vorig_jaar, 
   kNN2.1_Nationaliteit, kNN2.1_Geboorteland, kNN2.1_Onderwijsniveau, kNN2.1_Econ_status, 
   kNN2.1_Beroep, kNN2.1_SBI, kNN2.1_Burg_Staat)
rm(BkNN2.1_Geslacht, BkNN2.1_Leeftijd, BkNN2.1_HH_Pos, BkNN2.1_HH_grootte, BkNN2.1_Woonregio_vorig_jaar, 
   BkNN2.1_Nationaliteit, BkNN2.1_Geboorteland, BkNN2.1_Onderwijsniveau, BkNN2.1_Econ_status, 
   BkNN2.1_Beroep, BkNN2.1_SBI, BkNN2.1_Burg_Staat)
rm(BOkNN2.1_Geslacht, BOkNN2.1_Leeftijd, BOkNN2.1_HH_Pos, BOkNN2.1_HH_grootte, BOkNN2.1_Woonregio_vorig_jaar, 
   BOkNN2.1_Nationaliteit, BOkNN2.1_Geboorteland, BOkNN2.1_Onderwijsniveau, BOkNN2.1_Econ_status, 
   BOkNN2.1_Beroep, BOkNN2.1_SBI, BOkNN2.1_Burg_Staat)

# 2.2 data set 
BkNN2.2_Geslacht <- Bias(kNN2.2_Geslacht, Geslacht, "Geslacht")
BOkNN2.2_Geslacht <- sum(BkNN2.2_Geslacht$percentageABS)/nrow(BkNN2.2_Geslacht)

BkNN2.2_Leeftijd <- Bias(kNN2.2_Leeftijd, Leeftijd, "Leeftijd")
BOkNN2.2_Leeftijd <- sum(BkNN2.2_Leeftijd$percentageABS)/nrow(BkNN2.2_Leeftijd)

BkNN2.2_HH_Pos <- Bias(kNN2.2_HH_Pos, HH_Pos, "HH_Pos")
BOkNN2.2_HH_Pos <- sum(BkNN2.2_HH_Pos$percentageABS)/nrow(BkNN2.2_HH_Pos)

BkNN2.2_HH_grootte <- Bias(kNN2.2_HH_grootte, HH_grootte, "HH_grootte")
BOkNN2.2_HH_grootte <- sum(BkNN2.2_HH_grootte$percentageABS)/nrow(BkNN2.2_HH_grootte)

BkNN2.2_Woonregio_vorig_jaar <- Bias(kNN2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN2.2_Woonregio_vorig_jaar <- sum(BkNN2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN2.2_Woonregio_vorig_jaar)

BkNN2.2_Nationaliteit <- Bias(kNN2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN2.2_Nationaliteit <- sum(BkNN2.2_Nationaliteit$percentageABS)/nrow(BkNN2.2_Nationaliteit)

BkNN2.2_Geboorteland <- Bias(kNN2.2_Geboorteland, Geboorteland, "Geboorteland")
BOkNN2.2_Geboorteland <- sum(BkNN2.2_Geboorteland$percentageABS)/nrow(BkNN2.2_Geboorteland)

BkNN2.2_Onderwijsniveau <- Bias(kNN2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN2.2_Onderwijsniveau <- sum(BkNN2.2_Onderwijsniveau$percentageABS)/nrow(BkNN2.2_Onderwijsniveau)

BkNN2.2_Econ_status <- Bias(kNN2.2_Econ_status, Econ_status, "Econ._status")
BOkNN2.2_Econ_status <- sum(BkNN2.2_Econ_status$percentageABS)/nrow(BkNN2.2_Econ_status)

BkNN2.2_Beroep <- Bias(kNN2.2_Beroep, Beroep, "Beroep")
BOkNN2.2_Beroep <- sum(BkNN2.2_Beroep$percentageABS)/nrow(BkNN2.2_Beroep)

BkNN2.2_SBI <- Bias(kNN2.2_SBI, SBI, "SBI")
BOkNN2.2_SBI <- sum(BkNN2.2_SBI$percentageABS)/nrow(BkNN2.2_SBI)

BkNN2.2_Burg_Staat <- Bias(kNN2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN2.2_Burg_Staat <- sum(BkNN2.2_Burg_Staat$percentageABS)/nrow(BkNN2.2_Burg_Staat)

BiasOverallkNN2.2 <- sum(BOkNN2.2_Geslacht, BOkNN2.2_Leeftijd, BOkNN2.2_HH_Pos, BOkNN2.2_HH_grootte, BOkNN2.2_Woonregio_vorig_jaar, 
                         BOkNN2.2_Nationaliteit, BOkNN2.2_Geboorteland, BOkNN2.2_Onderwijsniveau, BOkNN2.2_Econ_status, 
                         BOkNN2.2_Beroep, BOkNN2.2_SBI, BOkNN2.2_Burg_Staat) / 12

rm(kNN2.2_Geslacht, kNN2.2_Leeftijd, kNN2.2_HH_Pos, kNN2.2_HH_grootte, kNN2.2_Woonregio_vorig_jaar, 
   kNN2.2_Nationaliteit, kNN2.2_Geboorteland, kNN2.2_Onderwijsniveau, kNN2.2_Econ_status, 
   kNN2.2_Beroep, kNN2.2_SBI, kNN2.2_Burg_Staat)
rm(BkNN2.2_Geslacht, BkNN2.2_Leeftijd, BkNN2.2_HH_Pos, BkNN2.2_HH_grootte, BkNN2.2_Woonregio_vorig_jaar, 
   BkNN2.2_Nationaliteit, BkNN2.2_Geboorteland, BkNN2.2_Onderwijsniveau, BkNN2.2_Econ_status, 
   BkNN2.2_Beroep, BkNN2.2_SBI, BkNN2.2_Burg_Staat)
rm(BOkNN2.2_Geslacht, BOkNN2.2_Leeftijd, BOkNN2.2_HH_Pos, BOkNN2.2_HH_grootte, BOkNN2.2_Woonregio_vorig_jaar, 
   BOkNN2.2_Nationaliteit, BOkNN2.2_Geboorteland, BOkNN2.2_Onderwijsniveau, BOkNN2.2_Econ_status, 
   BOkNN2.2_Beroep, BOkNN2.2_SBI, BOkNN2.2_Burg_Staat)

# 2.3 data set 
BkNN2.3_Geslacht <- Bias(kNN2.3_Geslacht, Geslacht, "Geslacht")
BOkNN2.3_Geslacht <- sum(BkNN2.3_Geslacht$percentageABS)/nrow(BkNN2.3_Geslacht)

BkNN2.3_Leeftijd <- Bias(kNN2.3_Leeftijd, Leeftijd, "Leeftijd")
BOkNN2.3_Leeftijd <- sum(BkNN2.3_Leeftijd$percentageABS)/nrow(BkNN2.3_Leeftijd)

BkNN2.3_HH_Pos <- Bias(kNN2.3_HH_Pos, HH_Pos, "HH_Pos")
BOkNN2.3_HH_Pos <- sum(BkNN2.3_HH_Pos$percentageABS)/nrow(BkNN2.3_HH_Pos)

BkNN2.3_HH_grootte <- Bias(kNN2.3_HH_grootte, HH_grootte, "HH_grootte")
BOkNN2.3_HH_grootte <- sum(BkNN2.3_HH_grootte$percentageABS)/nrow(BkNN2.3_HH_grootte)

BkNN2.3_Woonregio_vorig_jaar <- Bias(kNN2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN2.3_Woonregio_vorig_jaar <- sum(BkNN2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN2.3_Woonregio_vorig_jaar)

BkNN2.3_Nationaliteit <- Bias(kNN2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN2.3_Nationaliteit <- sum(BkNN2.3_Nationaliteit$percentageABS)/nrow(BkNN2.3_Nationaliteit)

BkNN2.3_Geboorteland <- Bias(kNN2.3_Geboorteland, Geboorteland, "Geboorteland")
BOkNN2.3_Geboorteland <- sum(BkNN2.3_Geboorteland$percentageABS)/nrow(BkNN2.3_Geboorteland)

BkNN2.3_Onderwijsniveau <- Bias(kNN2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN2.3_Onderwijsniveau <- sum(BkNN2.3_Onderwijsniveau$percentageABS)/nrow(BkNN2.3_Onderwijsniveau)

BkNN2.3_Econ_status <- Bias(kNN2.3_Econ_status, Econ_status, "Econ._status")
BOkNN2.3_Econ_status <- sum(BkNN2.3_Econ_status$percentageABS)/nrow(BkNN2.3_Econ_status)

BkNN2.3_Beroep <- Bias(kNN2.3_Beroep, Beroep, "Beroep")
BOkNN2.3_Beroep <- sum(BkNN2.3_Beroep$percentageABS)/nrow(BkNN2.3_Beroep)

BkNN2.3_SBI <- Bias(kNN2.3_SBI, SBI, "SBI")
BOkNN2.3_SBI <- sum(BkNN2.3_SBI$percentageABS)/nrow(BkNN2.3_SBI)

BkNN2.3_Burg_Staat <- Bias(kNN2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN2.3_Burg_Staat <- sum(BkNN2.3_Burg_Staat$percentageABS)/nrow(BkNN2.3_Burg_Staat)

BiasOverallkNN2.3 <- sum(BOkNN2.3_Geslacht, BOkNN2.3_Leeftijd, BOkNN2.3_HH_Pos, BOkNN2.3_HH_grootte, BOkNN2.3_Woonregio_vorig_jaar, 
                         BOkNN2.3_Nationaliteit, BOkNN2.3_Geboorteland, BOkNN2.3_Onderwijsniveau, BOkNN2.3_Econ_status, 
                         BOkNN2.3_Beroep, BOkNN2.3_SBI, BOkNN2.3_Burg_Staat) / 12

rm(kNN2.3_Geslacht, kNN2.3_Leeftijd, kNN2.3_HH_Pos, kNN2.3_HH_grootte, kNN2.3_Woonregio_vorig_jaar, 
   kNN2.3_Nationaliteit, kNN2.3_Geboorteland, kNN2.3_Onderwijsniveau, kNN2.3_Econ_status, 
   kNN2.3_Beroep, kNN2.3_SBI, kNN2.3_Burg_Staat)
rm(BkNN2.3_Geslacht, BkNN2.3_Leeftijd, BkNN2.3_HH_Pos, BkNN2.3_HH_grootte, BkNN2.3_Woonregio_vorig_jaar, 
   BkNN2.3_Nationaliteit, BkNN2.3_Geboorteland, BkNN2.3_Onderwijsniveau, BkNN2.3_Econ_status, 
   BkNN2.3_Beroep, BkNN2.3_SBI, BkNN2.3_Burg_Staat)
rm(BOkNN2.3_Geslacht, BOkNN2.3_Leeftijd, BOkNN2.3_HH_Pos, BOkNN2.3_HH_grootte, BOkNN2.3_Woonregio_vorig_jaar, 
   BOkNN2.3_Nationaliteit, BOkNN2.3_Geboorteland, BOkNN2.3_Onderwijsniveau, BOkNN2.3_Econ_status, 
   BOkNN2.3_Beroep, BOkNN2.3_SBI, BOkNN2.3_Burg_Staat)


# 5% data sets

# Counting values
kNN5.1_Geslacht <- plyr::count(S5df_kNN5.1, 'Geslacht')
kNN5.1_Leeftijd <- plyr::count(S5df_kNN5.1, 'Leeftijd')
kNN5.1_HH_Pos <- plyr::count(S5df_kNN5.1, 'HH_Pos')
kNN5.1_HH_grootte <- plyr::count(S5df_kNN5.1, 'HH_grootte')
kNN5.1_Woonregio_vorig_jaar <- plyr::count(S5df_kNN5.1, 'Woonregio_vorig_jaar')
kNN5.1_Nationaliteit <- plyr::count(S5df_kNN5.1, 'Nationaliteit')
kNN5.1_Geboorteland <- plyr::count(S5df_kNN5.1, 'Geboorteland')
kNN5.1_Onderwijsniveau <- plyr::count(S5df_kNN5.1, 'Onderwijsniveau')
kNN5.1_Econ_status <- plyr::count(S5df_kNN5.1, 'Econ._status')
kNN5.1_Beroep <- plyr::count(S5df_kNN5.1, 'Beroep')
kNN5.1_SBI <- plyr::count(S5df_kNN5.1, 'SBI')
kNN5.1_Burg_Staat <- plyr::count(S5df_kNN5.1, 'Burg._Staat')

kNN5.2_Geslacht <- plyr::count(S5df_kNN5.2, 'Geslacht')
kNN5.2_Leeftijd <- plyr::count(S5df_kNN5.2, 'Leeftijd')
kNN5.2_HH_Pos <- plyr::count(S5df_kNN5.2, 'HH_Pos')
kNN5.2_HH_grootte <- plyr::count(S5df_kNN5.2, 'HH_grootte')
kNN5.2_Woonregio_vorig_jaar <- plyr::count(S5df_kNN5.2, 'Woonregio_vorig_jaar')
kNN5.2_Nationaliteit <- plyr::count(S5df_kNN5.2, 'Nationaliteit')
kNN5.2_Geboorteland <- plyr::count(S5df_kNN5.2, 'Geboorteland')
kNN5.2_Onderwijsniveau <- plyr::count(S5df_kNN5.2, 'Onderwijsniveau')
kNN5.2_Econ_status <- plyr::count(S5df_kNN5.2, 'Econ._status')
kNN5.2_Beroep <- plyr::count(S5df_kNN5.2, 'Beroep')
kNN5.2_SBI <- plyr::count(S5df_kNN5.2, 'SBI')
kNN5.2_Burg_Staat <- plyr::count(S5df_kNN5.2, 'Burg._Staat')

kNN5.3_Geslacht <- plyr::count(S5df_kNN5.3, 'Geslacht')
kNN5.3_Leeftijd <- plyr::count(S5df_kNN5.3, 'Leeftijd')
kNN5.3_HH_Pos <- plyr::count(S5df_kNN5.3, 'HH_Pos')
kNN5.3_HH_grootte <- plyr::count(S5df_kNN5.3, 'HH_grootte')
kNN5.3_Woonregio_vorig_jaar <- plyr::count(S5df_kNN5.3, 'Woonregio_vorig_jaar')
kNN5.3_Nationaliteit <- plyr::count(S5df_kNN5.3, 'Nationaliteit')
kNN5.3_Geboorteland <- plyr::count(S5df_kNN5.3, 'Geboorteland')
kNN5.3_Onderwijsniveau <- plyr::count(S5df_kNN5.3, 'Onderwijsniveau')
kNN5.3_Econ_status <- plyr::count(S5df_kNN5.3, 'Econ._status')
kNN5.3_Beroep <- plyr::count(S5df_kNN5.3, 'Beroep')
kNN5.3_SBI <- plyr::count(S5df_kNN5.3, 'SBI')
kNN5.3_Burg_Staat <- plyr::count(S5df_kNN5.3, 'Burg._Staat')

rm(S5df_kNN5.1, S5df_kNN5.2, S5df_kNN5.3)


# 5.1 data set 
BkNN5.1_Geslacht <- Bias(kNN5.1_Geslacht, Geslacht, "Geslacht")
BOkNN5.1_Geslacht <- sum(BkNN5.1_Geslacht$percentageABS)/nrow(BkNN5.1_Geslacht)

BkNN5.1_Leeftijd <- Bias(kNN5.1_Leeftijd, Leeftijd, "Leeftijd")
BOkNN5.1_Leeftijd <- sum(BkNN5.1_Leeftijd$percentageABS)/nrow(BkNN5.1_Leeftijd)

BkNN5.1_HH_Pos <- Bias(kNN5.1_HH_Pos, HH_Pos, "HH_Pos")
BOkNN5.1_HH_Pos <- sum(BkNN5.1_HH_Pos$percentageABS)/nrow(BkNN5.1_HH_Pos)

BkNN5.1_HH_grootte <- Bias(kNN5.1_HH_grootte, HH_grootte, "HH_grootte")
BOkNN5.1_HH_grootte <- sum(BkNN5.1_HH_grootte$percentageABS)/nrow(BkNN5.1_HH_grootte)

BkNN5.1_Woonregio_vorig_jaar <- Bias(kNN5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN5.1_Woonregio_vorig_jaar <- sum(BkNN5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN5.1_Woonregio_vorig_jaar)

BkNN5.1_Nationaliteit <- Bias(kNN5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN5.1_Nationaliteit <- sum(BkNN5.1_Nationaliteit$percentageABS)/nrow(BkNN5.1_Nationaliteit)

BkNN5.1_Geboorteland <- Bias(kNN5.1_Geboorteland, Geboorteland, "Geboorteland")
BOkNN5.1_Geboorteland <- sum(BkNN5.1_Geboorteland$percentageABS)/nrow(BkNN5.1_Geboorteland)

BkNN5.1_Onderwijsniveau <- Bias(kNN5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN5.1_Onderwijsniveau <- sum(BkNN5.1_Onderwijsniveau$percentageABS)/nrow(BkNN5.1_Onderwijsniveau)

BkNN5.1_Econ_status <- Bias(kNN5.1_Econ_status, Econ_status, "Econ._status")
BOkNN5.1_Econ_status <- sum(BkNN5.1_Econ_status$percentageABS)/nrow(BkNN5.1_Econ_status)

BkNN5.1_Beroep <- Bias(kNN5.1_Beroep, Beroep, "Beroep")
BOkNN5.1_Beroep <- sum(BkNN5.1_Beroep$percentageABS)/nrow(BkNN5.1_Beroep)

BkNN5.1_SBI <- Bias(kNN5.1_SBI, SBI, "SBI")
BOkNN5.1_SBI <- sum(BkNN5.1_SBI$percentageABS)/nrow(BkNN5.1_SBI)

BkNN5.1_Burg_Staat <- Bias(kNN5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN5.1_Burg_Staat <- sum(BkNN5.1_Burg_Staat$percentageABS)/nrow(BkNN5.1_Burg_Staat)

BiasOverallkNN5.1 <- sum(BOkNN5.1_Geslacht, BOkNN5.1_Leeftijd, BOkNN5.1_HH_Pos, BOkNN5.1_HH_grootte, BOkNN5.1_Woonregio_vorig_jaar, 
                         BOkNN5.1_Nationaliteit, BOkNN5.1_Geboorteland, BOkNN5.1_Onderwijsniveau, BOkNN5.1_Econ_status, 
                         BOkNN5.1_Beroep, BOkNN5.1_SBI, BOkNN5.1_Burg_Staat) / 12

rm(kNN5.1_Geslacht, kNN5.1_Leeftijd, kNN5.1_HH_Pos, kNN5.1_HH_grootte, kNN5.1_Woonregio_vorig_jaar, 
   kNN5.1_Nationaliteit, kNN5.1_Geboorteland, kNN5.1_Onderwijsniveau, kNN5.1_Econ_status, 
   kNN5.1_Beroep, kNN5.1_SBI, kNN5.1_Burg_Staat)
rm(BkNN5.1_Geslacht, BkNN5.1_Leeftijd, BkNN5.1_HH_Pos, BkNN5.1_HH_grootte, BkNN5.1_Woonregio_vorig_jaar, 
   BkNN5.1_Nationaliteit, BkNN5.1_Geboorteland, BkNN5.1_Onderwijsniveau, BkNN5.1_Econ_status, 
   BkNN5.1_Beroep, BkNN5.1_SBI, BkNN5.1_Burg_Staat)
rm(BOkNN5.1_Geslacht, BOkNN5.1_Leeftijd, BOkNN5.1_HH_Pos, BOkNN5.1_HH_grootte, BOkNN5.1_Woonregio_vorig_jaar, 
   BOkNN5.1_Nationaliteit, BOkNN5.1_Geboorteland, BOkNN5.1_Onderwijsniveau, BOkNN5.1_Econ_status, 
   BOkNN5.1_Beroep, BOkNN5.1_SBI, BOkNN5.1_Burg_Staat)

# 5.2 data set 
BkNN5.2_Geslacht <- Bias(kNN5.2_Geslacht, Geslacht, "Geslacht")
BOkNN5.2_Geslacht <- sum(BkNN5.2_Geslacht$percentageABS)/nrow(BkNN5.2_Geslacht)

BkNN5.2_Leeftijd <- Bias(kNN5.2_Leeftijd, Leeftijd, "Leeftijd")
BOkNN5.2_Leeftijd <- sum(BkNN5.2_Leeftijd$percentageABS)/nrow(BkNN5.2_Leeftijd)

BkNN5.2_HH_Pos <- Bias(kNN5.2_HH_Pos, HH_Pos, "HH_Pos")
BOkNN5.2_HH_Pos <- sum(BkNN5.2_HH_Pos$percentageABS)/nrow(BkNN5.2_HH_Pos)

BkNN5.2_HH_grootte <- Bias(kNN5.2_HH_grootte, HH_grootte, "HH_grootte")
BOkNN5.2_HH_grootte <- sum(BkNN5.2_HH_grootte$percentageABS)/nrow(BkNN5.2_HH_grootte)

BkNN5.2_Woonregio_vorig_jaar <- Bias(kNN5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN5.2_Woonregio_vorig_jaar <- sum(BkNN5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN5.2_Woonregio_vorig_jaar)

BkNN5.2_Nationaliteit <- Bias(kNN5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN5.2_Nationaliteit <- sum(BkNN5.2_Nationaliteit$percentageABS)/nrow(BkNN5.2_Nationaliteit)

BkNN5.2_Geboorteland <- Bias(kNN5.2_Geboorteland, Geboorteland, "Geboorteland")
BOkNN5.2_Geboorteland <- sum(BkNN5.2_Geboorteland$percentageABS)/nrow(BkNN5.2_Geboorteland)

BkNN5.2_Onderwijsniveau <- Bias(kNN5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN5.2_Onderwijsniveau <- sum(BkNN5.2_Onderwijsniveau$percentageABS)/nrow(BkNN5.2_Onderwijsniveau)

BkNN5.2_Econ_status <- Bias(kNN5.2_Econ_status, Econ_status, "Econ._status")
BOkNN5.2_Econ_status <- sum(BkNN5.2_Econ_status$percentageABS)/nrow(BkNN5.2_Econ_status)

BkNN5.2_Beroep <- Bias(kNN5.2_Beroep, Beroep, "Beroep")
BOkNN5.2_Beroep <- sum(BkNN5.2_Beroep$percentageABS)/nrow(BkNN5.2_Beroep)

BkNN5.2_SBI <- Bias(kNN5.2_SBI, SBI, "SBI")
BOkNN5.2_SBI <- sum(BkNN5.2_SBI$percentageABS)/nrow(BkNN5.2_SBI)

BkNN5.2_Burg_Staat <- Bias(kNN5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN5.2_Burg_Staat <- sum(BkNN5.2_Burg_Staat$percentageABS)/nrow(BkNN5.2_Burg_Staat)

BiasOverallkNN5.2 <- sum(BOkNN5.2_Geslacht, BOkNN5.2_Leeftijd, BOkNN5.2_HH_Pos, BOkNN5.2_HH_grootte, BOkNN5.2_Woonregio_vorig_jaar, 
                         BOkNN5.2_Nationaliteit, BOkNN5.2_Geboorteland, BOkNN5.2_Onderwijsniveau, BOkNN5.2_Econ_status, 
                         BOkNN5.2_Beroep, BOkNN5.2_SBI, BOkNN5.2_Burg_Staat) / 12

rm(kNN5.2_Geslacht, kNN5.2_Leeftijd, kNN5.2_HH_Pos, kNN5.2_HH_grootte, kNN5.2_Woonregio_vorig_jaar, 
   kNN5.2_Nationaliteit, kNN5.2_Geboorteland, kNN5.2_Onderwijsniveau, kNN5.2_Econ_status, 
   kNN5.2_Beroep, kNN5.2_SBI, kNN5.2_Burg_Staat)
rm(BkNN5.2_Geslacht, BkNN5.2_Leeftijd, BkNN5.2_HH_Pos, BkNN5.2_HH_grootte, BkNN5.2_Woonregio_vorig_jaar, 
   BkNN5.2_Nationaliteit, BkNN5.2_Geboorteland, BkNN5.2_Onderwijsniveau, BkNN5.2_Econ_status, 
   BkNN5.2_Beroep, BkNN5.2_SBI, BkNN5.2_Burg_Staat)
rm(BOkNN5.2_Geslacht, BOkNN5.2_Leeftijd, BOkNN5.2_HH_Pos, BOkNN5.2_HH_grootte, BOkNN5.2_Woonregio_vorig_jaar, 
   BOkNN5.2_Nationaliteit, BOkNN5.2_Geboorteland, BOkNN5.2_Onderwijsniveau, BOkNN5.2_Econ_status, 
   BOkNN5.2_Beroep, BOkNN5.2_SBI, BOkNN5.2_Burg_Staat)

# 5.3 data set 
BkNN5.3_Geslacht <- Bias(kNN5.3_Geslacht, Geslacht, "Geslacht")
BOkNN5.3_Geslacht <- sum(BkNN5.3_Geslacht$percentageABS)/nrow(BkNN5.3_Geslacht)

BkNN5.3_Leeftijd <- Bias(kNN5.3_Leeftijd, Leeftijd, "Leeftijd")
BOkNN5.3_Leeftijd <- sum(BkNN5.3_Leeftijd$percentageABS)/nrow(BkNN5.3_Leeftijd)

BkNN5.3_HH_Pos <- Bias(kNN5.3_HH_Pos, HH_Pos, "HH_Pos")
BOkNN5.3_HH_Pos <- sum(BkNN5.3_HH_Pos$percentageABS)/nrow(BkNN5.3_HH_Pos)

BkNN5.3_HH_grootte <- Bias(kNN5.3_HH_grootte, HH_grootte, "HH_grootte")
BOkNN5.3_HH_grootte <- sum(BkNN5.3_HH_grootte$percentageABS)/nrow(BkNN5.3_HH_grootte)

BkNN5.3_Woonregio_vorig_jaar <- Bias(kNN5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN5.3_Woonregio_vorig_jaar <- sum(BkNN5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN5.3_Woonregio_vorig_jaar)

BkNN5.3_Nationaliteit <- Bias(kNN5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN5.3_Nationaliteit <- sum(BkNN5.3_Nationaliteit$percentageABS)/nrow(BkNN5.3_Nationaliteit)

BkNN5.3_Geboorteland <- Bias(kNN5.3_Geboorteland, Geboorteland, "Geboorteland")
BOkNN5.3_Geboorteland <- sum(BkNN5.3_Geboorteland$percentageABS)/nrow(BkNN5.3_Geboorteland)

BkNN5.3_Onderwijsniveau <- Bias(kNN5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN5.3_Onderwijsniveau <- sum(BkNN5.3_Onderwijsniveau$percentageABS)/nrow(BkNN5.3_Onderwijsniveau)

BkNN5.3_Econ_status <- Bias(kNN5.3_Econ_status, Econ_status, "Econ._status")
BOkNN5.3_Econ_status <- sum(BkNN5.3_Econ_status$percentageABS)/nrow(BkNN5.3_Econ_status)

BkNN5.3_Beroep <- Bias(kNN5.3_Beroep, Beroep, "Beroep")
BOkNN5.3_Beroep <- sum(BkNN5.3_Beroep$percentageABS)/nrow(BkNN5.3_Beroep)

BkNN5.3_SBI <- Bias(kNN5.3_SBI, SBI, "SBI")
BOkNN5.3_SBI <- sum(BkNN5.3_SBI$percentageABS)/nrow(BkNN5.3_SBI)

BkNN5.3_Burg_Staat <- Bias(kNN5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN5.3_Burg_Staat <- sum(BkNN5.3_Burg_Staat$percentageABS)/nrow(BkNN5.3_Burg_Staat)

BiasOverallkNN5.3 <- sum(BOkNN5.3_Geslacht, BOkNN5.3_Leeftijd, BOkNN5.3_HH_Pos, BOkNN5.3_HH_grootte, BOkNN5.3_Woonregio_vorig_jaar, 
                         BOkNN5.3_Nationaliteit, BOkNN5.3_Geboorteland, BOkNN5.3_Onderwijsniveau, BOkNN5.3_Econ_status, 
                         BOkNN5.3_Beroep, BOkNN5.3_SBI, BOkNN5.3_Burg_Staat) / 12

rm(kNN5.3_Geslacht, kNN5.3_Leeftijd, kNN5.3_HH_Pos, kNN5.3_HH_grootte, kNN5.3_Woonregio_vorig_jaar, 
   kNN5.3_Nationaliteit, kNN5.3_Geboorteland, kNN5.3_Onderwijsniveau, kNN5.3_Econ_status, 
   kNN5.3_Beroep, kNN5.3_SBI, kNN5.3_Burg_Staat)
rm(BkNN5.3_Geslacht, BkNN5.3_Leeftijd, BkNN5.3_HH_Pos, BkNN5.3_HH_grootte, BkNN5.3_Woonregio_vorig_jaar, 
   BkNN5.3_Nationaliteit, BkNN5.3_Geboorteland, BkNN5.3_Onderwijsniveau, BkNN5.3_Econ_status, 
   BkNN5.3_Beroep, BkNN5.3_SBI, BkNN5.3_Burg_Staat)
rm(BOkNN5.3_Geslacht, BOkNN5.3_Leeftijd, BOkNN5.3_HH_Pos, BOkNN5.3_HH_grootte, BOkNN5.3_Woonregio_vorig_jaar, 
   BOkNN5.3_Nationaliteit, BOkNN5.3_Geboorteland, BOkNN5.3_Onderwijsniveau, BOkNN5.3_Econ_status, 
   BOkNN5.3_Beroep, BOkNN5.3_SBI, BOkNN5.3_Burg_Staat)


# 10% data sets

# Counting values
kNN10.1_Geslacht <- plyr::count(S5df_kNN10.1, 'Geslacht')
kNN10.1_Leeftijd <- plyr::count(S5df_kNN10.1, 'Leeftijd')
kNN10.1_HH_Pos <- plyr::count(S5df_kNN10.1, 'HH_Pos')
kNN10.1_HH_grootte <- plyr::count(S5df_kNN10.1, 'HH_grootte')
kNN10.1_Woonregio_vorig_jaar <- plyr::count(S5df_kNN10.1, 'Woonregio_vorig_jaar')
kNN10.1_Nationaliteit <- plyr::count(S5df_kNN10.1, 'Nationaliteit')
kNN10.1_Geboorteland <- plyr::count(S5df_kNN10.1, 'Geboorteland')
kNN10.1_Onderwijsniveau <- plyr::count(S5df_kNN10.1, 'Onderwijsniveau')
kNN10.1_Econ_status <- plyr::count(S5df_kNN10.1, 'Econ._status')
kNN10.1_Beroep <- plyr::count(S5df_kNN10.1, 'Beroep')
kNN10.1_SBI <- plyr::count(S5df_kNN10.1, 'SBI')
kNN10.1_Burg_Staat <- plyr::count(S5df_kNN10.1, 'Burg._Staat')

kNN10.2_Geslacht <- plyr::count(S5df_kNN10.2, 'Geslacht')
kNN10.2_Leeftijd <- plyr::count(S5df_kNN10.2, 'Leeftijd')
kNN10.2_HH_Pos <- plyr::count(S5df_kNN10.2, 'HH_Pos')
kNN10.2_HH_grootte <- plyr::count(S5df_kNN10.2, 'HH_grootte')
kNN10.2_Woonregio_vorig_jaar <- plyr::count(S5df_kNN10.2, 'Woonregio_vorig_jaar')
kNN10.2_Nationaliteit <- plyr::count(S5df_kNN10.2, 'Nationaliteit')
kNN10.2_Geboorteland <- plyr::count(S5df_kNN10.2, 'Geboorteland')
kNN10.2_Onderwijsniveau <- plyr::count(S5df_kNN10.2, 'Onderwijsniveau')
kNN10.2_Econ_status <- plyr::count(S5df_kNN10.2, 'Econ._status')
kNN10.2_Beroep <- plyr::count(S5df_kNN10.2, 'Beroep')
kNN10.2_SBI <- plyr::count(S5df_kNN10.2, 'SBI')
kNN10.2_Burg_Staat <- plyr::count(S5df_kNN10.2, 'Burg._Staat')

kNN10.3_Geslacht <- plyr::count(S5df_kNN10.3, 'Geslacht')
kNN10.3_Leeftijd <- plyr::count(S5df_kNN10.3, 'Leeftijd')
kNN10.3_HH_Pos <- plyr::count(S5df_kNN10.3, 'HH_Pos')
kNN10.3_HH_grootte <- plyr::count(S5df_kNN10.3, 'HH_grootte')
kNN10.3_Woonregio_vorig_jaar <- plyr::count(S5df_kNN10.3, 'Woonregio_vorig_jaar')
kNN10.3_Nationaliteit <- plyr::count(S5df_kNN10.3, 'Nationaliteit')
kNN10.3_Geboorteland <- plyr::count(S5df_kNN10.3, 'Geboorteland')
kNN10.3_Onderwijsniveau <- plyr::count(S5df_kNN10.3, 'Onderwijsniveau')
kNN10.3_Econ_status <- plyr::count(S5df_kNN10.3, 'Econ._status')
kNN10.3_Beroep <- plyr::count(S5df_kNN10.3, 'Beroep')
kNN10.3_SBI <- plyr::count(S5df_kNN10.3, 'SBI')
kNN10.3_Burg_Staat <- plyr::count(S5df_kNN10.3, 'Burg._Staat')

rm(S5df_kNN10.1, S5df_kNN10.2, S5df_kNN10.3)


# 10.1 data set 
BkNN10.1_Geslacht <- Bias(kNN10.1_Geslacht, Geslacht, "Geslacht")
BOkNN10.1_Geslacht <- sum(BkNN10.1_Geslacht$percentageABS)/nrow(BkNN10.1_Geslacht)

BkNN10.1_Leeftijd <- Bias(kNN10.1_Leeftijd, Leeftijd, "Leeftijd")
BOkNN10.1_Leeftijd <- sum(BkNN10.1_Leeftijd$percentageABS)/nrow(BkNN10.1_Leeftijd)

BkNN10.1_HH_Pos <- Bias(kNN10.1_HH_Pos, HH_Pos, "HH_Pos")
BOkNN10.1_HH_Pos <- sum(BkNN10.1_HH_Pos$percentageABS)/nrow(BkNN10.1_HH_Pos)

BkNN10.1_HH_grootte <- Bias(kNN10.1_HH_grootte, HH_grootte, "HH_grootte")
BOkNN10.1_HH_grootte <- sum(BkNN10.1_HH_grootte$percentageABS)/nrow(BkNN10.1_HH_grootte)

BkNN10.1_Woonregio_vorig_jaar <- Bias(kNN10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN10.1_Woonregio_vorig_jaar <- sum(BkNN10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN10.1_Woonregio_vorig_jaar)

BkNN10.1_Nationaliteit <- Bias(kNN10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN10.1_Nationaliteit <- sum(BkNN10.1_Nationaliteit$percentageABS)/nrow(BkNN10.1_Nationaliteit)

BkNN10.1_Geboorteland <- Bias(kNN10.1_Geboorteland, Geboorteland, "Geboorteland")
BOkNN10.1_Geboorteland <- sum(BkNN10.1_Geboorteland$percentageABS)/nrow(BkNN10.1_Geboorteland)

BkNN10.1_Onderwijsniveau <- Bias(kNN10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN10.1_Onderwijsniveau <- sum(BkNN10.1_Onderwijsniveau$percentageABS)/nrow(BkNN10.1_Onderwijsniveau)

BkNN10.1_Econ_status <- Bias(kNN10.1_Econ_status, Econ_status, "Econ._status")
BOkNN10.1_Econ_status <- sum(BkNN10.1_Econ_status$percentageABS)/nrow(BkNN10.1_Econ_status)

BkNN10.1_Beroep <- Bias(kNN10.1_Beroep, Beroep, "Beroep")
BOkNN10.1_Beroep <- sum(BkNN10.1_Beroep$percentageABS)/nrow(BkNN10.1_Beroep)

BkNN10.1_SBI <- Bias(kNN10.1_SBI, SBI, "SBI")
BOkNN10.1_SBI <- sum(BkNN10.1_SBI$percentageABS)/nrow(BkNN10.1_SBI)

BkNN10.1_Burg_Staat <- Bias(kNN10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN10.1_Burg_Staat <- sum(BkNN10.1_Burg_Staat$percentageABS)/nrow(BkNN10.1_Burg_Staat)

BiasOverallkNN10.1 <- sum(BOkNN10.1_Geslacht, BOkNN10.1_Leeftijd, BOkNN10.1_HH_Pos, BOkNN10.1_HH_grootte, BOkNN10.1_Woonregio_vorig_jaar, 
                          BOkNN10.1_Nationaliteit, BOkNN10.1_Geboorteland, BOkNN10.1_Onderwijsniveau, BOkNN10.1_Econ_status, 
                          BOkNN10.1_Beroep, BOkNN10.1_SBI, BOkNN10.1_Burg_Staat) / 12

rm(kNN10.1_Geslacht, kNN10.1_Leeftijd, kNN10.1_HH_Pos, kNN10.1_HH_grootte, kNN10.1_Woonregio_vorig_jaar, 
   kNN10.1_Nationaliteit, kNN10.1_Geboorteland, kNN10.1_Onderwijsniveau, kNN10.1_Econ_status, 
   kNN10.1_Beroep, kNN10.1_SBI, kNN10.1_Burg_Staat)
rm(BkNN10.1_Geslacht, BkNN10.1_Leeftijd, BkNN10.1_HH_Pos, BkNN10.1_HH_grootte, BkNN10.1_Woonregio_vorig_jaar, 
   BkNN10.1_Nationaliteit, BkNN10.1_Geboorteland, BkNN10.1_Onderwijsniveau, BkNN10.1_Econ_status, 
   BkNN10.1_Beroep, BkNN10.1_SBI, BkNN10.1_Burg_Staat)
rm(BOkNN10.1_Geslacht, BOkNN10.1_Leeftijd, BOkNN10.1_HH_Pos, BOkNN10.1_HH_grootte, BOkNN10.1_Woonregio_vorig_jaar, 
   BOkNN10.1_Nationaliteit, BOkNN10.1_Geboorteland, BOkNN10.1_Onderwijsniveau, BOkNN10.1_Econ_status, 
   BOkNN10.1_Beroep, BOkNN10.1_SBI, BOkNN10.1_Burg_Staat)

# 10.2 data set 
BkNN10.2_Geslacht <- Bias(kNN10.2_Geslacht, Geslacht, "Geslacht")
BOkNN10.2_Geslacht <- sum(BkNN10.2_Geslacht$percentageABS)/nrow(BkNN10.2_Geslacht)

BkNN10.2_Leeftijd <- Bias(kNN10.2_Leeftijd, Leeftijd, "Leeftijd")
BOkNN10.2_Leeftijd <- sum(BkNN10.2_Leeftijd$percentageABS)/nrow(BkNN10.2_Leeftijd)

BkNN10.2_HH_Pos <- Bias(kNN10.2_HH_Pos, HH_Pos, "HH_Pos")
BOkNN10.2_HH_Pos <- sum(BkNN10.2_HH_Pos$percentageABS)/nrow(BkNN10.2_HH_Pos)

BkNN10.2_HH_grootte <- Bias(kNN10.2_HH_grootte, HH_grootte, "HH_grootte")
BOkNN10.2_HH_grootte <- sum(BkNN10.2_HH_grootte$percentageABS)/nrow(BkNN10.2_HH_grootte)

BkNN10.2_Woonregio_vorig_jaar <- Bias(kNN10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN10.2_Woonregio_vorig_jaar <- sum(BkNN10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN10.2_Woonregio_vorig_jaar)

BkNN10.2_Nationaliteit <- Bias(kNN10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN10.2_Nationaliteit <- sum(BkNN10.2_Nationaliteit$percentageABS)/nrow(BkNN10.2_Nationaliteit)

BkNN10.2_Geboorteland <- Bias(kNN10.2_Geboorteland, Geboorteland, "Geboorteland")
BOkNN10.2_Geboorteland <- sum(BkNN10.2_Geboorteland$percentageABS)/nrow(BkNN10.2_Geboorteland)

BkNN10.2_Onderwijsniveau <- Bias(kNN10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN10.2_Onderwijsniveau <- sum(BkNN10.2_Onderwijsniveau$percentageABS)/nrow(BkNN10.2_Onderwijsniveau)

BkNN10.2_Econ_status <- Bias(kNN10.2_Econ_status, Econ_status, "Econ._status")
BOkNN10.2_Econ_status <- sum(BkNN10.2_Econ_status$percentageABS)/nrow(BkNN10.2_Econ_status)

BkNN10.2_Beroep <- Bias(kNN10.2_Beroep, Beroep, "Beroep")
BOkNN10.2_Beroep <- sum(BkNN10.2_Beroep$percentageABS)/nrow(BkNN10.2_Beroep)

BkNN10.2_SBI <- Bias(kNN10.2_SBI, SBI, "SBI")
BOkNN10.2_SBI <- sum(BkNN10.2_SBI$percentageABS)/nrow(BkNN10.2_SBI)

BkNN10.2_Burg_Staat <- Bias(kNN10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN10.2_Burg_Staat <- sum(BkNN10.2_Burg_Staat$percentageABS)/nrow(BkNN10.2_Burg_Staat)

BiasOverallkNN10.2 <- sum(BOkNN10.2_Geslacht, BOkNN10.2_Leeftijd, BOkNN10.2_HH_Pos, BOkNN10.2_HH_grootte, BOkNN10.2_Woonregio_vorig_jaar, 
                          BOkNN10.2_Nationaliteit, BOkNN10.2_Geboorteland, BOkNN10.2_Onderwijsniveau, BOkNN10.2_Econ_status, 
                          BOkNN10.2_Beroep, BOkNN10.2_SBI, BOkNN10.2_Burg_Staat) / 12

rm(kNN10.2_Geslacht, kNN10.2_Leeftijd, kNN10.2_HH_Pos, kNN10.2_HH_grootte, kNN10.2_Woonregio_vorig_jaar, 
   kNN10.2_Nationaliteit, kNN10.2_Geboorteland, kNN10.2_Onderwijsniveau, kNN10.2_Econ_status, 
   kNN10.2_Beroep, kNN10.2_SBI, kNN10.2_Burg_Staat)
rm(BkNN10.2_Geslacht, BkNN10.2_Leeftijd, BkNN10.2_HH_Pos, BkNN10.2_HH_grootte, BkNN10.2_Woonregio_vorig_jaar, 
   BkNN10.2_Nationaliteit, BkNN10.2_Geboorteland, BkNN10.2_Onderwijsniveau, BkNN10.2_Econ_status, 
   BkNN10.2_Beroep, BkNN10.2_SBI, BkNN10.2_Burg_Staat)
rm(BOkNN10.2_Geslacht, BOkNN10.2_Leeftijd, BOkNN10.2_HH_Pos, BOkNN10.2_HH_grootte, BOkNN10.2_Woonregio_vorig_jaar, 
   BOkNN10.2_Nationaliteit, BOkNN10.2_Geboorteland, BOkNN10.2_Onderwijsniveau, BOkNN10.2_Econ_status, 
   BOkNN10.2_Beroep, BOkNN10.2_SBI, BOkNN10.2_Burg_Staat)

# 10.3 data set 
BkNN10.3_Geslacht <- Bias(kNN10.3_Geslacht, Geslacht, "Geslacht")
BOkNN10.3_Geslacht <- sum(BkNN10.3_Geslacht$percentageABS)/nrow(BkNN10.3_Geslacht)

BkNN10.3_Leeftijd <- Bias(kNN10.3_Leeftijd, Leeftijd, "Leeftijd")
BOkNN10.3_Leeftijd <- sum(BkNN10.3_Leeftijd$percentageABS)/nrow(BkNN10.3_Leeftijd)

BkNN10.3_HH_Pos <- Bias(kNN10.3_HH_Pos, HH_Pos, "HH_Pos")
BOkNN10.3_HH_Pos <- sum(BkNN10.3_HH_Pos$percentageABS)/nrow(BkNN10.3_HH_Pos)

BkNN10.3_HH_grootte <- Bias(kNN10.3_HH_grootte, HH_grootte, "HH_grootte")
BOkNN10.3_HH_grootte <- sum(BkNN10.3_HH_grootte$percentageABS)/nrow(BkNN10.3_HH_grootte)

BkNN10.3_Woonregio_vorig_jaar <- Bias(kNN10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOkNN10.3_Woonregio_vorig_jaar <- sum(BkNN10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BkNN10.3_Woonregio_vorig_jaar)

BkNN10.3_Nationaliteit <- Bias(kNN10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOkNN10.3_Nationaliteit <- sum(BkNN10.3_Nationaliteit$percentageABS)/nrow(BkNN10.3_Nationaliteit)

BkNN10.3_Geboorteland <- Bias(kNN10.3_Geboorteland, Geboorteland, "Geboorteland")
BOkNN10.3_Geboorteland <- sum(BkNN10.3_Geboorteland$percentageABS)/nrow(BkNN10.3_Geboorteland)

BkNN10.3_Onderwijsniveau <- Bias(kNN10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOkNN10.3_Onderwijsniveau <- sum(BkNN10.3_Onderwijsniveau$percentageABS)/nrow(BkNN10.3_Onderwijsniveau)

BkNN10.3_Econ_status <- Bias(kNN10.3_Econ_status, Econ_status, "Econ._status")
BOkNN10.3_Econ_status <- sum(BkNN10.3_Econ_status$percentageABS)/nrow(BkNN10.3_Econ_status)

BkNN10.3_Beroep <- Bias(kNN10.3_Beroep, Beroep, "Beroep")
BOkNN10.3_Beroep <- sum(BkNN10.3_Beroep$percentageABS)/nrow(BkNN10.3_Beroep)

BkNN10.3_SBI <- Bias(kNN10.3_SBI, SBI, "SBI")
BOkNN10.3_SBI <- sum(BkNN10.3_SBI$percentageABS)/nrow(BkNN10.3_SBI)

BkNN10.3_Burg_Staat <- Bias(kNN10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOkNN10.3_Burg_Staat <- sum(BkNN10.3_Burg_Staat$percentageABS)/nrow(BkNN10.3_Burg_Staat)

BiasOverallkNN10.3 <- sum(BOkNN10.3_Geslacht, BOkNN10.3_Leeftijd, BOkNN10.3_HH_Pos, BOkNN10.3_HH_grootte, BOkNN10.3_Woonregio_vorig_jaar, 
                          BOkNN10.3_Nationaliteit, BOkNN10.3_Geboorteland, BOkNN10.3_Onderwijsniveau, BOkNN10.3_Econ_status, 
                          BOkNN10.3_Beroep, BOkNN10.3_SBI, BOkNN10.3_Burg_Staat) / 12

rm(kNN10.3_Geslacht, kNN10.3_Leeftijd, kNN10.3_HH_Pos, kNN10.3_HH_grootte, kNN10.3_Woonregio_vorig_jaar, 
   kNN10.3_Nationaliteit, kNN10.3_Geboorteland, kNN10.3_Onderwijsniveau, kNN10.3_Econ_status, 
   kNN10.3_Beroep, kNN10.3_SBI, kNN10.3_Burg_Staat)
rm(BkNN10.3_Geslacht, BkNN10.3_Leeftijd, BkNN10.3_HH_Pos, BkNN10.3_HH_grootte, BkNN10.3_Woonregio_vorig_jaar, 
   BkNN10.3_Nationaliteit, BkNN10.3_Geboorteland, BkNN10.3_Onderwijsniveau, BkNN10.3_Econ_status, 
   BkNN10.3_Beroep, BkNN10.3_SBI, BkNN10.3_Burg_Staat)
rm(BOkNN10.3_Geslacht, BOkNN10.3_Leeftijd, BOkNN10.3_HH_Pos, BOkNN10.3_HH_grootte, BOkNN10.3_Woonregio_vorig_jaar, 
   BOkNN10.3_Nationaliteit, BOkNN10.3_Geboorteland, BOkNN10.3_Onderwijsniveau, BOkNN10.3_Econ_status, 
   BOkNN10.3_Beroep, BOkNN10.3_SBI, BOkNN10.3_Burg_Staat)