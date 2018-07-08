
# Installing packages -----------------------------------------------------------------------------------------------------------------
#install.packages("readxl")
#install.packages("mice")
#install.packages("devtools")
#install.packages("plyr")
#install.packages("dplyr")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("readxl")
library("mice")
library("devtools")
install_github("jabiru/tictoc")
library("tictoc")
library("plyr")
library("dplyr")


## Loading MCAR data frames and ipums data set ----------------------------------------------------------------------------------------------------
ipums <- get(load("IPUMS/ipums.Rdata"))


# Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

# Setting correct and total to '0' 
DT2.1_correct <- DT2.2_correct <- DT2.3_correct <- DT5.1_correct <- DT5.2_correct <- DT10.3_correct <- DT10.1_correct <- DT10.2_correct <- DT10.3_correct <- 0
DT2.1_total <- DT2.2_total <- DT2.3_total <- DT5.1_total <- DT5.2_total <- DT5.3_total <- DT10.1_total <- DT10.2_total <- DT10.3_total <- 0

## Decision Tree with 'mice' -------------------------------------------------------------------------------------------------------- 

limit <- memory.limit(size = 15000)

# 2% data sets

# 2.1 data set
MCAR2.1 <- get(load(file = "MCAR2_1.Rdata"))
DT_MCAR2.1 <- data.frame(MCAR2.1)
rm(MCAR2.1)
names(DT_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR2.1)) {
  DT_MCAR2.1[, i] <- as.factor(DT_MCAR2.1[, i])
}

tic("Decision Tree 2.1 processing time...")
decision_tree2.1 <- mice(DT_MCAR2.1, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt2.1 <- toc(log = TRUE)
process_timedt2.1
save(process_timedt2.1, file = "process_timedt21.Rdata")
rm(DT_MCAR2.1)

df2.1_complete <- complete(decision_tree2.1)
rm(decision_tree2.1)
df_decision_tree2.1 <- as.data.frame(df2.1_complete)
save(df_decision_tree2.1, file = "decision_tree21.Rdata")
DT2.1_correct <- DT2.1_correct + sum(ipums == df_decision_tree2.1)
DT2.1_total <- DT2.1_total + sum(!is.na(df_decision_tree2.1))

# 2.2 data set
MCAR2.2 <- get(load(file = "MCAR2_2.Rdata"))
DT_MCAR2.2 <- data.frame(MCAR2.2)
rm(MCAR2.2)
names(DT_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR2.2)) {
  DT_MCAR2.2[, i] <- as.factor(DT_MCAR2.2[, i])
}

tic("Decision Tree 2.2 processing time...")
decision_tree2.2 <- mice(DT_MCAR2.2, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt2.2 <- toc(log = TRUE)
process_timedt2.2
save(process_timedt2.2, file = "process_timedt22.Rdata")
rm(DT_MCAR2.2)

df2.2_complete <- complete(decision_tree2.2)
rm(decision_tree2.2)
df_decision_tree2.2 <- as.data.frame(df2.2_complete)
save(df_decision_tree2.2, file = "decision_tree22.Rdata")
DT2.2_correct <- DT2.2_correct + sum(ipums == df_decision_tree2.2)
DT2.2_total <- DT2.2_total + sum(!is.na(df_decision_tree2.2))

# 2.3 data set
MCAR2.3 <- get(load(file = "MCAR2_3.Rdata"))
DT_MCAR2.3 <- data.frame(MCAR2.3)
rm(MCAR2.3)
names(DT_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR2.3)) {
  DT_MCAR2.3[, i] <- as.factor(DT_MCAR2.3[, i])
}

tic("Decision Tree 2.3 processing time...")
decision_tree2.3 <- mice(DT_MCAR2.3, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt2.3 <- toc(log = TRUE)
process_timedt2.3
save(process_timedt2.3, file = "process_timedt23.Rdata")
rm(DT_MCAR2.3)

df2.3_complete <- complete(decision_tree2.3)
rm(decision_tree2.3)
df_decision_tree2.3 <- as.data.frame(df2.3_complete)
save(df_decision_tree2.3, file = "decision_tree23.Rdata")
DT2.3_correct <- DT2.3_correct + sum(ipums == df2.3_complete)
DT2.3_total <- DT2.3_total + sum(!is.na(df2.3_complete))


# 5% data sets

# 5.1 data set
MCAR5.1 <- get(load(file = "MCAR5_1.Rdata"))
DT_MCAR5.1 <- data.frame(MCAR5.1)
rm(MCAR5.1)
names(DT_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR5.1)) {
  DT_MCAR5.1[, i] <- as.factor(DT_MCAR5.1[, i])
}

tic("Decision Tree 5.1 processing time...")
decision_tree5.1 <- mice(DT_MCAR5.1, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt5.1 <- toc(log = TRUE)
process_timedt5.1
save(process_timedt5.1, file = "process_timedt51.Rdata")
rm(DT_MCAR5.1)

df5.1_complete <- complete(decision_tree5.1)
rm(decision_tree5.1)
df_decision_tree5.1 <- as.data.frame(df5.1_complete)
save(df_decision_tree5.1, file = "decision_tree51.Rdata")
DT5.1_correct <- DT5.1_correct + sum(ipums == df_decision_tree5.1)
DT5.1_total <- DT5.1_total + sum(!is.na(df_decision_tree5.1))

# 5.2 data set
MCAR5.2 <- get(load(file = "MCAR5_2.Rdata"))
DT_MCAR5.2 <- data.frame(MCAR5.2)
rm(MCAR5.2)
names(DT_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR5.2)) {
  DT_MCAR5.2[, i] <- as.factor(DT_MCAR5.2[, i])
}

tic("Decision Tree 5.2 processing time...")
decision_tree5.2 <- mice(DT_MCAR5.2, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt5.2 <- toc(log = TRUE)
process_timedt5.2
save(process_timedt5.2, file = "process_timedt52.Rdata")
rm(DT_MCAR5.2)

df5.2_complete <- complete(decision_tree5.2)
rm(decision_tree5.2)
df_decision_tree5.2 <- as.data.frame(df5.2_complete)
save(df_decision_tree5.2, file = "decision_tree52.Rdata")
DT5.2_correct <- DT5.2_correct + sum(ipums == df_decision_tree5.2)
DT5.2_total <- DT5.2_total + sum(!is.na(df_decision_tree5.2))

# 5.3 data set
MCAR5.3 <- get(load(file = "MCAR5_3.Rdata"))
DT_MCAR5.3 <- data.frame(MCAR5.3)
rm(MCAR5.3)
names(DT_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR5.3)) {
  DT_MCAR5.3[, i] <- as.factor(DT_MCAR5.3[, i])
}

tic("Decision Tree 5.3 processing time...")
decision_tree5.3 <- mice(DT_MCAR5.3, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt5.3 <- toc(log = TRUE)
process_timedt5.3
save(process_timedt5.3, file = "process_timedt53.Rdata")
rm(DT_MCAR5.3)

df5.3_complete <- complete(decision_tree5.3)
rm(decision_tree5.3)
df_decision_tree5.3 <- as.data.frame(df5.3_complete)
save(df_decision_tree5.3, file = "decision_tree53.Rdata")
DT5.3_correct <- DT5.3_correct + sum(ipums == df_decision_tree5.3)
DT5.3_total <- DT5.3_total + sum(!is.na(df_decision_tree5.3))


# 10% data sets 

# 10.1 data set
MCAR10.1 <- get(load(file = "MCAR10_1.Rdata"))
DT_MCAR10.1 <- data.frame(MCAR10.1)
rm(MCAR10.1)
names(DT_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR10.1)) {
  DT_MCAR10.1[, i] <- as.factor(DT_MCAR10.1[, i])
}

tic("Decision Tree 10.1 processing time...")
decision_tree10.1 <- mice(DT_MCAR10.1, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt10.1 <- toc(log = TRUE)
process_timedt10.1
save(process_timedt10.1, file = "process_timedt101.Rdata")
rm(DT_MCAR10.1)

df10.1_complete <- complete(decision_tree10.1)
rm(decision_tree10.1)
df_decision_tree10.1 <- as.data.frame(df10.1_complete)
save(df_decision_tree10.1, file = "decision_tree101.Rdata")
DT10.1_correct <- DT10.1_correct + sum(ipums == df_decision_tree10.1)
DT10.1_total <- DT10.1_total + sum(!is.na(df_decision_tree10.1))


# 10.2 data set
MCAR10.2 <- get(load(file = "MCAR10_2.Rdata"))
DT_MCAR10.2 <- data.frame(MCAR10.2)
rm(MCAR10.2)
names(DT_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR10.2)) {
  DT_MCAR10.2[, i] <- as.factor(DT_MCAR10.2[, i])
}

tic("Decision Tree 10.2 processing time...")
decision_tree10.2 <- mice(DT_MCAR10.2, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt10.2 <- toc(log = TRUE)
process_timedt10.2
save(process_timedt10.2, file = "process_timedt102.Rdata")
rm(DT_MCAR10.2)

df10.2_complete <- complete(decision_tree10.2)
rm(decision_tree10.2)
df_decision_tree10.2 <- as.data.frame(df10.2_complete)
save(df_decision_tree10.2, file = "decision_tree102.Rdata")
DT10.2_correct <- DT10.2_correct + sum(ipums == df_decision_tree10.2)
DT10.2_total <- DT10.2_total + sum(!is.na(df_decision_tree10.2))


# 10.3 data set
MCAR10.3 <- get(load(file = "MCAR10_3.Rdata"))
DT_MCAR10.3 <- data.frame(MCAR10.3)
rm(MCAR10.3)
names(DT_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(DT_MCAR10.3)) {
  DT_MCAR10.3[, i] <- as.factor(DT_MCAR10.3[, i])
}

tic("Decision Tree 10.3 processing time...")
decision_tree10.3 <- mice(DT_MCAR10.3, meth = 'cart', minbucket = 1)
toc(log = TRUE)
process_timedt10.3 <- toc(log = TRUE)
process_timedt10.3
save(process_timedt10.3, file = "process_timedt103.Rdata")
rm(DT_MCAR10.3)

df10.3_complete <- complete(decision_tree10.3)
rm(decision_tree10.3)
df_decision_tree10.3 <- as.data.frame(df10.3_complete)
save(df_decision_tree10.3, file = "decision_tree103.Rdata")
DT10.3_correct <- DT10.3_correct + sum(ipums == df_decision_tree10.3)
DT10.3_total <- DT10.3_total + sum(!is.na(df_decision_tree10.3))



# Check if all values are imputed 
# anyNA(c(df_decision_tree2.1, df_decision_tree2.2, df_decision_tree2.3, df_decision_tree5.1, df_decision_tree5.2, 
        # df_decision_tree5.3, df_decision_tree10.1, df_decision_tree10.2, df_decision_tree10.3))


## Computing accuracy (ML) ---------------------------------------------------------------------------------------------------------------------------------


# Computing the accuracy of imputation 
DT2.1_accuracy <- DT2.1_correct / DT2.1_total
DT2.2_accuracy <- DT2.2_correct / DT2.2_total
DT2.3_accuracy <- DT2.3_correct / DT2.3_total

DT5.1_accuracy <- DT5.1_correct / DT5.1_total
DT5.2_accuracy <- DT5.2_correct / DT5.2_total
DT5.3_accuracy <- DT5.3_correct / DT5.3_total

DT10.1_accuracy <- DT10.1_correct / DT10.1_total
DT10.2_accuracy <- DT10.2_correct / DT10.2_total
DT10.3_accuracy <- DT10.3_correct / DT10.3_total


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
DT2.1_Geslacht <- plyr::count(df_decision_tree2.1, 'Geslacht')
DT2.1_Leeftijd <- plyr::count(df_decision_tree2.1, 'Leeftijd')
DT2.1_HH_Pos <- plyr::count(df_decision_tree2.1, 'HH_Pos')
DT2.1_HH_grootte <- plyr::count(df_decision_tree2.1, 'HH_grootte')
DT2.1_Woonregio_vorig_jaar <- plyr::count(df_decision_tree2.1, 'Woonregio_vorig_jaar')
DT2.1_Nationaliteit <- plyr::count(df_decision_tree2.1, 'Nationaliteit')
DT2.1_Geboorteland <- plyr::count(df_decision_tree2.1, 'Geboorteland')
DT2.1_Onderwijsniveau <- plyr::count(df_decision_tree2.1, 'Onderwijsniveau')
DT2.1_Econ_status <- plyr::count(df_decision_tree2.1, 'Econ._status')
DT2.1_Beroep <- plyr::count(df_decision_tree2.1, 'Beroep')
DT2.1_SBI <- plyr::count(df_decision_tree2.1, 'SBI')
DT2.1_Burg_Staat <- plyr::count(df_decision_tree2.1, 'Burg._Staat')

DT2.2_Geslacht <- plyr::count(df_decision_tree2.2, 'Geslacht')
DT2.2_Leeftijd <- plyr::count(df_decision_tree2.2, 'Leeftijd')
DT2.2_HH_Pos <- plyr::count(df_decision_tree2.2, 'HH_Pos')
DT2.2_HH_grootte <- plyr::count(df_decision_tree2.2, 'HH_grootte')
DT2.2_Woonregio_vorig_jaar <- plyr::count(df_decision_tree2.2, 'Woonregio_vorig_jaar')
DT2.2_Nationaliteit <- plyr::count(df_decision_tree2.2, 'Nationaliteit')
DT2.2_Geboorteland <- plyr::count(df_decision_tree2.2, 'Geboorteland')
DT2.2_Onderwijsniveau <- plyr::count(df_decision_tree2.2, 'Onderwijsniveau')
DT2.2_Econ_status <- plyr::count(df_decision_tree2.2, 'Econ._status')
DT2.2_Beroep <- plyr::count(df_decision_tree2.2, 'Beroep')
DT2.2_SBI <- plyr::count(df_decision_tree2.2, 'SBI')
DT2.2_Burg_Staat <- plyr::count(df_decision_tree2.2, 'Burg._Staat')

DT2.3_Geslacht <- plyr::count(df_decision_tree2.3, 'Geslacht')
DT2.3_Leeftijd <- plyr::count(df_decision_tree2.3, 'Leeftijd')
DT2.3_HH_Pos <- plyr::count(df_decision_tree2.3, 'HH_Pos')
DT2.3_HH_grootte <- plyr::count(df_decision_tree2.3, 'HH_grootte')
DT2.3_Woonregio_vorig_jaar <- plyr::count(df_decision_tree2.3, 'Woonregio_vorig_jaar')
DT2.3_Nationaliteit <- plyr::count(df_decision_tree2.3, 'Nationaliteit')
DT2.3_Geboorteland <- plyr::count(df_decision_tree2.3, 'Geboorteland')
DT2.3_Onderwijsniveau <- plyr::count(df_decision_tree2.3, 'Onderwijsniveau')
DT2.3_Econ_status <- plyr::count(df_decision_tree2.3, 'Econ._status')
DT2.3_Beroep <- plyr::count(df_decision_tree2.3, 'Beroep')
DT2.3_SBI <- plyr::count(df_decision_tree2.3, 'SBI')
DT2.3_Burg_Staat <- plyr::count(df_decision_tree2.3, 'Burg._Staat')

rm(df_decision_tree2.1, df_decision_tree2.2, df_decision_tree2.3)

# 2.1 data set 
BDT2.1_Geslacht <- Bias(DT2.1_Geslacht, Geslacht, "Geslacht")
BODT2.1_Geslacht <- sum(BDT2.1_Geslacht$percentageABS)/nrow(BDT2.1_Geslacht)

BDT2.1_Leeftijd <- Bias(DT2.1_Leeftijd, Leeftijd, "Leeftijd")
BODT2.1_Leeftijd <- sum(BDT2.1_Leeftijd$percentageABS)/nrow(BDT2.1_Leeftijd)

BDT2.1_HH_Pos <- Bias(DT2.1_HH_Pos, HH_Pos, "HH_Pos")
BODT2.1_HH_Pos <- sum(BDT2.1_HH_Pos$percentageABS)/nrow(BDT2.1_HH_Pos)

BDT2.1_HH_grootte <- Bias(DT2.1_HH_grootte, HH_grootte, "HH_grootte")
BODT2.1_HH_grootte <- sum(BDT2.1_HH_grootte$percentageABS)/nrow(BDT2.1_HH_grootte)

BDT2.1_Woonregio_vorig_jaar <- Bias(DT2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT2.1_Woonregio_vorig_jaar <- sum(BDT2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BDT2.1_Woonregio_vorig_jaar)

BDT2.1_Nationaliteit <- Bias(DT2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT2.1_Nationaliteit <- sum(BDT2.1_Nationaliteit$percentageABS)/nrow(BDT2.1_Nationaliteit)

BDT2.1_Geboorteland <- Bias(DT2.1_Geboorteland, Geboorteland, "Geboorteland")
BODT2.1_Geboorteland <- sum(BDT2.1_Geboorteland$percentageABS)/nrow(BDT2.1_Geboorteland)

BDT2.1_Onderwijsniveau <- Bias(DT2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT2.1_Onderwijsniveau <- sum(BDT2.1_Onderwijsniveau$percentageABS)/nrow(BDT2.1_Onderwijsniveau)

BDT2.1_Econ_status <- Bias(DT2.1_Econ_status, Econ_status, "Econ._status")
BODT2.1_Econ_status <- sum(BDT2.1_Econ_status$percentageABS)/nrow(BDT2.1_Econ_status)

BDT2.1_Beroep <- Bias(DT2.1_Beroep, Beroep, "Beroep")
BODT2.1_Beroep <- sum(BDT2.1_Beroep$percentageABS)/nrow(BDT2.1_Beroep)

BDT2.1_SBI <- Bias(DT2.1_SBI, SBI, "SBI")
BODT2.1_SBI <- sum(BDT2.1_SBI$percentageABS)/nrow(BDT2.1_SBI)

BDT2.1_Burg_Staat <- Bias(DT2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT2.1_Burg_Staat <- sum(BDT2.1_Burg_Staat$percentageABS)/nrow(BDT2.1_Burg_Staat)

BiasOverallDT2.1 <- sum(BODT2.1_Geslacht, BODT2.1_Leeftijd, BODT2.1_HH_Pos, BODT2.1_HH_grootte, BODT2.1_Woonregio_vorig_jaar, 
                        BODT2.1_Nationaliteit, BODT2.1_Geboorteland, BODT2.1_Onderwijsniveau, BODT2.1_Econ_status, 
                        BODT2.1_Beroep, BODT2.1_SBI, BODT2.1_Burg_Staat) / 12

rm(DT2.1_Geslacht, DT2.1_Leeftijd, DT2.1_HH_Pos, DT2.1_HH_grootte, DT2.1_Woonregio_vorig_jaar, 
   DT2.1_Nationaliteit, DT2.1_Geboorteland, DT2.1_Onderwijsniveau, DT2.1_Econ_status, 
   DT2.1_Beroep, DT2.1_SBI, DT2.1_Burg_Staat)
rm(BDT2.1_Geslacht, BDT2.1_Leeftijd, BDT2.1_HH_Pos, BDT2.1_HH_grootte, BDT2.1_Woonregio_vorig_jaar, 
   BDT2.1_Nationaliteit, BDT2.1_Geboorteland, BDT2.1_Onderwijsniveau, BDT2.1_Econ_status, 
   BDT2.1_Beroep, BDT2.1_SBI, BDT2.1_Burg_Staat)
rm(BODT2.1_Geslacht, BODT2.1_Leeftijd, BODT2.1_HH_Pos, BODT2.1_HH_grootte, BODT2.1_Woonregio_vorig_jaar, 
   BODT2.1_Nationaliteit, BODT2.1_Geboorteland, BODT2.1_Onderwijsniveau, BODT2.1_Econ_status, 
   BODT2.1_Beroep, BODT2.1_SBI, BODT2.1_Burg_Staat)

# 2.2 data set 
BDT2.2_Geslacht <- Bias(DT2.2_Geslacht, Geslacht, "Geslacht")
BODT2.2_Geslacht <- sum(BDT2.2_Geslacht$percentageABS)/nrow(BDT2.2_Geslacht)

BDT2.2_Leeftijd <- Bias(DT2.2_Leeftijd, Leeftijd, "Leeftijd")
BODT2.2_Leeftijd <- sum(BDT2.2_Leeftijd$percentageABS)/nrow(BDT2.2_Leeftijd)

BDT2.2_HH_Pos <- Bias(DT2.2_HH_Pos, HH_Pos, "HH_Pos")
BODT2.2_HH_Pos <- sum(BDT2.2_HH_Pos$percentageABS)/nrow(BDT2.2_HH_Pos)

BDT2.2_HH_grootte <- Bias(DT2.2_HH_grootte, HH_grootte, "HH_grootte")
BODT2.2_HH_grootte <- sum(BDT2.2_HH_grootte$percentageABS)/nrow(BDT2.2_HH_grootte)

BDT2.2_Woonregio_vorig_jaar <- Bias(DT2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT2.2_Woonregio_vorig_jaar <- sum(BDT2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BDT2.2_Woonregio_vorig_jaar)

BDT2.2_Nationaliteit <- Bias(DT2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT2.2_Nationaliteit <- sum(BDT2.2_Nationaliteit$percentageABS)/nrow(BDT2.2_Nationaliteit)

BDT2.2_Geboorteland <- Bias(DT2.2_Geboorteland, Geboorteland, "Geboorteland")
BODT2.2_Geboorteland <- sum(BDT2.2_Geboorteland$percentageABS)/nrow(BDT2.2_Geboorteland)

BDT2.2_Onderwijsniveau <- Bias(DT2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT2.2_Onderwijsniveau <- sum(BDT2.2_Onderwijsniveau$percentageABS)/nrow(BDT2.2_Onderwijsniveau)

BDT2.2_Econ_status <- Bias(DT2.2_Econ_status, Econ_status, "Econ._status")
BODT2.2_Econ_status <- sum(BDT2.2_Econ_status$percentageABS)/nrow(BDT2.2_Econ_status)

BDT2.2_Beroep <- Bias(DT2.2_Beroep, Beroep, "Beroep")
BODT2.2_Beroep <- sum(BDT2.2_Beroep$percentageABS)/nrow(BDT2.2_Beroep)

BDT2.2_SBI <- Bias(DT2.2_SBI, SBI, "SBI")
BODT2.2_SBI <- sum(BDT2.2_SBI$percentageABS)/nrow(BDT2.2_SBI)

BDT2.2_Burg_Staat <- Bias(DT2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT2.2_Burg_Staat <- sum(BDT2.2_Burg_Staat$percentageABS)/nrow(BDT2.2_Burg_Staat)

BiasOverallDT2.2 <- sum(BODT2.2_Geslacht, BODT2.2_Leeftijd, BODT2.2_HH_Pos, BODT2.2_HH_grootte, BODT2.2_Woonregio_vorig_jaar, 
                        BODT2.2_Nationaliteit, BODT2.2_Geboorteland, BODT2.2_Onderwijsniveau, BODT2.2_Econ_status, 
                        BODT2.2_Beroep, BODT2.2_SBI, BODT2.2_Burg_Staat) / 12

rm(DT2.2_Geslacht, DT2.2_Leeftijd, DT2.2_HH_Pos, DT2.2_HH_grootte, DT2.2_Woonregio_vorig_jaar, 
   DT2.2_Nationaliteit, DT2.2_Geboorteland, DT2.2_Onderwijsniveau, DT2.2_Econ_status, 
   DT2.2_Beroep, DT2.2_SBI, DT2.2_Burg_Staat)
rm(BDT2.2_Geslacht, BDT2.2_Leeftijd, BDT2.2_HH_Pos, BDT2.2_HH_grootte, BDT2.2_Woonregio_vorig_jaar, 
   BDT2.2_Nationaliteit, BDT2.2_Geboorteland, BDT2.2_Onderwijsniveau, BDT2.2_Econ_status, 
   BDT2.2_Beroep, BDT2.2_SBI, BDT2.2_Burg_Staat)
rm(BODT2.2_Geslacht, BODT2.2_Leeftijd, BODT2.2_HH_Pos, BODT2.2_HH_grootte, BODT2.2_Woonregio_vorig_jaar, 
   BODT2.2_Nationaliteit, BODT2.2_Geboorteland, BODT2.2_Onderwijsniveau, BODT2.2_Econ_status, 
   BODT2.2_Beroep, BODT2.2_SBI, BODT2.2_Burg_Staat)

# 2.3 data set 
BDT2.3_Geslacht <- Bias(DT2.3_Geslacht, Geslacht, "Geslacht")
BODT2.3_Geslacht <- sum(BDT2.3_Geslacht$percentageABS)/nrow(BDT2.3_Geslacht)

BDT2.3_Leeftijd <- Bias(DT2.3_Leeftijd, Leeftijd, "Leeftijd")
BODT2.3_Leeftijd <- sum(BDT2.3_Leeftijd$percentageABS)/nrow(BDT2.3_Leeftijd)

BDT2.3_HH_Pos <- Bias(DT2.3_HH_Pos, HH_Pos, "HH_Pos")
BODT2.3_HH_Pos <- sum(BDT2.3_HH_Pos$percentageABS)/nrow(BDT2.3_HH_Pos)

BDT2.3_HH_grootte <- Bias(DT2.3_HH_grootte, HH_grootte, "HH_grootte")
BODT2.3_HH_grootte <- sum(BDT2.3_HH_grootte$percentageABS)/nrow(BDT2.3_HH_grootte)

BDT2.3_Woonregio_vorig_jaar <- Bias(DT2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT2.3_Woonregio_vorig_jaar <- sum(BDT2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BDT2.3_Woonregio_vorig_jaar)

BDT2.3_Nationaliteit <- Bias(DT2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT2.3_Nationaliteit <- sum(BDT2.3_Nationaliteit$percentageABS)/nrow(BDT2.3_Nationaliteit)

BDT2.3_Geboorteland <- Bias(DT2.3_Geboorteland, Geboorteland, "Geboorteland")
BODT2.3_Geboorteland <- sum(BDT2.3_Geboorteland$percentageABS)/nrow(BDT2.3_Geboorteland)

BDT2.3_Onderwijsniveau <- Bias(DT2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT2.3_Onderwijsniveau <- sum(BDT2.3_Onderwijsniveau$percentageABS)/nrow(BDT2.3_Onderwijsniveau)

BDT2.3_Econ_status <- Bias(DT2.3_Econ_status, Econ_status, "Econ._status")
BODT2.3_Econ_status <- sum(BDT2.3_Econ_status$percentageABS)/nrow(BDT2.3_Econ_status)

BDT2.3_Beroep <- Bias(DT2.3_Beroep, Beroep, "Beroep")
BODT2.3_Beroep <- sum(BDT2.3_Beroep$percentageABS)/nrow(BDT2.3_Beroep)

BDT2.3_SBI <- Bias(DT2.3_SBI, SBI, "SBI")
BODT2.3_SBI <- sum(BDT2.3_SBI$percentageABS)/nrow(BDT2.3_SBI)

BDT2.3_Burg_Staat <- Bias(DT2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT2.3_Burg_Staat <- sum(BDT2.3_Burg_Staat$percentageABS)/nrow(BDT2.3_Burg_Staat)

BiasOverallDT2.3 <- sum(BODT2.3_Geslacht, BODT2.3_Leeftijd, BODT2.3_HH_Pos, BODT2.3_HH_grootte, BODT2.3_Woonregio_vorig_jaar, 
                        BODT2.3_Nationaliteit, BODT2.3_Geboorteland, BODT2.3_Onderwijsniveau, BODT2.3_Econ_status, 
                        BODT2.3_Beroep, BODT2.3_SBI, BODT2.3_Burg_Staat) / 12

rm(DT2.3_Geslacht, DT2.3_Leeftijd, DT2.3_HH_Pos, DT2.3_HH_grootte, DT2.3_Woonregio_vorig_jaar, 
   DT2.3_Nationaliteit, DT2.3_Geboorteland, DT2.3_Onderwijsniveau, DT2.3_Econ_status, 
   DT2.3_Beroep, DT2.3_SBI, DT2.3_Burg_Staat)
rm(BDT2.3_Geslacht, BDT2.3_Leeftijd, BDT2.3_HH_Pos, BDT2.3_HH_grootte, BDT2.3_Woonregio_vorig_jaar, 
   BDT2.3_Nationaliteit, BDT2.3_Geboorteland, BDT2.3_Onderwijsniveau, BDT2.3_Econ_status, 
   BDT2.3_Beroep, BDT2.3_SBI, BDT2.3_Burg_Staat)
rm(BODT2.3_Geslacht, BODT2.3_Leeftijd, BODT2.3_HH_Pos, BODT2.3_HH_grootte, BODT2.3_Woonregio_vorig_jaar, 
   BODT2.3_Nationaliteit, BODT2.3_Geboorteland, BODT2.3_Onderwijsniveau, BODT2.3_Econ_status, 
   BODT2.3_Beroep, BODT2.3_SBI, BODT2.3_Burg_Staat)


# 5% data sets

# Counting values
DT5.1_Geslacht <- plyr::count(df_decision_tree5.1, 'Geslacht')
DT5.1_Leeftijd <- plyr::count(df_decision_tree5.1, 'Leeftijd')
DT5.1_HH_Pos <- plyr::count(df_decision_tree5.1, 'HH_Pos')
DT5.1_HH_grootte <- plyr::count(df_decision_tree5.1, 'HH_grootte')
DT5.1_Woonregio_vorig_jaar <- plyr::count(df_decision_tree5.1, 'Woonregio_vorig_jaar')
DT5.1_Nationaliteit <- plyr::count(df_decision_tree5.1, 'Nationaliteit')
DT5.1_Geboorteland <- plyr::count(df_decision_tree5.1, 'Geboorteland')
DT5.1_Onderwijsniveau <- plyr::count(df_decision_tree5.1, 'Onderwijsniveau')
DT5.1_Econ_status <- plyr::count(df_decision_tree5.1, 'Econ._status')
DT5.1_Beroep <- plyr::count(df_decision_tree5.1, 'Beroep')
DT5.1_SBI <- plyr::count(df_decision_tree5.1, 'SBI')
DT5.1_Burg_Staat <- plyr::count(df_decision_tree5.1, 'Burg._Staat')

DT5.2_Geslacht <- plyr::count(df_decision_tree5.2, 'Geslacht')
DT5.2_Leeftijd <- plyr::count(df_decision_tree5.2, 'Leeftijd')
DT5.2_HH_Pos <- plyr::count(df_decision_tree5.2, 'HH_Pos')
DT5.2_HH_grootte <- plyr::count(df_decision_tree5.2, 'HH_grootte')
DT5.2_Woonregio_vorig_jaar <- plyr::count(df_decision_tree5.2, 'Woonregio_vorig_jaar')
DT5.2_Nationaliteit <- plyr::count(df_decision_tree5.2, 'Nationaliteit')
DT5.2_Geboorteland <- plyr::count(df_decision_tree5.2, 'Geboorteland')
DT5.2_Onderwijsniveau <- plyr::count(df_decision_tree5.2, 'Onderwijsniveau')
DT5.2_Econ_status <- plyr::count(df_decision_tree5.2, 'Econ._status')
DT5.2_Beroep <- plyr::count(df_decision_tree5.2, 'Beroep')
DT5.2_SBI <- plyr::count(df_decision_tree5.2, 'SBI')
DT5.2_Burg_Staat <- plyr::count(df_decision_tree5.2, 'Burg._Staat')

DT5.3_Geslacht <- plyr::count(df_decision_tree5.3, 'Geslacht')
DT5.3_Leeftijd <- plyr::count(df_decision_tree5.3, 'Leeftijd')
DT5.3_HH_Pos <- plyr::count(df_decision_tree5.3, 'HH_Pos')
DT5.3_HH_grootte <- plyr::count(df_decision_tree5.3, 'HH_grootte')
DT5.3_Woonregio_vorig_jaar <- plyr::count(df_decision_tree5.3, 'Woonregio_vorig_jaar')
DT5.3_Nationaliteit <- plyr::count(df_decision_tree5.3, 'Nationaliteit')
DT5.3_Geboorteland <- plyr::count(df_decision_tree5.3, 'Geboorteland')
DT5.3_Onderwijsniveau <- plyr::count(df_decision_tree5.3, 'Onderwijsniveau')
DT5.3_Econ_status <- plyr::count(df_decision_tree5.3, 'Econ._status')
DT5.3_Beroep <- plyr::count(df_decision_tree5.3, 'Beroep')
DT5.3_SBI <- plyr::count(df_decision_tree5.3, 'SBI')
DT5.3_Burg_Staat <- plyr::count(df_decision_tree5.3, 'Burg._Staat')

rm(df_decision_tree5.1, df_decision_tree5.2, df_decision_tree5.3)


# 5.1 data set 
BDT5.1_Geslacht <- Bias(DT5.1_Geslacht, Geslacht, "Geslacht")
BODT5.1_Geslacht <- sum(BDT5.1_Geslacht$percentageABS)/nrow(BDT5.1_Geslacht)

BDT5.1_Leeftijd <- Bias(DT5.1_Leeftijd, Leeftijd, "Leeftijd")
BODT5.1_Leeftijd <- sum(BDT5.1_Leeftijd$percentageABS)/nrow(BDT5.1_Leeftijd)

BDT5.1_HH_Pos <- Bias(DT5.1_HH_Pos, HH_Pos, "HH_Pos")
BODT5.1_HH_Pos <- sum(BDT5.1_HH_Pos$percentageABS)/nrow(BDT5.1_HH_Pos)

BDT5.1_HH_grootte <- Bias(DT5.1_HH_grootte, HH_grootte, "HH_grootte")
BODT5.1_HH_grootte <- sum(BDT5.1_HH_grootte$percentageABS)/nrow(BDT5.1_HH_grootte)

BDT5.1_Woonregio_vorig_jaar <- Bias(DT5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT5.1_Woonregio_vorig_jaar <- sum(BDT5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BDT5.1_Woonregio_vorig_jaar)

BDT5.1_Nationaliteit <- Bias(DT5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT5.1_Nationaliteit <- sum(BDT5.1_Nationaliteit$percentageABS)/nrow(BDT5.1_Nationaliteit)

BDT5.1_Geboorteland <- Bias(DT5.1_Geboorteland, Geboorteland, "Geboorteland")
BODT5.1_Geboorteland <- sum(BDT5.1_Geboorteland$percentageABS)/nrow(BDT5.1_Geboorteland)

BDT5.1_Onderwijsniveau <- Bias(DT5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT5.1_Onderwijsniveau <- sum(BDT5.1_Onderwijsniveau$percentageABS)/nrow(BDT5.1_Onderwijsniveau)

BDT5.1_Econ_status <- Bias(DT5.1_Econ_status, Econ_status, "Econ._status")
BODT5.1_Econ_status <- sum(BDT5.1_Econ_status$percentageABS)/nrow(BDT5.1_Econ_status)

BDT5.1_Beroep <- Bias(DT5.1_Beroep, Beroep, "Beroep")
BODT5.1_Beroep <- sum(BDT5.1_Beroep$percentageABS)/nrow(BDT5.1_Beroep)

BDT5.1_SBI <- Bias(DT5.1_SBI, SBI, "SBI")
BODT5.1_SBI <- sum(BDT5.1_SBI$percentageABS)/nrow(BDT5.1_SBI)

BDT5.1_Burg_Staat <- Bias(DT5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT5.1_Burg_Staat <- sum(BDT5.1_Burg_Staat$percentageABS)/nrow(BDT5.1_Burg_Staat)

BiasOverallDT5.1 <- sum(BODT5.1_Geslacht, BODT5.1_Leeftijd, BODT5.1_HH_Pos, BODT5.1_HH_grootte, BODT5.1_Woonregio_vorig_jaar, 
                        BODT5.1_Nationaliteit, BODT5.1_Geboorteland, BODT5.1_Onderwijsniveau, BODT5.1_Econ_status, 
                        BODT5.1_Beroep, BODT5.1_SBI, BODT5.1_Burg_Staat) / 12

rm(DT5.1_Geslacht, DT5.1_Leeftijd, DT5.1_HH_Pos, DT5.1_HH_grootte, DT5.1_Woonregio_vorig_jaar, 
   DT5.1_Nationaliteit, DT5.1_Geboorteland, DT5.1_Onderwijsniveau, DT5.1_Econ_status, 
   DT5.1_Beroep, DT5.1_SBI, DT5.1_Burg_Staat)
rm(BDT5.1_Geslacht, BDT5.1_Leeftijd, BDT5.1_HH_Pos, BDT5.1_HH_grootte, BDT5.1_Woonregio_vorig_jaar, 
   BDT5.1_Nationaliteit, BDT5.1_Geboorteland, BDT5.1_Onderwijsniveau, BDT5.1_Econ_status, 
   BDT5.1_Beroep, BDT5.1_SBI, BDT5.1_Burg_Staat)
rm(BODT5.1_Geslacht, BODT5.1_Leeftijd, BODT5.1_HH_Pos, BODT5.1_HH_grootte, BODT5.1_Woonregio_vorig_jaar, 
   BODT5.1_Nationaliteit, BODT5.1_Geboorteland, BODT5.1_Onderwijsniveau, BODT5.1_Econ_status, 
   BODT5.1_Beroep, BODT5.1_SBI, BODT5.1_Burg_Staat)

# 5.2 data set 
BDT5.2_Geslacht <- Bias(DT5.2_Geslacht, Geslacht, "Geslacht")
BODT5.2_Geslacht <- sum(BDT5.2_Geslacht$percentageABS)/nrow(BDT5.2_Geslacht)

BDT5.2_Leeftijd <- Bias(DT5.2_Leeftijd, Leeftijd, "Leeftijd")
BODT5.2_Leeftijd <- sum(BDT5.2_Leeftijd$percentageABS)/nrow(BDT5.2_Leeftijd)

BDT5.2_HH_Pos <- Bias(DT5.2_HH_Pos, HH_Pos, "HH_Pos")
BODT5.2_HH_Pos <- sum(BDT5.2_HH_Pos$percentageABS)/nrow(BDT5.2_HH_Pos)

BDT5.2_HH_grootte <- Bias(DT5.2_HH_grootte, HH_grootte, "HH_grootte")
BODT5.2_HH_grootte <- sum(BDT5.2_HH_grootte$percentageABS)/nrow(BDT5.2_HH_grootte)

BDT5.2_Woonregio_vorig_jaar <- Bias(DT5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT5.2_Woonregio_vorig_jaar <- sum(BDT5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BDT5.2_Woonregio_vorig_jaar)

BDT5.2_Nationaliteit <- Bias(DT5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT5.2_Nationaliteit <- sum(BDT5.2_Nationaliteit$percentageABS)/nrow(BDT5.2_Nationaliteit)

BDT5.2_Geboorteland <- Bias(DT5.2_Geboorteland, Geboorteland, "Geboorteland")
BODT5.2_Geboorteland <- sum(BDT5.2_Geboorteland$percentageABS)/nrow(BDT5.2_Geboorteland)

BDT5.2_Onderwijsniveau <- Bias(DT5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT5.2_Onderwijsniveau <- sum(BDT5.2_Onderwijsniveau$percentageABS)/nrow(BDT5.2_Onderwijsniveau)

BDT5.2_Econ_status <- Bias(DT5.2_Econ_status, Econ_status, "Econ._status")
BODT5.2_Econ_status <- sum(BDT5.2_Econ_status$percentageABS)/nrow(BDT5.2_Econ_status)

BDT5.2_Beroep <- Bias(DT5.2_Beroep, Beroep, "Beroep")
BODT5.2_Beroep <- sum(BDT5.2_Beroep$percentageABS)/nrow(BDT5.2_Beroep)

BDT5.2_SBI <- Bias(DT5.2_SBI, SBI, "SBI")
BODT5.2_SBI <- sum(BDT5.2_SBI$percentageABS)/nrow(BDT5.2_SBI)

BDT5.2_Burg_Staat <- Bias(DT5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT5.2_Burg_Staat <- sum(BDT5.2_Burg_Staat$percentageABS)/nrow(BDT5.2_Burg_Staat)

BiasOverallDT5.2 <- sum(BODT5.2_Geslacht, BODT5.2_Leeftijd, BODT5.2_HH_Pos, BODT5.2_HH_grootte, BODT5.2_Woonregio_vorig_jaar, 
                        BODT5.2_Nationaliteit, BODT5.2_Geboorteland, BODT5.2_Onderwijsniveau, BODT5.2_Econ_status, 
                        BODT5.2_Beroep, BODT5.2_SBI, BODT5.2_Burg_Staat) / 12

rm(DT5.2_Geslacht, DT5.2_Leeftijd, DT5.2_HH_Pos, DT5.2_HH_grootte, DT5.2_Woonregio_vorig_jaar, 
   DT5.2_Nationaliteit, DT5.2_Geboorteland, DT5.2_Onderwijsniveau, DT5.2_Econ_status, 
   DT5.2_Beroep, DT5.2_SBI, DT5.2_Burg_Staat)
rm(BDT5.2_Geslacht, BDT5.2_Leeftijd, BDT5.2_HH_Pos, BDT5.2_HH_grootte, BDT5.2_Woonregio_vorig_jaar, 
   BDT5.2_Nationaliteit, BDT5.2_Geboorteland, BDT5.2_Onderwijsniveau, BDT5.2_Econ_status, 
   BDT5.2_Beroep, BDT5.2_SBI, BDT5.2_Burg_Staat)
rm(BODT5.2_Geslacht, BODT5.2_Leeftijd, BODT5.2_HH_Pos, BODT5.2_HH_grootte, BODT5.2_Woonregio_vorig_jaar, 
   BODT5.2_Nationaliteit, BODT5.2_Geboorteland, BODT5.2_Onderwijsniveau, BODT5.2_Econ_status, 
   BODT5.2_Beroep, BODT5.2_SBI, BODT5.2_Burg_Staat)

# 5.3 data set 
BDT5.3_Geslacht <- Bias(DT5.3_Geslacht, Geslacht, "Geslacht")
BODT5.3_Geslacht <- sum(BDT5.3_Geslacht$percentageABS)/nrow(BDT5.3_Geslacht)

BDT5.3_Leeftijd <- Bias(DT5.3_Leeftijd, Leeftijd, "Leeftijd")
BODT5.3_Leeftijd <- sum(BDT5.3_Leeftijd$percentageABS)/nrow(BDT5.3_Leeftijd)

BDT5.3_HH_Pos <- Bias(DT5.3_HH_Pos, HH_Pos, "HH_Pos")
BODT5.3_HH_Pos <- sum(BDT5.3_HH_Pos$percentageABS)/nrow(BDT5.3_HH_Pos)

BDT5.3_HH_grootte <- Bias(DT5.3_HH_grootte, HH_grootte, "HH_grootte")
BODT5.3_HH_grootte <- sum(BDT5.3_HH_grootte$percentageABS)/nrow(BDT5.3_HH_grootte)

BDT5.3_Woonregio_vorig_jaar <- Bias(DT5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT5.3_Woonregio_vorig_jaar <- sum(BDT5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BDT5.3_Woonregio_vorig_jaar)

BDT5.3_Nationaliteit <- Bias(DT5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT5.3_Nationaliteit <- sum(BDT5.3_Nationaliteit$percentageABS)/nrow(BDT5.3_Nationaliteit)

BDT5.3_Geboorteland <- Bias(DT5.3_Geboorteland, Geboorteland, "Geboorteland")
BODT5.3_Geboorteland <- sum(BDT5.3_Geboorteland$percentageABS)/nrow(BDT5.3_Geboorteland)

BDT5.3_Onderwijsniveau <- Bias(DT5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT5.3_Onderwijsniveau <- sum(BDT5.3_Onderwijsniveau$percentageABS)/nrow(BDT5.3_Onderwijsniveau)

BDT5.3_Econ_status <- Bias(DT5.3_Econ_status, Econ_status, "Econ._status")
BODT5.3_Econ_status <- sum(BDT5.3_Econ_status$percentageABS)/nrow(BDT5.3_Econ_status)

BDT5.3_Beroep <- Bias(DT5.3_Beroep, Beroep, "Beroep")
BODT5.3_Beroep <- sum(BDT5.3_Beroep$percentageABS)/nrow(BDT5.3_Beroep)

BDT5.3_SBI <- Bias(DT5.3_SBI, SBI, "SBI")
BODT5.3_SBI <- sum(BDT5.3_SBI$percentageABS)/nrow(BDT5.3_SBI)

BDT5.3_Burg_Staat <- Bias(DT5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT5.3_Burg_Staat <- sum(BDT5.3_Burg_Staat$percentageABS)/nrow(BDT5.3_Burg_Staat)

BiasOverallDT5.3 <- sum(BODT5.3_Geslacht, BODT5.3_Leeftijd, BODT5.3_HH_Pos, BODT5.3_HH_grootte, BODT5.3_Woonregio_vorig_jaar, 
                        BODT5.3_Nationaliteit, BODT5.3_Geboorteland, BODT5.3_Onderwijsniveau, BODT5.3_Econ_status, 
                        BODT5.3_Beroep, BODT5.3_SBI, BODT5.3_Burg_Staat) / 12

rm(DT5.3_Geslacht, DT5.3_Leeftijd, DT5.3_HH_Pos, DT5.3_HH_grootte, DT5.3_Woonregio_vorig_jaar, 
   DT5.3_Nationaliteit, DT5.3_Geboorteland, DT5.3_Onderwijsniveau, DT5.3_Econ_status, 
   DT5.3_Beroep, DT5.3_SBI, DT5.3_Burg_Staat)
rm(BDT5.3_Geslacht, BDT5.3_Leeftijd, BDT5.3_HH_Pos, BDT5.3_HH_grootte, BDT5.3_Woonregio_vorig_jaar, 
   BDT5.3_Nationaliteit, BDT5.3_Geboorteland, BDT5.3_Onderwijsniveau, BDT5.3_Econ_status, 
   BDT5.3_Beroep, BDT5.3_SBI, BDT5.3_Burg_Staat)
rm(BODT5.3_Geslacht, BODT5.3_Leeftijd, BODT5.3_HH_Pos, BODT5.3_HH_grootte, BODT5.3_Woonregio_vorig_jaar, 
   BODT5.3_Nationaliteit, BODT5.3_Geboorteland, BODT5.3_Onderwijsniveau, BODT5.3_Econ_status, 
   BODT5.3_Beroep, BODT5.3_SBI, BODT5.3_Burg_Staat)


# 10% data sets

# Counting values
DT10.1_Geslacht <- plyr::count(df_decision_tree10.1, 'Geslacht')
DT10.1_Leeftijd <- plyr::count(df_decision_tree10.1, 'Leeftijd')
DT10.1_HH_Pos <- plyr::count(df_decision_tree10.1, 'HH_Pos')
DT10.1_HH_grootte <- plyr::count(df_decision_tree10.1, 'HH_grootte')
DT10.1_Woonregio_vorig_jaar <- plyr::count(df_decision_tree10.1, 'Woonregio_vorig_jaar')
DT10.1_Nationaliteit <- plyr::count(df_decision_tree10.1, 'Nationaliteit')
DT10.1_Geboorteland <- plyr::count(df_decision_tree10.1, 'Geboorteland')
DT10.1_Onderwijsniveau <- plyr::count(df_decision_tree10.1, 'Onderwijsniveau')
DT10.1_Econ_status <- plyr::count(df_decision_tree10.1, 'Econ._status')
DT10.1_Beroep <- plyr::count(df_decision_tree10.1, 'Beroep')
DT10.1_SBI <- plyr::count(df_decision_tree10.1, 'SBI')
DT10.1_Burg_Staat <- plyr::count(df_decision_tree10.1, 'Burg._Staat')

DT10.2_Geslacht <- plyr::count(df_decision_tree10.2, 'Geslacht')
DT10.2_Leeftijd <- plyr::count(df_decision_tree10.2, 'Leeftijd')
DT10.2_HH_Pos <- plyr::count(df_decision_tree10.2, 'HH_Pos')
DT10.2_HH_grootte <- plyr::count(df_decision_tree10.2, 'HH_grootte')
DT10.2_Woonregio_vorig_jaar <- plyr::count(df_decision_tree10.2, 'Woonregio_vorig_jaar')
DT10.2_Nationaliteit <- plyr::count(df_decision_tree10.2, 'Nationaliteit')
DT10.2_Geboorteland <- plyr::count(df_decision_tree10.2, 'Geboorteland')
DT10.2_Onderwijsniveau <- plyr::count(df_decision_tree10.2, 'Onderwijsniveau')
DT10.2_Econ_status <- plyr::count(df_decision_tree10.2, 'Econ._status')
DT10.2_Beroep <- plyr::count(df_decision_tree10.2, 'Beroep')
DT10.2_SBI <- plyr::count(df_decision_tree10.2, 'SBI')
DT10.2_Burg_Staat <- plyr::count(df_decision_tree10.2, 'Burg._Staat')

DT10.3_Geslacht <- plyr::count(df_decision_tree10.3, 'Geslacht')
DT10.3_Leeftijd <- plyr::count(df_decision_tree10.3, 'Leeftijd')
DT10.3_HH_Pos <- plyr::count(df_decision_tree10.3, 'HH_Pos')
DT10.3_HH_grootte <- plyr::count(df_decision_tree10.3, 'HH_grootte')
DT10.3_Woonregio_vorig_jaar <- plyr::count(df_decision_tree10.3, 'Woonregio_vorig_jaar')
DT10.3_Nationaliteit <- plyr::count(df_decision_tree10.3, 'Nationaliteit')
DT10.3_Geboorteland <- plyr::count(df_decision_tree10.3, 'Geboorteland')
DT10.3_Onderwijsniveau <- plyr::count(df_decision_tree10.3, 'Onderwijsniveau')
DT10.3_Econ_status <- plyr::count(df_decision_tree10.3, 'Econ._status')
DT10.3_Beroep <- plyr::count(df_decision_tree10.3, 'Beroep')
DT10.3_SBI <- plyr::count(df_decision_tree10.3, 'SBI')
DT10.3_Burg_Staat <- plyr::count(df_decision_tree10.3, 'Burg._Staat')

rm(df_decision_tree10.1, df_decision_tree10.2, df_decision_tree10.3)


# 10.1 data set 
BDT10.1_Geslacht <- Bias(DT10.1_Geslacht, Geslacht, "Geslacht")
BODT10.1_Geslacht <- sum(BDT10.1_Geslacht$percentageABS)/nrow(BDT10.1_Geslacht)

BDT10.1_Leeftijd <- Bias(DT10.1_Leeftijd, Leeftijd, "Leeftijd")
BODT10.1_Leeftijd <- sum(BDT10.1_Leeftijd$percentageABS)/nrow(BDT10.1_Leeftijd)

BDT10.1_HH_Pos <- Bias(DT10.1_HH_Pos, HH_Pos, "HH_Pos")
BODT10.1_HH_Pos <- sum(BDT10.1_HH_Pos$percentageABS)/nrow(BDT10.1_HH_Pos)

BDT10.1_HH_grootte <- Bias(DT10.1_HH_grootte, HH_grootte, "HH_grootte")
BODT10.1_HH_grootte <- sum(BDT10.1_HH_grootte$percentageABS)/nrow(BDT10.1_HH_grootte)

BDT10.1_Woonregio_vorig_jaar <- Bias(DT10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT10.1_Woonregio_vorig_jaar <- sum(BDT10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BDT10.1_Woonregio_vorig_jaar)

BDT10.1_Nationaliteit <- Bias(DT10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT10.1_Nationaliteit <- sum(BDT10.1_Nationaliteit$percentageABS)/nrow(BDT10.1_Nationaliteit)

BDT10.1_Geboorteland <- Bias(DT10.1_Geboorteland, Geboorteland, "Geboorteland")
BODT10.1_Geboorteland <- sum(BDT10.1_Geboorteland$percentageABS)/nrow(BDT10.1_Geboorteland)

BDT10.1_Onderwijsniveau <- Bias(DT10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT10.1_Onderwijsniveau <- sum(BDT10.1_Onderwijsniveau$percentageABS)/nrow(BDT10.1_Onderwijsniveau)

BDT10.1_Econ_status <- Bias(DT10.1_Econ_status, Econ_status, "Econ._status")
BODT10.1_Econ_status <- sum(BDT10.1_Econ_status$percentageABS)/nrow(BDT10.1_Econ_status)

BDT10.1_Beroep <- Bias(DT10.1_Beroep, Beroep, "Beroep")
BODT10.1_Beroep <- sum(BDT10.1_Beroep$percentageABS)/nrow(BDT10.1_Beroep)

BDT10.1_SBI <- Bias(DT10.1_SBI, SBI, "SBI")
BODT10.1_SBI <- sum(BDT10.1_SBI$percentageABS)/nrow(BDT10.1_SBI)

BDT10.1_Burg_Staat <- Bias(DT10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT10.1_Burg_Staat <- sum(BDT10.1_Burg_Staat$percentageABS)/nrow(BDT10.1_Burg_Staat)

BiasOverallDT10.1 <- sum(BODT10.1_Geslacht, BODT10.1_Leeftijd, BODT10.1_HH_Pos, BODT10.1_HH_grootte, BODT10.1_Woonregio_vorig_jaar, 
                        BODT10.1_Nationaliteit, BODT10.1_Geboorteland, BODT10.1_Onderwijsniveau, BODT10.1_Econ_status, 
                        BODT10.1_Beroep, BODT10.1_SBI, BODT10.1_Burg_Staat) / 12

rm(DT10.1_Geslacht, DT10.1_Leeftijd, DT10.1_HH_Pos, DT10.1_HH_grootte, DT10.1_Woonregio_vorig_jaar, 
   DT10.1_Nationaliteit, DT10.1_Geboorteland, DT10.1_Onderwijsniveau, DT10.1_Econ_status, 
   DT10.1_Beroep, DT10.1_SBI, DT10.1_Burg_Staat)
rm(BDT10.1_Geslacht, BDT10.1_Leeftijd, BDT10.1_HH_Pos, BDT10.1_HH_grootte, BDT10.1_Woonregio_vorig_jaar, 
   BDT10.1_Nationaliteit, BDT10.1_Geboorteland, BDT10.1_Onderwijsniveau, BDT10.1_Econ_status, 
   BDT10.1_Beroep, BDT10.1_SBI, BDT10.1_Burg_Staat)
rm(BODT10.1_Geslacht, BODT10.1_Leeftijd, BODT10.1_HH_Pos, BODT10.1_HH_grootte, BODT10.1_Woonregio_vorig_jaar, 
   BODT10.1_Nationaliteit, BODT10.1_Geboorteland, BODT10.1_Onderwijsniveau, BODT10.1_Econ_status, 
   BODT10.1_Beroep, BODT10.1_SBI, BODT10.1_Burg_Staat)

# 10.2 data set 
BDT10.2_Geslacht <- Bias(DT10.2_Geslacht, Geslacht, "Geslacht")
BODT10.2_Geslacht <- sum(BDT10.2_Geslacht$percentageABS)/nrow(BDT10.2_Geslacht)

BDT10.2_Leeftijd <- Bias(DT10.2_Leeftijd, Leeftijd, "Leeftijd")
BODT10.2_Leeftijd <- sum(BDT10.2_Leeftijd$percentageABS)/nrow(BDT10.2_Leeftijd)

BDT10.2_HH_Pos <- Bias(DT10.2_HH_Pos, HH_Pos, "HH_Pos")
BODT10.2_HH_Pos <- sum(BDT10.2_HH_Pos$percentageABS)/nrow(BDT10.2_HH_Pos)

BDT10.2_HH_grootte <- Bias(DT10.2_HH_grootte, HH_grootte, "HH_grootte")
BODT10.2_HH_grootte <- sum(BDT10.2_HH_grootte$percentageABS)/nrow(BDT10.2_HH_grootte)

BDT10.2_Woonregio_vorig_jaar <- Bias(DT10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT10.2_Woonregio_vorig_jaar <- sum(BDT10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BDT10.2_Woonregio_vorig_jaar)

BDT10.2_Nationaliteit <- Bias(DT10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT10.2_Nationaliteit <- sum(BDT10.2_Nationaliteit$percentageABS)/nrow(BDT10.2_Nationaliteit)

BDT10.2_Geboorteland <- Bias(DT10.2_Geboorteland, Geboorteland, "Geboorteland")
BODT10.2_Geboorteland <- sum(BDT10.2_Geboorteland$percentageABS)/nrow(BDT10.2_Geboorteland)

BDT10.2_Onderwijsniveau <- Bias(DT10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT10.2_Onderwijsniveau <- sum(BDT10.2_Onderwijsniveau$percentageABS)/nrow(BDT10.2_Onderwijsniveau)

BDT10.2_Econ_status <- Bias(DT10.2_Econ_status, Econ_status, "Econ._status")
BODT10.2_Econ_status <- sum(BDT10.2_Econ_status$percentageABS)/nrow(BDT10.2_Econ_status)

BDT10.2_Beroep <- Bias(DT10.2_Beroep, Beroep, "Beroep")
BODT10.2_Beroep <- sum(BDT10.2_Beroep$percentageABS)/nrow(BDT10.2_Beroep)

BDT10.2_SBI <- Bias(DT10.2_SBI, SBI, "SBI")
BODT10.2_SBI <- sum(BDT10.2_SBI$percentageABS)/nrow(BDT10.2_SBI)

BDT10.2_Burg_Staat <- Bias(DT10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT10.2_Burg_Staat <- sum(BDT10.2_Burg_Staat$percentageABS)/nrow(BDT10.2_Burg_Staat)

BiasOverallDT10.2 <- sum(BODT10.2_Geslacht, BODT10.2_Leeftijd, BODT10.2_HH_Pos, BODT10.2_HH_grootte, BODT10.2_Woonregio_vorig_jaar, 
                        BODT10.2_Nationaliteit, BODT10.2_Geboorteland, BODT10.2_Onderwijsniveau, BODT10.2_Econ_status, 
                        BODT10.2_Beroep, BODT10.2_SBI, BODT10.2_Burg_Staat) / 12

rm(DT10.2_Geslacht, DT10.2_Leeftijd, DT10.2_HH_Pos, DT10.2_HH_grootte, DT10.2_Woonregio_vorig_jaar, 
   DT10.2_Nationaliteit, DT10.2_Geboorteland, DT10.2_Onderwijsniveau, DT10.2_Econ_status, 
   DT10.2_Beroep, DT10.2_SBI, DT10.2_Burg_Staat)
rm(BDT10.2_Geslacht, BDT10.2_Leeftijd, BDT10.2_HH_Pos, BDT10.2_HH_grootte, BDT10.2_Woonregio_vorig_jaar, 
   BDT10.2_Nationaliteit, BDT10.2_Geboorteland, BDT10.2_Onderwijsniveau, BDT10.2_Econ_status, 
   BDT10.2_Beroep, BDT10.2_SBI, BDT10.2_Burg_Staat)
rm(BODT10.2_Geslacht, BODT10.2_Leeftijd, BODT10.2_HH_Pos, BODT10.2_HH_grootte, BODT10.2_Woonregio_vorig_jaar, 
   BODT10.2_Nationaliteit, BODT10.2_Geboorteland, BODT10.2_Onderwijsniveau, BODT10.2_Econ_status, 
   BODT10.2_Beroep, BODT10.2_SBI, BODT10.2_Burg_Staat)

# 10.3 data set 
BDT10.3_Geslacht <- Bias(DT10.3_Geslacht, Geslacht, "Geslacht")
BODT10.3_Geslacht <- sum(BDT10.3_Geslacht$percentageABS)/nrow(BDT10.3_Geslacht)

BDT10.3_Leeftijd <- Bias(DT10.3_Leeftijd, Leeftijd, "Leeftijd")
BODT10.3_Leeftijd <- sum(BDT10.3_Leeftijd$percentageABS)/nrow(BDT10.3_Leeftijd)

BDT10.3_HH_Pos <- Bias(DT10.3_HH_Pos, HH_Pos, "HH_Pos")
BODT10.3_HH_Pos <- sum(BDT10.3_HH_Pos$percentageABS)/nrow(BDT10.3_HH_Pos)

BDT10.3_HH_grootte <- Bias(DT10.3_HH_grootte, HH_grootte, "HH_grootte")
BODT10.3_HH_grootte <- sum(BDT10.3_HH_grootte$percentageABS)/nrow(BDT10.3_HH_grootte)

BDT10.3_Woonregio_vorig_jaar <- Bias(DT10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BODT10.3_Woonregio_vorig_jaar <- sum(BDT10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BDT10.3_Woonregio_vorig_jaar)

BDT10.3_Nationaliteit <- Bias(DT10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BODT10.3_Nationaliteit <- sum(BDT10.3_Nationaliteit$percentageABS)/nrow(BDT10.3_Nationaliteit)

BDT10.3_Geboorteland <- Bias(DT10.3_Geboorteland, Geboorteland, "Geboorteland")
BODT10.3_Geboorteland <- sum(BDT10.3_Geboorteland$percentageABS)/nrow(BDT10.3_Geboorteland)

BDT10.3_Onderwijsniveau <- Bias(DT10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BODT10.3_Onderwijsniveau <- sum(BDT10.3_Onderwijsniveau$percentageABS)/nrow(BDT10.3_Onderwijsniveau)

BDT10.3_Econ_status <- Bias(DT10.3_Econ_status, Econ_status, "Econ._status")
BODT10.3_Econ_status <- sum(BDT10.3_Econ_status$percentageABS)/nrow(BDT10.3_Econ_status)

BDT10.3_Beroep <- Bias(DT10.3_Beroep, Beroep, "Beroep")
BODT10.3_Beroep <- sum(BDT10.3_Beroep$percentageABS)/nrow(BDT10.3_Beroep)

BDT10.3_SBI <- Bias(DT10.3_SBI, SBI, "SBI")
BODT10.3_SBI <- sum(BDT10.3_SBI$percentageABS)/nrow(BDT10.3_SBI)

BDT10.3_Burg_Staat <- Bias(DT10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BODT10.3_Burg_Staat <- sum(BDT10.3_Burg_Staat$percentageABS)/nrow(BDT10.3_Burg_Staat)

BiasOverallDT10.3 <- sum(BODT10.3_Geslacht, BODT10.3_Leeftijd, BODT10.3_HH_Pos, BODT10.3_HH_grootte, BODT10.3_Woonregio_vorig_jaar, 
                        BODT10.3_Nationaliteit, BODT10.3_Geboorteland, BODT10.3_Onderwijsniveau, BODT10.3_Econ_status, 
                        BODT10.3_Beroep, BODT10.3_SBI, BODT10.3_Burg_Staat) / 12

rm(DT10.3_Geslacht, DT10.3_Leeftijd, DT10.3_HH_Pos, DT10.3_HH_grootte, DT10.3_Woonregio_vorig_jaar, 
   DT10.3_Nationaliteit, DT10.3_Geboorteland, DT10.3_Onderwijsniveau, DT10.3_Econ_status, 
   DT10.3_Beroep, DT10.3_SBI, DT10.3_Burg_Staat)
rm(BDT10.3_Geslacht, BDT10.3_Leeftijd, BDT10.3_HH_Pos, BDT10.3_HH_grootte, BDT10.3_Woonregio_vorig_jaar, 
   BDT10.3_Nationaliteit, BDT10.3_Geboorteland, BDT10.3_Onderwijsniveau, BDT10.3_Econ_status, 
   BDT10.3_Beroep, BDT10.3_SBI, BDT10.3_Burg_Staat)
rm(BODT10.3_Geslacht, BODT10.3_Leeftijd, BODT10.3_HH_Pos, BODT10.3_HH_grootte, BODT10.3_Woonregio_vorig_jaar, 
   BODT10.3_Nationaliteit, BODT10.3_Geboorteland, BODT10.3_Onderwijsniveau, BODT10.3_Econ_status, 
   BODT10.3_Beroep, BODT10.3_SBI, BODT10.3_Burg_Staat)


