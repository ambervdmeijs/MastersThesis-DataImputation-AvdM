
# Installing packages -----------------------------------------------------------------------------------------------------------------
# install.packages("hot.deck")
# install.packages("devtools")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("hot.deck")
library("devtools")
# install_github("jabiru/tictoc")
library("tictoc")


## Loading ipums data set to environment ----------------------------------------------------------------------------------------------------
ipums <- get(load("input/ipums.Rdata"))


## Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

# Setting correct and total to '0' 
HD2.11_correct <- HD2.12_correct <- HD2.13_correct <- HD2.14_correct <- HD2.15_correct <- 0 
HD2.21_correct <- HD2.22_correct <- HD2.23_correct <- HD2.24_correct <- HD2.25_correct <- 0 
HD2.31_correct <- HD2.32_correct <- HD2.33_correct <- HD2.34_correct <- HD2.35_correct <- 0 

HD5.11_correct <- HD5.12_correct <- HD5.13_correct <- HD5.14_correct <- HD5.15_correct <- 0 
HD5.21_correct <- HD5.22_correct <- HD5.23_correct <- HD5.24_correct <- HD5.25_correct <- 0 
HD5.31_correct <- HD5.32_correct <- HD5.33_correct <- HD5.34_correct <- HD5.35_correct <- 0 

HD10.11_correct <- HD10.12_correct <- HD10.13_correct <- HD10.14_correct <- HD10.15_correct <- 0 
HD10.21_correct <- HD10.22_correct <- HD10.23_correct <- HD10.24_correct <- HD10.25_correct <- 0 
HD10.31_correct <- HD10.32_correct <- HD10.33_correct <- HD10.34_correct <- HD10.35_correct <- 0 

HD2.11_total <- HD2.12_total <- HD2.13_total <- HD2.14_total <- HD2.15_total <- 0 
HD2.21_total <- HD2.22_total <- HD2.23_total <- HD2.24_total <- HD2.25_total <- 0 
HD2.31_total <- HD2.32_total <- HD2.33_total <- HD2.34_total <- HD2.35_total <- 0 

HD5.11_total <- HD5.12_total <- HD5.13_total <- HD5.14_total <- HD5.15_total <- 0 
HD5.21_total <- HD5.22_total <- HD5.23_total <- HD5.24_total <- HD5.25_total <- 0 
HD5.31_total <- HD5.32_total <- HD5.33_total <- HD5.34_total <- HD5.35_total <- 0 

HD10.11_total <- HD10.12_total <- HD10.13_total <- HD10.14_total <- HD10.15_total <- 0 
HD10.21_total <- HD10.22_total <- HD10.23_total <- HD10.24_total <- HD10.25_total <- 0 
HD10.31_total <- HD10.32_total <- HD10.33_total <- HD10.34_total <- HD10.35_total <- 0 

## Hot Deck imputation with 'hot.deck' ------------------------------------------------------------------------------------------------------

# 2% data sets

# 2.1 data set
MCAR2.1 <- get(load(file = "MCAR data/MCAR2_1.Rdata"))
HD_MCAR2.1 <- data.frame(MCAR2.1)
names(HD_MCAR2.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR2.1)) {
  HD_MCAR2.1[, i] <- as.factor(HD_MCAR2.1[, i])
}
rm(MCAR2.1)

tic("HotDeck Imputation 2.1 processing time...")
hot_deck2.1 <- hot.deck(HD_MCAR2.1, m = 3, method = "p.draw")
process_timehd2.1 <- toc(log = TRUE)
process_timehd2.1
save(process_timehd2.1, file = "process_timehd21")
rm(HD_MCAR2.1)

df_hot_deck2.11 <- as.data.frame(hot_deck2.1[["data"]][[1]])
df_hot_deck2.12 <- as.data.frame(hot_deck2.1[["data"]][[2]])
df_hot_deck2.13 <- as.data.frame(hot_deck2.1[["data"]][[3]])
df_hot_deck2.14 <- as.data.frame(hot_deck2.1[["data"]][[4]])
df_hot_deck2.15 <- as.data.frame(hot_deck2.1[["data"]][[5]])
save(df_hot_deck2.11, file = "hot_deck211.Rdata")
save(df_hot_deck2.12, file = "hot_deck212.Rdata")
save(df_hot_deck2.13, file = "hot_deck213.Rdata")
save(df_hot_deck2.14, file = "hot_deck214.Rdata")
save(df_hot_deck2.15, file = "hot_deck215.Rdata")
rm(hot_deck2.1)

HD2.11_correct <- HD2.11_correct + sum(ipums == df_hot_deck2.11[["data"]][[1]])
HD2.12_correct <- HD2.12_correct + sum(ipums == df_hot_deck2.12[["data"]][[2]])
HD2.13_correct <- HD2.13_correct + sum(ipums == df_hot_deck2.13[["data"]][[3]])
HD2.14_correct <- HD2.14_correct + sum(ipums == df_hot_deck2.14[["data"]][[4]])
HD2.15_correct <- HD2.15_correct + sum(ipums == df_hot_deck2.15[["data"]][[5]])
HD2.1_correct <- (sum(HD2.11_correct, HD2.12_correct, HD2.13_correct, HD2.14_correct, HD2.15_correct) / length(hot_deck2.1[["data"]]))
HD2.1_total <- HD2.11_total + sum(!is.na(hot_deck2.1[["data"]][[1]]))

rm(df_hot_deck2.11, df_hot_deck2.12, df_hot_deck2.13, df_hot_deck2.14, df_hot_deck2.15)

# 2.2 data set
MCAR2.2 <- get(load(file = "MCAR data/MCAR2_2.Rdata"))
HD_MCAR2.2 <- data.frame(MCAR2.2)
names(HD_MCAR2.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR2.2)) {
  HD_MCAR2.2[, i] <- as.factor(HD_MCAR2.2[, i])
}
rm(MCAR2.2)

tic("HotDeck Imputation 2.2 processing time...")
hot_deck2.2 <- hot.deck(HD_MCAR2.2, m = 3, method = "p.draw")
process_timehd2.2 <- toc(log = TRUE)
process_timehd2.2
save(process_timehd2.2, file = "process_timehd22")
rm(HD_MCAR2.2)

df_hot_deck2.21 <- as.data.frame(hot_deck2.2[["data"]][[1]])
df_hot_deck2.22 <- as.data.frame(hot_deck2.2[["data"]][[2]])
df_hot_deck2.23 <- as.data.frame(hot_deck2.2[["data"]][[3]])
df_hot_deck2.24 <- as.data.frame(hot_deck2.2[["data"]][[4]])
df_hot_deck2.25 <- as.data.frame(hot_deck2.2[["data"]][[5]])
save(df_hot_deck2.21, file = "hot_deck221.Rdata")
save(df_hot_deck2.22, file = "hot_deck222.Rdata")
save(df_hot_deck2.23, file = "hot_deck223.Rdata")
save(df_hot_deck2.24, file = "hot_deck224.Rdata")
save(df_hot_deck2.25, file = "hot_deck225.Rdata")
rm(hot_deck2.2)

HD2.21_correct <- HD2.21_correct + sum(ipums == df_hot_deck2.21[["data"]][[1]])
HD2.22_correct <- HD2.22_correct + sum(ipums == df_hot_deck2.22[["data"]][[2]])
HD2.23_correct <- HD2.23_correct + sum(ipums == df_hot_deck2.23[["data"]][[3]])
HD2.24_correct <- HD2.24_correct + sum(ipums == df_hot_deck2.24[["data"]][[4]])
HD2.25_correct <- HD2.25_correct + sum(ipums == df_hot_deck2.25[["data"]][[5]])
HD2.2_correct <- (sum(HD2.21_correct, HD2.22_correct, HD2.23_correct, HD2.24_correct, HD2.25_correct) / length(hot_deck2.2[["data"]]))
HD2.2_total <- HD2.21_total + sum(!is.na(hot_deck2.1[["data"]][[1]]))

rm(df_hot_deck2.21, df_hot_deck2.22, df_hot_deck2.23, df_hot_deck2.24, df_hot_deck2.25)

# 2.3 data set
MCAR2.3 <- get(load(file = "MCAR data/MCAR2_3.Rdata"))
HD_MCAR2.3 <- data.frame(MCAR2.3)
names(HD_MCAR2.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR2.3)) {
  HD_MCAR2.3[, i] <- as.factor(HD_MCAR2.3[, i])
}
rm(MCAR2.3)

tic("HotDeck Imputation 2.3 processing time...")
hot_deck2.3 <- hot.deck(HD_MCAR2.3, m = 3, method = "p.draw")
process_timehd2.3 <- toc(log = TRUE)
process_timehd2.3
save(process_timehd2.3, file = "process_timehd23")
rm(HD_MCAR2.3)

df_hot_deck2.31 <- as.data.frame(hot_deck2.3[["data"]][[1]])
df_hot_deck2.32 <- as.data.frame(hot_deck2.3[["data"]][[2]])
df_hot_deck2.33 <- as.data.frame(hot_deck2.3[["data"]][[3]])
df_hot_deck2.34 <- as.data.frame(hot_deck2.3[["data"]][[4]])
df_hot_deck2.35 <- as.data.frame(hot_deck2.3[["data"]][[5]])
save(df_hot_deck2.31, file = "hot_deck231.Rdata")
save(df_hot_deck2.32, file = "hot_deck232.Rdata")
save(df_hot_deck2.33, file = "hot_deck233.Rdata")
save(df_hot_deck2.34, file = "hot_deck234.Rdata")
save(df_hot_deck2.35, file = "hot_deck235.Rdata")
rm(hot_deck2.3)

HD2.31_correct <- HD2.31_correct + sum(ipums == df_hot_deck2.31[["data"]][[1]])
HD2.32_correct <- HD2.32_correct + sum(ipums == df_hot_deck2.32[["data"]][[2]])
HD2.33_correct <- HD2.33_correct + sum(ipums == df_hot_deck2.33[["data"]][[3]])
HD2.34_correct <- HD2.34_correct + sum(ipums == df_hot_deck2.34[["data"]][[4]])
HD2.35_correct <- HD2.35_correct + sum(ipums == df_hot_deck2.35[["data"]][[5]])
HD2.3_correct <- (sum(HD2.31_correct, HD2.32_correct, HD2.33_correct, HD2.34_correct, HD2.35_correct) / length(hot_deck2.3[["data"]]))
HD2.3_total <- HD2.31_total + sum(!is.na(hot_deck2.3[["data"]][[1]]))

rm(df_hot_deck2.31, df_hot_deck2.32, df_hot_deck2.33, df_hot_deck2.34, df_hot_deck2.35)


# 5% data sets 

# 5.1 data set 
MCAR5.1 <- get(load(file = "MCAR data/MCAR5_1.Rdata"))
HD_MCAR5.1 <- data.frame(MCAR5.1)
names(HD_MCAR5.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR5.1)) {
  HD_MCAR5.1[, i] <- as.factor(HD_MCAR5.1[, i])
}
rm(MCAR5.1)

tic("HotDeck Imputation 5.1 processing time...")
hot_deck5.1 <- hot.deck(HD_MCAR5.1, m = 3, method = "p.draw")
process_timehd5.1 <- toc(log = TRUE)
process_timehd5.1
save(process_timehd5.1, file = "process_timehd51")
rm(HD_MCAR5.1)

df_hot_deck5.11 <- as.data.frame(hot_deck5.1[["data"]][[1]])
df_hot_deck5.12 <- as.data.frame(hot_deck5.1[["data"]][[2]])
df_hot_deck5.13 <- as.data.frame(hot_deck5.1[["data"]][[3]])
df_hot_deck5.14 <- as.data.frame(hot_deck5.1[["data"]][[4]])
df_hot_deck5.15 <- as.data.frame(hot_deck5.1[["data"]][[5]])
save(df_hot_deck5.11, file = "hot_deck511.Rdata")
save(df_hot_deck5.12, file = "hot_deck512.Rdata")
save(df_hot_deck5.13, file = "hot_deck513.Rdata")
save(df_hot_deck5.14, file = "hot_deck514.Rdata")
save(df_hot_deck5.15, file = "hot_deck515.Rdata")
rm(hot_deck5.1)

HD5.11_correct <- HD5.11_correct + sum(ipums == df_hot_deck5.11[["data"]][[1]])
HD5.12_correct <- HD5.12_correct + sum(ipums == df_hot_deck5.12[["data"]][[2]])
HD5.13_correct <- HD5.13_correct + sum(ipums == df_hot_deck5.13[["data"]][[3]])
HD5.14_correct <- HD5.14_correct + sum(ipums == df_hot_deck5.14[["data"]][[4]])
HD5.15_correct <- HD5.15_correct + sum(ipums == df_hot_deck5.15[["data"]][[5]])
HD5.1_correct <- (sum(HD5.11_correct, HD5.12_correct, HD5.13_correct, HD5.14_correct, HD5.15_correct) / length(hot_deck5.1[["data"]]))
HD5.1_total <- HD5.11_total + sum(!is.na(hot_deck5.1[["data"]][[1]]))

rm(df_hot_deck5.11, df_hot_deck5.12, df_hot_deck5.13, df_hot_deck5.14, df_hot_deck5.15)

# 5.2 data set 
MCAR5.2 <- get(load(file = "MCAR data/MCAR5_2.Rdata"))
HD_MCAR5.2 <- data.frame(MCAR5.2)
names(HD_MCAR5.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR5.2)) {
  HD_MCAR5.2[, i] <- as.factor(HD_MCAR5.2[, i])
}
rm(MCAR5.2)

tic("HotDeck Imputation 5.2 processing time...")
hot_deck5.2 <- hot.deck(HD_MCAR5.2, m = 3, method = "p.draw")
process_timehd5.2 <- toc(log = TRUE)
process_timehd5.2
save(process_timehd5.2, file = "process_timehd52")
rm(HD_MCAR5.2)

df_hot_deck5.21 <- as.data.frame(hot_deck5.2[["data"]][[1]])
df_hot_deck5.22 <- as.data.frame(hot_deck5.2[["data"]][[2]])
df_hot_deck5.23 <- as.data.frame(hot_deck5.2[["data"]][[3]])
df_hot_deck5.24 <- as.data.frame(hot_deck5.2[["data"]][[4]])
df_hot_deck5.25 <- as.data.frame(hot_deck5.2[["data"]][[5]])
save(df_hot_deck5.21, file = "hot_deck521.Rdata")
save(df_hot_deck5.22, file = "hot_deck522.Rdata")
save(df_hot_deck5.23, file = "hot_deck523.Rdata")
save(df_hot_deck5.24, file = "hot_deck524.Rdata")
save(df_hot_deck5.25, file = "hot_deck525.Rdata")
rm(hot_deck5.2)

HD5.21_correct <- HD5.21_correct + sum(ipums == df_hot_deck5.21[["data"]][[1]])
HD5.22_correct <- HD5.22_correct + sum(ipums == df_hot_deck5.22[["data"]][[2]])
HD5.23_correct <- HD5.23_correct + sum(ipums == df_hot_deck5.23[["data"]][[3]])
HD5.24_correct <- HD5.24_correct + sum(ipums == df_hot_deck5.24[["data"]][[4]])
HD5.25_correct <- HD5.25_correct + sum(ipums == df_hot_deck5.25[["data"]][[5]])
HD5.2_correct <- (sum(HD5.21_correct, HD5.22_correct, HD5.23_correct, HD5.24_correct, HD5.25_correct) / length(hot_deck5.2[["data"]]))
HD5.2_total <- HD5.21_total + sum(!is.na(hot_deck5.2[["data"]][[1]]))

rm(df_hot_deck5.21, df_hot_deck5.22, df_hot_deck5.23, df_hot_deck5.24, df_hot_deck5.25)

# 5.3 data set 
MCAR5.3 <- get(load(file = "MCAR data/MCAR5_3.Rdata"))
HD_MCAR5.3 <- data.frame(MCAR5.3)
names(HD_MCAR5.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR5.3)) {
  HD_MCAR5.3[, i] <- as.factor(HD_MCAR5.3[, i])
}
rm(MCAR5.3)

tic("HotDeck Imputation 5.3 processing time...")
hot_deck5.3 <- hot.deck(HD_MCAR5.3, m = 3, method = "p.draw")
process_timehd5.3 <- toc(log = TRUE)
process_timehd5.3
save(process_timehd5.3, file = "process_timehd53")
rm(HD_MCAR5.3)

df_hot_deck5.31 <- as.data.frame(hot_deck5.3[["data"]][[1]])
df_hot_deck5.32 <- as.data.frame(hot_deck5.3[["data"]][[2]])
df_hot_deck5.33 <- as.data.frame(hot_deck5.3[["data"]][[3]])
df_hot_deck5.34 <- as.data.frame(hot_deck5.3[["data"]][[4]])
df_hot_deck5.35 <- as.data.frame(hot_deck5.3[["data"]][[5]])
save(df_hot_deck5.31, file = "hot_deck531.Rdata")
save(df_hot_deck5.32, file = "hot_deck532.Rdata")
save(df_hot_deck5.33, file = "hot_deck533.Rdata")
save(df_hot_deck5.34, file = "hot_deck534.Rdata")
save(df_hot_deck5.35, file = "hot_deck535.Rdata")
rm(hot_deck5.3)

HD5.31_correct <- HD5.31_correct + sum(ipums == df_hot_deck5.31[["data"]][[1]])
HD5.32_correct <- HD5.32_correct + sum(ipums == df_hot_deck5.32[["data"]][[2]])
HD5.33_correct <- HD5.33_correct + sum(ipums == df_hot_deck5.33[["data"]][[3]])
HD5.34_correct <- HD5.34_correct + sum(ipums == df_hot_deck5.34[["data"]][[4]])
HD5.35_correct <- HD5.35_correct + sum(ipums == df_hot_deck5.35[["data"]][[5]])
HD5.3_correct <- (sum(HD5.31_correct, HD5.32_correct, HD5.33_correct, HD5.34_correct, HD5.35_correct) / length(hot_deck5.3[["data"]]))
HD5.3_total <- HD5.31_total + sum(!is.na(hot_deck5.3[["data"]][[1]]))

rm(df_hot_deck5.31, df_hot_deck5.32, df_hot_deck5.33, df_hot_deck5.34, df_hot_deck5.35)


# 10% data sets

# 10.1 data set
MCAR10.1 <- get(load(file = "MCAR data/MCAR10_1.Rdata"))
HD_MCAR10.1 <- data.frame(MCAR10.1)
names(HD_MCAR10.1) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR10.1)) {
  HD_MCAR10.1[, i] <- as.factor(HD_MCAR10.1[, i])
}
rm(MCAR10.1)

tic("HotDeck Imputation 10.1 processing time...")
hot_deck10.1 <- hot.deck(HD_MCAR10.1, m = 3, method = "p.draw")
process_timehd10.1 <- toc(log = TRUE)
process_timehd10.1
save(process_timehd10.1, file = "process_timehd101")
rm(HD_MCAR10.1)

df_hot_deck10.11 <- as.data.frame(hot_deck10.1[["data"]][[1]])
df_hot_deck10.12 <- as.data.frame(hot_deck10.1[["data"]][[2]])
df_hot_deck10.13 <- as.data.frame(hot_deck10.1[["data"]][[3]])
df_hot_deck10.14 <- as.data.frame(hot_deck10.1[["data"]][[4]])
df_hot_deck10.15 <- as.data.frame(hot_deck10.1[["data"]][[5]])
save(df_hot_deck10.11, file = "hot_deck1011.Rdata")
save(df_hot_deck10.12, file = "hot_deck1012.Rdata")
save(df_hot_deck10.13, file = "hot_deck1013.Rdata")
save(df_hot_deck10.14, file = "hot_deck1014.Rdata")
save(df_hot_deck10.15, file = "hot_deck1015.Rdata")
rm(hot_deck10.1)

HD10.11_correct <- HD10.11_correct + sum(ipums == df_hot_deck10.11[["data"]][[1]])
HD10.12_correct <- HD10.12_correct + sum(ipums == df_hot_deck10.12[["data"]][[2]])
HD10.13_correct <- HD10.13_correct + sum(ipums == df_hot_deck10.13[["data"]][[3]])
HD10.14_correct <- HD10.14_correct + sum(ipums == df_hot_deck10.14[["data"]][[4]])
HD10.15_correct <- HD10.15_correct + sum(ipums == df_hot_deck10.15[["data"]][[5]])
HD10.1_correct <- (sum(HD10.11_correct, HD10.12_correct, HD10.13_correct, HD10.14_correct, HD10.15_correct) / length(hot_deck10.1[["data"]]))
HD10.1_total <- HD10.11_total + sum(!is.na(hot_deck10.1[["data"]][[1]]))

rm(df_hot_deck10.11, df_hot_deck10.12, df_hot_deck10.13, df_hot_deck10.14, df_hot_deck10.15)

# 10.2 data set
MCAR10.2 <- get(load(file = "MCAR data/MCAR10_2.Rdata"))
HD_MCAR10.2 <- data.frame(MCAR10.2)
names(HD_MCAR10.2) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR10.2)) {
  HD_MCAR10.2[, i] <- as.factor(HD_MCAR10.2[, i])
}
rm(MCAR10.2)

tic("HotDeck Imputation 10.2 processing time...")
hot_deck10.2 <- hot.deck(HD_MCAR10.2, m = 3, method = "p.draw")
process_timehd10.2 <- toc(log = TRUE)
process_timehd10.2
save(process_timehd10.2, file = "process_timehd102")
rm(HD_MCAR10.2)

df_hot_deck10.21 <- as.data.frame(hot_deck10.2[["data"]][[1]])
df_hot_deck10.22 <- as.data.frame(hot_deck10.2[["data"]][[2]])
df_hot_deck10.23 <- as.data.frame(hot_deck10.2[["data"]][[3]])
df_hot_deck10.24 <- as.data.frame(hot_deck10.2[["data"]][[4]])
df_hot_deck10.25 <- as.data.frame(hot_deck10.2[["data"]][[5]])
save(df_hot_deck10.21, file = "hot_deck1021.Rdata")
save(df_hot_deck10.22, file = "hot_deck1022.Rdata")
save(df_hot_deck10.23, file = "hot_deck1023.Rdata")
save(df_hot_deck10.24, file = "hot_deck1024.Rdata")
save(df_hot_deck10.25, file = "hot_deck1025.Rdata")
rm(hot_deck10.2)

HD10.21_correct <- HD10.21_correct + sum(ipums == df_hot_deck10.21[["data"]][[1]])
HD10.22_correct <- HD10.22_correct + sum(ipums == df_hot_deck10.22[["data"]][[2]])
HD10.23_correct <- HD10.23_correct + sum(ipums == df_hot_deck10.23[["data"]][[3]])
HD10.24_correct <- HD10.24_correct + sum(ipums == df_hot_deck10.24[["data"]][[4]])
HD10.25_correct <- HD10.25_correct + sum(ipums == df_hot_deck10.25[["data"]][[5]])
HD10.2_correct <- (sum(HD10.21_correct, HD10.22_correct, HD10.23_correct, HD10.24_correct, HD10.25_correct) / length(hot_deck10.2[["data"]]))
HD10.2_total <- HD10.21_total + sum(!is.na(hot_deck10.2[["data"]][[1]]))

rm(df_hot_deck10.21, df_hot_deck10.22, df_hot_deck10.23, df_hot_deck10.24, df_hot_deck10.25)

# 10.3 data set
MCAR10.3 <- get(load(file = "MCAR data/MCAR10_3.Rdata"))
HD_MCAR10.3 <- data.frame(MCAR10.3)
names(HD_MCAR10.3) <- gsub(" ", "_", names(ipums), fixed=TRUE)
for (i in 1:ncol(HD_MCAR10.3)) {
  HD_MCAR10.3[, i] <- as.factor(HD_MCAR10.3[, i])
}
rm(MCAR10.3)

tic("HotDeck Imputation 10.3 processing time...")
hot_deck10.3 <- hot.deck(HD_MCAR10.3, m = 3, method = "p.draw")
process_timehd10.3 <- toc(log = TRUE)
process_timehd10.3
save(process_time10.3, file = "process_timehd103")
rm(HD_MCAR10.3)

df_hot_deck10.31 <- as.data.frame(hot_deck10.3[["data"]][[1]])
df_hot_deck10.32 <- as.data.frame(hot_deck10.3[["data"]][[2]])
df_hot_deck10.33 <- as.data.frame(hot_deck10.3[["data"]][[3]])
df_hot_deck10.34 <- as.data.frame(hot_deck10.3[["data"]][[4]])
df_hot_deck10.35 <- as.data.frame(hot_deck10.3[["data"]][[5]])
save(df_hot_deck10.31, file = "hot_deck1031.Rdata")
save(df_hot_deck10.32, file = "hot_deck1032.Rdata")
save(df_hot_deck10.33, file = "hot_deck1033.Rdata")
save(df_hot_deck10.34, file = "hot_deck1034.Rdata")
save(df_hot_deck10.35, file = "hot_deck1035.Rdata")
rm(hot_deck10.3)

HD10.31_correct <- HD10.31_correct + sum(ipums == df_hot_deck10.31[["data"]][[1]])
HD10.32_correct <- HD10.32_correct + sum(ipums == df_hot_deck10.32[["data"]][[2]])
HD10.33_correct <- HD10.33_correct + sum(ipums == df_hot_deck10.33[["data"]][[3]])
HD10.34_correct <- HD10.34_correct + sum(ipums == df_hot_deck10.34[["data"]][[4]])
HD10.35_correct <- HD10.35_correct + sum(ipums == df_hot_deck10.35[["data"]][[5]])
HD10.3_correct <- (sum(HD10.31_correct, HD10.32_correct, HD10.33_correct, HD10.34_correct, HD10.35_correct) / length(hot_deck10.3[["data"]]))
HD10.3_total <- HD10.31_total + sum(!is.na(hot_deck10.3[["data"]][[1]]))

rm(df_hot_deck10.31, df_hot_deck10.32, df_hot_deck10.33, df_hot_deck10.34, df_hot_deck10.35)


# Check if all values are imputed 
#anyNA(c(df_hot_deck2.11, df_hot_deck2.12, df_hot_deck2.13, df_hot_deck2.14, df_hot_deck2.15, 
        #df_hot_deck2.21, df_hot_deck2.22, df_hot_deck2.23, df_hot_deck2.24, df_hot_deck2.25, 
       # df_hot_deck2.31, df_hot_deck2.32, df_hot_deck2.33, df_hot_deck2.34, df_hot_deck2.35, 
        
        #df_hot_deck5.11, df_hot_deck5.12, df_hot_deck5.13, df_hot_deck5.14, df_hot_deck5.15,
        #df_hot_deck5.21, df_hot_deck5.22, df_hot_deck5.23, df_hot_deck5.24, df_hot_deck5.25, 
        #df_hot_deck5.31, df_hot_deck5.32, df_hot_deck5.33, df_hot_deck5.34, df_hot_deck5.35, 
        
        #df_hot_deck10.11, df_hot_deck10.12, df_hot_deck10.13, df_hot_deck10.14, df_hot_deck10.15, 
        #df_hot_deck10.21, df_hot_deck10.22, df_hot_deck10.23, df_hot_deck10.24, df_hot_deck10.25, 
        #df_hot_deck10.31, df_hot_deck10.32, df_hot_deck10.33, df_hot_deck10.34, df_hot_deck10.35))


# Computing the accuracy of imputation ------------------------------------------------------------------------------------------------
HD2.1_accuracy <- HD2.1_correct / HD2.1_total
HD2.2_accuracy <- HD2.2_correct / HD2.2_total
HD2.3_accuracy <- HD2.3_correct / HD2.3_total

HD5.1_accuracy <- HD5.1_correct / HD5.1_total
HD5.2_accuracy <- HD5.2_correct / HD5.2_total
HD5.3_accuracy <- HD5.3_correct / HD5.3_total

HD10.1_accuracy <- HD10.1_correct / HD10.1_total
HD10.2_accuracy <- HD10.2_correct / HD10.2_total
HD10.3_accuracy <- HD10.3_correct / HD10.3_total


## Dataframe with accuracy -----------------------------------------------------------------------------------------------------------------------------------------------
HD_results <- data.frame(Data.set = c("2% - version 1",
                                      "2% - version 2",
                                      "2% - version 3", 
                                      "5% - version 1", 
                                      "5% - version 2", 
                                      "5% - version 3",
                                      "10% - version 1",
                                      "10% - version 2", 
                                      "10% - version 3"),
                         
                         
                         Accuracy = c(HD2.1_accuracy, 
                                      HD2.2_accuracy, 
                                      HD2.3_accuracy, 
                                      HD5.1_accuracy,
                                      HD5.2_accuracy,
                                      HD5.3_accuracy,
                                      HD10.1_accuracy, 
                                      HD10.2_accuracy,
                                      HD10.3_accuracy))  

save(HD_results, file = "HD_results.Rdata")
save.image(file='HotDeck_environment.RData')


# Computing F1-score
# install.packages("MLmetrics")
# library("MLmetrics")

# F1_Score(MCAR2.1, ipums, positive = NULL)



