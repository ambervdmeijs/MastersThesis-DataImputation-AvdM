
# Installing packages -----------------------------------------------------------------------------------------------------------------
#install.packages("hot.deck")
#install.packages("devtools")
#install.packages("dplyr")
#install.packages("plyr")


# Loading packages --------------------------------------------------------------------------------------------------------------------
library("hot.deck")
library("devtools")
# install_github("jabiru/tictoc")
library("tictoc")
library("dplyr")
library("plyr")


## Loading ipums data set to environment ----------------------------------------------------------------------------------------------------
sub_ipums5 <- get(load("sub_ipums5.Rdata"))


## Training, testing, predicting and imputing -----------------------------------------------------------------------------------------

# Setting correct and total to '0' 
SHD2.1_correct <- SHD2.2_correct <- SHD2.3_correct <- 0
SHD5.1_correct <- SHD5.2_correct <- SHD5.3_correct <- 0
SHD10.1_correct <- SHD10.2_correct <- SHD10.3_correct <- 0

SHD2.11_correct <- SHD2.12_correct <- SHD2.13_correct <- SHD2.14_correct <- SHD2.15_correct <- 0 
SHD2.21_correct <- SHD2.22_correct <- SHD2.23_correct <- SHD2.24_correct <- SHD2.25_correct <- 0 
SHD2.31_correct <- SHD2.32_correct <- SHD2.33_correct <- SHD2.34_correct <- SHD2.35_correct <- 0 

SHD5.11_correct <- SHD5.12_correct <- SHD5.13_correct <- SHD5.14_correct <- SHD5.15_correct <- 0 
SHD5.21_correct <- SHD5.22_correct <- SHD5.23_correct <- SHD5.24_correct <- SHD5.25_correct <- 0 
SHD5.31_correct <- SHD5.32_correct <- SHD5.33_correct <- SHD5.34_correct <- SHD5.35_correct <- 0 

SHD10.11_correct <- SHD10.12_correct <- SHD10.13_correct <- SHD10.14_correct <- SHD10.15_correct <- 0 
SHD10.21_correct <- SHD10.22_correct <- SHD10.23_correct <- SHD10.24_correct <- SHD10.25_correct <- 0 
SHD10.31_correct <- SHD10.32_correct <- SHD10.33_correct <- SHD10.34_correct <- SHD10.35_correct <- 0 

# Total
SHD2.1_total <- SHD2.2_total <- SHD2.3_total <- 0
SHD5.1_total <- SHD5.2_total <- SHD5.3_total <- 0
SHD10.1_total <- SHD10.2_total <- SHD10.3_total <- 0


## Hot Deck imputation with 'hot.deck' ------------------------------------------------------------------------------------------------------

# 2% data sets

# 2.1 data set
sub5_MCAR2.1 <- get(load(file = "sub5_MCAR21.Rdata"))
SHD_MCAR2.1 <- data.frame(sub5_MCAR2.1)
names(SHD_MCAR2.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR2.1)) {
  SHD_MCAR2.1[, i] <- as.factor(SHD_MCAR2.1[, i])
}
rm(sub5_MCAR2.1)

tic("HotDeck Imputation 2.1 processing time...")
S5hot_deck2.1 <- hot.deck(SHD_MCAR2.1, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR2.1)

df_Shot_deck2.11 <- as.data.frame(S5hot_deck2.1[["data"]][[1]])
df_Shot_deck2.12 <- as.data.frame(S5hot_deck2.1[["data"]][[2]])
df_Shot_deck2.13 <- as.data.frame(S5hot_deck2.1[["data"]][[3]])
df_Shot_deck2.14 <- as.data.frame(S5hot_deck2.1[["data"]][[4]])
df_Shot_deck2.15 <- as.data.frame(S5hot_deck2.1[["data"]][[5]])
save(df_Shot_deck2.11, file = "S5hot_deck211.Rdata")
save(df_Shot_deck2.12, file = "S5hot_deck212.Rdata")
save(df_Shot_deck2.13, file = "S5hot_deck213.Rdata")
save(df_Shot_deck2.14, file = "S5hot_deck214.Rdata")
save(df_Shot_deck2.15, file = "S5hot_deck215.Rdata")

SHD2.11_correct <- SHD2.11_correct + sum(sub_ipums5 == df_Shot_deck2.11)
SHD2.12_correct <- SHD2.12_correct + sum(sub_ipums5 == df_Shot_deck2.12)
SHD2.13_correct <- SHD2.13_correct + sum(sub_ipums5 == df_Shot_deck2.13)
SHD2.14_correct <- SHD2.14_correct + sum(sub_ipums5 == df_Shot_deck2.14)
SHD2.15_correct <- SHD2.15_correct + sum(sub_ipums5 == df_Shot_deck2.15)
SHD2.1_correct <- (sum(SHD2.11_correct, SHD2.12_correct, SHD2.13_correct, SHD2.14_correct, SHD2.15_correct)) / 5
SHD2.1_total <- SHD2.1_total + sum(!is.na(df_Shot_deck2.11))

rm(S5hot_deck2.1)
rm(df_Shot_deck2.11, df_Shot_deck2.12, df_Shot_deck2.13, df_Shot_deck2.14, df_Shot_deck2.15)

# 2.2 data set
sub5_MCAR2.2 <- get(load(file = "sub5_MCAR22.Rdata"))
SHD_MCAR2.2 <- data.frame(sub5_MCAR2.2)
names(SHD_MCAR2.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR2.2)) {
  SHD_MCAR2.2[, i] <- as.factor(SHD_MCAR2.2[, i])
}
rm(sub5_MCAR2.2)

tic("HotDeck Imputation 2.2 processing time...")
S5hot_deck2.2 <- hot.deck(SHD_MCAR2.2, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR2.2)

df_Shot_deck2.21 <- as.data.frame(S5hot_deck2.2[["data"]][[1]])
df_Shot_deck2.22 <- as.data.frame(S5hot_deck2.2[["data"]][[2]])
df_Shot_deck2.23 <- as.data.frame(S5hot_deck2.2[["data"]][[3]])
df_Shot_deck2.24 <- as.data.frame(S5hot_deck2.2[["data"]][[4]])
df_Shot_deck2.25 <- as.data.frame(S5hot_deck2.2[["data"]][[5]])
save(df_Shot_deck2.21, file = "S5hot_deck221.Rdata")
save(df_Shot_deck2.22, file = "S5hot_deck222.Rdata")
save(df_Shot_deck2.23, file = "S5hot_deck223.Rdata")
save(df_Shot_deck2.24, file = "S5hot_deck224.Rdata")
save(df_Shot_deck2.25, file = "S5hot_deck225.Rdata")

SHD2.21_correct <- SHD2.21_correct + sum(sub_ipums5 == df_Shot_deck2.21)
SHD2.22_correct <- SHD2.22_correct + sum(sub_ipums5 == df_Shot_deck2.22)
SHD2.23_correct <- SHD2.23_correct + sum(sub_ipums5 == df_Shot_deck2.23)
SHD2.24_correct <- SHD2.24_correct + sum(sub_ipums5 == df_Shot_deck2.24)
SHD2.25_correct <- SHD2.25_correct + sum(sub_ipums5 == df_Shot_deck2.25)
SHD2.2_correct <- (sum(SHD2.21_correct, SHD2.22_correct, SHD2.23_correct, SHD2.24_correct, SHD2.25_correct)) / 5
SHD2.2_total <- SHD2.2_total + sum(!is.na(df_Shot_deck2.21))

rm(S5hot_deck2.2)
rm(df_Shot_deck2.21, df_Shot_deck2.22, df_Shot_deck2.23, df_Shot_deck2.24, df_Shot_deck2.25)

# 2.3 data set
sub5_MCAR2.3 <- get(load(file = "sub5_MCAR23.Rdata"))
SHD_MCAR2.3 <- data.frame(sub5_MCAR2.3)
names(SHD_MCAR2.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR2.3)) {
  SHD_MCAR2.3[, i] <- as.factor(SHD_MCAR2.3[, i])
}
rm(sub5_MCAR2.3)

tic("HotDeck Imputation 2.3 processing time...")
S5hot_deck2.3 <- hot.deck(SHD_MCAR2.3, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR2.3)

df_Shot_deck2.31 <- as.data.frame(S5hot_deck2.3[["data"]][[1]])
df_Shot_deck2.32 <- as.data.frame(S5hot_deck2.3[["data"]][[2]])
df_Shot_deck2.33 <- as.data.frame(S5hot_deck2.3[["data"]][[3]])
df_Shot_deck2.34 <- as.data.frame(S5hot_deck2.3[["data"]][[4]])
df_Shot_deck2.35 <- as.data.frame(S5hot_deck2.3[["data"]][[5]])
save(df_Shot_deck2.31, file = "S5hot_deck231.Rdata")
save(df_Shot_deck2.32, file = "S5hot_deck232.Rdata")
save(df_Shot_deck2.33, file = "S5hot_deck233.Rdata")
save(df_Shot_deck2.34, file = "S5hot_deck234.Rdata")
save(df_Shot_deck2.35, file = "S5hot_deck235.Rdata")

SHD2.31_correct <- SHD2.31_correct + sum(sub_ipums5 == df_Shot_deck2.31)
SHD2.32_correct <- SHD2.32_correct + sum(sub_ipums5 == df_Shot_deck2.32)
SHD2.33_correct <- SHD2.33_correct + sum(sub_ipums5 == df_Shot_deck2.33)
SHD2.34_correct <- SHD2.34_correct + sum(sub_ipums5 == df_Shot_deck2.34)
SHD2.35_correct <- SHD2.35_correct + sum(sub_ipums5 == df_Shot_deck2.35)
SHD2.3_correct <- (sum(SHD2.31_correct, SHD2.32_correct, SHD2.33_correct, SHD2.34_correct, SHD2.35_correct)) / 5
SHD2.3_total <- SHD2.3_total + sum(!is.na(df_Shot_deck2.31))

rm(S5hot_deck2.3)
rm(df_Shot_deck2.31, df_Shot_deck2.32, df_Shot_deck2.33, df_Shot_deck2.34, df_Shot_deck2.35)

# 5% data sets 

# 5.1 data set 
sub5_MCAR5.1 <- get(load(file = "sub5_MCAR51.Rdata"))
SHD_MCAR5.1 <- data.frame(sub5_MCAR5.1)
names(SHD_MCAR5.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR5.1)) {
  SHD_MCAR5.1[, i] <- as.factor(SHD_MCAR5.1[, i])
}
rm(sub5_MCAR5.1)

tic("HotDeck Imputation 5.1 processing time...")
S5hot_deck5.1 <- hot.deck(SHD_MCAR5.1, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR5.1)

df_Shot_deck5.11 <- as.data.frame(S5hot_deck5.1[["data"]][[1]])
df_Shot_deck5.12 <- as.data.frame(S5hot_deck5.1[["data"]][[2]])
df_Shot_deck5.13 <- as.data.frame(S5hot_deck5.1[["data"]][[3]])
df_Shot_deck5.14 <- as.data.frame(S5hot_deck5.1[["data"]][[4]])
df_Shot_deck5.15 <- as.data.frame(S5hot_deck5.1[["data"]][[5]])
save(df_Shot_deck5.11, file = "S5hot_deck511.Rdata")
save(df_Shot_deck5.12, file = "S5hot_deck512.Rdata")
save(df_Shot_deck5.13, file = "S5hot_deck513.Rdata")
save(df_Shot_deck5.14, file = "S5hot_deck514.Rdata")
save(df_Shot_deck5.15, file = "S5hot_deck515.Rdata")

SHD5.11_correct <- SHD5.11_correct + sum(sub_ipums5 == df_Shot_deck5.11)
SHD5.12_correct <- SHD5.12_correct + sum(sub_ipums5 == df_Shot_deck5.12)
SHD5.13_correct <- SHD5.13_correct + sum(sub_ipums5 == df_Shot_deck5.13)
SHD5.14_correct <- SHD5.14_correct + sum(sub_ipums5 == df_Shot_deck5.14)
SHD5.15_correct <- SHD5.15_correct + sum(sub_ipums5 == df_Shot_deck5.15)
SHD5.1_correct <- (sum(SHD5.11_correct, SHD5.12_correct, SHD5.13_correct, SHD5.14_correct, SHD5.15_correct)) / 5
SHD5.1_total <- SHD5.1_total + sum(!is.na(df_Shot_deck5.11))

rm(S5hot_deck5.1)
rm(df_Shot_deck5.11, df_Shot_deck5.12, df_Shot_deck5.13, df_Shot_deck5.14, df_Shot_deck5.15)

# 5.2 data set 
sub5_MCAR5.2 <- get(load(file = "sub5_MCAR52.Rdata"))
SHD_MCAR5.2 <- data.frame(sub5_MCAR5.2)
names(SHD_MCAR5.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR5.2)) {
  SHD_MCAR5.2[, i] <- as.factor(SHD_MCAR5.2[, i])
}
rm(sub5_MCAR5.2)

tic("HotDeck Imputation 5.2 processing time...")
S5hot_deck5.2 <- hot.deck(SHD_MCAR5.2, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR5.2)

df_Shot_deck5.21 <- as.data.frame(S5hot_deck5.2[["data"]][[1]])
df_Shot_deck5.22 <- as.data.frame(S5hot_deck5.2[["data"]][[2]])
df_Shot_deck5.23 <- as.data.frame(S5hot_deck5.2[["data"]][[3]])
df_Shot_deck5.24 <- as.data.frame(S5hot_deck5.2[["data"]][[4]])
df_Shot_deck5.25 <- as.data.frame(S5hot_deck5.2[["data"]][[5]])
save(df_Shot_deck5.21, file = "S5hot_deck521.Rdata")
save(df_Shot_deck5.22, file = "S5hot_deck522.Rdata")
save(df_Shot_deck5.23, file = "S5hot_deck523.Rdata")
save(df_Shot_deck5.24, file = "S5hot_deck524.Rdata")
save(df_Shot_deck5.25, file = "S5hot_deck525.Rdata")

SHD5.21_correct <- SHD5.21_correct + sum(sub_ipums5 == df_Shot_deck5.21)
SHD5.22_correct <- SHD5.22_correct + sum(sub_ipums5 == df_Shot_deck5.22)
SHD5.23_correct <- SHD5.23_correct + sum(sub_ipums5 == df_Shot_deck5.23)
SHD5.24_correct <- SHD5.24_correct + sum(sub_ipums5 == df_Shot_deck5.24)
SHD5.25_correct <- SHD5.25_correct + sum(sub_ipums5 == df_Shot_deck5.25)
SHD5.2_correct <- (sum(SHD5.21_correct, SHD5.22_correct, SHD5.23_correct, SHD5.24_correct, SHD5.25_correct)) / 5
SHD5.2_total <- SHD5.2_total + sum(!is.na(df_Shot_deck5.21))

rm(S5hot_deck5.2)
rm(df_Shot_deck5.21, df_Shot_deck5.22, df_Shot_deck5.23, df_Shot_deck5.24, df_Shot_deck5.25)

# 5.3 data set 
sub5_MCAR5.3 <- get(load(file = "sub5_MCAR53.Rdata"))
SHD_MCAR5.3 <- data.frame(sub5_MCAR5.3)
names(SHD_MCAR5.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR5.3)) {
  SHD_MCAR5.3[, i] <- as.factor(SHD_MCAR5.3[, i])
}
rm(sub5_MCAR5.3)

tic("HotDeck Imputation 5.3 processing time...")
S5hot_deck5.3 <- hot.deck(SHD_MCAR5.3, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR5.3)

df_Shot_deck5.31 <- as.data.frame(S5hot_deck5.3[["data"]][[1]])
df_Shot_deck5.32 <- as.data.frame(S5hot_deck5.3[["data"]][[2]])
df_Shot_deck5.33 <- as.data.frame(S5hot_deck5.3[["data"]][[3]])
df_Shot_deck5.34 <- as.data.frame(S5hot_deck5.3[["data"]][[4]])
df_Shot_deck5.35 <- as.data.frame(S5hot_deck5.3[["data"]][[5]])
save(df_Shot_deck5.31, file = "S5hot_deck531.Rdata")
save(df_Shot_deck5.32, file = "S5hot_deck532.Rdata")
save(df_Shot_deck5.33, file = "S5hot_deck533.Rdata")
save(df_Shot_deck5.34, file = "S5hot_deck534.Rdata")
save(df_Shot_deck5.35, file = "S5hot_deck535.Rdata")

SHD5.31_correct <- SHD5.31_correct + sum(sub_ipums5 == df_Shot_deck5.31)
SHD5.32_correct <- SHD5.32_correct + sum(sub_ipums5 == df_Shot_deck5.32)
SHD5.33_correct <- SHD5.33_correct + sum(sub_ipums5 == df_Shot_deck5.33)
SHD5.34_correct <- SHD5.34_correct + sum(sub_ipums5 == df_Shot_deck5.34)
SHD5.35_correct <- SHD5.35_correct + sum(sub_ipums5 == df_Shot_deck5.35)
SHD5.3_correct <- (sum(SHD5.31_correct, SHD5.32_correct, SHD5.33_correct, SHD5.34_correct, SHD5.35_correct)) / 5
SHD5.3_total <- SHD5.3_total + sum(!is.na(df_Shot_deck5.31))

rm(S5hot_deck5.3)
rm(df_Shot_deck5.31, df_Shot_deck5.32, df_Shot_deck5.33, df_Shot_deck5.34, df_Shot_deck5.35)


# 10% data sets

# 10.1 data set
sub5_MCAR10.1 <- get(load(file = "sub5_MCAR101.Rdata"))
SHD_MCAR10.1 <- data.frame(sub5_MCAR10.1)
names(SHD_MCAR10.1) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR10.1)) {
  SHD_MCAR10.1[, i] <- as.factor(SHD_MCAR10.1[, i])
}
rm(sub5_MCAR10.1)

tic("HotDeck Imputation 10.1 processing time...")
S5hot_deck10.1 <- hot.deck(SHD_MCAR10.1, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR10.1)

df_Shot_deck10.11 <- as.data.frame(S5hot_deck10.1[["data"]][[1]])
df_Shot_deck10.12 <- as.data.frame(S5hot_deck10.1[["data"]][[2]])
df_Shot_deck10.13 <- as.data.frame(S5hot_deck10.1[["data"]][[3]])
df_Shot_deck10.14 <- as.data.frame(S5hot_deck10.1[["data"]][[4]])
df_Shot_deck10.15 <- as.data.frame(S5hot_deck10.1[["data"]][[5]])
save(df_Shot_deck10.11, file = "S5hot_deck1011.Rdata")
save(df_Shot_deck10.12, file = "S5hot_deck1012.Rdata")
save(df_Shot_deck10.13, file = "S5hot_deck1013.Rdata")
save(df_Shot_deck10.14, file = "S5hot_deck1014.Rdata")
save(df_Shot_deck10.15, file = "S5hot_deck1015.Rdata")

SHD10.11_correct <- SHD10.11_correct + sum(sub_ipums5 == df_Shot_deck10.11)
SHD10.12_correct <- SHD10.12_correct + sum(sub_ipums5 == df_Shot_deck10.12)
SHD10.13_correct <- SHD10.13_correct + sum(sub_ipums5 == df_Shot_deck10.13)
SHD10.14_correct <- SHD10.14_correct + sum(sub_ipums5 == df_Shot_deck10.14)
SHD10.15_correct <- SHD10.15_correct + sum(sub_ipums5 == df_Shot_deck10.15)
SHD10.1_correct <- (sum(SHD10.11_correct, SHD10.12_correct, SHD10.13_correct, SHD10.14_correct, SHD10.15_correct)) / 5
SHD10.1_total <- SHD10.1_total + sum(!is.na(df_Shot_deck10.11))

rm(S5hot_deck10.1)
rm(df_Shot_deck10.11, df_Shot_deck10.12, df_Shot_deck10.13, df_Shot_deck10.14, df_Shot_deck10.15)

# 10.2 data set
sub5_MCAR10.2 <- get(load(file = "sub5_MCAR102.Rdata"))
SHD_MCAR10.2 <- data.frame(sub5_MCAR10.2)
names(SHD_MCAR10.2) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR10.2)) {
  SHD_MCAR10.2[, i] <- as.factor(SHD_MCAR10.2[, i])
}
rm(sub5_MCAR10.2)

tic("HotDeck Imputation 10.2 processing time...")
S5hot_deck10.2 <- hot.deck(SHD_MCAR10.2, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR10.2)

df_Shot_deck10.21 <- as.data.frame(S5hot_deck10.2[["data"]][[1]])
df_Shot_deck10.22 <- as.data.frame(S5hot_deck10.2[["data"]][[2]])
df_Shot_deck10.23 <- as.data.frame(S5hot_deck10.2[["data"]][[3]])
df_Shot_deck10.24 <- as.data.frame(S5hot_deck10.2[["data"]][[4]])
df_Shot_deck10.25 <- as.data.frame(S5hot_deck10.2[["data"]][[5]])
save(df_Shot_deck10.21, file = "S5hot_deck1021.Rdata")
save(df_Shot_deck10.22, file = "S5hot_deck1022.Rdata")
save(df_Shot_deck10.23, file = "S5hot_deck1023.Rdata")
save(df_Shot_deck10.24, file = "S5hot_deck1024.Rdata")
save(df_Shot_deck10.25, file = "S5hot_deck1025.Rdata")

SHD10.21_correct <- SHD10.21_correct + sum(sub_ipums5 == df_Shot_deck10.21)
SHD10.22_correct <- SHD10.22_correct + sum(sub_ipums5 == df_Shot_deck10.22)
SHD10.23_correct <- SHD10.23_correct + sum(sub_ipums5 == df_Shot_deck10.23)
SHD10.24_correct <- SHD10.24_correct + sum(sub_ipums5 == df_Shot_deck10.24)
SHD10.25_correct <- SHD10.25_correct + sum(sub_ipums5 == df_Shot_deck10.25)
SHD10.2_correct <- (sum(SHD10.21_correct, SHD10.22_correct, SHD10.23_correct, SHD10.24_correct, SHD10.25_correct)) / 5
SHD10.2_total <- SHD10.2_total + sum(!is.na(df_Shot_deck10.21))

rm(S5hot_deck10.2)
rm(df_Shot_deck10.21, df_Shot_deck10.22, df_Shot_deck10.23, df_Shot_deck10.24, df_Shot_deck10.25)

# 10.3 data set
sub5_MCAR10.3 <- get(load(file = "sub5_MCAR103.Rdata"))
SHD_MCAR10.3 <- data.frame(sub5_MCAR10.3)
names(SHD_MCAR10.3) <- gsub(" ", "_", names(sub_ipums5), fixed=TRUE)
for (i in 1:ncol(SHD_MCAR10.3)) {
  SHD_MCAR10.3[, i] <- as.factor(SHD_MCAR10.3[, i])
}
rm(sub5_MCAR10.3)

tic("HotDeck Imputation 10.3 processing time...")
S5hot_deck10.3 <- hot.deck(SHD_MCAR10.3, m = 5, method = "p.draw")
toc(log = TRUE)
rm(SHD_MCAR10.3)

df_Shot_deck10.31 <- as.data.frame(S5hot_deck10.3[["data"]][[1]])
df_Shot_deck10.32 <- as.data.frame(S5hot_deck10.3[["data"]][[2]])
df_Shot_deck10.33 <- as.data.frame(S5hot_deck10.3[["data"]][[3]])
df_Shot_deck10.34 <- as.data.frame(S5hot_deck10.3[["data"]][[4]])
df_Shot_deck10.35 <- as.data.frame(S5hot_deck10.3[["data"]][[5]])
save(df_Shot_deck10.31, file = "S5hot_deck1031.Rdata")
save(df_Shot_deck10.32, file = "S5hot_deck1032.Rdata")
save(df_Shot_deck10.33, file = "S5hot_deck1033.Rdata")
save(df_Shot_deck10.34, file = "S5hot_deck1034.Rdata")
save(df_Shot_deck10.35, file = "S5hot_deck1035.Rdata")

SHD10.31_correct <- SHD10.31_correct + sum(sub_ipums5 == df_Shot_deck10.31)
SHD10.32_correct <- SHD10.32_correct + sum(sub_ipums5 == df_Shot_deck10.32)
SHD10.33_correct <- SHD10.33_correct + sum(sub_ipums5 == df_Shot_deck10.33)
SHD10.34_correct <- SHD10.34_correct + sum(sub_ipums5 == df_Shot_deck10.34)
SHD10.35_correct <- SHD10.35_correct + sum(sub_ipums5 == df_Shot_deck10.35)
SHD10.3_correct <- (sum(SHD10.31_correct, SHD10.32_correct, SHD10.33_correct, SHD10.34_correct, SHD10.35_correct)) / 5
SHD10.3_total <- SHD10.3_total + sum(!is.na(df_Shot_deck10.31))

rm(S5hot_deck10.3)
rm(df_Shot_deck10.31, df_Shot_deck10.32, df_Shot_deck10.33, df_Shot_deck10.34, df_Shot_deck10.35)

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
SHD2.1_accuracy <- SHD2.1_correct / SHD2.1_total
SHD2.2_accuracy <- SHD2.2_correct / SHD2.2_total
SHD2.3_accuracy <- SHD2.3_correct / SHD2.3_total

SHD5.1_accuracy <- SHD5.1_correct / SHD5.1_total
SHD5.2_accuracy <- SHD5.2_correct / SHD5.2_total
SHD5.3_accuracy <- SHD5.3_correct / SHD5.3_total

SHD10.1_accuracy <- SHD10.1_correct / SHD10.1_total
SHD10.2_accuracy <- SHD10.2_correct / SHD10.2_total
SHD10.3_accuracy <- SHD10.3_correct / SHD10.3_total




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
S5HD2.1_Geslacht <- plyr::count(df_Shot_deck2.13, 'Geslacht')
S5HD2.1_Leeftijd <- plyr::count(df_Shot_deck2.13, 'Leeftijd')
S5HD2.1_HH_Pos <- plyr::count(df_Shot_deck2.13, 'HH_Pos')
S5HD2.1_HH_grootte <- plyr::count(df_Shot_deck2.13, 'HH_grootte')
S5HD2.1_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck2.13, 'Woonregio_vorig_jaar')
S5HD2.1_Nationaliteit <- plyr::count(df_Shot_deck2.13, 'Nationaliteit')
S5HD2.1_Geboorteland <- plyr::count(df_Shot_deck2.13, 'Geboorteland')
S5HD2.1_Onderwijsniveau <- plyr::count(df_Shot_deck2.13, 'Onderwijsniveau')
S5HD2.1_Econ_status <- plyr::count(df_Shot_deck2.13, 'Econ._status')
S5HD2.1_Beroep <- plyr::count(df_Shot_deck2.13, 'Beroep')
S5HD2.1_SBI <- plyr::count(df_Shot_deck2.13, 'SBI')
S5HD2.1_Burg_Staat <- plyr::count(df_Shot_deck2.13, 'Burg._Staat')

S5HD2.2_Geslacht <- plyr::count(df_Shot_deck2.23, 'Geslacht')
S5HD2.2_Leeftijd <- plyr::count(df_Shot_deck2.23, 'Leeftijd')
S5HD2.2_HH_Pos <- plyr::count(df_Shot_deck2.23, 'HH_Pos')
S5HD2.2_HH_grootte <- plyr::count(df_Shot_deck2.23, 'HH_grootte')
S5HD2.2_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck2.23, 'Woonregio_vorig_jaar')
S5HD2.2_Nationaliteit <- plyr::count(df_Shot_deck2.23, 'Nationaliteit')
S5HD2.2_Geboorteland <- plyr::count(df_Shot_deck2.23, 'Geboorteland')
S5HD2.2_Onderwijsniveau <- plyr::count(df_Shot_deck2.23, 'Onderwijsniveau')
S5HD2.2_Econ_status <- plyr::count(df_Shot_deck2.23, 'Econ._status')
S5HD2.2_Beroep <- plyr::count(df_Shot_deck2.23, 'Beroep')
S5HD2.2_SBI <- plyr::count(df_Shot_deck2.23, 'SBI')
S5HD2.2_Burg_Staat <- plyr::count(df_Shot_deck2.23, 'Burg._Staat')

S5HD2.3_Geslacht <- plyr::count(df_Shot_deck2.33, 'Geslacht')
S5HD2.3_Leeftijd <- plyr::count(df_Shot_deck2.33, 'Leeftijd')
S5HD2.3_HH_Pos <- plyr::count(df_Shot_deck2.33, 'HH_Pos')
S5HD2.3_HH_grootte <- plyr::count(df_Shot_deck2.33, 'HH_grootte')
S5HD2.3_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck2.33, 'Woonregio_vorig_jaar')
S5HD2.3_Nationaliteit <- plyr::count(df_Shot_deck2.33, 'Nationaliteit')
S5HD2.3_Geboorteland <- plyr::count(df_Shot_deck2.33, 'Geboorteland')
S5HD2.3_Onderwijsniveau <- plyr::count(df_Shot_deck2.33, 'Onderwijsniveau')
S5HD2.3_Econ_status <- plyr::count(df_Shot_deck2.33, 'Econ._status')
S5HD2.3_Beroep <- plyr::count(df_Shot_deck2.33, 'Beroep')
S5HD2.3_SBI <- plyr::count(df_Shot_deck2.33, 'SBI')
S5HD2.3_Burg_Staat <- plyr::count(df_Shot_deck2.33, 'Burg._Staat')

rm(df_Shot_deck2.13, df_Shot_deck2.23, df_Shot_deck2.33)

# 2.1 data set 
BS5HD2.1_Geslacht <- Bias(S5HD2.1_Geslacht, Geslacht, "Geslacht")
BOS5HD2.1_Geslacht <- sum(BS5HD2.1_Geslacht$percentageABS)/nrow(BS5HD2.1_Geslacht)

BS5HD2.1_Leeftijd <- Bias(S5HD2.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD2.1_Leeftijd <- sum(BS5HD2.1_Leeftijd$percentageABS)/nrow(BS5HD2.1_Leeftijd)

BS5HD2.1_HH_Pos <- Bias(S5HD2.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD2.1_HH_Pos <- sum(BS5HD2.1_HH_Pos$percentageABS)/nrow(BS5HD2.1_HH_Pos)

BS5HD2.1_HH_grootte <- Bias(S5HD2.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD2.1_HH_grootte <- sum(BS5HD2.1_HH_grootte$percentageABS)/nrow(BS5HD2.1_HH_grootte)

BS5HD2.1_Woonregio_vorig_jaar <- Bias(S5HD2.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD2.1_Woonregio_vorig_jaar <- sum(BS5HD2.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD2.1_Woonregio_vorig_jaar)

BS5HD2.1_Nationaliteit <- Bias(S5HD2.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD2.1_Nationaliteit <- sum(BS5HD2.1_Nationaliteit$percentageABS)/nrow(BS5HD2.1_Nationaliteit)

BS5HD2.1_Geboorteland <- Bias(S5HD2.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD2.1_Geboorteland <- sum(BS5HD2.1_Geboorteland$percentageABS)/nrow(BS5HD2.1_Geboorteland)

BS5HD2.1_Onderwijsniveau <- Bias(S5HD2.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD2.1_Onderwijsniveau <- sum(BS5HD2.1_Onderwijsniveau$percentageABS)/nrow(BS5HD2.1_Onderwijsniveau)

BS5HD2.1_Econ_status <- Bias2(S5HD2.1_Econ_status, Econ_status, "Econ._status")
BOS5HD2.1_Econ_status <- sum(BS5HD2.1_Econ_status$percentageABS)/nrow(BS5HD2.1_Econ_status)

BS5HD2.1_Beroep <- Bias(S5HD2.1_Beroep, Beroep, "Beroep")
BOS5HD2.1_Beroep <- sum(BS5HD2.1_Beroep$percentageABS)/nrow(BS5HD2.1_Beroep)

BS5HD2.1_SBI <- Bias(S5HD2.1_SBI, SBI, "SBI")
BOS5HD2.1_SBI <- sum(BS5HD2.1_SBI$percentageABS)/nrow(BS5HD2.1_SBI)

BS5HD2.1_Burg_Staat <- Bias2(S5HD2.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD2.1_Burg_Staat <- sum(BS5HD2.1_Burg_Staat$percentageABS)/nrow(BS5HD2.1_Burg_Staat)

BiasOverallS5HD2.1 <- sum(BOS5HD2.1_Geslacht, BOS5HD2.1_Leeftijd, BOS5HD2.1_HH_Pos, BOS5HD2.1_HH_grootte, BOS5HD2.1_Woonregio_vorig_jaar, 
                        BOS5HD2.1_Nationaliteit, BOS5HD2.1_Geboorteland, BOS5HD2.1_Onderwijsniveau, BOS5HD2.1_Econ_status, 
                        BOS5HD2.1_Beroep, BOS5HD2.1_SBI, BOS5HD2.1_Burg_Staat) / 12

rm(S5HD2.1_Geslacht, S5HD2.1_Leeftijd, S5HD2.1_HH_Pos, S5HD2.1_HH_grootte, S5HD2.1_Woonregio_vorig_jaar, 
   S5HD2.1_Nationaliteit, S5HD2.1_Geboorteland, S5HD2.1_Onderwijsniveau, S5HD2.1_Econ_status, 
   S5HD2.1_Beroep, S5HD2.1_SBI, S5HD2.1_Burg_Staat)
rm(BS5HD2.1_Geslacht, BS5HD2.1_Leeftijd, BS5HD2.1_HH_Pos, BS5HD2.1_HH_grootte, BS5HD2.1_Woonregio_vorig_jaar, 
   BS5HD2.1_Nationaliteit, BS5HD2.1_Geboorteland, BS5HD2.1_Onderwijsniveau, BS5HD2.1_Econ_status, 
   BS5HD2.1_Beroep, BS5HD2.1_SBI, BS5HD2.1_Burg_Staat)
rm(BOS5HD2.1_Geslacht, BOS5HD2.1_Leeftijd, BOS5HD2.1_HH_Pos, BOS5HD2.1_HH_grootte, BOS5HD2.1_Woonregio_vorig_jaar, 
   BOS5HD2.1_Nationaliteit, BOS5HD2.1_Geboorteland, BOS5HD2.1_Onderwijsniveau, BOS5HD2.1_Econ_status, 
   BOS5HD2.1_Beroep, BOS5HD2.1_SBI, BOS5HD2.1_Burg_Staat)

# 2.2 data set 
BS5HD2.2_Geslacht <- Bias(S5HD2.2_Geslacht, Geslacht, "Geslacht")
BOS5HD2.2_Geslacht <- sum(BS5HD2.2_Geslacht$percentageABS)/nrow(BS5HD2.2_Geslacht)

BS5HD2.2_Leeftijd <- Bias(S5HD2.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD2.2_Leeftijd <- sum(BS5HD2.2_Leeftijd$percentageABS)/nrow(BS5HD2.2_Leeftijd)

BS5HD2.2_HH_Pos <- Bias(S5HD2.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD2.2_HH_Pos <- sum(BS5HD2.2_HH_Pos$percentageABS)/nrow(BS5HD2.2_HH_Pos)

BS5HD2.2_HH_grootte <- Bias(S5HD2.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD2.2_HH_grootte <- sum(BS5HD2.2_HH_grootte$percentageABS)/nrow(BS5HD2.2_HH_grootte)

BS5HD2.2_Woonregio_vorig_jaar <- Bias(S5HD2.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD2.2_Woonregio_vorig_jaar <- sum(BS5HD2.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD2.2_Woonregio_vorig_jaar)

BS5HD2.2_Nationaliteit <- Bias(S5HD2.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD2.2_Nationaliteit <- sum(BS5HD2.2_Nationaliteit$percentageABS)/nrow(BS5HD2.2_Nationaliteit)

BS5HD2.2_Geboorteland <- Bias(S5HD2.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD2.2_Geboorteland <- sum(BS5HD2.2_Geboorteland$percentageABS)/nrow(BS5HD2.2_Geboorteland)

BS5HD2.2_Onderwijsniveau <- Bias(S5HD2.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD2.2_Onderwijsniveau <- sum(BS5HD2.2_Onderwijsniveau$percentageABS)/nrow(BS5HD2.2_Onderwijsniveau)

BS5HD2.2_Econ_status <- Bias2(S5HD2.2_Econ_status, Econ_status, "Econ._status")
BOS5HD2.2_Econ_status <- sum(BS5HD2.2_Econ_status$percentageABS)/nrow(BS5HD2.2_Econ_status)

BS5HD2.2_Beroep <- Bias(S5HD2.2_Beroep, Beroep, "Beroep")
BOS5HD2.2_Beroep <- sum(BS5HD2.2_Beroep$percentageABS)/nrow(BS5HD2.2_Beroep)

BS5HD2.2_SBI <- Bias(S5HD2.2_SBI, SBI, "SBI")
BOS5HD2.2_SBI <- sum(BS5HD2.2_SBI$percentageABS)/nrow(BS5HD2.2_SBI)

BS5HD2.2_Burg_Staat <- Bias2(S5HD2.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD2.2_Burg_Staat <- sum(BS5HD2.2_Burg_Staat$percentageABS)/nrow(BS5HD2.2_Burg_Staat)

BiasOverallS5HD2.2 <- sum(BOS5HD2.2_Geslacht, BOS5HD2.2_Leeftijd, BOS5HD2.2_HH_Pos, BOS5HD2.2_HH_grootte, BOS5HD2.2_Woonregio_vorig_jaar, 
                        BOS5HD2.2_Nationaliteit, BOS5HD2.2_Geboorteland, BOS5HD2.2_Onderwijsniveau, BOS5HD2.2_Econ_status, 
                        BOS5HD2.2_Beroep, BOS5HD2.2_SBI, BOS5HD2.2_Burg_Staat) / 12

rm(S5HD2.2_Geslacht, S5HD2.2_Leeftijd, S5HD2.2_HH_Pos, S5HD2.2_HH_grootte, S5HD2.2_Woonregio_vorig_jaar, 
   S5HD2.2_Nationaliteit, S5HD2.2_Geboorteland, S5HD2.2_Onderwijsniveau, S5HD2.2_Econ_status, 
   S5HD2.2_Beroep, S5HD2.2_SBI, S5HD2.2_Burg_Staat)
rm(BS5HD2.2_Geslacht, BS5HD2.2_Leeftijd, BS5HD2.2_HH_Pos, BS5HD2.2_HH_grootte, BS5HD2.2_Woonregio_vorig_jaar, 
   BS5HD2.2_Nationaliteit, BS5HD2.2_Geboorteland, BS5HD2.2_Onderwijsniveau, BS5HD2.2_Econ_status, 
   BS5HD2.2_Beroep, BS5HD2.2_SBI, BS5HD2.2_Burg_Staat)
rm(BOS5HD2.2_Geslacht, BOS5HD2.2_Leeftijd, BOS5HD2.2_HH_Pos, BOS5HD2.2_HH_grootte, BOS5HD2.2_Woonregio_vorig_jaar, 
   BOS5HD2.2_Nationaliteit, BOS5HD2.2_Geboorteland, BOS5HD2.2_Onderwijsniveau, BOS5HD2.2_Econ_status, 
   BOS5HD2.2_Beroep, BOS5HD2.2_SBI, BOS5HD2.2_Burg_Staat)

# 2.3 data set 
BS5HD2.3_Geslacht <- Bias(S5HD2.3_Geslacht, Geslacht, "Geslacht")
BOS5HD2.3_Geslacht <- sum(BS5HD2.3_Geslacht$percentageABS)/nrow(BS5HD2.3_Geslacht)

BS5HD2.3_Leeftijd <- Bias(S5HD2.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD2.3_Leeftijd <- sum(BS5HD2.3_Leeftijd$percentageABS)/nrow(BS5HD2.3_Leeftijd)

BS5HD2.3_HH_Pos <- Bias(S5HD2.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD2.3_HH_Pos <- sum(BS5HD2.3_HH_Pos$percentageABS)/nrow(BS5HD2.3_HH_Pos)

BS5HD2.3_HH_grootte <- Bias(S5HD2.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD2.3_HH_grootte <- sum(BS5HD2.3_HH_grootte$percentageABS)/nrow(BS5HD2.3_HH_grootte)

BS5HD2.3_Woonregio_vorig_jaar <- Bias(S5HD2.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD2.3_Woonregio_vorig_jaar <- sum(BS5HD2.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD2.3_Woonregio_vorig_jaar)

BS5HD2.3_Nationaliteit <- Bias(S5HD2.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD2.3_Nationaliteit <- sum(BS5HD2.3_Nationaliteit$percentageABS)/nrow(BS5HD2.3_Nationaliteit)

BS5HD2.3_Geboorteland <- Bias(S5HD2.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD2.3_Geboorteland <- sum(BS5HD2.3_Geboorteland$percentageABS)/nrow(BS5HD2.3_Geboorteland)

BS5HD2.3_Onderwijsniveau <- Bias(S5HD2.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD2.3_Onderwijsniveau <- sum(BS5HD2.3_Onderwijsniveau$percentageABS)/nrow(BS5HD2.3_Onderwijsniveau)

BS5HD2.3_Econ_status <- Bias2(S5HD2.3_Econ_status, Econ_status, "Econ._status")
BOS5HD2.3_Econ_status <- sum(BS5HD2.3_Econ_status$percentageABS)/nrow(BS5HD2.3_Econ_status)

BS5HD2.3_Beroep <- Bias(S5HD2.3_Beroep, Beroep, "Beroep")
BOS5HD2.3_Beroep <- sum(BS5HD2.3_Beroep$percentageABS)/nrow(BS5HD2.3_Beroep)

BS5HD2.3_SBI <- Bias(S5HD2.3_SBI, SBI, "SBI")
BOS5HD2.3_SBI <- sum(BS5HD2.3_SBI$percentageABS)/nrow(BS5HD2.3_SBI)

BS5HD2.3_Burg_Staat <- Bias2(S5HD2.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD2.3_Burg_Staat <- sum(BS5HD2.3_Burg_Staat$percentageABS)/nrow(BS5HD2.3_Burg_Staat)

BiasOverallS5HD2.3 <- sum(BOS5HD2.3_Geslacht, BOS5HD2.3_Leeftijd, BOS5HD2.3_HH_Pos, BOS5HD2.3_HH_grootte, BOS5HD2.3_Woonregio_vorig_jaar, 
                        BOS5HD2.3_Nationaliteit, BOS5HD2.3_Geboorteland, BOS5HD2.3_Onderwijsniveau, BOS5HD2.3_Econ_status, 
                        BOS5HD2.3_Beroep, BOS5HD2.3_SBI, BOS5HD2.3_Burg_Staat) / 12

rm(S5HD2.3_Geslacht, S5HD2.3_Leeftijd, S5HD2.3_HH_Pos, S5HD2.3_HH_grootte, S5HD2.3_Woonregio_vorig_jaar, 
   S5HD2.3_Nationaliteit, S5HD2.3_Geboorteland, S5HD2.3_Onderwijsniveau, S5HD2.3_Econ_status, 
   S5HD2.3_Beroep, S5HD2.3_SBI, S5HD2.3_Burg_Staat)
rm(BS5HD2.3_Geslacht, BS5HD2.3_Leeftijd, BS5HD2.3_HH_Pos, BS5HD2.3_HH_grootte, BS5HD2.3_Woonregio_vorig_jaar, 
   BS5HD2.3_Nationaliteit, BS5HD2.3_Geboorteland, BS5HD2.3_Onderwijsniveau, BS5HD2.3_Econ_status, 
   BS5HD2.3_Beroep, BS5HD2.3_SBI, BS5HD2.3_Burg_Staat)
rm(BOS5HD2.3_Geslacht, BOS5HD2.3_Leeftijd, BOS5HD2.3_HH_Pos, BOS5HD2.3_HH_grootte, BOS5HD2.3_Woonregio_vorig_jaar, 
   BOS5HD2.3_Nationaliteit, BOS5HD2.3_Geboorteland, BOS5HD2.3_Onderwijsniveau, BOS5HD2.3_Econ_status, 
   BOS5HD2.3_Beroep, BOS5HD2.3_SBI, BOS5HD2.3_Burg_Staat)


# 5% data sets

# Counting values
S5HD5.1_Geslacht <- plyr::count(df_Shot_deck5.13, 'Geslacht')
S5HD5.1_Leeftijd <- plyr::count(df_Shot_deck5.13, 'Leeftijd')
S5HD5.1_HH_Pos <- plyr::count(df_Shot_deck5.13, 'HH_Pos')
S5HD5.1_HH_grootte <- plyr::count(df_Shot_deck5.13, 'HH_grootte')
S5HD5.1_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck5.13, 'Woonregio_vorig_jaar')
S5HD5.1_Nationaliteit <- plyr::count(df_Shot_deck5.13, 'Nationaliteit')
S5HD5.1_Geboorteland <- plyr::count(df_Shot_deck5.13, 'Geboorteland')
S5HD5.1_Onderwijsniveau <- plyr::count(df_Shot_deck5.13, 'Onderwijsniveau')
S5HD5.1_Econ_status <- plyr::count(df_Shot_deck5.13, 'Econ._status')
S5HD5.1_Beroep <- plyr::count(df_Shot_deck5.13, 'Beroep')
S5HD5.1_SBI <- plyr::count(df_Shot_deck5.13, 'SBI')
S5HD5.1_Burg_Staat <- plyr::count(df_Shot_deck5.13, 'Burg._Staat')

S5HD5.2_Geslacht <- plyr::count(df_Shot_deck5.23, 'Geslacht')
S5HD5.2_Leeftijd <- plyr::count(df_Shot_deck5.23, 'Leeftijd')
S5HD5.2_HH_Pos <- plyr::count(df_Shot_deck5.23, 'HH_Pos')
S5HD5.2_HH_grootte <- plyr::count(df_Shot_deck5.23, 'HH_grootte')
S5HD5.2_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck5.23, 'Woonregio_vorig_jaar')
S5HD5.2_Nationaliteit <- plyr::count(df_Shot_deck5.23, 'Nationaliteit')
S5HD5.2_Geboorteland <- plyr::count(df_Shot_deck5.23, 'Geboorteland')
S5HD5.2_Onderwijsniveau <- plyr::count(df_Shot_deck5.23, 'Onderwijsniveau')
S5HD5.2_Econ_status <- plyr::count(df_Shot_deck5.23, 'Econ._status')
S5HD5.2_Beroep <- plyr::count(df_Shot_deck5.23, 'Beroep')
S5HD5.2_SBI <- plyr::count(df_Shot_deck5.23, 'SBI')
S5HD5.2_Burg_Staat <- plyr::count(df_Shot_deck5.23, 'Burg._Staat')

S5HD5.3_Geslacht <- plyr::count(df_Shot_deck5.33, 'Geslacht')
S5HD5.3_Leeftijd <- plyr::count(df_Shot_deck5.33, 'Leeftijd')
S5HD5.3_HH_Pos <- plyr::count(df_Shot_deck5.33, 'HH_Pos')
S5HD5.3_HH_grootte <- plyr::count(df_Shot_deck5.33, 'HH_grootte')
S5HD5.3_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck5.33, 'Woonregio_vorig_jaar')
S5HD5.3_Nationaliteit <- plyr::count(df_Shot_deck5.33, 'Nationaliteit')
S5HD5.3_Geboorteland <- plyr::count(df_Shot_deck5.33, 'Geboorteland')
S5HD5.3_Onderwijsniveau <- plyr::count(df_Shot_deck5.33, 'Onderwijsniveau')
S5HD5.3_Econ_status <- plyr::count(df_Shot_deck5.33, 'Econ._status')
S5HD5.3_Beroep <- plyr::count(df_Shot_deck5.33, 'Beroep')
S5HD5.3_SBI <- plyr::count(df_Shot_deck5.33, 'SBI')
S5HD5.3_Burg_Staat <- plyr::count(df_Shot_deck5.33, 'Burg._Staat')

rm(df_Shot_deck5.13, df_Shot_deck5.23, df_Shot_deck5.33)


# 5.1 data set 
BS5HD5.1_Geslacht <- Bias(S5HD5.1_Geslacht, Geslacht, "Geslacht")
BOS5HD5.1_Geslacht <- sum(BS5HD5.1_Geslacht$percentageABS)/nrow(BS5HD5.1_Geslacht)

BS5HD5.1_Leeftijd <- Bias(S5HD5.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD5.1_Leeftijd <- sum(BS5HD5.1_Leeftijd$percentageABS)/nrow(BS5HD5.1_Leeftijd)

BS5HD5.1_HH_Pos <- Bias(S5HD5.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD5.1_HH_Pos <- sum(BS5HD5.1_HH_Pos$percentageABS)/nrow(BS5HD5.1_HH_Pos)

BS5HD5.1_HH_grootte <- Bias(S5HD5.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD5.1_HH_grootte <- sum(BS5HD5.1_HH_grootte$percentageABS)/nrow(BS5HD5.1_HH_grootte)

BS5HD5.1_Woonregio_vorig_jaar <- Bias(S5HD5.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD5.1_Woonregio_vorig_jaar <- sum(BS5HD5.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD5.1_Woonregio_vorig_jaar)

BS5HD5.1_Nationaliteit <- Bias(S5HD5.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD5.1_Nationaliteit <- sum(BS5HD5.1_Nationaliteit$percentageABS)/nrow(BS5HD5.1_Nationaliteit)

BS5HD5.1_Geboorteland <- Bias(S5HD5.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD5.1_Geboorteland <- sum(BS5HD5.1_Geboorteland$percentageABS)/nrow(BS5HD5.1_Geboorteland)

BS5HD5.1_Onderwijsniveau <- Bias(S5HD5.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD5.1_Onderwijsniveau <- sum(BS5HD5.1_Onderwijsniveau$percentageABS)/nrow(BS5HD5.1_Onderwijsniveau)

BS5HD5.1_Econ_status <- Bias2(S5HD5.1_Econ_status, Econ_status, "Econ._status")
BOS5HD5.1_Econ_status <- sum(BS5HD5.1_Econ_status$percentageABS)/nrow(BS5HD5.1_Econ_status)

BS5HD5.1_Beroep <- Bias(S5HD5.1_Beroep, Beroep, "Beroep")
BOS5HD5.1_Beroep <- sum(BS5HD5.1_Beroep$percentageABS)/nrow(BS5HD5.1_Beroep)

BS5HD5.1_SBI <- Bias(S5HD5.1_SBI, SBI, "SBI")
BOS5HD5.1_SBI <- sum(BS5HD5.1_SBI$percentageABS)/nrow(BS5HD5.1_SBI)

BS5HD5.1_Burg_Staat <- Bias2(S5HD5.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD5.1_Burg_Staat <- sum(BS5HD5.1_Burg_Staat$percentageABS)/nrow(BS5HD5.1_Burg_Staat)

BiasOverallS5HD5.1 <- sum(BOS5HD5.1_Geslacht, BOS5HD5.1_Leeftijd, BOS5HD5.1_HH_Pos, BOS5HD5.1_HH_grootte, BOS5HD5.1_Woonregio_vorig_jaar, 
                        BOS5HD5.1_Nationaliteit, BOS5HD5.1_Geboorteland, BOS5HD5.1_Onderwijsniveau, BOS5HD5.1_Econ_status, 
                        BOS5HD5.1_Beroep, BOS5HD5.1_SBI, BOS5HD5.1_Burg_Staat) / 12

rm(S5HD5.1_Geslacht, S5HD5.1_Leeftijd, S5HD5.1_HH_Pos, S5HD5.1_HH_grootte, S5HD5.1_Woonregio_vorig_jaar, 
   S5HD5.1_Nationaliteit, S5HD5.1_Geboorteland, S5HD5.1_Onderwijsniveau, S5HD5.1_Econ_status, 
   S5HD5.1_Beroep, S5HD5.1_SBI, S5HD5.1_Burg_Staat)
rm(BS5HD5.1_Geslacht, BS5HD5.1_Leeftijd, BS5HD5.1_HH_Pos, BS5HD5.1_HH_grootte, BS5HD5.1_Woonregio_vorig_jaar, 
   BS5HD5.1_Nationaliteit, BS5HD5.1_Geboorteland, BS5HD5.1_Onderwijsniveau, BS5HD5.1_Econ_status, 
   BS5HD5.1_Beroep, BS5HD5.1_SBI, BS5HD5.1_Burg_Staat)
rm(BOS5HD5.1_Geslacht, BOS5HD5.1_Leeftijd, BOS5HD5.1_HH_Pos, BOS5HD5.1_HH_grootte, BOS5HD5.1_Woonregio_vorig_jaar, 
   BOS5HD5.1_Nationaliteit, BOS5HD5.1_Geboorteland, BOS5HD5.1_Onderwijsniveau, BOS5HD5.1_Econ_status, 
   BOS5HD5.1_Beroep, BOS5HD5.1_SBI, BOS5HD5.1_Burg_Staat)

# 5.2 data set 
BS5HD5.2_Geslacht <- Bias(S5HD5.2_Geslacht, Geslacht, "Geslacht")
BOS5HD5.2_Geslacht <- sum(BS5HD5.2_Geslacht$percentageABS)/nrow(BS5HD5.2_Geslacht)

BS5HD5.2_Leeftijd <- Bias(S5HD5.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD5.2_Leeftijd <- sum(BS5HD5.2_Leeftijd$percentageABS)/nrow(BS5HD5.2_Leeftijd)

BS5HD5.2_HH_Pos <- Bias(S5HD5.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD5.2_HH_Pos <- sum(BS5HD5.2_HH_Pos$percentageABS)/nrow(BS5HD5.2_HH_Pos)

BS5HD5.2_HH_grootte <- Bias(S5HD5.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD5.2_HH_grootte <- sum(BS5HD5.2_HH_grootte$percentageABS)/nrow(BS5HD5.2_HH_grootte)

BS5HD5.2_Woonregio_vorig_jaar <- Bias(S5HD5.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD5.2_Woonregio_vorig_jaar <- sum(BS5HD5.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD5.2_Woonregio_vorig_jaar)

BS5HD5.2_Nationaliteit <- Bias(S5HD5.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD5.2_Nationaliteit <- sum(BS5HD5.2_Nationaliteit$percentageABS)/nrow(BS5HD5.2_Nationaliteit)

BS5HD5.2_Geboorteland <- Bias(S5HD5.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD5.2_Geboorteland <- sum(BS5HD5.2_Geboorteland$percentageABS)/nrow(BS5HD5.2_Geboorteland)

BS5HD5.2_Onderwijsniveau <- Bias(S5HD5.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD5.2_Onderwijsniveau <- sum(BS5HD5.2_Onderwijsniveau$percentageABS)/nrow(BS5HD5.2_Onderwijsniveau)

BS5HD5.2_Econ_status <- Bias2(S5HD5.2_Econ_status, Econ_status, "Econ._status")
BOS5HD5.2_Econ_status <- sum(BS5HD5.2_Econ_status$percentageABS)/nrow(BS5HD5.2_Econ_status)

BS5HD5.2_Beroep <- Bias(S5HD5.2_Beroep, Beroep, "Beroep")
BOS5HD5.2_Beroep <- sum(BS5HD5.2_Beroep$percentageABS)/nrow(BS5HD5.2_Beroep)

BS5HD5.2_SBI <- Bias(S5HD5.2_SBI, SBI, "SBI")
BOS5HD5.2_SBI <- sum(BS5HD5.2_SBI$percentageABS)/nrow(BS5HD5.2_SBI)

BS5HD5.2_Burg_Staat <- Bias2(S5HD5.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD5.2_Burg_Staat <- sum(BS5HD5.2_Burg_Staat$percentageABS)/nrow(BS5HD5.2_Burg_Staat)

BiasOverallS5HD5.2 <- sum(BOS5HD5.2_Geslacht, BOS5HD5.2_Leeftijd, BOS5HD5.2_HH_Pos, BOS5HD5.2_HH_grootte, BOS5HD5.2_Woonregio_vorig_jaar, 
                        BOS5HD5.2_Nationaliteit, BOS5HD5.2_Geboorteland, BOS5HD5.2_Onderwijsniveau, BOS5HD5.2_Econ_status, 
                        BOS5HD5.2_Beroep, BOS5HD5.2_SBI, BOS5HD5.2_Burg_Staat) / 12

rm(S5HD5.2_Geslacht, S5HD5.2_Leeftijd, S5HD5.2_HH_Pos, S5HD5.2_HH_grootte, S5HD5.2_Woonregio_vorig_jaar, 
   S5HD5.2_Nationaliteit, S5HD5.2_Geboorteland, S5HD5.2_Onderwijsniveau, S5HD5.2_Econ_status, 
   S5HD5.2_Beroep, S5HD5.2_SBI, S5HD5.2_Burg_Staat)
rm(BS5HD5.2_Geslacht, BS5HD5.2_Leeftijd, BS5HD5.2_HH_Pos, BS5HD5.2_HH_grootte, BS5HD5.2_Woonregio_vorig_jaar, 
   BS5HD5.2_Nationaliteit, BS5HD5.2_Geboorteland, BS5HD5.2_Onderwijsniveau, BS5HD5.2_Econ_status, 
   BS5HD5.2_Beroep, BS5HD5.2_SBI, BS5HD5.2_Burg_Staat)
rm(BOS5HD5.2_Geslacht, BOS5HD5.2_Leeftijd, BOS5HD5.2_HH_Pos, BOS5HD5.2_HH_grootte, BOS5HD5.2_Woonregio_vorig_jaar, 
   BOS5HD5.2_Nationaliteit, BOS5HD5.2_Geboorteland, BOS5HD5.2_Onderwijsniveau, BOS5HD5.2_Econ_status, 
   BOS5HD5.2_Beroep, BOS5HD5.2_SBI, BOS5HD5.2_Burg_Staat)

# 5.3 data set 
BS5HD5.3_Geslacht <- Bias(S5HD5.3_Geslacht, Geslacht, "Geslacht")
BOS5HD5.3_Geslacht <- sum(BS5HD5.3_Geslacht$percentageABS)/nrow(BS5HD5.3_Geslacht)

BS5HD5.3_Leeftijd <- Bias(S5HD5.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD5.3_Leeftijd <- sum(BS5HD5.3_Leeftijd$percentageABS)/nrow(BS5HD5.3_Leeftijd)

BS5HD5.3_HH_Pos <- Bias(S5HD5.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD5.3_HH_Pos <- sum(BS5HD5.3_HH_Pos$percentageABS)/nrow(BS5HD5.3_HH_Pos)

BS5HD5.3_HH_grootte <- Bias(S5HD5.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD5.3_HH_grootte <- sum(BS5HD5.3_HH_grootte$percentageABS)/nrow(BS5HD5.3_HH_grootte)

BS5HD5.3_Woonregio_vorig_jaar <- Bias(S5HD5.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD5.3_Woonregio_vorig_jaar <- sum(BS5HD5.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD5.3_Woonregio_vorig_jaar)

BS5HD5.3_Nationaliteit <- Bias(S5HD5.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD5.3_Nationaliteit <- sum(BS5HD5.3_Nationaliteit$percentageABS)/nrow(BS5HD5.3_Nationaliteit)

BS5HD5.3_Geboorteland <- Bias(S5HD5.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD5.3_Geboorteland <- sum(BS5HD5.3_Geboorteland$percentageABS)/nrow(BS5HD5.3_Geboorteland)

BS5HD5.3_Onderwijsniveau <- Bias(S5HD5.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD5.3_Onderwijsniveau <- sum(BS5HD5.3_Onderwijsniveau$percentageABS)/nrow(BS5HD5.3_Onderwijsniveau)

BS5HD5.3_Econ_status <- Bias2(S5HD5.3_Econ_status, Econ_status, "Econ._status")
BOS5HD5.3_Econ_status <- sum(BS5HD5.3_Econ_status$percentageABS)/nrow(BS5HD5.3_Econ_status)

BS5HD5.3_Beroep <- Bias(S5HD5.3_Beroep, Beroep, "Beroep")
BOS5HD5.3_Beroep <- sum(BS5HD5.3_Beroep$percentageABS)/nrow(BS5HD5.3_Beroep)

BS5HD5.3_SBI <- Bias(S5HD5.3_SBI, SBI, "SBI")
BOS5HD5.3_SBI <- sum(BS5HD5.3_SBI$percentageABS)/nrow(BS5HD5.3_SBI)

BS5HD5.3_Burg_Staat <- Bias2(S5HD5.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD5.3_Burg_Staat <- sum(BS5HD5.3_Burg_Staat$percentageABS)/nrow(BS5HD5.3_Burg_Staat)

BiasOverallS5HD5.3 <- sum(BOS5HD5.3_Geslacht, BOS5HD5.3_Leeftijd, BOS5HD5.3_HH_Pos, BOS5HD5.3_HH_grootte, BOS5HD5.3_Woonregio_vorig_jaar, 
                        BOS5HD5.3_Nationaliteit, BOS5HD5.3_Geboorteland, BOS5HD5.3_Onderwijsniveau, BOS5HD5.3_Econ_status, 
                        BOS5HD5.3_Beroep, BOS5HD5.3_SBI, BOS5HD5.3_Burg_Staat) / 12

rm(S5HD5.3_Geslacht, S5HD5.3_Leeftijd, S5HD5.3_HH_Pos, S5HD5.3_HH_grootte, S5HD5.3_Woonregio_vorig_jaar, 
   S5HD5.3_Nationaliteit, S5HD5.3_Geboorteland, S5HD5.3_Onderwijsniveau, S5HD5.3_Econ_status, 
   S5HD5.3_Beroep, S5HD5.3_SBI, S5HD5.3_Burg_Staat)
rm(BS5HD5.3_Geslacht, BS5HD5.3_Leeftijd, BS5HD5.3_HH_Pos, BS5HD5.3_HH_grootte, BS5HD5.3_Woonregio_vorig_jaar, 
   BS5HD5.3_Nationaliteit, BS5HD5.3_Geboorteland, BS5HD5.3_Onderwijsniveau, BS5HD5.3_Econ_status, 
   BS5HD5.3_Beroep, BS5HD5.3_SBI, BS5HD5.3_Burg_Staat)
rm(BOS5HD5.3_Geslacht, BOS5HD5.3_Leeftijd, BOS5HD5.3_HH_Pos, BOS5HD5.3_HH_grootte, BOS5HD5.3_Woonregio_vorig_jaar, 
   BOS5HD5.3_Nationaliteit, BOS5HD5.3_Geboorteland, BOS5HD5.3_Onderwijsniveau, BOS5HD5.3_Econ_status, 
   BOS5HD5.3_Beroep, BOS5HD5.3_SBI, BOS5HD5.3_Burg_Staat)


# 10% data sets

# Counting values
S5HD10.1_Geslacht <- plyr::count(df_Shot_deck10.13, 'Geslacht')
S5HD10.1_Leeftijd <- plyr::count(df_Shot_deck10.13, 'Leeftijd')
S5HD10.1_HH_Pos <- plyr::count(df_Shot_deck10.13, 'HH_Pos')
S5HD10.1_HH_grootte <- plyr::count(df_Shot_deck10.13, 'HH_grootte')
S5HD10.1_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck10.13, 'Woonregio_vorig_jaar')
S5HD10.1_Nationaliteit <- plyr::count(df_Shot_deck10.13, 'Nationaliteit')
S5HD10.1_Geboorteland <- plyr::count(df_Shot_deck10.13, 'Geboorteland')
S5HD10.1_Onderwijsniveau <- plyr::count(df_Shot_deck10.13, 'Onderwijsniveau')
S5HD10.1_Econ_status <- plyr::count(df_Shot_deck10.13, 'Econ._status')
S5HD10.1_Beroep <- plyr::count(df_Shot_deck10.13, 'Beroep')
S5HD10.1_SBI <- plyr::count(df_Shot_deck10.13, 'SBI')
S5HD10.1_Burg_Staat <- plyr::count(df_Shot_deck10.13, 'Burg._Staat')

S5HD10.2_Geslacht <- plyr::count(df_Shot_deck10.23, 'Geslacht')
S5HD10.2_Leeftijd <- plyr::count(df_Shot_deck10.23, 'Leeftijd')
S5HD10.2_HH_Pos <- plyr::count(df_Shot_deck10.23, 'HH_Pos')
S5HD10.2_HH_grootte <- plyr::count(df_Shot_deck10.23, 'HH_grootte')
S5HD10.2_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck10.23, 'Woonregio_vorig_jaar')
S5HD10.2_Nationaliteit <- plyr::count(df_Shot_deck10.23, 'Nationaliteit')
S5HD10.2_Geboorteland <- plyr::count(df_Shot_deck10.23, 'Geboorteland')
S5HD10.2_Onderwijsniveau <- plyr::count(df_Shot_deck10.23, 'Onderwijsniveau')
S5HD10.2_Econ_status <- plyr::count(df_Shot_deck10.23, 'Econ._status')
S5HD10.2_Beroep <- plyr::count(df_Shot_deck10.23, 'Beroep')
S5HD10.2_SBI <- plyr::count(df_Shot_deck10.23, 'SBI')
S5HD10.2_Burg_Staat <- plyr::count(df_Shot_deck10.23, 'Burg._Staat')

S5HD10.3_Geslacht <- plyr::count(df_Shot_deck10.33, 'Geslacht')
S5HD10.3_Leeftijd <- plyr::count(df_Shot_deck10.33, 'Leeftijd')
S5HD10.3_HH_Pos <- plyr::count(df_Shot_deck10.33, 'HH_Pos')
S5HD10.3_HH_grootte <- plyr::count(df_Shot_deck10.33, 'HH_grootte')
S5HD10.3_Woonregio_vorig_jaar <- plyr::count(df_Shot_deck10.33, 'Woonregio_vorig_jaar')
S5HD10.3_Nationaliteit <- plyr::count(df_Shot_deck10.33, 'Nationaliteit')
S5HD10.3_Geboorteland <- plyr::count(df_Shot_deck10.33, 'Geboorteland')
S5HD10.3_Onderwijsniveau <- plyr::count(df_Shot_deck10.33, 'Onderwijsniveau')
S5HD10.3_Econ_status <- plyr::count(df_Shot_deck10.33, 'Econ._status')
S5HD10.3_Beroep <- plyr::count(df_Shot_deck10.33, 'Beroep')
S5HD10.3_SBI <- plyr::count(df_Shot_deck10.33, 'SBI')
S5HD10.3_Burg_Staat <- plyr::count(df_Shot_deck10.33, 'Burg._Staat')

rm(df_Shot_deck10.13, df_Shot_deck10.23, df_Shot_deck10.33)


# 10.1 data set 
BS5HD10.1_Geslacht <- Bias(S5HD10.1_Geslacht, Geslacht, "Geslacht")
BOS5HD10.1_Geslacht <- sum(BS5HD10.1_Geslacht$percentageABS)/nrow(BS5HD10.1_Geslacht)

BS5HD10.1_Leeftijd <- Bias(S5HD10.1_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD10.1_Leeftijd <- sum(BS5HD10.1_Leeftijd$percentageABS)/nrow(BS5HD10.1_Leeftijd)

BS5HD10.1_HH_Pos <- Bias(S5HD10.1_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD10.1_HH_Pos <- sum(BS5HD10.1_HH_Pos$percentageABS)/nrow(BS5HD10.1_HH_Pos)

BS5HD10.1_HH_grootte <- Bias(S5HD10.1_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD10.1_HH_grootte <- sum(BS5HD10.1_HH_grootte$percentageABS)/nrow(BS5HD10.1_HH_grootte)

BS5HD10.1_Woonregio_vorig_jaar <- Bias(S5HD10.1_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD10.1_Woonregio_vorig_jaar <- sum(BS5HD10.1_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD10.1_Woonregio_vorig_jaar)

BS5HD10.1_Nationaliteit <- Bias(S5HD10.1_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD10.1_Nationaliteit <- sum(BS5HD10.1_Nationaliteit$percentageABS)/nrow(BS5HD10.1_Nationaliteit)

BS5HD10.1_Geboorteland <- Bias(S5HD10.1_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD10.1_Geboorteland <- sum(BS5HD10.1_Geboorteland$percentageABS)/nrow(BS5HD10.1_Geboorteland)

BS5HD10.1_Onderwijsniveau <- Bias(S5HD10.1_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD10.1_Onderwijsniveau <- sum(BS5HD10.1_Onderwijsniveau$percentageABS)/nrow(BS5HD10.1_Onderwijsniveau)

BS5HD10.1_Econ_status <- Bias2(S5HD10.1_Econ_status, Econ_status, "Econ._status")
BOS5HD10.1_Econ_status <- sum(BS5HD10.1_Econ_status$percentageABS)/nrow(BS5HD10.1_Econ_status)

BS5HD10.1_Beroep <- Bias(S5HD10.1_Beroep, Beroep, "Beroep")
BOS5HD10.1_Beroep <- sum(BS5HD10.1_Beroep$percentageABS)/nrow(BS5HD10.1_Beroep)

BS5HD10.1_SBI <- Bias(S5HD10.1_SBI, SBI, "SBI")
BOS5HD10.1_SBI <- sum(BS5HD10.1_SBI$percentageABS)/nrow(BS5HD10.1_SBI)

BS5HD10.1_Burg_Staat <- Bias2(S5HD10.1_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD10.1_Burg_Staat <- sum(BS5HD10.1_Burg_Staat$percentageABS)/nrow(BS5HD10.1_Burg_Staat)

BiasOverallS5HD10.1 <- sum(BOS5HD10.1_Geslacht, BOS5HD10.1_Leeftijd, BOS5HD10.1_HH_Pos, BOS5HD10.1_HH_grootte, BOS5HD10.1_Woonregio_vorig_jaar, 
                         BOS5HD10.1_Nationaliteit, BOS5HD10.1_Geboorteland, BOS5HD10.1_Onderwijsniveau, BOS5HD10.1_Econ_status, 
                         BOS5HD10.1_Beroep, BOS5HD10.1_SBI, BOS5HD10.1_Burg_Staat) / 12

rm(S5HD10.1_Geslacht, S5HD10.1_Leeftijd, S5HD10.1_HH_Pos, S5HD10.1_HH_grootte, S5HD10.1_Woonregio_vorig_jaar, 
   S5HD10.1_Nationaliteit, S5HD10.1_Geboorteland, S5HD10.1_Onderwijsniveau, S5HD10.1_Econ_status, 
   S5HD10.1_Beroep, S5HD10.1_SBI, S5HD10.1_Burg_Staat)
rm(BS5HD10.1_Geslacht, BS5HD10.1_Leeftijd, BS5HD10.1_HH_Pos, BS5HD10.1_HH_grootte, BS5HD10.1_Woonregio_vorig_jaar, 
   BS5HD10.1_Nationaliteit, BS5HD10.1_Geboorteland, BS5HD10.1_Onderwijsniveau, BS5HD10.1_Econ_status, 
   BS5HD10.1_Beroep, BS5HD10.1_SBI, BS5HD10.1_Burg_Staat)
rm(BOS5HD10.1_Geslacht, BOS5HD10.1_Leeftijd, BOS5HD10.1_HH_Pos, BOS5HD10.1_HH_grootte, BOS5HD10.1_Woonregio_vorig_jaar, 
   BOS5HD10.1_Nationaliteit, BOS5HD10.1_Geboorteland, BOS5HD10.1_Onderwijsniveau, BOS5HD10.1_Econ_status, 
   BOS5HD10.1_Beroep, BOS5HD10.1_SBI, BOS5HD10.1_Burg_Staat)

# 10.2 data set 
BS5HD10.2_Geslacht <- Bias(S5HD10.2_Geslacht, Geslacht, "Geslacht")
BOS5HD10.2_Geslacht <- sum(BS5HD10.2_Geslacht$percentageABS)/nrow(BS5HD10.2_Geslacht)

BS5HD10.2_Leeftijd <- Bias(S5HD10.2_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD10.2_Leeftijd <- sum(BS5HD10.2_Leeftijd$percentageABS)/nrow(BS5HD10.2_Leeftijd)

BS5HD10.2_HH_Pos <- Bias(S5HD10.2_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD10.2_HH_Pos <- sum(BS5HD10.2_HH_Pos$percentageABS)/nrow(BS5HD10.2_HH_Pos)

BS5HD10.2_HH_grootte <- Bias(S5HD10.2_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD10.2_HH_grootte <- sum(BS5HD10.2_HH_grootte$percentageABS)/nrow(BS5HD10.2_HH_grootte)

BS5HD10.2_Woonregio_vorig_jaar <- Bias(S5HD10.2_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD10.2_Woonregio_vorig_jaar <- sum(BS5HD10.2_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD10.2_Woonregio_vorig_jaar)

BS5HD10.2_Nationaliteit <- Bias(S5HD10.2_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD10.2_Nationaliteit <- sum(BS5HD10.2_Nationaliteit$percentageABS)/nrow(BS5HD10.2_Nationaliteit)

BS5HD10.2_Geboorteland <- Bias(S5HD10.2_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD10.2_Geboorteland <- sum(BS5HD10.2_Geboorteland$percentageABS)/nrow(BS5HD10.2_Geboorteland)

BS5HD10.2_Onderwijsniveau <- Bias(S5HD10.2_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD10.2_Onderwijsniveau <- sum(BS5HD10.2_Onderwijsniveau$percentageABS)/nrow(BS5HD10.2_Onderwijsniveau)

BS5HD10.2_Econ_status <- Bias2(S5HD10.2_Econ_status, Econ_status, "Econ._status")
BOS5HD10.2_Econ_status <- sum(BS5HD10.2_Econ_status$percentageABS)/nrow(BS5HD10.2_Econ_status)

BS5HD10.2_Beroep <- Bias(S5HD10.2_Beroep, Beroep, "Beroep")
BOS5HD10.2_Beroep <- sum(BS5HD10.2_Beroep$percentageABS)/nrow(BS5HD10.2_Beroep)

BS5HD10.2_SBI <- Bias(S5HD10.2_SBI, SBI, "SBI")
BOS5HD10.2_SBI <- sum(BS5HD10.2_SBI$percentageABS)/nrow(BS5HD10.2_SBI)

BS5HD10.2_Burg_Staat <- Bias2(S5HD10.2_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD10.2_Burg_Staat <- sum(BS5HD10.2_Burg_Staat$percentageABS)/nrow(BS5HD10.2_Burg_Staat)

BiasOverallS5HD10.2 <- sum(BOS5HD10.2_Geslacht, BOS5HD10.2_Leeftijd, BOS5HD10.2_HH_Pos, BOS5HD10.2_HH_grootte, BOS5HD10.2_Woonregio_vorig_jaar, 
                         BOS5HD10.2_Nationaliteit, BOS5HD10.2_Geboorteland, BOS5HD10.2_Onderwijsniveau, BOS5HD10.2_Econ_status, 
                         BOS5HD10.2_Beroep, BOS5HD10.2_SBI, BOS5HD10.2_Burg_Staat) / 12

rm(S5HD10.2_Geslacht, S5HD10.2_Leeftijd, S5HD10.2_HH_Pos, S5HD10.2_HH_grootte, S5HD10.2_Woonregio_vorig_jaar, 
   S5HD10.2_Nationaliteit, S5HD10.2_Geboorteland, S5HD10.2_Onderwijsniveau, S5HD10.2_Econ_status, 
   S5HD10.2_Beroep, S5HD10.2_SBI, S5HD10.2_Burg_Staat)
rm(BS5HD10.2_Geslacht, BS5HD10.2_Leeftijd, BS5HD10.2_HH_Pos, BS5HD10.2_HH_grootte, BS5HD10.2_Woonregio_vorig_jaar, 
   BS5HD10.2_Nationaliteit, BS5HD10.2_Geboorteland, BS5HD10.2_Onderwijsniveau, BS5HD10.2_Econ_status, 
   BS5HD10.2_Beroep, BS5HD10.2_SBI, BS5HD10.2_Burg_Staat)
rm(BOS5HD10.2_Geslacht, BOS5HD10.2_Leeftijd, BOS5HD10.2_HH_Pos, BOS5HD10.2_HH_grootte, BOS5HD10.2_Woonregio_vorig_jaar, 
   BOS5HD10.2_Nationaliteit, BOS5HD10.2_Geboorteland, BOS5HD10.2_Onderwijsniveau, BOS5HD10.2_Econ_status, 
   BOS5HD10.2_Beroep, BOS5HD10.2_SBI, BOS5HD10.2_Burg_Staat)

# 10.3 data set 
BS5HD10.3_Geslacht <- Bias(S5HD10.3_Geslacht, Geslacht, "Geslacht")
BOS5HD10.3_Geslacht <- sum(BS5HD10.3_Geslacht$percentageABS)/nrow(BS5HD10.3_Geslacht)

BS5HD10.3_Leeftijd <- Bias(S5HD10.3_Leeftijd, Leeftijd, "Leeftijd")
BOS5HD10.3_Leeftijd <- sum(BS5HD10.3_Leeftijd$percentageABS)/nrow(BS5HD10.3_Leeftijd)

BS5HD10.3_HH_Pos <- Bias(S5HD10.3_HH_Pos, HH_Pos, "HH_Pos")
BOS5HD10.3_HH_Pos <- sum(BS5HD10.3_HH_Pos$percentageABS)/nrow(BS5HD10.3_HH_Pos)

BS5HD10.3_HH_grootte <- Bias(S5HD10.3_HH_grootte, HH_grootte, "HH_grootte")
BOS5HD10.3_HH_grootte <- sum(BS5HD10.3_HH_grootte$percentageABS)/nrow(BS5HD10.3_HH_grootte)

BS5HD10.3_Woonregio_vorig_jaar <- Bias(S5HD10.3_Woonregio_vorig_jaar, Woonregio_vorig_jaar, "Woonregio_vorig_jaar")
BOS5HD10.3_Woonregio_vorig_jaar <- sum(BS5HD10.3_Woonregio_vorig_jaar$percentageABS)/nrow(BS5HD10.3_Woonregio_vorig_jaar)

BS5HD10.3_Nationaliteit <- Bias(S5HD10.3_Nationaliteit, Nationaliteit, "Nationaliteit")
BOS5HD10.3_Nationaliteit <- sum(BS5HD10.3_Nationaliteit$percentageABS)/nrow(BS5HD10.3_Nationaliteit)

BS5HD10.3_Geboorteland <- Bias(S5HD10.3_Geboorteland, Geboorteland, "Geboorteland")
BOS5HD10.3_Geboorteland <- sum(BS5HD10.3_Geboorteland$percentageABS)/nrow(BS5HD10.3_Geboorteland)

BS5HD10.3_Onderwijsniveau <- Bias(S5HD10.3_Onderwijsniveau, Onderwijsniveau, "Onderwijsniveau")
BOS5HD10.3_Onderwijsniveau <- sum(BS5HD10.3_Onderwijsniveau$percentageABS)/nrow(BS5HD10.3_Onderwijsniveau)

BS5HD10.3_Econ_status <- Bias2(S5HD10.3_Econ_status, Econ_status, "Econ._status")
BOS5HD10.3_Econ_status <- sum(BS5HD10.3_Econ_status$percentageABS)/nrow(BS5HD10.3_Econ_status)

BS5HD10.3_Beroep <- Bias(S5HD10.3_Beroep, Beroep, "Beroep")
BOS5HD10.3_Beroep <- sum(BS5HD10.3_Beroep$percentageABS)/nrow(BS5HD10.3_Beroep)

BS5HD10.3_SBI <- Bias(S5HD10.3_SBI, SBI, "SBI")
BOS5HD10.3_SBI <- sum(BS5HD10.3_SBI$percentageABS)/nrow(BS5HD10.3_SBI)

BS5HD10.3_Burg_Staat <- Bias2(S5HD10.3_Burg_Staat, Burg_Staat, "Burg._Staat")
BOS5HD10.3_Burg_Staat <- sum(BS5HD10.3_Burg_Staat$percentageABS)/nrow(BS5HD10.3_Burg_Staat)

BiasOverallS5HD10.3 <- sum(BOS5HD10.3_Geslacht, BOS5HD10.3_Leeftijd, BOS5HD10.3_HH_Pos, BOS5HD10.3_HH_grootte, BOS5HD10.3_Woonregio_vorig_jaar, 
                         BOS5HD10.3_Nationaliteit, BOS5HD10.3_Geboorteland, BOS5HD10.3_Onderwijsniveau, BOS5HD10.3_Econ_status, 
                         BOS5HD10.3_Beroep, BOS5HD10.3_SBI, BOS5HD10.3_Burg_Staat) / 12

rm(S5HD10.3_Geslacht, S5HD10.3_Leeftijd, S5HD10.3_HH_Pos, S5HD10.3_HH_grootte, S5HD10.3_Woonregio_vorig_jaar, 
   S5HD10.3_Nationaliteit, S5HD10.3_Geboorteland, S5HD10.3_Onderwijsniveau, S5HD10.3_Econ_status, 
   S5HD10.3_Beroep, S5HD10.3_SBI, S5HD10.3_Burg_Staat)
rm(BS5HD10.3_Geslacht, BS5HD10.3_Leeftijd, BS5HD10.3_HH_Pos, BS5HD10.3_HH_grootte, BS5HD10.3_Woonregio_vorig_jaar, 
   BS5HD10.3_Nationaliteit, BS5HD10.3_Geboorteland, BS5HD10.3_Onderwijsniveau, BS5HD10.3_Econ_status, 
   BS5HD10.3_Beroep, BS5HD10.3_SBI, BS5HD10.3_Burg_Staat)
rm(BOS5HD10.3_Geslacht, BOS5HD10.3_Leeftijd, BOS5HD10.3_HH_Pos, BOS5HD10.3_HH_grootte, BOS5HD10.3_Woonregio_vorig_jaar, 
   BOS5HD10.3_Nationaliteit, BOS5HD10.3_Geboorteland, BOS5HD10.3_Onderwijsniveau, BOS5HD10.3_Econ_status, 
   BOS5HD10.3_Beroep, BOS5HD10.3_SBI, BOS5HD10.3_Burg_Staat)


