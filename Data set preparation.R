
## Installing 'readxl' package --------------------------------------------------------------------------------------------------------
# install.packages("readxl")
# install.packages("imputeR")
# install.packages("plyr")
# install.packages("dplyr")

## Loading packages --------------------------------------------------------------------------------------------------------------------
library("readxl")
library("imputeR")
library("plyr")
library("dplyr")


## Function for creating probability ---------------------------------------------------------------------------------------------------
SimIm <- function(data, p = 0.1) {
  vec <- c(unlist(data))
  missing <- rbinom(length(vec), 1, p)
  vec[missing == 1] <- NA
  dim(vec) <- dim(data)
  return(vec)
}


## Reading in 'Ipums2001 deel 1' and 'Ipums2001 deel 2' data sets ----------------------------------------------------------------------
ipumsdeel1 <- read_excel("input/IPUMS2001 deel 1.xlsx")
ipumsdeel2 <- read_excel("input/IPUMS2001 deel 2.xlsx")


## Combining the two data sets ---------------------------------------------------------------------------------------------------------
ipums <- rbind(ipumsdeel1, ipumsdeel2)


## Deleting 'nr' column and the 'Gewicht' column ---------------------------------------------------------------------------------------
ipums <- ipums[-1]
ipums$Gewicht <- NULL 

rm(ipumsdeel1, ipumsdeel2)
save(ipums, file = "input/ipums.Rdata")


## Creating MCAR data sets with different percentages: 2%, 5% and 10% ------------------------------------------------------------------

# 2% MCAR
# MCAR2.1 <- SimIm(ipums, p = 0.02)
# MCAR2.2 <- SimIm(ipums, p = 0.02)
# MCAR2.3 <- SimIm(ipums, p = 0.02)

# 5% MCAR
# MCAR5.1 <- SimIm(ipums, p = 0.05)
# MCAR5.2 <- SimIm(ipums, p = 0.05)
# MCAR5.3 <- SimIm(ipums, p = 0.05)

# 10% MCAR
# MCAR10.1 <- SimIm(ipums, p = 0.10)
# MCAR10.2 <- SimIm(ipums, p = 0.10)
# MCAR10.3 <- SimIm(ipums, p = 0.10)


## Saving data sets to use in every model/code -----------------------------------------------------------------------
# save(MCAR2.1, file = "MCAR2_1.Rdata")
# save(MCAR2.2, file = "MCAR2_2.Rdata")
# save(MCAR2.3, file = "MCAR2_3.Rdata")

# save(MCAR5.1, file = "MCAR5_1.Rdata")
# save(MCAR5.2, file = "MCAR5_2.Rdata")
# save(MCAR5.3, file = "MCAR5_3.Rdata")

# save(MCAR10.1, file = "MCAR10_1.Rdata")
# save(MCAR10.2, file = "MCAR10_2.Rdata")
# save(MCAR10.3, file = "MCAR10_3.Rdata")


## Creating subsets with 10% and 5% sample size ---------------------------------------------------------------------

# Taking random sample from data set for subset ---------------------------------------------------------------------------------------
my_rows10 <- sample(1:nrow(ipums), 18972, replace = F) # 10% sample size
my_rows5 <- sample(1:nrow(ipums), 9486, replace = F) # 5% sample size


# Creating ipums subset --------------------------------------------------------------------------------------------------------------------
sub_ipums10 <- ipums[my_rows10, c(1:12)]
sub_ipums5 <- ipums[my_rows5, c(1:12)]
save(sub_ipums10, file = "sub_ipums10.Rdata")
save(sub_ipums5, file = "sub_ipums5.Rdata")


# Creating MCAR subsets -------------------------------------------------------------------
sub10_MCAR2.1 <- MCAR2.1[my_rows10, c(1:12)]
sub10_MCAR2.2 <- MCAR2.2[my_rows10, c(1:12)]
sub10_MCAR2.3 <- MCAR2.3[my_rows10, c(1:12)]
sub10_MCAR5.1 <- MCAR5.1[my_rows10, c(1:12)]
sub10_MCAR5.2 <- MCAR5.2[my_rows10, c(1:12)]
sub10_MCAR5.3 <- MCAR5.3[my_rows10, c(1:12)]
sub10_MCAR10.1 <- MCAR10.1[my_rows10, c(1:12)]
sub10_MCAR10.2 <- MCAR10.2[my_rows10, c(1:12)]
sub10_MCAR10.3 <- MCAR10.3[my_rows10, c(1:12)]

sub5_MCAR2.1 <- MCAR2.1[my_rows5, c(1:12)]
sub5_MCAR2.2 <- MCAR2.2[my_rows5, c(1:12)]
sub5_MCAR2.3 <- MCAR2.3[my_rows5, c(1:12)]
sub5_MCAR5.1 <- MCAR5.1[my_rows5, c(1:12)]
sub5_MCAR5.2 <- MCAR5.2[my_rows5, c(1:12)]
sub5_MCAR5.3 <- MCAR5.3[my_rows5, c(1:12)]
sub5_MCAR10.1 <- MCAR10.1[my_rows5, c(1:12)]
sub5_MCAR10.2 <- MCAR10.2[my_rows5, c(1:12)]
sub5_MCAR10.3 <- MCAR10.3[my_rows5, c(1:12)]

# Saving all the subsets ---------------------------------------------------------------------------------
save(sub10_MCAR2.1, file = "sub10_MCAR21.Rdata")
save(sub10_MCAR2.2, file = "sub10_MCAR22.Rdata")
save(sub10_MCAR2.3, file = "sub10_MCAR23.Rdata")
save(sub10_MCAR5.1, file = "sub10_MCAR51.Rdata")
save(sub10_MCAR5.2, file = "sub10_MCAR52.Rdata")
save(sub10_MCAR5.3, file = "sub10_MCAR53.Rdata")
save(sub10_MCAR10.1, file = "sub10_MCAR101.Rdata")
save(sub10_MCAR10.2, file = "sub10_MCAR102.Rdata")
save(sub10_MCAR10.3, file = "sub10_MCAR103.Rdata")

save(sub5_MCAR2.1, file = "sub5_MCAR21.Rdata")
save(sub5_MCAR2.2, file = "sub5_MCAR22.Rdata")
save(sub5_MCAR2.3, file = "sub5_MCAR23.Rdata")
save(sub5_MCAR5.1, file = "sub5_MCAR51.Rdata")
save(sub5_MCAR5.2, file = "sub5_MCAR52.Rdata")
save(sub5_MCAR5.3, file = "sub5_MCAR53.Rdata")
save(sub5_MCAR10.1, file = "sub5_MCAR101.Rdata")
save(sub5_MCAR10.2, file = "sub5_MCAR102.Rdata")
save(sub5_MCAR10.3, file = "sub5_MCAR103.Rdata")

# Counting values in ipums to compute bias (later) -----------------------------------------------------

names(ipums) <- gsub(" ", "_", names(ipums), fixed=TRUE)

Geslacht <- plyr::count(ipums, 'Geslacht')
Leeftijd <- plyr::count(ipums, 'Leeftijd')
HH_Pos <- plyr::count(ipums, 'HH_Pos')
HH_grootte <- plyr::count(ipums, 'HH_grootte')
Woonregio_vorig_jaar <- plyr::count(ipums, 'Woonregio_vorig_jaar')
Nationaliteit <- plyr::count(ipums, 'Nationaliteit')
Geboorteland <- plyr::count(ipums, 'Geboorteland')
Onderwijsniveau <- plyr::count(ipums, 'Onderwijsniveau')
Econ_status <- plyr::count(ipums, 'Econ._status')
Beroep <- plyr::count(ipums, 'Beroep')
SBI <- plyr::count(ipums, 'SBI')
Burg_Staat <- plyr::count(ipums, 'Burg._Staat') 

save(Geslacht, file = "Freq_Geslacht.Rdata")
save(Leeftijd, file = "Freq_Leeftijd.Rdata")
save(HH_Pos, file = "Freq_HH_Pos.Rdata")
save(HH_grootte, file = "Freq_HH_grootte.Rdata")
save(Woonregio_vorig_jaar, file = "Freq_Woonregio.Rdata")
save(Nationaliteit, file = "Freq_Nationaliteit.Rdata")
save(Geboorteland, file = "Freq_Geboorteland.Rdata")
save(Onderwijsniveau, file = "Freq_Onderwijsniveau.Rdata")
save(Econ_status, file = "Freq_Econstatus.Rdata")
save(Beroep, file = "Freq_Beroep.Rdata")
save(SBI, file = "Freq_SBI.Rdata")
save(Burg_Staat, file = "Freq_Burgstaat.Rdata")

