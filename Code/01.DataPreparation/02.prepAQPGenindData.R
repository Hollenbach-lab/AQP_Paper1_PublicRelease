# 02.prepAQPGenindData.R
# This code prepares the object of class genind for samples included in AQP survey

rm(list=ls())

# 01. Libraries loading ---------------------------------------------------
library("readr")
library("dplyr")
library("adegenet")

# 02. Importing plink genetics data ---------------------------------------

# Importing .map file
cat("\n# Importing .map file\n")
map <- read_delim("Input/aqp2.ped/aqp2.map", delim = " ", col_names = FALSE)
table(map$X2, useNA = "ifany")
cat("\n\t .map file successfully imported\n\n")

# Importing .ped file
cat("\n# Importing .ped file\n")
colType = paste(rep("c",6360), collapse = "")
ped <- read_delim("Input/aqp2.ped/aqp2.ped", delim = " ", col_names = FALSE, col_types = colType)
rm(colType)
cat("\n# Removing NA columns from .ped file ###\n")
ped <- ped[, -seq(7, 6358, by=3)]
cat("\n\t .ped file successfully imported\n\n")

# Quick quality check
cat("\n# 1.3 Quality control\n")
stopifnot(ncol(ped) == nrow(map)*2+6)
stopifnot(!anyDuplicated(ped$X2)) #Check there is no duplicate in the individual IDs column
table(is.na(ped$X2)) # 1 Id with NA
ped <- ped %>% filter(!is.na(X2)) #Remove the genetic data of the individual with NA as ID
ped$X2 <- as.numeric(ped$X2)
ped <- ped %>% arrange(X2) #Ordering data in increasing IDs
cat("\n\t Quality control done \n\n")


# 03. Creating the matrix -------------------------------------------------

cat("\n# Creating input matrix for genind object creation ...\n")
ids <- ped$X2
inputDf <- ped %>% select(-c(X1:X6)) #Remove header columns of ped file
indexOdd <- seq(1, ncol(inputDf), by = 2)
indexEven <- indexOdd + 1
newInputDf <- matrix(paste(as.matrix(inputDf[,indexOdd]), as.matrix(inputDf[,indexEven]), sep = "/"),
                     nrow = nrow(inputDf), 
                     ncol = ncol(inputDf)/2,
                     dimnames = list(ids, paste(map$X2, map$X4, sep = "_")) )

# Have a quick look
newInputDf[1:20, 1:15]
cat("\n\t Input matrix created\n\n")

cat("\n# Removing monomorphic SNPs from the input matrix ... \n")
snpToRemove <- c()
for (i in 1:ncol(newInputDf)) {
  temp <- newInputDf[,i]
  if(length(temp[!duplicated(temp)])==1) {
    snpToRemove <- c(snpToRemove, i)
  }
}
newInputDf <- newInputDf[, -snpToRemove]
cat("\n= Removed",length(snpToRemove), "monomorphic SNPs on", length(snpToRemove) + ncol(newInputDf), "SNPs =\n\n")

cat("\n# Creating genind object ...\n")
genindAllData <- df2genind(newInputDf, ploidy = 2, sep = "/")
genindAllData
save(genindAllData, file = "Data/genindAllData.RData")

cat("\n\n\t=== genindAllData.RData created and saved ===\n\n")

rm(list=ls())
