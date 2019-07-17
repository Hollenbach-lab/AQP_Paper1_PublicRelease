# Code to create table summarizing the number of races and number of ancestries selected by respondents

rm(list = ls())

# Libraries ---------------------------------------------------------------

library(dplyr)
library(xtable)

# Load functions ----------------------------------------------------------
source("Code/02.Analyses/generalHelpers.R")

# Load data ---------------------------------------------------------------
load("Data/AQP_DataForAnalysis.RData")

# Create input data -------------------------------------------------------

AQP <- AQP.Data$AQP.race %>% inner_join(AQP.Data$AQP.ancestry)
AQP$racesCombi <- apply(AQP[, AQP.DataCols$raceCols], 1, collapse = "", paste)
AQP$ancCombi <- apply(AQP[, AQP.DataCols$ancestryCols], 1, collapse = "", paste)
AQP$RacesAncCombi <- paste(AQP$racesCombi, AQP$ancCombi, sep = "-")

# Create table cutoff 0 ---------------------------------------------------
cat("There are", nrow(as.data.frame(table(AQP$RacesAncCombi))), "different populations\n")
RaceAnc_Summary.0 <- as.data.frame.matrix(table(AQP$race_selection_sum, AQP$anc_selection_sum, useNA = "ifany"), stringsAsFactors = F)

# Add rows that are missing
missing.nRace <- setdiff(1:7, rownames(RaceAnc_Summary.0))
for (i in 1:length(missing.nRace)) {
  rowToAdd <- rep(0, length(colnames(RaceAnc_Summary.0)))
  RaceAnc_Summary.0 <- rbind(RaceAnc_Summary.0, rowToAdd)
  rownames(RaceAnc_Summary.0)[nrow(RaceAnc_Summary.0)] <- missing.nRace[i]
}

# Add columns that are missing
missing.nAnc <- setdiff(1:16, colnames(RaceAnc_Summary.0))
for (i in 1:length(missing.nAnc)) {
  colToAdd <- rep(0, length(rownames(RaceAnc_Summary.0)))
  RaceAnc_Summary.0 <- data.frame(RaceAnc_Summary.0, colToAdd, stringsAsFactors = F)
  colnames(RaceAnc_Summary.0)[ncol(RaceAnc_Summary.0)] <- paste0("X", missing.nAnc[i])
}
RaceAnc_Summary.0 <- RaceAnc_Summary.0[,paste0("X", 1:length(colnames(RaceAnc_Summary.0)))]
colnames(RaceAnc_Summary.0) <- gsub("X", "", colnames(RaceAnc_Summary.0))

RaceAnc_Summary.0$Total <- rowSums(RaceAnc_Summary.0)
RaceAnc_Summary.0 <- rbind(RaceAnc_Summary.0, Total = colSums(RaceAnc_Summary.0))
RaceAnc_Summary.0[RaceAnc_Summary.0 == 0] <- "-"

# Create table cutoff 50 --------------------------------------------------
# Create table with only populations with more than 50 individuals

popN50RaceAnc <- as.data.frame(table(AQP$RacesAncCombi)) %>% filter(Freq >= 50)
nrow(popN50RaceAnc)
popToKeep <- as.character(popN50RaceAnc$Var1)
cat("With a cutoff of 50 ind min, there are", length(popToKeep), "different populations\n")

AQP.50 <- AQP %>% filter(RacesAncCombi %in% popToKeep)

RaceAnc_Summary.50 <- as.data.frame.matrix(table(AQP.50$race_selection_sum, AQP.50$anc_selection_sum, useNA = "ifany"), stringsAsFactors = F)

# Add rows that are missing
missing.nRace <- setdiff(1:7, rownames(RaceAnc_Summary.50))
for (i in 1:length(missing.nRace)) {
  rowToAdd <- rep(0, length(colnames(RaceAnc_Summary.50)))
  RaceAnc_Summary.50 <- rbind(RaceAnc_Summary.50, rowToAdd)
  rownames(RaceAnc_Summary.50)[nrow(RaceAnc_Summary.50)] <- missing.nRace[i]
}

# Add columns that are missing
missing.nAnc <- setdiff(1:16, colnames(RaceAnc_Summary.50))
for (i in 1:length(missing.nAnc)) {
  colToAdd <- rep(0, length(rownames(RaceAnc_Summary.50)))
  RaceAnc_Summary.50 <- data.frame(RaceAnc_Summary.50, colToAdd, stringsAsFactors = F)
  colnames(RaceAnc_Summary.50)[ncol(RaceAnc_Summary.50)] <- paste0("X", missing.nAnc[i])
}
RaceAnc_Summary.50 <- RaceAnc_Summary.50[,paste0("X", 1:length(colnames(RaceAnc_Summary.50)))]
colnames(RaceAnc_Summary.50) <- gsub("X", "", colnames(RaceAnc_Summary.50))

RaceAnc_Summary.50$Total <- rowSums(RaceAnc_Summary.50)
RaceAnc_Summary.50 <- rbind(RaceAnc_Summary.50, Total = colSums(RaceAnc_Summary.50))
RaceAnc_Summary.50[RaceAnc_Summary.50 == 0] <- "-"


# Create PDF --------------------------------------------------------------

if (!dir.exists("Output/SupplementaryTable3")) { dir.create("Output/SupplementaryTable3") }

SweaveToPdf(filePath = "Code/02.Analyses/SupplementaryTable3/SupplementaryTable3.Rnw", 
            outputPath = file.path("Output/SupplementaryTable3/", "SupplementaryTable3.pdf"))
