# Code to create table summarizing the number of races and number of ancestries selected by respondents

rm(list = ls())

# Libraries ---------------------------------------------------------------

library(dplyr)
library(xtable)
library(Publish)

# Load functions ----------------------------------------------------------
source("Code/02.Analyses/generalHelpers.R")

# Load data ---------------------------------------------------------------
load("Data/AQP_DataForAnalysis.RData")

# Create input data -------------------------------------------------------

# Gender data
AQP.gender <- AQP.Data$AQP.demo %>% select(studyid, gender) %>% filter(!is.na(gender))
AQP.gender$gender <- gsub("M", "Male", AQP.gender$gender)
AQP.gender$gender <- gsub("F", "Female", AQP.gender$gender)

# Age data
AQP.age <- AQP.Data$AQP.demo %>% select(studyid, age)  %>% filter(!is.na(age))

# Born data
AQP.born <- AQP.Data$AQP.demo %>% mutate(born_US_f = ifelse(is.na(born_f), NA,
                                                            ifelse(born_f == 1, 1,
                                                                   ifelse(born_f == 2, 0,
                                                                          ifelse(born_f == 3, 0, NA)))),
                                         
                                         born_US_m = ifelse(is.na(born_m), NA,
                                                            ifelse(born_m == 1, 1,
                                                                   ifelse(born_m == 2, 0,
                                                                          ifelse(born_m == 3, 0, NA)))),
                                         
                                         born_US_pgf = ifelse(is.na(born_pgf), NA,
                                                              ifelse(born_pgf == 1, 1,
                                                                     ifelse(born_pgf == 2, 0,
                                                                            ifelse(born_pgf == 3, 0, NA)))),
                                         
                                         born_US_mgf = ifelse(is.na(born_mgf), NA,
                                                              ifelse(born_mgf == 1, 1,
                                                                     ifelse(born_mgf == 2, 0,
                                                                            ifelse(born_mgf == 3, 0, NA)))),
                                         
                                         born_US_pgm = ifelse(is.na(born_pgm), NA,
                                                              ifelse(born_pgm == 1, 1,
                                                                     ifelse(born_pgm == 2, 0,
                                                                            ifelse(born_pgm == 3, 0, NA)))),
                                         
                                         born_US_mgm = ifelse(is.na(born_mgm), NA,
                                                              ifelse(born_mgm == 1, 1,
                                                                     ifelse(born_mgm == 2, 0,
                                                                            ifelse(born_mgm == 3, 0, NA))))) %>%
  mutate(Parents_US_Born = born_US_f + born_US_m,
         GrandParents_US_Born = born_US_pgf + born_US_pgm + born_US_mgf + born_US_mgm) %>%
  select(studyid, born_US, Parents_US_Born, GrandParents_US_Born)

AQP.born$born_US[AQP.born$born_US == 1] <- "Yes"
AQP.born$born_US[AQP.born$born_US == 2] <- "No"
AQP.born$Parents_US_Born[AQP.born$Parents_US_Born == 0] <- "Neither"
AQP.born$Parents_US_Born[AQP.born$Parents_US_Born == 1] <- "One"
AQP.born$Parents_US_Born[AQP.born$Parents_US_Born == 2] <- "Both"
AQP.born$GrandParents_US_Born[AQP.born$GrandParents_US_Born == 0] <- "None"
AQP.born$GrandParents_US_Born[AQP.born$GrandParents_US_Born == 1] <- "One"
AQP.born$GrandParents_US_Born[AQP.born$GrandParents_US_Born == 2] <- "Two"
AQP.born$GrandParents_US_Born[AQP.born$GrandParents_US_Born == 3] <- "Three"
AQP.born$GrandParents_US_Born[AQP.born$GrandParents_US_Born == 4] <- "Four"


# Create table Born Vs Gender and Age -------------------------------------

# Gender vs born
table.genderVsBorn <- AQP.born %>% right_join(AQP.gender)

uniTable.genderVsBorn <- function (table) {
  table$born_US <- factor(table$born_US, levels = levels(factor(table$born_US))[c(2,1)])
  table$Parents_US_Born <- factor(table$Parents_US_Born, levels = levels(factor(table$Parents_US_Born))[c(2,3,1)])
  table$GrandParents_US_Born <- factor(table$GrandParents_US_Born, levels = levels(factor(table$GrandParents_US_Born))[c(2,3,5,4,1)])
  table <- univariateTable(gender ~  born_US + Parents_US_Born +  GrandParents_US_Born, data = table, n = F, freq.format = "percent(x)", column.percent = F, compare.groups = F)
  summary(table)
}

genderTableVsBorn <- uniTable.genderVsBorn(table.genderVsBorn)
genderTableVsBorn <- genderTableVsBorn %>% select(-starts_with("Total"))

# Age vs born
table.ageVsBorn <- AQP.born %>% right_join(AQP.age)

uniTable.ageVsBorn <- function (table) {
  table$born_US <- factor(table$born_US, levels = levels(factor(table$born_US))[c(2,1)])
  table$Parents_US_Born <- factor(table$Parents_US_Born, levels = levels(factor(table$Parents_US_Born))[c(2,3,1)])
  table$GrandParents_US_Born <- factor(table$GrandParents_US_Born, levels = levels(factor(table$GrandParents_US_Born))[c(2,3,5,4,1)])
  table <- univariateTable(age ~  born_US + Parents_US_Born +  GrandParents_US_Born, data = table, n = F, freq.format = "percent(x)", column.percent = F, compare.groups = F)
  summary(table)
}

ageTableVsBorn <- uniTable.ageVsBorn(table.ageVsBorn)
ageTableVsBorn <- ageTableVsBorn %>% select(-starts_with("Total"))

# Bind tables
table(ageTableVsBorn$Variable == genderTableVsBorn$Variable)
table(ageTableVsBorn$Level == genderTableVsBorn$Level)
bornTable <- cbind(genderTableVsBorn, ageTableVsBorn %>% select(-Variable, -Level))
bornTable <- bornTable %>% filter(Level != "missing")

# Create Total column
bornTable$Total[bornTable$Variable == "n"] <- nrow(AQP.Data$AQP.demo)
bornTable$Total[bornTable$Level == "Yes"] <- nrow(AQP.born %>% filter(born_US == "Yes"))
bornTable$Total[bornTable$Level == "No"] <- nrow(AQP.born %>% filter(born_US == "No"))
bornTable$Total[bornTable$Level == "Neither"] <- nrow(AQP.born %>% filter(Parents_US_Born == "Neither"))
bornTable$Total[5] <- nrow(AQP.born %>% filter(Parents_US_Born == "One"))
bornTable$Total[bornTable$Level == "Both"] <- nrow(AQP.born %>% filter(Parents_US_Born == "Both"))
bornTable$Total[bornTable$Level == "None"] <- nrow(AQP.born %>% filter(GrandParents_US_Born == "None"))
bornTable$Total[8] <- nrow(AQP.born %>% filter(GrandParents_US_Born == "One"))
bornTable$Total[bornTable$Level == "Two"] <- nrow(AQP.born %>% filter(GrandParents_US_Born == "Two"))
bornTable$Total[bornTable$Level == "Three"] <- nrow(AQP.born %>% filter(GrandParents_US_Born == "Three"))
bornTable$Total[bornTable$Level == "Four"] <- nrow(AQP.born %>% filter(GrandParents_US_Born == "Four"))



# Formatting Supplementary Table 1 ----------------------------------------

suppTable1 <- bornTable[, c(1:2, 11, 3:10)]
suppTable1[suppTable1$Variable == "n", "Variable"] <- "All respondents"
suppTable1$Variable <- gsub("_", " ", suppTable1$Variable)
suppTable1[suppTable1$Variable == "born US", "Variable"] <- "Respondents US born"

for (row in 2:nrow(suppTable1)) {
  for (col in colnames(suppTable1)[!colnames(suppTable1) %in% c("Variable", "Level", "Total")]) {
    suppTable1[row, col] <- paste(suppTable1[row, col], "%")
  }
}

colnames(suppTable1) <- gsub("Variable", "", colnames(suppTable1))
colnames(suppTable1) <- gsub("Level", "", colnames(suppTable1))
suppTable1


# Import to PDF -----------------------------------------------------------

if (!dir.exists("Output/SupplementaryTable1")) { dir.create("Output/SupplementaryTable1") }

SweaveToPdf(filePath = "Code/02.Analyses/SupplementaryTable1/SupplementaryTable1.Rnw", 
            outputPath = file.path("Output/SupplementaryTable1/", "SupplementaryTable1.pdf"))


