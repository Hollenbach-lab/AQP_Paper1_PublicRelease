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
AQP.age <- AQP.Data$AQP.demo %>% select(studyid, age) %>% filter(!is.na(age))


# Race data
AQP.race <- AQP.Data$AQP.race %>% mutate(Race = ifelse(race_selection_sum > 1, "Multi-race",
                                                       ifelse(Asian == 1, "Asian",
                                                              ifelse(Black == 1, "Black",
                                                                     ifelse(Hispanic == 1, "Hispanic",
                                                                            ifelse(NHPI == 1, "Native Hawaiian or Pacific Islander",
                                                                                   ifelse(White == 1, "White",
                                                                                          ifelse(Other == 1, "Other",
                                                                                                 ifelse(Amin == 1, "American Indian", NA))))))))) %>%
  select(studyid, Race)

table(AQP.race$Race, useNA = "ifany")
table(AQP.Data$AQP.ancestry$WesternEurope[AQP.Data$AQP.ancestry$AmericanIndian==1], useNA = "ifany")
test.2 <- AQP.Data$AQP.ancestry %>% filter(AmericanIndian == 1) %>% filter(anc_selection_sum %in% c(2))
test.3 <- AQP.Data$AQP.ancestry %>% filter(AmericanIndian == 1) %>% filter(anc_selection_sum %in% c(3))

# Create table Race Vs Gender and Age -------------------------------------

# Age vs race
table.ageVsRace <- AQP.race %>% right_join(AQP.age)

uniTable.ageVsRace <- function (table) {
  table$Race <- factor(table$Race, levels = levels(factor(table$Race))[c(1:4, 6, 8, 7, 5)])
  table <- univariateTable(age ~  Race, data = table, n = F, freq.format = "percent(x)", column.percent = F, compare.groups = F)
  summary(table)
}

ageTableVsRace <- uniTable.ageVsRace(table.ageVsRace)
ageTableVsRace <- ageTableVsRace %>% select(-starts_with("Total"))


# Gender vs race
table.genderVsRace <- AQP.race %>% right_join(AQP.gender)

uniTable.genderVsRace <- function (table) {
  table$Race <- factor(table$Race, levels = levels(factor(table$Race))[c(1:4, 6, 8, 7, 5)])
  table <- univariateTable(gender ~  Race, data = table, n = F, freq.format = "percent(x)", column.percent = F, compare.groups = F)
  summary(table)
}

genderTableVsRace <- uniTable.genderVsRace(table.genderVsRace)
genderTableVsRace <- genderTableVsRace %>% select(-starts_with("Total"))

# Bind tables
table(ageTableVsRace$Variable == genderTableVsRace$Variable)
table(ageTableVsRace$Level == genderTableVsRace$Level)
raceTable <- cbind(genderTableVsRace, ageTableVsRace %>% select(-Variable, -Level))
raceTable <- raceTable %>% filter(Level != "missing")

# Create Total column
raceTable$Total[raceTable$Variable == "n"] <- nrow(AQP.Data$AQP.demo)
raceTable$Total[raceTable$Level == "American Indian"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, Amin == 1))
raceTable$Total[raceTable$Level == "Asian"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, Asian == 1))
raceTable$Total[raceTable$Level == "Black"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, Black == 1))
raceTable$Total[raceTable$Level == "Hispanic"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, Hispanic == 1))
raceTable$Total[raceTable$Level == "Native Hawaiian or Pacific Islander"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, NHPI == 1))
raceTable$Total[raceTable$Level == "White"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, White == 1))
raceTable$Total[raceTable$Level == "Other"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum == 1, Other == 1))
raceTable$Total[raceTable$Level == "Multi-race"] <- nrow(AQP.Data$AQP.race %>% filter(race_selection_sum > 1))


# Formatting Supplementary Table 2 ----------------------------------------

suppTable2 <- raceTable[, c(1:2, 11, 3:10)]
suppTable2[suppTable2$Variable == "n", "Variable"] <- "All respondents"
suppTable2$Variable <- gsub("_", " ", suppTable2$Variable)
suppTable2[suppTable2$Variable == "born US", "Variable"] <- "Respondents US born"

for (row in 2:nrow(suppTable2)) {
  for (col in colnames(suppTable2)[!colnames(suppTable2) %in% c("Variable", "Level", "Total")]) {
    suppTable2[row, col] <- paste(suppTable2[row, col], "%")
  }
}

colnames(suppTable2) <- gsub("Variable", "", colnames(suppTable2))
colnames(suppTable2) <- gsub("Level", "", colnames(suppTable2))
suppTable2


# Import to PDF -----------------------------------------------------------

if (!dir.exists("Output/SupplementaryTable2")) { dir.create("Output/SupplementaryTable2") }

SweaveToPdf(filePath = "Code/02.Analyses/SupplementaryTable2/SupplementaryTable2.Rnw", 
            outputPath = file.path("Output/SupplementaryTable2", "SupplementaryTable2.pdf"))


