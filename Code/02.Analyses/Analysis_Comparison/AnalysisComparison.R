# Description -----------------------------------------------------------------------------------------------
# Comparison analyses

rm(list=ls())

# Library and functions -------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Create output directory -------------------------------------------------

if (!dir.exists("Output/Analysis_Comparison")) { dir.create("Output/Analysis_Comparison") }

# Helpers functions -------------------------------------------------------

source("Code/02.Analyses/Analysis_Comparison/AnalysisComparison_Helpers.R")
my_ggsave <- function(...) {
  ggsave(..., width = 18, height = 8, units = "in")
}

# Load data -------------------------------------------------------------------------------------------------
load("Data/AQP_DataForAnalysis.RData")


# Prepare data for analysis -----------------------------------------------

# Flag individuals that only select a single race
AQP.race <- AQP.Data$AQP.race
AQP.race$single_race <- AQP.race$race_selection_sum == 1
raceCols <- AQP.DataCols$raceCols

# Flag individuals that only select a single ancestry
AQP.anc <- AQP.Data$AQP.ancestry
AQP.anc$single_anc <- AQP.anc$anc_selection_sum == 1
ancCols <- AQP.DataCols$ancestryCols

# Ancestry percentages
AQP.ancPct <- AQP.Data$AQP.ancestryPct
pctCols <- AQP.DataCols$ancestryPctCols

# Replace -99 with NA in the columns for information seeking/genetic testing
geneTest <- AQP.Data$AQP.seeking %>% select(studyid, seeking_genetictest)
geneTest[geneTest == -99] <- NA

bornInfo <- AQP.Data$AQP.demo %>% select(studyid, born_US)

# Merge data
inputData <- AQP.race %>% inner_join(AQP.anc) %>%
  left_join(AQP.ancPct) %>%
  inner_join(geneTest) %>%
  inner_join(bornInfo)

# Analyses to compare individuals with genetic testing vs those without genetic testing ---------------------

# Subset the data based on whether individuals seek genetic testing
inputData_geneTest <- inputData %>% filter(seeking_genetictest == 1)
inputData_nogeneTest <- inputData %>% filter(seeking_genetictest == 0)
inputData.list <- list(geneTest = inputData_geneTest, nogeneTest = inputData_nogeneTest)

# Create output directory
if (!dir.exists("Output/Analysis_Comparison/GeneticTesting")) { dir.create("Output/Analysis_Comparison/GeneticTesting") }

# Analyses
anc_results <- compare_selectionNum(df_list = inputData.list, 
                                              selectionNum_var = "anc_selection_sum")
write.csv(anc_results, file = "Output/Analysis_Comparison/GeneticTesting/compare_ancNum.csv", row.names = F)

compare_race_others(df_list = inputData.list, 
                    change_names = c("GeneticTest", "No_GeneticTest"), dot_max = 120, x_label_size = 9, 
                    output_dir = "Output/Analysis_Comparison/GeneticTesting", 
                    output_prefix = "geneTest", y_limits = c(0, 120))

res <- nrow(inputData_geneTest %>% filter(Black == 1, SubSaharanAfrica == 1, born_US == 1))/nrow(inputData_geneTest %>% filter(Black == 1, SubSaharanAfrica == 1))
print(res)


# Analyses focusing on Black RC -------------------------------------------

# Create output directory
if (!dir.exists("Output/Analysis_Comparison/Comparison_misc")) { dir.create("Output/Analysis_Comparison/Comparison_misc") }

# Number of individuals who selected Black only as race
blackOnly <- inputData %>% filter(Black == 1, race_selection_sum == 1)
n.blackOnly = nrow(blackOnly)
print(n.blackOnly)

# Proportion of individuals who selected African American ancestry among individuals who selected Black only as race
blackOnly.AfAm  <- blackOnly %>% filter(AfricanAmerican == 1)
print(100*nrow(blackOnly.AfAm)/n.blackOnly)

# Proportion of individuals who selected Sub-Saharan Africa ancestry among individuals who selected Black only as race
blackOnly.SubAf <- blackOnly %>% filter(SubSaharanAfrica == 1)
print(100*nrow(blackOnly.SubAf)/n.blackOnly)

# Comparison US born within individuals who reported Black only as race and who selected or not SubSaharan Africa as ancestry
blackOnly.SubAf <- blackOnly.SubAf %>% filter(!is.na(born_US))
blackOnly.nonSubAf <- blackOnly %>% filter(SubSaharanAfrica != 1) %>% filter(!is.na(born_US))
SubAf_nonSubAf_USbirths <- many_fisher(df1 = blackOnly.SubAf, df2 = blackOnly.nonSubAf, 
                                 df1_name = "Black Sub-Saharan African (with possible other ancestries)", 
                                 df2_name = "Black non Sub-Saharan African", 
                                 compare_vars = "born_US")
write.csv(SubAf_nonSubAf_USbirths, file = "Output/Analysis_Comparison/Comparison_misc/BlackSubAfvsBlacknonSubAf_USborn.csv", row.names = F)


# Comparison of reported percentages for SubSarahan africa ancestry between US born (1) and non-US born (1) ndividuals
# who reported Black only as race and who selected SubSaharan Africa as ancestry
print(t.test(blackOnly.SubAf$SubSaharanAfrica_pct[blackOnly.SubAf$born_US==1], blackOnly.SubAf$SubSaharanAfrica_pct[blackOnly.SubAf$born_US==2]))


