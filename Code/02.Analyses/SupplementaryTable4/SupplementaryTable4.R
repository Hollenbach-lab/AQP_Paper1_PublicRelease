rm(list=ls())

# Library and functions ---------------------------------------------------

library(dplyr)
library(ggplot2)
library(pheatmap)

# Load functions ----------------------------------------------------------
source("Code/02.Analyses/generalHelpers.R")

# Load data ---------------------------------------------------------------

load("Data/AQP_DataForAnalysis.RData")

# Convert the family proportion to percentage -----------------------------

fam_pct <- AQP.Data$AQP.ancestryPctFam
for (col in AQP.DataCols$ancestryPctFamCols) {
  fam_pct[, col] <- fam_pct[, col]*100
}

# Combine the family percentage and self-identified percentage into one dataframe --------
anc_pct <- AQP.Data$AQP.ancestryPct
anc_fam_pct <- fam_pct %>% inner_join(anc_pct) %>% select(-studyid)


# Compute the correlation matrix and significance -------------------------

# Set up the matrices to store correlatin values and p values
ancFam_r <- matrix(NA, nrow = length(AQP.DataCols$ancestryPctFamCols), ncol = length(AQP.DataCols$ancestryPctCols))
rownames(ancFam_r) <- AQP.DataCols$ancestryPctFamCols
colnames(ancFam_r) <- AQP.DataCols$ancestryPctCols
ancFam_p <- ancFam_r

for (famPct_col in AQP.DataCols$ancestryPctFamCols) {
  for (ancPct_col in AQP.DataCols$ancestryPctCols) {
    # Select the family percentage column and the ancestry percentage column
    selected_df <- anc_fam_pct %>% select_(.dots = c(famPct_col, ancPct_col))
    
    # Remove paired 0s
    paired0_idx <- which(selected_df[ , 1] == 0 & selected_df[ , 2] == 0)
    clean_df <- selected_df[-paired0_idx, ]
    
    # Compute correlation and p-value
    cor_test <- cor.test(x = clean_df[ , 1], y = clean_df[ , 2], 
                         method = "pearson")
    
    # Store the values
    ancFam_r[famPct_col, ancPct_col] <- round(cor_test$estimate, digits = 2)
    ancFam_p[famPct_col, ancPct_col] <- ifelse(cor_test$p.value <= 0.001, "< 0.001", cor.test$p.value)
  }
}

# Save the diagonal values in a data frame, sorted by correlation -------------------------------------------
diag_r <- diag(ancFam_r)
diag_pval <- diag(ancFam_p)
ancPct_famPct_corrDiag <- data.frame("Ancestry" = AQP.DataCols$ancestryPctCols, 
                                     "Correlation" = diag_r, 
                                     "P-value" = diag_pval, 
                                     check.names = F)
ancPct_famPct_corrDiag$Ancestry <- gsub("_pct", "", ancPct_famPct_corrDiag$Ancestry)
ancPct_famPct_corrDiag <- ancPct_famPct_corrDiag %>% arrange(desc(Correlation)) %>% filter(Ancestry != "Unknown")
suppTable4 <- ancPct_famPct_corrDiag

# Import to PDF -----------------------------------------------------------

if (!dir.exists("Output/SupplementaryTable4")) { dir.create("Output/SupplementaryTable4") }

SweaveToPdf(filePath = "Code/02.Analyses/SupplementaryTable4/SupplementaryTable4.Rnw", 
            outputPath = file.path("Output/SupplementaryTable4", "SupplementaryTable4.pdf"))
