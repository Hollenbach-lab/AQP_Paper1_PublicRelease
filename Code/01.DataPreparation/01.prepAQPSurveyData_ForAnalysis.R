# Code to clean and prepare AQP survey data  for analysis

rm(list = ls())

# 01. Libraries loading ---------------------------------------------------

library("readr")
library("dplyr")


# 02. Load or read data ---------------------------------------------------
load("Data/PrepData.RData")
# Read demographics data from NMDP registry
demo.nmdp <- read.table("Input/studyid.hla_demographics.2015-12-23", sep = "\t", quote = " ", fill = T, stringsAsFactors = F)
# Read some meta data about the survey
AQP.emailPar <- suppressWarnings(read_csv("Input/rspns_lst_w_srvy_vs20151221.csv"))


# 03. Some cleaning -------------------------------------------------------
AQP <- AQP %>% arrange(studyid)
AQPMetaData <- AQPMetaData %>% arrange(studyid)
demo.nmdp <- demo.nmdp %>% select(studyid = V1, gender = V2) %>% 
  arrange(studyid)
AQP.emailPar <- AQP.emailPar %>% select(studyid, Message_Name) %>%
  arrange(studyid)
# Cleaning duplicated Ids in AQP.emailPar
AQP.emailPar <- AQP.emailPar[!duplicated(AQP.emailPar),]
AQP.emailPar <- AQP.emailPar %>% arrange(studyid, desc(Message_Name)) # when several emails sent to same individuals, keep the last batch
AQP.emailPar <- AQP.emailPar[!duplicated(AQP.emailPar$studyid),]
# Cleaning column names
colnames(AQP)[grepl("SomeUnknown", x = colnames(AQP))] # Cleaning mother and father Unknown ancestry columns
colnames(AQP) <- gsub("SomeUnknown", "Unknown", colnames(AQP)) # Cleaning mother and father Unknown ancestry columns


# 04. Prepare Race data ---------------------------------------------------
cat("\n# Preparing race data ... ###")
# Extract race columns
AQP.race <- AQP %>% select_(.dots = c("studyid", raceCols))
# Cleaning column names
colnames(AQP.race) <- sub("race_", "", colnames(AQP.race))
raceCols <- sub("race_", "", raceCols)
# Remove weird observations
idx99 <- apply(AQP.race[raceCols], 1, function(row) any(row[] == -99, na.rm = TRUE))
AQP.race <- AQP.race[!idx99,]
rm(idx99)
# Flag individuals that only select a single race
AQP.race$race_selection_sum <- rowSums(AQP.race[, raceCols], na.rm = T)


# 05. Prepare Ancestry data -----------------------------------------------
cat("\n# Preparing ancestry data ... ###")

# Extract ancestry columns
AQP.ancestry <- AQP %>% select_(.dots = c("studyid", ancestryCols))
# Remove weird observations
idx99 <- apply(AQP.ancestry[ancestryCols], 1, function(row) any(row[] == -99, na.rm = TRUE))
AQP.ancestry <- AQP.ancestry[!idx99,]
rm(idx99)
# Flag individuals that only select a single ancestry
AQP.ancestry$anc_selection_sum <- rowSums(AQP.ancestry[, ancestryCols], na.rm = T)


# 06. Prepare Ancestry Percentages data -----------------------------------
cat("\n# Preparing ancestry percentages data ... ###")

# Ancestry percentages columns
ancestryPctCols <- c(paste(ancestryCols, "pct", sep = "_"))
# Filter out individuals with sum_pct = 0
AQP.ancestryPct <- AQP %>% filter(sum_pct != 0)
# Extract ancestry percentages columns
AQP.ancestryPct <- AQP.ancestryPct %>% select_(.dots = c("studyid", ancestryPctCols))
# Remove weird observations
idx99 <- apply(AQP.ancestryPct[ancestryPctCols], 1, function(row) any(row[] == -99, na.rm = TRUE))
AQP.ancestryPct <- AQP.ancestryPct[!idx99,]
rm(idx99)


# 07. Prepare Normalized Ancestry Percentages data ------------------------
cat("\n# Preparing normalized ancestry percentages data ... ###")

AQP.ancestryPct.normalized <- AQP.ancestryPct %>% left_join(AQP %>% select(studyid, sum_pct), by = "studyid")
# Normalize percentages
for (j in ancestryPctCols) {
  AQP.ancestryPct.normalized[[j]] <- AQP.ancestryPct.normalized[[j]]/AQP.ancestryPct.normalized[["sum_pct"]]
}
AQP.ancestryPct.normalized <- AQP.ancestryPct.normalized %>% select(-sum_pct)
# Ancestry percentages columns
ancestryPctNormalizedCols <- c(paste(ancestryCols, "pct", "norm", sep = "_"))
colnames(AQP.ancestryPct.normalized) <- c("studyid", ancestryPctNormalizedCols)


# 08. Prepare Familial Ancestry data --------------------------------------
cat("\n# Preparing familial ancestry data ... ###")

# Parents or GrandParents ancestry columns
ancestryMotherCols <- paste0("m", ancestryCols)
ancestryFatherCols <- paste0("f", ancestryCols)
ancestryMgmCols <- paste0("mgm", ancestryCols)
ancestryMgfCols <- paste0("mgf", ancestryCols)
ancestryPgmCols <- paste0("pgm", ancestryCols)
ancestryPgfCols <- paste0("pgf", ancestryCols)

ancestryFamCols <- c(ancestryMotherCols, ancestryFatherCols, ancestryMgmCols, ancestryMgfCols, ancestryPgmCols, ancestryPgfCols)

# Extract familial ancestry columns
AQP.ancestryFam <- AQP %>% select_(.dots = c("studyid", ancestryFamCols))
table(colnames(AQP.ancestryFam)[!colnames(AQP.ancestryFam)=="studyid"] == ancestryFamCols)

# Remove weird observations
idx99 <- apply(AQP.ancestryFam[ancestryFamCols], 1, function(row) any(row[] == -99, na.rm = TRUE))
AQP.ancestryFam <- AQP.ancestryFam[!idx99,]
rm(idx99)


# 09. Prepare percentages-derived Familial Ancestry data ------------------
cat("\n# Preparing percentages-derived familial ancestry data ... ###")

# Grand-Parents data
cat("\n# Working on grand-parents ancestry data")
AQP.ancestryFam.gp <- AQP.ancestryFam[c("studyid",
                                        ancestryMgmCols, ancestryMgfCols,
                                        ancestryPgmCols, ancestryPgfCols)]

AQP.ancestryFam.gp$mgm.nAnc <- apply(AQP.ancestryFam %>% select_(.dots = ancestryMgmCols), 1, sum) # Compute n Ancestries selected for maternal grandmother
AQP.ancestryFam.gp$mgf.nAnc <- apply(AQP.ancestryFam %>% select_(.dots = ancestryMgfCols), 1, sum) # Compute n Ancestries selected for maternal grandfqther
AQP.ancestryFam.gp$pgm.nAnc <- apply(AQP.ancestryFam %>% select_(.dots = ancestryPgmCols), 1, sum) # Compute n Ancestries selected for paternal grandmother
AQP.ancestryFam.gp$pgf.nAnc <- apply(AQP.ancestryFam %>% select_(.dots = ancestryPgfCols), 1, sum) # Compute n Ancestries selected for paternal grandmother
AQP.ancestryFam.gp$gp.withoutAnc <- apply(AQP.ancestryFam.gp %>% select(mgm.nAnc, mgf.nAnc, pgm.nAnc, pgf.nAnc), 1, function(x) sum(is.na(x))) # Compute number of grandparents without any Ancestries selected
AQP.ancestryFam.gp <- AQP.ancestryFam.gp %>% filter(gp.withoutAnc == 0) %>% select(-gp.withoutAnc) # Keep individuals with ancestries info for 4 grand-parents

# Transform the binary variables in percentages, per ancestry, per grandparent
for (col in c(ancestryMgmCols, ancestryMgfCols, ancestryPgmCols, ancestryPgfCols)) {
  pctColname <- paste(col, "pct", sep = "_")
  gp.name <- substr(col, 1, 3)
  AQP.ancestryFam.gp[, pctColname] <- ifelse(is.na(AQP.ancestryFam.gp[, paste(gp.name, "nAnc", sep = ".")]) & grepl("Unknown", x = col), 0.25, # Condition not needed as we kept only individuals who selected ancestries for the 4 grandparents
                                                ifelse(is.na(AQP.ancestryFam.gp[, paste(gp.name, "nAnc", sep = ".")]), 0, # Condition not needed as we kept only individuals who selected ancestries for the 4 grandparents
                                                       AQP.ancestryFam.gp[, col]*0.25/AQP.ancestryFam.gp[, paste(gp.name, "nAnc", sep = ".")]))
}

# Check
AQP.ancestryFam.gp$sumAncPct <- apply(AQP.ancestryFam.gp %>% select_( .dots = paste(c(ancestryMgmCols, ancestryMgfCols, ancestryPgmCols, ancestryPgfCols), "pct", sep = "_")), 1, sum)
table(AQP.ancestryFam.gp$sumAncPct, useNA = "ifany") # Check

# Create ancestry data for the 4 grandparents combined
for (anc.gp.col in ancestryCols) {
  gpColName <- paste0("gp", anc.gp.col)
  AQP.ancestryFam.gp[, gpColName] <- apply(AQP.ancestryFam.gp %>% select_(.dots = paste0(c("mgm", "mgf", "pgm", "pgf"), anc.gp.col, "_pct")), 1, sum)
}
AQP.ancestryFam.gp <- AQP.ancestryFam.gp %>% select(studyid, contains("gp"))
colnames(AQP.ancestryFam.gp) <- gsub("gp", "", colnames(AQP.ancestryFam.gp))

# Parents data
cat("\n# Working on parents ancestry data")
AQP.ancestryFam.p <- AQP.ancestryFam[c("studyid", ancestryMotherCols, ancestryFatherCols)]

AQP.ancestryFam.p$m.nAnc <- apply(AQP.ancestryFam.p %>% select_(.dots = ancestryMotherCols), 1, sum) # Compute n Ancestries selected for mother
AQP.ancestryFam.p$f.nAnc <- apply(AQP.ancestryFam.p %>% select_(.dots = ancestryFatherCols), 1, sum) # Compute n Ancestries selected for father
AQP.ancestryFam.p$p.withoutAnc <- apply(AQP.ancestryFam.p %>% select(m.nAnc, f.nAnc), 1, function(x) sum(is.na(x))) # Compute number of parents without any Ancestries selected
AQP.ancestryFam.p <- AQP.ancestryFam.p %>% filter(p.withoutAnc <= 1) %>% select(-p.withoutAnc) # Keep individuals with ancestries info for at least one parent

# Transform the binary variables in percentages, per ancestry, per parent
for (col in c(ancestryMotherCols, ancestryFatherCols)) {
  pctColname <- paste(col, "pct", sep = "_")
  p.name <- substr(col, 1, 1)
  AQP.ancestryFam.p[, pctColname] <- ifelse(is.na(AQP.ancestryFam.p[, paste(p.name, "nAnc", sep = ".")]) & grepl("Unknown", x = col), 0.5,
                                           ifelse(is.na(AQP.ancestryFam.p[, paste(p.name, "nAnc", sep = ".")]), 0, 
                                                  AQP.ancestryFam.p[, col]*0.5/AQP.ancestryFam.p[, paste(p.name, "nAnc", sep = ".")]))
}
# Check
AQP.ancestryFam.p$sumAncPct <- apply(AQP.ancestryFam.p %>% select_( .dots = paste(c(ancestryMotherCols, ancestryFatherCols), "pct", sep = "_")), 1, sum)
table(AQP.ancestryFam.p$sumAncPct, useNA = "ifany") # Check

# Create ancestry data for the 2 parents combined
for (anc.p.col in ancestryCols) {
  pColName <- paste0("p", anc.p.col)
  AQP.ancestryFam.p[, pColName] <- apply(AQP.ancestryFam.p %>% select_(.dots = paste0(c("m", "f"), anc.p.col, "_pct")), 1, sum)
}
AQP.ancestryFam.p <- AQP.ancestryFam.p %>% select(studyid, starts_with("p"))
colnames(AQP.ancestryFam.p)[-1] <- substr(colnames(AQP.ancestryFam.p)[-1], 2, nchar(colnames(AQP.ancestryFam.p)[-1])) # removing the "p" from the column names

# Check that column names are in the same order between the grand-parents and parents ancestry dataframes
table(colnames(AQP.ancestryFam.p) == colnames(AQP.ancestryFam.gp))

# Familial Ancestry information (Bind grand-parents and parents data.frame)
table(AQP.ancestryFam.p$studyid %in% AQP.ancestryFam.gp$studyid) # Check that we don't have any overlapping individuals in parents and grandparents data.frame
table(AQP.ancestryFam.gp$studyid %in% AQP.ancestryFam.p$studyid) # Check that we don't have any overlapping individuals in parents and grandparents data.frame
AQP.ancestryPctFam <- rbind(AQP.ancestryFam.gp, AQP.ancestryFam.p)
rm(AQP.ancestryFam.gp, AQP.ancestryFam.p)
stopifnot(colnames(AQP.ancestryPctFam)[-1] == ancestryCols)
# AQP.ancestryFam$nAnc <- rowSums(AQP.ancestryFam %>% select(-studyid))
# table(AQP.ancestryFam$nAnc, useNA = "ifany") # Check
# unknownOnly <- AQP.ancestryFam %>% filter(nAnc == 1 & Unknown == 1)
# AQP.ancestryFam <- AQP.ancestryFam %>% filter(!studyid %in% unknownOnly$studyid) # Remove individuals with only "Unkown" as familial ancestry
# rm(unknownOnly)
# AQP.ancestryFam <- AQP.ancestryFam %>% select(-nAnc, -Unknown)
colnames(AQP.ancestryPctFam)[-1] <- paste(colnames(AQP.ancestryPctFam)[-1], "fam_pct", sep = "_")
AQP.ancestryPctFam <- AQP.ancestryPctFam %>% arrange(studyid)

ancestryPctFamCols <- colnames(AQP.ancestryPctFam)[colnames(AQP.ancestryPctFam)!="studyid"]


# 10. Prepare Reflected Race ----------------------------------------------
cat("\n# Preparing reflected race data ... ###")
# Extract reflected race columns
AQP.reflected <- AQP %>% select(studyid, reflected = other_classify)
AQP.reflected$reflected <- ifelse(AQP.reflected$reflected == 1, "American Indian or Alaska Native",
                                  ifelse(AQP.reflected$reflected == 2, "Asian",
                                         ifelse(AQP.reflected$reflected == 3, "Black or African American",
                                                ifelse(AQP.reflected$reflected == 4, "Hispanic or Latino",
                                                       ifelse(AQP.reflected$reflected == 5, "Native Hawaiian or other Pacific Islander",
                                                              ifelse(AQP.reflected$reflected == 6, "White",
                                                                     ifelse(AQP.reflected$reflected == 7, "Other", AQP.reflected$reflected)))))))

reflectedCols <- "reflected"

# Remove weird observations
idx99 <- apply(AQP.reflected[reflectedCols], 1, function(row) any(row[] == -99, na.rm = TRUE))
AQP.reflected <- AQP.reflected[!idx99,]
rm(idx99)


# 11. Prepare free text info ----------------------------------------------
cat("\n# Preparing free text data ... ###")
# Column names
freeTextCols <- c("race_Oth_specify", "other_classify_text")
# Extract free text columns
AQP.freeText <- AQP %>% select(studyid, freeTextCols)
# Check weird observations
table(grepl(pattern = -99, x = AQP.freeText$race_Oth_specify), useNA = "ifany") # No -99 in race_Oth_specify column
table(grepl(pattern = -99, x = AQP.freeText$other_classify_text), useNA = "ifany") # No -99 in other_classify_text column


# 12. Prepare demographics data -------------------------------------------
cat("\n# Preparing demographics data ... ###")

# Demographics column names
demoCols <- c("age", "highestdegree_i", "state_current", "gender",
              "born_US", "born_f", "born_m", "born_pgf", "born_pgm", "born_mgf", "born_mgm")
# Extract demographics columns
AQP.demo <- AQP %>% select_(.dots = c("studyid", demoCols[demoCols != "gender"])) %>%
  left_join(demo.nmdp)
# Replace weird observations by NA
AQP.demo[AQP.demo == -99] <- NA
# Replace numbers by correct values
ageLabels <- c("[18-24]", "[25-34]", "[35-44]", "[45-54]", "[55-64]", "[65+]")
degreeLabels <- c("DidNotFinishHighSchool", "HighSchoolDegreeOrEquivalent", "AssociatesDegree", "BachelorsDegree", "GraduateOrProfessionalDegree")
statesNames <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
                 "Florida", "Georgia", "Hawai", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
                 "Maine", "Maryland", "Massachussets", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
                 "Nevada", "NewHampshire", "NewJersey", "NewMexico", "NewYork", "NorthCarolina", "NorthDakota", "Ohio", "Oklahoma", "Oregon",
                 "Pennsylvania", "RhodeIsland", "SouthCarolina", "SouthDakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                 "Washington", "WestVirginia", "Wisconsin", "Wyoming", "PuertoRico", "DistrictOfColumbia", "OutsideOfUSA")

AQP.demo$age <- as.character(factor(AQP.demo$age, labels = ageLabels))
AQP.demo$highestdegree_i <- as.character(factor(AQP.demo$highestdegree_i, labels = degreeLabels))
AQP.demo$state_current <- as.character(factor(AQP.demo$state_current, labels = statesNames))
rm(demo.nmdp)


# 13. Prepare seeking data ------------------------------------------------
cat("\n# Preparing seeking data ... ###")
# Seeking column names
seekingCols <- c("seeking_askfamilymember", "seeking_familydocuments",
                 "seeking_website", "seeking_officaldocuments",
                 "seeking_library", "seeking_genetictest",
                 "seeking_others", "seeking_others_text", "seeking_none")
seekingCols <- seekingCols[seekingCols!="seeking_others_text"]
# Extract seeking columns
AQP.seeking <- AQP %>% select("studyid", seekingCols)
# Replace weird observations by NA
AQP.seeking[AQP.seeking == ""] <- NA


# 14. Prepare Meta data ---------------------------------------------------
cat("\n# Preparing meta data ... ###")
# Meta data column names
metaCols <- c("display_order_knowldege_FL12", "display_order_Race_Ancestry", "email_Type", "email_Batch")
# Extract meta data columns
AQP.meta <- AQPMetaData %>% select_(.dots = c("studyid", metaCols[!metaCols %in% c("email_Type", "email_Batch")])) %>%
  left_join(AQP.emailPar)
# Replace weird observations by NA
AQP.meta[AQP.meta == ""] <- NA
# Some replacement for ease to use
AQP.meta <- AQP.meta %>% mutate(email_Type = ifelse(grepl("Opportunity to Inform", x = AQP.meta$Message_Name), "Type_1",
                                                    ifelse(grepl("Invitation to Participate", x = AQP.meta$Message_Name), "Type_2", NA)),
                                email_Batch = ifelse(grepl("Invitation to Participate RESEND Additional", x = AQP.meta$Message_Name), "Batch_3",
                                                     ifelse(grepl("Opportunity to Inform- RESEND Additional", x = AQP.meta$Message_Name), "Batch_3",
                                                            ifelse(grepl("Invitation to Participate - RESEND", x = AQP.meta$Message_Name), "Batch_2",
                                                                   ifelse(grepl("Opportunity to Inform - RESEND", x = AQP.meta$Message_Name), "Batch_2",
                                                                          ifelse(grepl("Invitation to Participate", x = AQP.meta$Message_Name), "Batch_1",
                                                                                 ifelse(grepl("Opportunity to Inform", x = AQP.meta$Message_Name), "Batch_1", NA)))))))

AQP.meta$display_order_knowldege_FL12 <- ifelse(AQP.meta$display_order_knowldege_FL12 == "Knowledge check|FL_12", "K1",
                                                ifelse(AQP.meta$display_order_knowldege_FL12 == "FL_12|Knowledge check", "K2", NA)) # Convention for Knowledge Check order: first = K1, second = K2
AQP.meta$display_order_Race_Ancestry <- ifelse(AQP.meta$display_order_Race_Ancestry == "Race and Ethnicity|Ancestry", "R1",
                                               ifelse(AQP.meta$display_order_Race_Ancestry == "Ancestry|Race and Ethnicity", "R2", NA)) # Convention for Race and Ancestry questions order: Race first = R1, Race second (Ancestry first) = R2
AQP.meta <- AQP.meta %>% select(-Message_Name)
rm(AQP.emailPar)


# 15. Save data -----------------------------------------------------------
cat("\n# Saving data ... ###")
AQP.Data <- list(AQP.race = AQP.race, AQP.ancestry = AQP.ancestry, AQP.ancestryPct = AQP.ancestryPct, AQP.ancestryPct.normalized = AQP.ancestryPct.normalized,
                 AQP.ancestryFam = AQP.ancestryFam, AQP.ancestryPctFam = AQP.ancestryPctFam, AQP.reflected = AQP.reflected, AQP.freeText = AQP.freeText,
                 AQP.seeking = AQP.seeking, AQP.demo = AQP.demo, AQP.meta = AQP.meta)
AQP.DataCols <- list(raceCols = raceCols, ancestryCols = ancestryCols, ancestryPctCols = ancestryPctCols, ancestryPctNormalizedCols = ancestryPctNormalizedCols,
                     ancestryFamCols = ancestryFamCols, ancestryPctFamCols = ancestryPctFamCols, reflectedCols = reflectedCols, freeTextCols = freeTextCols,
                     seekingCols = seekingCols, demoCols = demoCols, metaCols = metaCols)
stopifnot(length(AQP.Data) == length(AQP.DataCols))

save(list = c("AQP.Data", "AQP.DataCols"), 
    file = "Data/AQP_DataForAnalysis.RData")



