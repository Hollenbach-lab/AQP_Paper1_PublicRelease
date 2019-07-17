rm(list = ls())

# Libraries loading -------------------------------------------------------

library("readr")
library("dplyr")
library("ggplot2")

# Loading functions -------------------------------------------------------

source("Code/02.Analyses/Figure2/Figure2_Helpers.R")

# Create output directory -------------------------------------------------

if (!dir.exists("Output/Figure2")) { dir.create("Output/Figure2") }

# Load and read data ------------------------------------------------------

load("Data/AQP_DataForAnalysis.RData")
haplos <- read.csv("Input/out.bayes.studyid.2017-03-03", header = F, stringsAsFactors = F)
colnames(haplos) <- c("index", "studyid", "Haplotype_1", "HapPop_1", "Haplotype_2", "HapPop_2")

# Create input data -------------------------------------------------------

table(haplos$studyid %in% AQP.Data$AQP.race$studyid, useNA = "ifany")
table(AQP.Data$AQP.race$studyid %in% haplos$studyid, useNA = "ifany")

# Haplotypes data
haplos.qc <- haplos %>% select(studyid, HapPop_1, HapPop_2)

# Survey data
surveyData <- list()

### Race data
raceData <- AQP.Data$AQP.race 
raceData <- raceData %>% select(studyid, AQP.DataCols$raceCols) %>%
  arrange(studyid)
surveyData[["PR.bin"]] <- raceData
rm(raceData)

### Ancestry data
ancestryData <- AQP.Data$AQP.ancestry
unknownOnly <- ancestryData %>% filter(anc_selection_sum == 1 & Unknown == 1)
ancestryData <- ancestryData %>% filter(!studyid %in% unknownOnly$studyid) # Removing individuals who selected only 'Unknown' as ancestry
ancestryData <- ancestryData %>% select(-anc_selection_sum) %>%
  arrange(studyid)
surveyData[["PA.bin"]] <- ancestryData
rm(ancestryData)

### Race and Ancestry data
RaceAncestryData <- surveyData[["PR.bin"]] %>% inner_join(surveyData[["PA.bin"]], by = "studyid") %>%
  arrange(studyid)
surveyData[["PR.bin/PA.bin"]] <- RaceAncestryData
rm(RaceAncestryData)

### Reflected data
reflectedData <- AQP.Data$AQP.reflected
reflectedData$reflected.AIAN <- (reflectedData$reflected == "American Indian or Alaska Native")*1
reflectedData$reflected.ASN <- (reflectedData$reflected == "Asian")*1
reflectedData$reflected.BAA <- (reflectedData$reflected == "Black or African American")*1
reflectedData$reflected.HL <- (reflectedData$reflected == "Hispanic or Latino")*1
reflectedData$reflected.NHPI <- (reflectedData$reflected == "Native Hawaiian or other Pacific Islander")*1
reflectedData$reflected.WHT <- (reflectedData$reflected == "White")*1
reflectedData$reflected.OTHER <- (reflectedData$reflected == "Other")*1
reflectedData <- reflectedData %>% select(-reflected) %>%
  arrange(studyid)
surveyData[["Reflected"]] <- reflectedData
rm(reflectedData)

### Ancestry Percentages data (Salience)
ancPctData <- AQP.Data$AQP.ancestryPct
surveyData[["PAS.qt"]] <- ancPctData
rm(ancPctData)

### Race and Ancestry Percentages data
RaceAncestryPctData <- surveyData[["PR.bin"]] %>% inner_join(surveyData[["PAS.qt"]], by = "studyid") %>%
  arrange(studyid)
surveyData[["PR.bin/PAS.qt"]] <- RaceAncestryPctData
rm(RaceAncestryPctData)

### Ancestry Percentages (salience) cutoff
for (cutoff in seq(0, 0.5, by = 0.05)) {
  pop.pct <- AQP.Data$AQP.ancestryPct.normalized
  pop.cut <- surveyData[["PA.bin"]] %>% filter(studyid %in% pop.pct$studyid)
  stopifnot(pop.cut$studyid == pop.pct$studyid)
  for (col in colnames(pop.cut)[colnames(pop.cut) != "studyid"]) {
    pop.cut[, col] <- ifelse((pop.cut[, col] == 1) & (pop.pct[, paste(col, "pct", "norm", sep = "_")] <= cutoff), 0, pop.cut[, col])
  }
  
  # AncPct: AS.bin
  pop.cut.name <- paste("PAS.bin", cutoff*100, sep = ".")
  surveyData[[pop.cut.name]] <- pop.cut
  
  # AncPct + Race: PR.bin/AS.bin
  pop.cut.race <- pop.cut %>% inner_join(surveyData[["PR.bin"]], by = "studyid")
  pop.cut.race.name <- paste("PR.bin/PAS.bin", cutoff*100, sep = ".")
  surveyData[[pop.cut.race.name]] <- pop.cut.race
  
  # rm(pop.cut.name, pop.cut.race.name, pop.cut, pop.cut.race, pop.cut.race.reflected, pop.cut.race.reflected.name, cutoff)
  rm(pop.cut.race.name, pop.cut, pop.cut.race, cutoff)
  
}

### Family pct data: FA.qt
famAncPctData <- AQP.Data$AQP.ancestryPctFam
famAncPctData$sum <- rowSums(famAncPctData %>% select(-studyid))
table(famAncPctData$sum, useNA = "ifany") # Check
unknownOnly <- famAncPctData %>% filter(sum == 1 & Unknown_fam_pct == 1)
famAncPctData <- famAncPctData %>% filter(!studyid %in% unknownOnly$studyid) # Remove individuals with only "Unknown" as familial ancestry
rm(unknownOnly)
famAncPctData <- famAncPctData %>% select(-sum) %>%
  arrange(studyid)
famAncPctData[,2:ncol(famAncPctData)] <- famAncPctData[,2:ncol(famAncPctData)]*100
surveyData[["FA.qt"]] <- famAncPctData
rm(famAncPctData)

### Race and Familial Ancestry Pct data: PR.bin/FFA.qt
RacefamAncPctData <- surveyData[["PR.bin"]] %>% inner_join(surveyData[["FA.qt"]], by = "studyid") %>%
  arrange(studyid)
surveyData[["PR.bin/FA.qt"]] <- RacefamAncPctData
rm(RacefamAncPctData)

### Familial Ancestry cutoff
for (cutoff in seq(0, 0.5, by = 0.05)) {
  pop.cut <- surveyData[["FA.qt"]]
  for (col in colnames(pop.cut)[colnames(pop.cut) != "studyid"]) {
    pop.cut[, col] <- ifelse(pop.cut[, col] <= cutoff*100, 0, 1)
  }
  # FFA.bin
  pop.cut <- pop.cut #%>% select(-Unknown_fam_pct)
  pop.cut.name <- paste("FA.bin", cutoff*100, sep = ".")
  surveyData[[pop.cut.name]] <- pop.cut
  
  # AncFam + Race
  pop.cut.race <- pop.cut %>% inner_join(surveyData[["PR.bin"]], by = "studyid")
  pop.cut.race.name <- paste("PR.bin/FA.bin", cutoff*100, sep = ".")
  surveyData[[pop.cut.race.name]] <- pop.cut.race
  
  # rm(pop.cut.name, pop.cut.race.name, pop.cut, pop.cut.race, cutoff)
  rm(pop.cut.race.name, pop.cut, pop.cut.race, cutoff)
  
}

# Find IDs in common between all datasets (survey and haplotypes data)
commonIds <- Reduce(intersect, lapply(surveyData, "[", "studyid"))
commonIds <- commonIds %>% filter(studyid %in% haplos.qc$studyid)

# Keep only common IDs
# Haplotypes data
haplos.qc <- haplos.qc %>% filter(studyid %in% commonIds$studyid)
# Survey data
for (element in names(surveyData)) {
  surveyData[[element]] <- surveyData[[element]] %>% filter(studyid %in% commonIds$studyid)
  stopifnot(surveyData[[element]]$studyid == haplos.qc$studyid)
}
rm(commonIds)

# Add demographics
covar <- AQP.Data$AQP.demo %>% full_join(AQP.Data$AQP.meta) %>% select(studyid, display_order_Race_Ancestry, age, gender, email_Type, highestdegree_i)
covar <- covar[complete.cases(covar), ]
covar[,-1] <- lapply(covar[,-1], as.factor) 
for (element in names(surveyData)) {
  surveyData[[element]] <- surveyData[[element]] %>% inner_join(covar)
}
haplos.qc <- haplos.qc %>% filter(studyid %in% covar$studyid)

cat("\tThere are", nrow(surveyData[[1]]), "individuals included in this analysis.\n")


# Cluster -----------------------------------------------------------------

grpGene.List <- list()

haps <- paste(haplos.qc$HapPop_1, haplos.qc$HapPop_2, sep = "+")
unique.haps <- unique(haps)

for (surveyData.element in names(surveyData)) {
  cat("# Treating", surveyData.element, "...\n")
  data <- surveyData[[surveyData.element]]
  cluster.data <- data.frame(haps, data %>% select(-studyid), stringsAsFactors = F)
  cluster.data[,-1] <- lapply(cluster.data[,-1], as.numeric)
  
  # Calculate each variable average for each haplotype level
  cluster.data.avg <- NULL # initializing the matrice
  for (i in 1:length(unique.haps)) {
    cluster.data.avg <- rbind(cluster.data.avg,
                              colMeans(cluster.data[cluster.data$haps == unique.haps[i], 2:ncol(data)]))
  }
  rownames(cluster.data.avg) <- unique.haps
  
  # K means method
  set.seed(123456789) ## to fix the random starting clusters
  grpGene <- kmeans(cluster.data.avg[, 1:ncol(data)-1], centers = 18, nstart = 10)
  grpGene.List[[surveyData.element]] <- grpGene
  
  rm(data, cluster.data.avg, grpGene)
}


# Multinomial Logistic Regression -----------------------------------------

output.List <- pbmcapply::pbmclapply(names(surveyData), FUN = mln.function, mc.cores = 2)
output <- do.call(rbind, output.List)
write_csv(output, "Output/Figure2/modelsMetrics.csv")

# Plot --------------------------------------------------------------------

metrics <- read_csv("Output/Figure2/modelsMetrics.csv")

modelsToPlot <- c("PR.bin", "Reflected", "PA.bin", "PAS.qt", "FA.qt", "FA.bin.0",
                  "PR.bin/PA.bin", "PR.bin/PAS.qt", "PR.bin/FA.qt", "PR.bin/FA.bin.0")

inputData.plot5 <- metrics %>% filter(Analysis %in% modelsToPlot)
inputData.plot5 <- inputData.plot5[match(modelsToPlot, inputData.plot5$Analysis),]

inputData.plot5$shape <- c(rep("Single measure", 6), rep("RC + Ancestry measure", 4))
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", 
               "#0072B2", "#D55E00", "#CC79A7",
               "#009E73","#0072B2", "#D55E00", "#CC79A7")
inputData.plot5$color <- c("a","b", "c", "d", "e", "f", "c", "d", "e", "f")

plot <- ggplot(inputData.plot5, aes(x = Misclassication.testing,
                                     y = r2MF, size = 1/aic, color = color)) +
  
  geom_point(aes(fill = color, shape = shape)) +
  xlab("Test Misclassification Error") +
  ylab(expression(paste("McFadden's R"^"2"))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size = 2),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(color = "Single Measure", #fill = "Models",
       shape = "Combined measures", size = "1/AIC") +
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), legend.box = "vertical",
        legend.key.size = unit(0.4,"cm"))+
  scale_y_continuous(limits = c(0.15,0.32)) +
  scale_x_continuous(limits = c(0.14,0.36)) +
  scale_shape_manual(values = c(19,1)) +
  scale_color_manual(labels = c("RC", "RR", "PA", "PAS", "FFA", "FA"),
                    values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_size_continuous(range = c(5,15)) +
  guides(fill = FALSE,
         color = guide_legend(order = 3),
         size = guide_legend(order = 1),
         shape = guide_legend(order = 2))


pdf(file = "Output/Figure2/Figure2.pdf")
plot
dev.off()

# anova -------------------------------------------------------------------

# PA.bin
mln.data.1 <- data.frame(haps, surveyData$PA.bin %>% select(-studyid), stringsAsFactors = F)

# Replace haplotypes 171 levels by 18 clusters
cluster <- as.numeric(unlist(strsplit(as.character(grpGene.List$PA.bin$cluster),' ')))
gene.cluster <- data.frame(unique.haps = unique.haps, cluster, stringsAsFactors = F)

mln.data.1$code <- sapply(mln.data.1$haps, gene.cluster.input = gene.cluster, FUN = find.Cluster)
mln.data.1$code <- as.factor(mln.data.1$code)
mln.data.1$code2 <- relevel(mln.data.1$code, ref = 1)
# Model building on Training dataset
formula.mln.1 <- as.formula(paste("code2", "~",
                                  paste(colnames(surveyData$PA.bin)[colnames(surveyData$PA.bin) != "studyid"], collapse = "+"),
                                sep = ""))
mln.data.1.training <- mln.data.1[1:floor(nrow(mln.data.1)*0.9),]

mlogistic.1 <- nnet::multinom(formula.mln.1, maxit = 1000, data = mln.data.1.training)


# PR.bin/PA.bin
mln.data.2 <- data.frame(haps, surveyData$`PR.bin/PA.bin` %>% select(-studyid), stringsAsFactors = F)

# Replace haplotypes 171 levels by 18 clusters
cluster <- as.numeric(unlist(strsplit(as.character(grpGene.List$`PR.bin/PA.bin`$cluster),' ')))
gene.cluster <- data.frame(unique.haps = unique.haps, cluster, stringsAsFactors = F)

mln.data.2$code <- sapply(mln.data.2$haps, gene.cluster.input = gene.cluster, FUN = find.Cluster)
mln.data.2$code <- as.factor(mln.data.2$code)
mln.data.2$code2 <- relevel(mln.data.2$code, ref = 1)
# Model building on Training dataset
formula.mln.2 <- as.formula(paste("code2", "~",
                                  paste(colnames(surveyData$`PR.bin/PA.bin`)[colnames(surveyData$`PR.bin/PA.bin`) != "studyid"], collapse = "+"),
                                  sep = ""))
mln.data.2.training <- mln.data.2[1:floor(nrow(mln.data.2)*0.9),]

mlogistic.2 <- nnet::multinom(formula.mln.2, maxit = 1000, data = mln.data.2.training)

anova.test_1vs2 <- anova(mlogistic.1, mlogistic.2)
print(anova.test_1vs2)
