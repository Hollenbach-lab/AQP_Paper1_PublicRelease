
rm(list = ls())

# 01. Libraries loading ---------------------------------------------------

library(readr)
library(dplyr)
library(adegenet)

# 02. Parameters setting --------------------------------------------------

# Analysis
analysisName = "Analysis_GeneticDistances_3"

# Response Categories and Modifiers
responseCateg = "Races"
modifier = "Ancestries"

# Number of permutation to use
nPerm = 999 # Type 0 if no permutations needed

# Number of selected races
nRacesSelectedMin = 1 # Min
nRacesSelectedMax = 1 # Max
# Number of selected ancestries
nAncestrySelectedMin = NA # Min
nAncestrySelectedMax = NA # Max

# Race and/or Ancestry to focus the analysis on
raceFocus = "Black" # NA, "Asian" ...
ancestryFocus = NA # NA, "EastAsia" ...

# Seeking Genetic Testing to focus on
seekingGenTestFocus <- "Yes"

# Minimum size of the populations 
nIndPerPopFilter = 50 # --> To type 0 if no filtering needed


# 03. Creating output folders and paths -----------------------------------

# Creation output folder
outputFolder <- analysisName

outputPath <- file.path("Output", outputFolder); dir.create(outputPath)
outputPathData <- file.path(outputPath,"Data"); dir.create(outputPathData)
outputPathPlots <- file.path(outputPath,"Plots"); dir.create(outputPathPlots)
outputPathPopFrequencies <- file.path(outputPath,"PopFrequencies"); dir.create(outputPathPopFrequencies)
outputPathDistMatrices <- file.path(outputPath,"DistancesMatrices"); dir.create(outputPathDistMatrices)
outputPathPermMatrices <- file.path(outputPath,"PermMatrices"); dir.create(outputPathPermMatrices)

# 04. Source codes --------------------------------------------------------
source("Code/02.Analyses/AnalysisGeneticDistances_3/Edwards_Helpers.R")

# 05. Save parameters in parameters file ----------------------------------

paramFile <- file.path(outputPath, "setParameters.txt")

cat("Analysis launched on", format(Sys.time()), "\n\n", file = paramFile)  
cat("Parameters:\n\n", file = paramFile, append = TRUE)  
# Response Categories and Modifiers
cat("responseCateg :", responseCateg, "\n", file = paramFile, append = TRUE)
cat("modifier :", modifier, "\n\n", file = paramFile, append = TRUE)
# Methods
cat("distanceMethods : Edwards\n\n", file = paramFile, append = TRUE)
cat("nPerm :", nPerm, "\n\n", file = paramFile, append = TRUE)
# Number of selected races
cat("nRacesSelectedMin :", nRacesSelectedMin, "\n", file = paramFile, append = TRUE)
cat("nRacesSelectedMax :", nRacesSelectedMax, "\n", file = paramFile, append = TRUE)
# Number of selected ancestries
cat("nAncestrySelectedMin :", nAncestrySelectedMin, "\n", file = paramFile, append = TRUE)
cat("nAncestrySelectedMax :", nAncestrySelectedMax, "\n\n", file = paramFile, append = TRUE)
# Race and/or Ancestry to focus the analysis on
cat("raceFocus :", raceFocus, "\n", file = paramFile, append = TRUE)
cat("ancestryFocus :", ancestryFocus, "\n\n", file = paramFile, append = TRUE)
# Seeking Genetic Testing to focus on
cat("seekingGenTestFocus :", seekingGenTestFocus, "\n\n", file = paramFile, append = TRUE)

# Minimum size of the populations 
cat("nIndPerPopFilter :", nIndPerPopFilter, "\n\n", file = paramFile, append = TRUE)


# 06. Load data -----------------------------------------------------------
cat("\n# Loading input data ... ###")

load("Data/genindAllData.RData")
load("Data/PrepData.RData")

# 06. Create population information ---------------------------------------

cat("\n# Creating population information data ... ###")
# Remove weird observations
AQP2 <- data.frame(AQPData, AQPMetaData['studyid'])
idx99 <- apply(AQP2[c(raceCols, ancestryCols)], 1, function(row) any(row[] == -99, na.rm = TRUE))
AQP2 <- AQP2[!idx99,]

# Race Data
cat("\n### Race data ###\n")
PopRace <- AQP2[c("studyid", raceCols)] 
colnames(PopRace) <- sub("race_", "", colnames(PopRace))
PopRace <- PopRace %>% filter(studyid %in% indNames(genindAllData))
PopRace <- PopRace %>% arrange(studyid)
PopRace$nRaces <- rowSums(PopRace %>% select(-studyid))
PopRace <- PopRace %>% filter(nRaces >= nRacesSelectedMin, nRaces <= nRacesSelectedMax)
PopRace <- PopRace %>% filter(Black == 1)
PopRace$raceCombi <- apply(sapply(colnames(PopRace %>% select(-studyid, -nRaces)), function(colName) {
  res <- rep("*", nrow(PopRace))
  boolIndex <- as.logical(PopRace[[colName]]) & !is.na(PopRace[[colName]])
  res[boolIndex] <- colName
  res
}), 1, paste, collapse = "_")
PopRace$raceCombi <- gsub("[*]_", "", PopRace$raceCombi)
PopRace$raceCombi <- gsub("_[*]", "", PopRace$raceCombi)
PopRace <- PopRace %>% select(studyid, Desc_Races = raceCombi)

# Ancestry Data
cat("\n### Ancestry data ###\n")
PopAnc <- AQP2[,c("studyid", ancestryCols)]
PopAnc <- PopAnc %>% filter(studyid %in% indNames(genindAllData))
PopAnc <- PopAnc %>% arrange(studyid)
PopAnc <- PopAnc %>% select(studyid, SubSaharanAfrica)
PopAnc <- PopAnc %>% mutate(Desc_Ancestries = ifelse(PopAnc$SubSaharanAfrica == 1, "SubSaharanAfrica.Yes", "SubSaharanAfrica.No"))
PopAnc <- PopAnc %>% select(studyid, Desc_Ancestries)

# Seeking Genetic Testing data
PopSeekingGenTest <- AQP2[,c("studyid", "seeking_genetictest")]
PopSeekingGenTest <- PopSeekingGenTest %>% filter(seeking_genetictest == 1) %>% select(studyid)

PopData <- PopRace %>% inner_join(PopAnc, by = "studyid") %>% inner_join(PopSeekingGenTest, by = "studyid")

if (!anyNA(responseCateg)) {
  PopData <- PopData %>% mutate(Desc_respCateg = pasteDescColsRespCateg(PopData))
}
if (!anyNA(modifier)) {
  PopData <- PopData %>% mutate(Desc_modifier = pasteDescColsModifier(PopData))
}

PopData <- PopData %>% mutate(Pop = pasteRespcategModifier(PopData))
if (!(NA %in% responseCateg) & !(NA %in% modifier)) {
  colToSelect.popData <- c("studyid", "Desc_respCateg", "Desc_modifier", "Pop")
} else if (!(NA %in% responseCateg) & (NA %in% modifier)) {
  colToSelect.popData <- c("studyid", "Desc_respCateg", "Pop")
} else if ((NA %in% responseCateg) & !(NA %in% modifier)) {
  colToSelect.popData <- c("studyid", "Desc_modifier", "Pop")
}
PopData <- PopData %>% select_(.dots = colToSelect.popData)


# 07. Create Genind object ------------------------------------------------

cat("\n# Creating Genind object ... ###")

genind <- genindAllData[indNames(genindAllData) %in% PopData$studyid]
stopifnot(indNames(genind) == PopData$studyid )
pop(genind) <- PopData$Pop

cat("\tBefore filtering populations with less than", nIndPerPopFilter, "there are:\n")
cat("\t\t", nInd(genind), "individuals\n")
cat("\t\t", nPop(genind), "populations\n\n")
popFreq <- data.frame(table(pop(genind)), stringsAsFactors = F)
colnames(popFreq) <- c("Pop", "nInd")
popFreq <- popFreq %>% arrange(nInd)
write.csv(popFreq, file = file.path(outputPathPopFrequencies, paste0("PopFreqPreFiltering.csv")))

#Removing individuals from populations with less than 'nIndperpopFilter' individuals (nIndperpopFilter : variable defined at the beginning)
cat("\tKeeping only individuals from population with more than", nIndPerPopFilter, "individuals\n\n")

genind <- selPopSize(genind, nMin = nIndPerPopFilter)

cat("\tAfter filtering populations with less than" ,nIndPerPopFilter, "there are:\n")
cat("\t\t", nInd(genind), "individuals\n")
cat("\t\t", nPop(genind), "populations\n\n")

if (nPop(genind) > 0) {
  popFreq <- data.frame(table(pop(genind)), stringsAsFactors = F)
  colnames(popFreq) <- c("Pop", "nInd")
  popFreq <- popFreq %>% arrange(desc(nInd))
  write.csv(popFreq, file = file.path(outputPathPopFrequencies, paste0("PopFreqPostFiltering.csv")))
}

save(genind, file = file.path(outputPathData, "genindObject.RData"))
cat("\n\t=== genind objects created and saved ===\n\n")


# 08. Creation of Genpop object -------------------------------------------

cat("\n# Creating Genpop object ... ###")
genpop <- genind2genpop(genind)
cat("\n\t=== genpop object created ===\n\n")


# 09. Distances computation -----------------------------------------------

cat("\n# Computing distances ... ###")

if (nPop(genpop) == 1) {
  cat("\t\t Only one population, nothing to compute...\n\n")
} else {
  # Distances matrix computation using dist.genpop function from adegenet package  
  distgenpop <- dist.genpop(genpop, method = 2, diag = T, upper = T)
  matrixName = paste0("DistancesMatrix.txt")
  MASS::write.matrix(distgenpop, file = file.path(outputPathDistMatrices, matrixName))
  
  cat("\t\tGenetic distances computed and matrix created and saved\n\n")
}


# 10. Plots creation ------------------------------------------------------

# Plots 
cat("\t## Creating plots ...\n")

if (nPop(genpop) <= 2) {
  cat("\t\t 2 populations or less, can not create nj plot.\n\n")
} else {
  
  pdfName = paste0("GeneticDistancesBetweenPopulations.pdf")
  heightPdf = 30
  widthPdf = 30
  
  mainTitleNJ = "Genetic Distances between populations\nNeighbor-Joining Tree Estimation"
  mainTitleAB = "Genetic Distances between populations\nBootstrap"
  mainTitleHM = "Genetic Distances between populations\nHeatmap"
  
  pdf(file = file.path(outputPathPlots, pdfName), height = heightPdf, width = widthPdf)
  
  # NEIGHBOR-JOINING plotting from ade package
  cat("\n# NJ plot \n")
  njPlot <- ape::nj(distgenpop)
  plot(njPlot, main = mainTitleNJ)
  
  # ABOOT from poppr package
  cat("\n# ABOOT plot \n")
  abootPlot <- poppr::aboot(genpop, dist = "edwards.dist", root = FALSE)
  # plot(abootPlot, main = mainTitleAB)
  
  # HEATMAP plotting from pheatmap package
  cat("\n# HeatMap plot \n")
  pheatmap::pheatmap(distgenpop, labels_row = rownames(genpop@tab), labels_col = rownames(genpop@tab), main = mainTitleHM)
  
  dev.off()
  
  cat("\n\t=== Plot created and saved ===\n\n")
}


# 11. Permutations --------------------------------------------------------

cat("\t## Performing permutations ...\n")

if (nPerm == 0) { cat("No permutations wanted\n") 
  
} else {
  
  # Creating couples of populations to permute
  popToPermute <- unique(as.character(pop(genind)))
  popToPermute <- popToPermute[order(popToPermute)]
  nPopToPermute <- length(popToPermute)
  
  popCouples <- expand.grid(popToPermute, popToPermute, KEEP.OUT.ATTRS = FALSE)
  popCouples <- data.frame(t(apply(popCouples, 1, sort)), stringsAsFactors = F)
  names(popCouples) <- c("Pop1", "Pop2")
  popCouples <- popCouples %>% filter( Pop1 != Pop2)
  popCouples <- popCouples[!duplicated(popCouples),]  
  popCouples <- popCouples %>% arrange(Pop1, Pop2)
  
  perm.pop <- function(genind, perm) {
    coupleGenindResampled <- genind
    set.seed(perm)
    #Shuffle population data
    coupleGenindResampled@pop <- sample(coupleGenindResampled@pop, size = length(coupleGenind@pop))
    #Create the new genpop object
    genpopResampled <- genind2genpop(coupleGenindResampled, quiet = T)
    #Compute the new distance matrice
    res <- dist.genpop(genpopResampled, method = 2, diag = F, upper = F)
  }
  
  pVal.List <- list()
  for (coupleIndex in 1:nrow(popCouples)) {
    couple <- popCouples[coupleIndex,]
    coupleGenind <- genind[pop = c(couple$Pop1, couple$Pop2)]
    coupleGenpop <- genind2genpop(coupleGenind, quiet = T )
    cat("\n\tAnalysis of", couple$Pop1, "and", couple$Pop2, "populations (", coupleIndex, "/", nrow(popCouples), ")\n")
    #Observed
    obsDistances <- dist.genpop(coupleGenpop, method = 2, diag = F, upper = F)
    
    permDistances <- pbmcapply::pbmclapply(X = 1:nPerm, genind = coupleGenind, FUN = perm.pop, mc.cores = 3)
    permDistances <- unlist(permDistances)
    
    #p-value computation
    cat("\n\t\tStatistics computation\n")
    shortPermDistances <- permDistances[permDistances > obsDistances]
    num <- 1 + length(shortPermDistances)
    den <- 1 + nPerm
    stat <- num / den
    
    pVal.List[[coupleIndex]] <- data.frame(Pop1 = couple$Pop1, Pop2 = couple$Pop2, pValue = stat, stringsAsFactors = F)
    
  }
  
  perm.Results <- do.call(rbind, pVal.List)
  
  perm.Results <- tidyr::spread(perm.Results, Pop2, pValue)
  write_csv(perm.Results, file.path(outputPathPermMatrices, "perm.Results.csv"))
}
