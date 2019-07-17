# To plot all the Sankey diagrams

rm(list = ls())

# Library and functions -------------------------------------------------------------------------------------
library(networkD3) # Sankey diagram
library(htmlwidgets) # for saving html files
library(dplyr) # data processing


# Source helpers ----------------------------------------------------------
source("Code/02.Analyses/Figure1/Figure1_Helpers.R")

# Meta parameters -------------------------------------------------------------------------------------------
proj_dir <- getwd()
launch_time <- Sys.time() # launch time in Pacific Time
OUTPUT_DIR <- "Output/Figure1" # the directory to save the output
PROP_CUTOFF <- 0 # the proportion cutoff to show up in the Sankey diagrams

# Specifications for Sankey diagram plotting
WIDTH_PIX <- 750 # the width of the diagrams in pixels
HEIGHT_PIX <- 500 # the height of the diagrams in pixels
FONT_SIZE <- 12 # the font size for the diagrams
NODE_WIDTH <- 30 # the node width in pixels
ITER <- 32 # the number of iterations to find the optimal positioning of the nodes

# Make sankey plotting functions that inherit the above specifications --------------------------------------
my_AtoBtoA <- function(...) {
  sankey_AtoBtoA(..., fontSize = FONT_SIZE, nodeWidth = NODE_WIDTH, iterations = ITER, 
                 height = HEIGHT_PIX, width = WIDTH_PIX)
}

my_2to1 <- function(...) {
  sankey_2to1(..., fontSize = FONT_SIZE, nodeWidth = NODE_WIDTH, iterations = ITER, 
              height = HEIGHT_PIX, width = WIDTH_PIX)
}

my_AtoB <- function(...) {
  sankey_AtoB(..., fontSize = FONT_SIZE, nodeWidth = NODE_WIDTH, iterations = ITER, 
              height = HEIGHT_PIX, width = WIDTH_PIX)
}

# Create output path ----------------------------------------------------------------------------------------
if (!dir.exists(OUTPUT_DIR)) { dir.create(OUTPUT_DIR) }
output_path <- file.path(OUTPUT_DIR)
html_path <- file.path(output_path, "html")
if (!dir.exists(html_path)) {dir.create(html_path)}

# Load clean data -------------------------------------------------------------------------------------------
load("Data/AQP_DataForAnalysis.RData") # clean data

# Create input data -------------------------------------------------------
AQP <- AQP.Data$AQP.race %>% inner_join(AQP.Data$AQP.ancestry)
raceCols <- AQP.DataCols$raceCols
ancestryCols <- AQP.DataCols$ancestryCols

# Clean the variable names for visualization ----------------------------------------------------------------
ancestryCols_clean <- gsub("([a-z])([A-Z])", "\\1 \\2", ancestryCols)
ancestryCols_clean <- gsub("Sub Saharan Africa", "Sub-Saharan Africa", ancestryCols_clean)
ancestryCols_clean <- gsub("Centralor South America", "Central or South America", ancestryCols_clean)

raceCols_clean <- gsub(pattern = "Amin", replacement = "American Indian", x = raceCols)

# Subset the individuals with single ancestry/race selection ------------------------------------------------
AQP_singleRace <- AQP %>% filter(race_selection_sum == 1)
AQP_singleAnc <- AQP %>% filter(anc_selection_sum == 1)
AQP_singleRaceAnc <- AQP %>% filter(race_selection_sum == 1, anc_selection_sum == 1)

# Select the relevant columns -------------------------------------------------------------------------------
# Include race, ancestry for the whole clean data set
AQP_cols <- c(raceCols,ancestryCols)
AQP_groups <- c(rep("race", times = length(raceCols)), rep("anc", times = length(ancestryCols)))

AQP_rel <- AQP %>% select(AQP_cols)
AQP_singleRace_rel <- AQP_singleRace %>% select(AQP_cols)
AQP_singleAnc_rel <- AQP_singleAnc %>% select(AQP_cols)
AQP_singleRaceAnc_rel <- AQP_singleRaceAnc %>% select(AQP_cols)

# Construct the relation matrices ---------------------------------------------------------------------------
AQP_list <- list("all" = AQP_rel, "singleRace" = AQP_singleRace_rel, 
                 "singleAnc" = AQP_singleAnc_rel, 
                 "singleRaceAnc" = AQP_singleRaceAnc_rel)

AQP_overlap_list <- list()

for (item in names(AQP_list)) {
  AQP_overlap_list[[item]] <- percent_overlap(df = AQP_list[[item]], cutoff = PROP_CUTOFF, 
                                              grouping = AQP_groups)
}

# Save the matrices
save(list = c("AQP_overlap_list"), file = file.path(output_path, "overlap_matrices.RData"))

# A list to store the Sankey diagrams -----------------------------------------------------------------------
id_sankeys <- list()

# Plot the Sankey diagrams for ancestry and race ------------------------------------------------------------
id_sankeys[["anc_race_anc_all"]] <- my_AtoBtoA(AQP_overlap_list$all, ancestryCols, raceCols, 
                                               ancestryCols_clean, raceCols_clean)

id_sankeys[["anc_race_anc_singleRaceAnc"]] <- my_AtoBtoA(AQP_overlap_list$singleRaceAnc, 
                                                         ancestryCols, raceCols, 
                                                         ancestryCols_clean, raceCols_clean)

# Save the Sankey html widgets ------------------------------------------------------------------------------
for (i in 1:length(id_sankeys)) {
  sankey_name <- names(id_sankeys)[i]
  sankey_widget <- id_sankeys[[i]]
  
  setwd(file.path(html_path)) # set the working directory to the html directory
  saveWidget(sankey_widget, paste(sankey_name, "html", sep = "."))
  setwd(proj_dir) # reset to project directroy
}


# Create the pdf files ----------------------------------------------------

# The code Code/02.Analyses/Figure1/html_to_pdf.R doesn't work from RStudio and should be run from the command line
# In the html_to_pdf.R code, on line 7, the value "Output/Figure1" must be assigned to the object OUTPUT_DIR

# Log the process -------------------------------------------------------------------------------------------
log_file <- file.path(output_path, "log.txt")
my_cat <- function(...) {
  cat(..., file = log_file, append = T)
}

cat("Plotting launched on", format(launch_time), "\n", file = log_file)
my_cat("Output directory:", output_path, "\n")
my_cat("Proportion cutoff:", PROP_CUTOFF, "\n")
my_cat("Image size:", paste(WIDTH_PIX, "x", HEIGHT_PIX), "\n")
my_cat("Font size:", FONT_SIZE, "\n")
my_cat("Node width:", NODE_WIDTH, "\n")
my_cat("Iterations:", ITER, "\n")
