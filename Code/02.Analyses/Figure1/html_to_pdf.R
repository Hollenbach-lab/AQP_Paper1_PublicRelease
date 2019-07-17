# To convert html widgets/pages to pdf files
# This script does not work from RStudio because RStudio has a different PATH from the system PATH. This 
# script should be run from command line.

# Code ------------------------------------------------------------------------------------------------------
# User-specified directory that conatins a subdirectory called "html" which contains the html files
OUTPUT_DIR <- "Output/Figure1"

# Find the path to the html files
html_dir <- file.path(OUTPUT_DIR, "html")
html_files <- list.files(path = html_dir, pattern = "*.html")
html_paths <- file.path(html_dir, list.files(path = html_dir, pattern = "*.html"))

# Create the pdf subdirectory to store the pdf files
pdf_files <- gsub(pattern = "html", replacement = "pdf", x = html_files)
pdf_dir <- file.path(OUTPUT_DIR, "pdf")
if (!dir.exists(pdf_dir)) {dir.create(pdf_dir)}
pdf_paths <- file.path(pdf_dir, pdf_files)

# The command line phantomjs operations to convert html to pdf files
phantom_ops <- paste0("phantomjs Code/rasterize.js ", "'", html_paths, "' ", pdf_paths)

# System call the command line operations from R
for(i in 1:length(phantom_ops)) {
  cat("Converting html to pdf...", "\n")
  system(command = phantom_ops[i])
  progress <- paste0(i, "/", length(phantom_ops), " files")
  cat(progress, "\n")
  if (i == length(phantom_ops)) {cat("Done", "\n")}
}