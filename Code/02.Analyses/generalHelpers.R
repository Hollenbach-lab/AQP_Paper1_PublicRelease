# Helpers

# Output a pdf file from a sweave file ------------------------------------------------------------------------------------------
SweaveToPdf <- function(filePath, clean = T, outputPath) {
  cat("##### Creating the pdf from Sweave for",filePath," #####\n")
  fileRoot <- strsplit(basename(filePath), ".Rnw")[1][[1]]
  folder <- dirname(filePath)
  oldDir <- getwd()
  
  tryCatch({
    setwd(folder)
    texFileName <- paste0(fileRoot, ".tex")
    pdfFileName <- paste0(fileRoot, ".pdf")
    figuresFolder <<- file.path(oldDir, dirname(outputPath), "Figures")
    cat("### Creating the tex files \n")
    Sweave(paste0(fileRoot, ".Rnw"), encoding = "UTF-8")
    cat ("### Converting to pdf...\n")
    tools::texi2pdf(texFileName)
    if (!missing(outputPath)) {
      file.copy(pdfFileName, file.path(oldDir, outputPath), overwrite = TRUE) & file.remove(pdfFileName)
      file.copy(texFileName, file.path(oldDir, dirname(outputPath), texFileName), overwrite = TRUE) & file.remove(texFileName)
      # try(rstudio::viewer(file.path(oldDir, outputPath)))
    }
    if (clean) {
      cat("### Cleaning up...\n")
      filesToRemove <- paste0(fileRoot, c("-concordance.tex", ".aux", ".log"))
      file.remove(filesToRemove)
      file.remove(dir(".", ".*figure.*\\.png$", ignore.case = T))
    }
    cat("### Done.\n")
  }, finally = setwd(oldDir)
  )
}