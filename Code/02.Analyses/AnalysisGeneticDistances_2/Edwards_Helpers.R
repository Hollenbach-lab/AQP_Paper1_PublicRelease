
catTXT <- function(..., txtfile, appendFile = TRUE) {
  cat(..., file = "")
  cat(..., file = txtfile, append = appendFile)
}


# Function to create vector of Desc
pasteDescColsRespCateg <- function(table) {
  cols <- paste("Desc", responseCateg, sep = "_")
  Desc_respCateg <- apply(table %>% select_(.dots = cols), 1, paste, collapse = "-")
  return(Desc_respCateg)
}

pasteDescColsModifier <- function(table) {
  if(grepl("seeking", modifier)) {modifier = "seeking"}
  cols <- paste("Desc", modifier, sep = "_")
  Desc_modifier <- apply(table %>% select_(.dots = cols), 1, paste, collapse = "-")
  return(Desc_modifier)
}

pasteRespcategModifier <- function(table) {
  if (!anyNA(responseCateg) & !anyNA(modifier)) {
    Pop <- apply(table %>% select(Desc_respCateg, Desc_modifier), 1, paste, collapse = "/")
  } else if (anyNA(responseCateg) & !anyNA(modifier)) {
    Pop <- table$Desc_modifier
  } else if (!anyNA(responseCateg) & anyNA(modifier)) {
    Pop <- table$Desc_respCateg
  }
  return(Pop)
}

# Function to perform permutations
