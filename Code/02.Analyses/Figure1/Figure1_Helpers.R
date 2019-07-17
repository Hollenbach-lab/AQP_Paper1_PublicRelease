# A script containing the functions that are used to plot Sankey diagrams

# Functions ---------------------------------------------------------------------------------------

# The percentage of overlap between two variables (normalized by each variable count) ---------------------------------
# Purpose: To compute the percentages of overlap between binary variables. 0 if the variables belong to the same group
# or if the proportion is lower than the cutoff.

# Arguments
# df: data frame; each variable is binary (1 = present and 0 = absent)
# cutoff: numeric; indicating the minimum proportion to be considered (range 0-1); default to 0
# grouping: character; indicating how the variables are grouped
# percentage: logical; indicating whether the output should be in percentage (True) or proportion (False)

# Output: a square matrix (the column and row numbers are the same as the number of variables in the df); should be 
# read from rows to columns. For example, there are 10% selection of A in those who select B, and 20% selection of B in 
# those who select A.
#    A   B  
# A  0  10 
# B 20   0

percent_overlap <- function(df, cutoff = 0, grouping = colnames(df), percentage = T) {
  p <- ncol(df)
  col_names <- colnames(df)
  relation_mat <- matrix(data = NA, ncol = p, nrow = p)
  colnames(relation_mat) <- colnames(df)
  rownames(relation_mat) <- colnames(df)
  for (i in 1:p) { # i for rows
    for (j in 1:p) { # j for columns
      if (grouping[i] == grouping[j]) {
        relation_mat[i, j] <- 0
      } else {
        total_col <- sum(df[ , j]) # total count for the column variable
        row_in_col <- sum(df[ , j] == 1 & df[ , i] == 1) # count of overlap
        prop <- row_in_col/total_col
        if (prop <= cutoff) {
          relation_mat[i, j] <- 0
        } else {
          if (percentage == T) {
            relation_mat[i, j] <- prop*100
          } else {
            relation_mat[i, j] <- prop
          }
        }
      }
    }
  }
  return(relation_mat)
}



# Plot the Sankey diagram for one group of variables ----------------------------------------------
# Purpose: To plot the Sankey diagram for one group of variables, with each variable flowing into
# all the other variables. This shows the relationships between the variables in the group.
# Dependency: the package "networkD3"
# Arguments
## relation_mat: A matrix that maps relationships within a group of variables from rows to columns;
## the row names and column names should be exactly the same
## var_names: the names of the variables in the same order as the matrix's rows and columns
## ... is pased into the function sankeyNetwork
# Output: a Sankey diagram

sankey_within <- function(relation_mat, var_names = colnames(relation_mat), ...) {
  
  # Construct the source and target ids (with 0 index for JavaScript)
  n_vars <- length(var_names)
  source_ids <- 0:(n_vars - 1)
  target_ids <- source_ids + n_vars
  
  # Make the row names the source ids and the column names the target ids
  rownames(relation_mat) <- source_ids
  colnames(relation_mat) <- target_ids
  
  # Construct the data frame for the nodes
  nodes <- data.frame(name = rep(var_names, times = 2))
  
  # Construct the variables for the links
  links_source <- rep(source_ids, each = n_vars)
  links_target <- rep(target_ids, times = n_vars)
  links_group <- rep(var_names, each = n_vars)
  links_value <- rep(NA, times = length(links_source)) # initiate with NAs
  
  # Specify the value for each link
  for (i in 1:length(links_value)) {
    source_id <- as.character(links_source[i])
    target_id <- as.character(links_target[i])
    relation_value <- relation_mat[source_id, target_id]
    if (relation_value > 0) { # only record the value if greater than 0
      links_value[i] <- relation_value
    }
  }
  
  # Combine all the variables into a data frame for the links
  links <- data.frame(source = links_source, target = links_target, group = links_group, 
                      value = links_value)
  links <- links[which(!is.na(links$value)), ] # remove links with NA values
  
  # Plot the Sankey diagram
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                Value = "value", LinkGroup = "group", NodeID = "name", ...)
}
# -------------------------------------------------------------------------------------------------

# Plot the Sankey diagram for two groups' relations with another group ----------------------------
# Purpose: Plot the Sankey diagram to show how two groups flow into another group (which is in the
# middle).
# Dependency: the package "networkD3"
# Arguments:
## relation_mat: A matrix that maps relationships within a group of variables from rows to columns;
## the row names and column names should be exactly the same
## left: character; the variable names for the left group 
## middle: character; the variable names for the middle group
## right: character; the variable names for the right group
## ... is passed to the function sankeyNetwork
# Output: a Sankey diagram

sankey_2to1 <- function(relation_mat, left, middle, right, 
                        left_clean = left, middle_clean = middle, right_clean = right, ...) {
  
  # The number of variables for each group
  left_n <- length(left)
  middle_n <- length(middle)
  right_n <- length(right)
  
  # Construct data frame for the nodes with node names and ids
  nodes <- data.frame(name = c(left, middle, right), id = 0:(left_n + middle_n + right_n - 1), 
                      type = c(left_clean, middle_clean, right_clean))
  
  # Link source names and ids
  links_source <- c(rep(left, each = middle_n), rep(middle, each = right_n))
  links_source_id <- sapply(links_source, function(source) nodes$id[nodes$name == source])
  
  # Link target names and ids
  links_target <- c(rep(middle, times = left_n), rep(right, times = middle_n))
  links_target_id <- sapply(links_target, function(target) nodes$id[nodes$name == target])
  
  # Link group names based on the left and right groups (not the middle group at all)
  links_group <- c(rep(left_clean, each = middle_n), rep(right_clean, times = middle_n))
  
  # Link values
  links_value <- rep(NA, times = length(links_source))
  
  for (i in 1:length(links_value)) {
    if (i <= left_n*middle_n) { # if we consider the links between the left and middle
      relation_value <- relation_mat[links_source[i], links_target[i]] # source as row
    } else { # if we consider the links between the middle and right
      relation_value <- relation_mat[links_target[i], links_source[i]] # source as column
    }
    if (relation_value > 0) {
      links_value[i] <- relation_value
    }
  }
  
  # Construct the link data frame
  links <- data.frame(source = links_source_id, target = links_target_id, group = links_group, 
                      value = links_value)
  links <- links[which(!is.na(links$value)), ] # remove rows with NA values
  
  # Sankey plot
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                Value = "value", LinkGroup = "group", NodeID = "type", ...)
}
# -------------------------------------------------------------------------------------------------

# Plot the Sankey diagram for two groups with unidirectional relations ----------------------------
# Purpose: Plot the Sankey diagram to show how group A flows into B and then B into A
# Dependency: the package "networkD3"
# Arguments:
## relation_mat: A matrix that maps relationships within a group of variables from rows to columns;
## the row names and column names should be exactly the same
## outer: character; the variable names for the outer group
## middle: character; the variable names for the middle group
## ... is passed to the function sankeyNetwork
# Output: a Sankey diagram
sankey_AtoBtoA <- function(relation_mat, outer, middle, 
                           outer_clean_name = outer, middle_clean_name = middle, ...) {
  
  left <- paste0(outer, 1)
  right <- paste0(outer, 2)
  
  left_n <- length(left)
  right_n <- length(right)
  middle_n <- length(middle)
  
  nodes <- data.frame(name = c(left, middle, right), id = 0:(left_n + middle_n + right_n -1), 
                      type = c(outer_clean_name, middle_clean_name, outer_clean_name))
  
  links_source <- c(rep(left, each = middle_n), rep(middle, each = right_n))
  links_source_id <- sapply(links_source, function(source) nodes$id[nodes$name == source])
  links_source_ori <- c(rep(outer, each = middle_n), rep(middle, each = right_n))
  
  links_target <- c(rep(middle, times = left_n), rep(right, times = middle_n))
  links_target_id <- sapply(links_target, function(target) nodes$id[nodes$name == target])
  links_target_ori <- c(rep(middle, times = left_n), rep(outer, times = middle_n))
  
  links_group <- c(rep(outer_clean_name, each = middle_n), rep(middle_clean_name, each = right_n))
  links_value <- rep(NA, times = length(links_source))
  for (i in 1:length(links_value)) {
    relation_value <- relation_mat[links_source_ori[i], links_target_ori[i]]
    if (relation_value > 0) {
      links_value[i] <- relation_value
    }
  }
  
  links <- data.frame(source = links_source_id, target = links_target_id, value = links_value, 
                      group = links_group)
  links <- links[which(!is.na(links$value)), ] # remove rows with NA values
  
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                Value = "value", NodeID = "type", LinkGroup = "group", ...)
}

# -------------------------------------------------------------------------------------------------

# Plot the Sankey diagram for one group's relations with another group ----------------------------
# Purpose: Plot the Sankey diagram to show how group A flows into B
# Dependency: the package "networkD3"
# Arguments:
## relation_mat: A matrix that maps relationships within a group of variables from rows to columns;
## the row names and column names should be exactly the same
## left: character; the variable names for the left group
## right: character; the variable names for the right group
## ... is passed to the function sankeyNetwork
# Output: a Sankey diagram
sankey_AtoB <- function(relation_mat, left, right, ...) {
  # The number of variables for each group 
  left_n <- length(left)
  right_n <- length(right)
  
  # Construct data frame for the nodes with node names and ids
  nodes <- data.frame(name = c(left, right), id = 0:(left_n + right_n - 1))
  
  # Link source names and ids
  links_source <- rep(left, each = right_n)
  links_source_id <- sapply(links_source, function(source) nodes$id[nodes$name == source])
  
  # Link target names and ids
  links_target <- rep(right, times = left_n)
  links_target_id <- sapply(links_target, function(target) nodes$id[nodes$name == target])
  
  # Link group names based on the left group
  links_group <- links_source
  
  # Link values
  links_value <- rep(NA, times = length(links_source))
  for (i in 1:length(links_value)) {
    relation_value <- relation_mat[links_source[i], links_target[i]]
    if (relation_value > 0) {
      links_value[i] <- relation_value
    }
  }
  
  # Construct the link data frame
  links <- data.frame(source = links_source_id, target = links_target_id, group = links_group, 
                      value = links_value)
  links <- links[which(!is.na(links$value)), ] # remove rows with NA values
  
  # Sankey plot 
  sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                Value = "value", LinkGroup = "group", NodeID = "name", ...)
}