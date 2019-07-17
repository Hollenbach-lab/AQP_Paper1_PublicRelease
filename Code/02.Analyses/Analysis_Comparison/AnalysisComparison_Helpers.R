# Description ---------------------------------------------------------------------------------------------------------
# Functions for performing analyses

# Compare individuals who seek info vs. those who do not: -------------------------------------------------------------
# given a genetic category of self-identity  -------------------------------------------------------
# compare the percentages of another set of genetic categories --------------------------------------------------------
seekCompare_catPct <- function(seek_df, noseek_df, given_col, set_cols, new_given_name, new_set_name, 
                               info_name) {
  # Isolate the data frames to those who belong to the given genetic category
  seek_given_df <- seek_df %>% slice(which(seek_df[ , given_col] == 1))
  noseek_given_df <- noseek_df %>% slice(which(noseek_df[ , given_col] == 1))
  
  # Analyze for each category of the set
  output_list <- list()
  for (set_cat in set_cols) {
    # For those who seek info
    num_seek_cat <- sum(seek_given_df[ , set_cat], na.rm = T) # belonging to the category
    num_seek_notCat <- nrow(seek_given_df) - num_seek_cat # not belonging to the category
    percent_seek_cat <- num_seek_cat / nrow(seek_given_df) * 100 # percentage belonging to the category
    
    # For those who do not seek info
    num_noseek_cat <- sum(noseek_given_df[ , set_cat], na.rm = T)
    num_noseek_notCat <- nrow(noseek_given_df) - num_noseek_cat
    percent_noseek_cat <- num_noseek_cat / nrow(noseek_given_df) * 100
    
    # Construct the contingency table and perform Fisher's exact test
    cont_table <- cbind(c(num_seek_cat, num_seek_notCat), 
                        c(num_noseek_cat, num_noseek_notCat))
    fisher_test <- fisher.test(cont_table)
    cat_df <- data.frame(given_col, set_cat, 
                         percent_seek_cat, percent_noseek_cat, 
                         fisher_test$p.value, fisher_test$estimate)
    colnames(cat_df) <- c(new_given_name, new_set_name, 
                          paste("seek", info_name, "percentage", sep = "_"),
                          paste("no_seek", info_name, "percentage", sep = "_"), 
                          "p_value", "odds_ratio")
    output_list[[set_cat]] <- cat_df
  }
  output_df <- do.call(rbind, output_list)
  return(output_df)
}

# Apply the function seekCompare_catPct to many given categories ------------------------------------------------------
# Also performs p value correction and indicate corrected significant results -----------------------------------------
seekCompare_catPct_many <- function(seek_df, noseek_df, given_cols, set_cols, 
                                    new_given_name, new_set_name, info_name, 
                                    adjust = "bonferroni", alpha = 0.05) {
  many_output_list <- list()
  for (given_col in given_cols) {
    many_output_list[[given_col]] <- seekCompare_catPct(seek_df = seek_df, 
                                                        noseek_df = noseek_df, 
                                                        given_col = given_col, 
                                                        set_cols = set_cols, 
                                                        new_given_name = new_given_name, 
                                                        new_set_name = new_set_name, 
                                                        info_name = info_name)
  }
  many_output_df <- do.call(rbind, many_output_list)
  many_output_df$corrected_p_value <- p.adjust(p = many_output_df$p_value, method = adjust)
  many_output_df$corrected_significant <- many_output_df$corrected_p_value < alpha
  return(many_output_df)
}



# Abstract function to perform the comparison analysis with race as the given genetic attribute -----------------------
compare_race_others <- function(df_list, info_name = NULL, change_names = NULL, 
                                dot_max, y_limits = NULL, x_label_size = NULL, 
                                output_dir, output_prefix, ...) {
  
  # Ancestry
  cat("Analyzing the ancestries ...", "\n")
  race_anc <- seekCompare_catPct_many(seek_df = df_list[[1]], noseek_df = df_list[[2]], 
                                      given_cols = raceCols, set_cols = ancCols, 
                                      new_given_name = "race", new_set_name = "ancestry", 
                                      info_name = info_name)
  
  # Ancestry with single race selection
  cat("Analyzing the ancestries for single race selection ...", "\n")
  race_anc_singleRace <- seekCompare_catPct_many(seek_df = df_list[[1]] %>% filter(single_race == T), 
                                                 noseek_df = df_list[[2]] %>% filter(single_race == T), 
                                                 given_cols = raceCols, set_cols = ancCols, 
                                                 new_given_name = "race", new_set_name = "ancestry", 
                                                 info_name = info_name)
 
   # Change the column names if necessary
  if (!is.null(change_names)) {
    colnames(race_anc)[3:4] <- change_names
    colnames(race_anc_singleRace)[3:4] <- change_names
  }
  
  # Create the output directories
  cat("Writing the output ...", "\n")

  # Output the csv files
  write.csv(race_anc, file = file.path(output_dir, paste(output_prefix, "race_anc.csv", sep = "_")))
  write.csv(race_anc_singleRace, file = file.path(output_dir, paste(output_prefix, "race_anc_singleRace.csv", 
                                                                 sep = "_")))

  cat("Done!", "\n")
}


# Compute the means of selection numbers for two groups and compare them using t-test -----------------------
compare_selectionNum <- function(df_list, selectionNum_var) {
  mean1 <- mean(df_list[[1]][ , selectionNum_var])
  mean2 <- mean(df_list[[2]][ , selectionNum_var])
  t_test <- t.test(x = df_list[[1]][ , selectionNum_var], 
                   y = df_list[[2]][ , selectionNum_var], 
                   alternative = "two.sided")
  results <- data.frame("comparison" = paste(names(df_list)[1], names(df_list)[2], sep = " vs "), 
                        "mean1" = mean1, "mean2" = mean2, "p_value" = t_test$p.value)
  return(results)
}

# Perform multiple Fisher's exact tests and correct the p-values --------------------------------------------
many_fisher <- function(df1, df2, df1_name, df2_name, compare_vars, adjust = "bonferroni", alpha = 0.05) {
  output_list <- list()
  for (compare_var in compare_vars) {
    # Compute the numbers of positive and negative in data set 1
    yes1 <- sum(df1[ , compare_var] == 1)
    no1 <- nrow(df1) - yes1
    percent1 <- yes1 / nrow(df1) * 100
    
    # Compute the numbers of positive and negative in data set 2
    yes2 <- sum(df2[ , compare_var] == 1)
    no2 <- nrow(df2) - yes2
    percent2 <- yes2 / nrow(df2) * 100
    
    # Construct contingency table and perform Fisher's exact test
    cont_table <- cbind(c(yes1, no1), 
                        c(yes2, no2))
    fisher_test <- fisher.test(cont_table)
    
    # Output data frame
    output_df <- data.frame("term" = compare_var, "percent1" = percent1, "percent2" = percent2, 
                            "p_value" = fisher_test$p.value, "odds_ratio" = fisher_test$estimate, 
                            stringsAsFactors = F)
    output_list[[compare_var]] <- output_df
  }
  overall_df <- do.call(rbind, output_list)
  colnames(overall_df)[2:3] <- c(df1_name, df2_name)
  overall_df$corrected_p_value <- p.adjust(p = overall_df$p_value, method = adjust)
  overall_df$corrected_significant <- overall_df$corrected_p_value < alpha
  return(overall_df)
}