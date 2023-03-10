mizr_long_to_wide <- function(input_df, factor_col_fac, response_col, trial_id = NULL) {
  #' Converts a long data frame to a wide data frame.
  #'
  #' Uses the reshape2 package
  #' @param input_df the long data frame to make wide
  #' @param factor_col_fac the column that are the factors
  #' @param response_col the name of the response column
  #' @param trial_id trial_id column. NULL if no trial_id column exists.
  #' @return output_df the wide data frame
  if (is.null(trial_id)) {
    output_df <- input_df %>% rownames_to_column()
    output_df$rowname <- as.numeric(output_df$rowname)
    dcast_formula <- formula(paste(
      paste(names(output_df)[!(names(output_df) %in% c(factor_col_fac, response_col))],
        collapse = " + "
      ),
      paste(factor_col_fac),
      sep = " ~ "
    ))
    output_df <- dcast(output_df, dcast_formula, value.var = response_col)
    output_df$rowname <- NULL
  } else {
    output_df <- input_df
    dcast_formula <- formula(paste(
      paste(names(output_df)[!(names(output_df) %in% c(factor_col_fac, response_col))],
        collapse = " + "
      ),
      paste(factor_col_fac),
      sep = " ~ "
    ))
    output_df <- dcast(output_df, dcast_formula, value.var = response_col)
  }
  return(output_df)
}

mizr_wide_to_long <- function(input_df, id_col_vec) {
  #' Converts a wide data frame to a long data frame.
  #'
  #' Uses the reshape2 package
  #' @param input_df the wide data frame to make long
  #' @param id_col_vec the vector of the id columns
  #' @return output_df the long data frame
  output_df <- melt(input_df, id.vars = id_col_vec)
  output_df <- output_df[!(is.na(output_df$value)), ]
  # reset row names to eliminate missing rows
  rownames(output_df) <- NULL
  return(output_df)
}


mizr_weighted_to_unweighted <- function(input_df, x_col_vec, response_var) {
  #' Provides an aggregated data frame that weighs examples by category
  #'
  #' Takes an unbalanced (or with unbalanced replactions)
}

mizr_two_level_dataset <- function(input_df, col_vec, col_medians) {
  #'
}
