mizr_block_plot <- function(df, x_var_vec, treatment_var, response_var,
                            value_text_size = rel(2.0),
                            comp_func = mean, ...) {
  #' Produces a block plot
  #' @param df the data frame
  #' @param x_var_vec the vector of columns to display on the x-axis.
  #' Multiple factors can be combined to enumerate out the different values.
  #' @param treatment_var the blocking factor or effect to plot separately as different symbols
  #' @param response_var the response column to plot on the y axis;
  #' @param value_text_size the size of the text within the plot. Often specified
  #' as rel(Y) or a relative text size
  #' @param comp_func mean by default. The computation function used to aggregate the multiple
  #' values into a single value.
  #' @return mizr_plot a plot object with this plot to display

  mizr_df <- block_compute_df(df, x_var_vec, treatment_var, response_var, comp_func, ...)
  block_box_df <- block_compute_box_df(
    mizr_df, x_var_vec, treatment_var, response_var,
    comp_func, ...
  )
  mizr_plot <- block_produce_plot(
    mizr_df, block_box_df, x_var_vec, treatment_var,
    response_var, value_text_size
  )
  return(mizr_plot)
}

mizr_sign_test_plot <- function(df, x_var_vec, treatment_var, response_var,
                                higher_is_better = TRUE, use_margins = FALSE,
                                include_equal_comparisons = TRUE, alpha = 0.05,
                                p_adj = "bonferonni",
                                low_color = "#FF0000",
                                mid_color = "#FFFFFF",
                                high_color = "#0074FF",
                                tile_text_size = rel(2.0),
                                comp_func = mean, ...) {
  #' Produces a block rank plot
  #' @param df the data frame
  #' @param x_var_vec the vector of columns to display on the x-axis.
  #' Multiple factors can be combined to enumerate out the different values.
  #' @param treatment_var the blocking factor or effect to plot separately as different symbols
  #' @param response_var the response column to plot on the y axis;
  #' @param higher_is_better TRUE if higher scores are better, FALSE if lower scores are
  #' better
  #' @param use_margins if TRUE, add marginal sum columns
  #' @param alpha type I significance
  #' @param p_adj either "none" or "bonferonni". "none" does not adjust the p-values for Type I
  #' inflation while "bonferonni" uses a bonferonni correction to the Type I error
  #' @param low_color the color for the lowest value.
  #' @param mid_color the color for the midpoint value.
  #' @param high_color the color for the highest value.
  #' @param tile_text_size the size of the text within the plot. Often specified
  #' as rel(Y) or a relative text size
  #' @param comp_func the computation function used to aggregate multiple values
  #' into a single value. Defaults to the arithmetic mean.
  #' @return mizr_plot a plot object with this plot to display

  mizr_df <- sign_test_compute_df(
    df, x_var_vec, treatment_var, response_var,
    higher_is_better, use_margins, alpha, p_adj, comp_func, ...
  )
  mizr_plot <- sign_test_produce_plot(
    mizr_df, x_var_vec, treatment_var,
    response_var, use_margins, include_equal_comparisons, alpha, p_adj,
    low_color, mid_color, high_color, tile_text_size
  )
  return(mizr_plot)
}

mizr_box_plot <- function(df, variable_fac, value_num, points_df = NULL,
                          point_name = NULL, show_points = TRUE, xlab = NULL, ylab = NULL) {
  #' Produces a set of box plots, allows for the inclusion of specific points
  #' @param df the data frame with the data, already aggregated
  #' @param variable_fac the column name representing the column variables
  #' @param value_num the column name representing the values of the factors in
  #' column `variable_fac`
  #' @param points_df the data frame with the points you want on the box plot
  #' @param point_name a label for the legend for your data frame
  #' @param show_points TRUE if we wish to view all of the points. FALSE otherwise.
  #' Defaults to TRUE.
  #' @param xlab a label for the x axis otherwise defaults to variable_fac
  #' @param ylab a label for the y axis otherwise defaults to value_num
  #' @return mizr_plot a plot object

  mizr_df <- box_compute_df(
    df, variable_fac, value_num, points_df, point_name,
    xlab, ylab
  )
  mizr_plot <- box_produce_plot(
    mizr_df, variable_fac, value_num, points_df, point_name,
    show_points, xlab, ylab
  )
  return(mizr_plot)
}

mizr_det_plot <- function(df, score_col, key_col, key_values = c(0, 1), dec_col = NULL,
                          cp_miss = 1, cp_fa = 1, pp_hkv = 0.5,
                          cs_miss = 1, cs_fa = 1, ps_hkv = 0.9,
                          sys_name = "") {
  #' Produce a Detection Error Tradeoff (DET) Curve Plot
  #'
  #' This produces a Detection Error Tradeoff (DET) Curve following the work of:
  #'
  #' Martin, Alvin F., George R. Doddington, Terri Kamm, Mark Ordowski, and Mark A. Przybocki.
  #' 1997. “The DET Curve in Assessment of Detection Task Performance.”
  #' In Fifth European Conference on Speech Communication and Technology, EUROSPEECH 1997.
  #' Rhodes, Greece. http://www.isca-speech.org/archive/eurospeech_1997/e97_1895.html.
  #'
  #' This produces a DET curve for a single system, and has options to support actual decisions
  #' in addition to minimum scores, and allows for parameterizable costs.
  #' Parameterization is supported both in costs of a miss
  #' and false alarm as well as the prior probability of a target, or "high_key_value".
  #'
  #' The DET curve supports two decision cost functions: a primary cost function (p) and a
  #' secondary cost function (s). This allows for scenarios such as a forensic and investigative
  #' analysis that provides a system's performance according to two cost functions
  #'
  #' @param df the data frame of scores and decisions to produce the DET curve for
  #' @param score_col the column containing the system scores.
  #' @param key_col the column of the data frame that contains the key (ground truth)
  #' @param key_values a two-element vector with (low_key_value, high_key_value), where
  #' the high_key_value is interpreted as the target or the positive trait.
  #' @param dec_col the column containing the system's actual decisions. Set to NULL if there are
  #' no decisions provided. Defaults to NULL.
  #' @param cp_miss the cost of a miss for the primary cost function. Defaults to 1.
  #' @param cp_fa the cost of a false alarm for the primary cost function. Defaults to 1.
  #' @param pp_hkv The prior probability of the high_key_value for the primary cost function,
  #' which is the prior probability of
  #' the target class or positive class. Defaults to 0.5, meaning that with the defaults misses
  #' and false alarms are equally weighted.
  #' @param cs_miss the cost of a miss for the secondary cost function. Defaults to 1.
  #' @param cs_fa the cost of a false alarm for the secondary cost function. Defaults to 1.
  #' @param ps_hkv The prior probability of the high_key_value for the secondary cost function,
  #' which is the prior probability of
  #' the target class or positive class. Defaults to 0.9, meaning that with the defaults misses
  #' and false alarms are qually weighted.
  #' @param sys_name the name of the system. Defaults to "".
  #' @return mizr_plot a plot object

  mizr_df <- det_compute_df(df, score_col, key_col, key_values, dec_col)
  costs_df <- det_compute_costs_df(
    df, mizr_df, score_col, key_col, key_values, dec_col,
    cp_miss, cp_fa, pp_hkv,
    cs_miss, cs_fa, ps_hkv
  )
  mizr_plot <- det_produce_plot(
    mizr_df, costs_df, score_col, key_col, key_values,
    dec_col, cp_miss, cp_fa, pp_hkv,
    cs_miss, cs_fa, ps_hkv,
    sys_name
  )
  return(mizr_plot)
}

mizr_effects_plot <- function(df, effects_var_vec, response_var, interaction_depth = 2,
                              max_value_only = TRUE) {
  #' Produce effects Plot
  #'
  #' @param df the data frame
  #' @param effects_var_vec the vector of effects or factors to compute the effects of
  #' @param response_var the response column to plot on the y axis;
  #' @param interaction_depth 2. The number of interaction terms to compute.
  #' Currently 2 is only supported value.
  #' @param max_value_only if TRUE, only give the maximum value for each effect or interaction
  #' (For multiple level effects); if FALSE, give all effects
  #' @return mizr_plot a plot object with this plot to display

  mizr_df <- effects_compute_df(
    df, effects_var_vec, response_var, interaction_depth,
    max_value_only
  )
  # Get the grand mean
  grand_mean <- compute_grand_mean(df, effects_var_vec, response_var)

  mizr_plot <- effects_produce_plot(mizr_df, effects_var_vec, grand_mean)
  return(mizr_plot)
}

mizr_histogram_plot <- function(df, x_val, bins = NULL, binwidth = NULL, closed = "left",
                                add_text = TRUE) {
  #' Plots a histogram
  #' @param df the data frame
  #' @param x_val the column name for the x row
  #' @param bins the number of bins to use Please specify one of bins or binwidth.
  #' @param binwidth the width of each bin. Please specify one of bins or binwidth.
  #' @param closed "left" closed by default. Can have "right" closed histograms.
  #' @param add_text if TRUE, add the number count of each bin as a text label
  #' @return mizr_plot a plot object

  hist_df <- histogram_compute_df(df, x_val, bins, binwidth, closed)
  # We use the precomputed hist_df to add the text to the plot
  hist_plot <- histogram_produce_plot(
    df, hist_df, x_val, bins, binwidth, closed,
    add_text
  )
  return(hist_plot)
}


mizr_interact_compare_plot <- function(df, effects_var_vec, treatment_var, response_var,
                                       bar_ci = FALSE,
                                       plot_NA_values = FALSE,
                                       comp_func = mean, ...) {
  #' Produces a two-term interaction plot with respect to the treatment_var comparison factor
  #' @param df the data frame with the data
  #' @param effects_var_vec the list of colum names of the effect factors, specified in order
  #' @param treatment_var the treatment col to compare in all of these plots
  #' @param response_var the column name of the response variable
  #' @param bar_ci if TRUE, include confidence intervals; hide if FALSE.
  #' @param plot_NA_values if TRUE, plot missing values (and keep columns and rows
  #' with missing values). Hide if FALSE.
  #' @param comp_func the computation function used to aggregate multiple values
  #' into a single value. Defaults to the arithmetic mean.
  #' @return mizr_plot a plot object

  mizr_df <- interact_compare_compute_df(
    df, effects_var_vec, treatment_var, response_var,
    bar_ci, plot_NA_values, comp_func, ...
  )
  labels_vec <- interact_compare_labels(
    df, effects_var_vec, treatment_var,
    response_var, bar_ci,
    plot_NA_values, comp_func, ...
  )
  mizr_plot <- interact_compare_produce_plot(
    mizr_df, labels_vec, effects_var_vec,
    treatment_var, response_var,
    plot_NA_values
  )
  return(mizr_plot)
}


mizr_lag_plot <- function(df, response_var, run_var = NULL, group_fac_vec = NULL,
                          label_facets_with_variables = TRUE) {
  #' The Lag Plot of Filliben's Fourplots
  #'
  #' Produces a lag plot comparing the response to the previous response's value
  #' @param df the data frame
  #' @param response_var the variable to plot
  #' @param run_var the variable with the run number, or NULL to use the row index number
  #' @param group_fac_vec the factor vector to group variables. NULL by default.
  #' @param label_facets_with_variables if TRUE, gives the variable (as well as value) label
  #' to each facet. TRUE by default.
  #' @return mizr_plot mizr_plot the plot object
  lag_df <- lag_compute_df(df, response_var, group_fac_vec, run_var)
  if (is.null(run_var)) {
    run_var <- "row_ind"
  }
  mizr_plot <- lag_produce_plot(
    lag_df, response_var, group_fac_vec, run_var,
    label_facets_with_variables
  )
  return(mizr_plot)
}

mizr_main_effects_plot <- function(df, effects_var_vec, response_var,
                                   plot_NA_values = FALSE,
                                   bar_ci = FALSE,
                                   alpha = 0.05, bar_sd = FALSE,
                                   weighted = FALSE) {
  #' Produces a main effects plot
  #' @param df the data frame with the data
  #' @param effects_var_vec the list of colum names of the effect factors, specified in order
  #' @param response_var the column name of the response variable
  #' @param plot_NA_values if TRUE, plot missing values. FALSE by default
  #' @param bar_ci If TRUE, plot bars that are the standard error.
  #' @param alpha The alpha used for statistical significance tests and to provide (1 - alpha)
  #' confidence intervals
  #' @param bar_sd If TRUE, plot bars that are the standard deviation.
  #' @param weighted TRUE if the main effect plots should be weighted by effect grouping and then
  #' averaged or FALSE if each category should be averaged separately
  #' @return mizr_plot a plot object

  ef_df <- df
  if (weighted == TRUE) {
    ef_df <- compute_mean_df(df, effects_var_vec, response_var)
  }
  mizr_df <- main_effects_compute_df(
    ef_df, effects_var_vec, response_var, plot_NA_values,
    bar_ci, alpha
  )
  sd_df <- NULL
  if (bar_sd) {
    sd_df <- main_effects_compute_sd_df(df, effects_var_vec, response_var, plot_NA_values)
  }
  mean_val <- compute_grand_mean(df, effects_var_vec, response_var, weighted = weighted)
  labels_vec <- main_effects_compute_labels(
    df, effects_var_vec, response_var,
    plot_NA_values, bar_ci, alpha
  )
  mizr_plot <- main_effects_produce_plot(
    mizr_df, labels_vec, mean_val, effects_var_vec,
    response_var, sd_df, plot_NA_values,
    bar_ci, alpha, bar_sd
  )
  return(mizr_plot)
}

mizr_main_effects_hsd_plot <- function(df, effects_var_vec, response_var,
                                       plot_NA_values = FALSE,
                                       alpha = 0.05) {
  #' Produces Honest Significant Differences (HSD) Confidence Intervals on the Effects
  #'
  #' Uses Tukey's Honest Significant Differences (HSD) to compute the confidence intervals to
  #' prevent Type I inflaction error when comparing the error bars
  #' @param df the data frame with the data
  #' @param effects_var_vec the list of colum names of the effect factors, specified in order
  #' @param response_var the column name of the response variable
  #' @param plot_NA_values if TRUE, plot missing values. FALSE by default.
  #' @param alpha The alpha used for statistical significance tests and to provide (1 - alpha)
  #' confidence intervals
  #' @return mizr_plot a plot object

  mizr_df <- main_effects_hsd_compute_df(
    df, effects_var_vec, response_var,
    plot_NA_values, alpha
  )
  labels_vec <- main_effects_hsd_labels(
    df, effects_var_vec, response_var,
    plot_NA_values, alpha
  )
  mizr_plot <- main_effects_hsd_produce_plot(
    mizr_df, labels_vec, effects_var_vec, response_var,
    plot_NA_values, alpha
  )
  return(mizr_plot)
}

mizr_main_effects_mar_plot <- function(df, variable_fac, value_fac, response_var) {
  #' Produces a marginal main effects plot
  #' @param df the data frame with the data
  #' @param variable_fac the column name representing the column variables
  #' @param value_fac the column name representing the values of the factors in
  #' column `variable_fac`
  #' @param response_var the column name of the response variable
  #' @return mizr_plot a plot object
  marginal_df <- main_effects_mar_compute_df(
    df, variable_fac, value_fac, response_var
  )
  labels_vec <- main_effects_mar_labels(
    marginal_df, variable_fac, value_fac,
    response_var
  )
  mizr_plot <- main_effects_mar_produce_plot(
    marginal_df, labels_vec, variable_fac, value_fac,
    response_var
  )
  return(mizr_plot)
}

mizr_normal_prob_plot <- function(df, y_var) {
  #' Produce a normal probability plot
  #' @param df the data frame
  #' @param y_var the name of the variable to produce the probability plot for
  #' @return mizr_plot a plot object

  # We need not use the computed frame prob_df, which
  # is returned from normal_probability_compute_df(df, y_var)
  mizr_plot <- normal_prob_produce_plot(df, y_var)
  return(mizr_plot)
}

mizr_order_plot <- function(df, fac_vec, response_var,
                            digits = 2, remove_NA_factors = FALSE) {
  #' Produce an Order Plot of the 10-step series
  #' @param df the data frame
  #' @param fac_vec the factors to plot on the x column
  #' @param response_var the response column to plot on the y axis.
  #' @param digits the number of digits to round values to
  #' @param remove_NA_factors if TRUE, remove all values where one or more of the factors
  #' is NA. FALSE by default
  #' @return mizr_plot a plot object

  mizr_df <- order_compute_df(
    df, fac_vec, response_var, digits,
    remove_NA_factors
  )
  mizr_plot <- order_produce_plot(mizr_df, fac_vec, response_var)
  return(mizr_plot)
}

mizr_run_plot <- function(df, response_var, group_fac_vec = NULL, run_var = NULL,
                          label_facets_with_variables = TRUE) {
  #' The Run Plot of Filliben's Fourplots
  #'
  #' Produces a run plot comparing the run number to the response variable
  #' @param df the data frame
  #' @param response_var the variable to plot
  #' @param group_var_vec the factor variables to group by for a run plot faceted by group
  #' @param run_var the variable with the run number, or NULL to use the row index number
  #' @param label_facets_with_variables if TRUE, gives the variable (as well as value) label
  #' to each facet. TRUE by default.
  #' @return mizr_plot mizr_plot the plot object
  run_df <- run_compute_df(df, response_var, group_fac_vec, run_var)
  if (is.null(run_var)) {
    run_var <- "row_ind"
  }
  mizr_plot <- run_produce_plot(
    run_df, response_var, group_fac_vec, run_var,
    label_facets_with_variables
  )
  return(mizr_plot)
}

mizr_scatter_plot <- function(df, x_var, response_var,
                              shape_style = "point") {
  #' Produces a basic scatter plot that colors and shapes by the treatment factor
  #' @param df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param response_var the response variable to plot on the y axis
  #' @param shape_style a string indicating if the shape should be points ('point') or textual
  #' symbols 'txt'. Defaults to point.
  #' @return mizr_plot the plot object

  mizr_df <- scatter_compute_df(df, x_var, response_var)
  mizr_plot <- scatter_produce_plot(mizr_df, x_var, response_var, shape_style)
  return(mizr_plot)
}

mizr_scatter_doe_plot <- function(df, fac_vec, response_var, treatment_fac = NULL,
                                  doe_plot_style = "enhanced",
                                  plot_NA_values = FALSE) {
  #' Produces a scatter plot that colors and shapes by the treatment factor
  #' @param df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param treatment_fac the treatment factor
  #' @param response_var the response variable to plot on the y axis
  #' @param doe_plot_style Specifies the Plot Style. Use "plain" to mimic the DOE Scatter Plot
  #' in dataplot, and "enhanced" to use an enhanced version.
  #' "boxplot" to place the boxplot in front of the points, and "sd" to replace the boxplot
  #' To produce a mean and standard deviation rather than quantiles. Defaults to "enhanced".
  #' @param plot_NA_values FALSE by default. If TRUE, plot all values with a factor of a
  #' NA value.
  #' @return mizr_plot the plot object

  mizr_df <- scatter_doe_compute_df(
    df, fac_vec, response_var, treatment_fac,
    plot_NA_values
  )
  sd_df <- NULL
  if (doe_plot_style == "sd") {
    sd_df <- scatter_doe_compute_sd_df(
      df, fac_vec, response_var, treatment_fac,
      plot_NA_values
    )
  }
  labels_vec <- scatter_doe_compute_labels(
    df, fac_vec, response_var, treatment_fac,
    plot_NA_values
  )
  mean_val <- scatter_doe_compute_mean(df, response_var)
  mizr_plot <- scatter_doe_produce_plot(
    mizr_df, labels_vec, fac_vec, response_var,
    mean_val, sd_df, treatment_fac,
    doe_plot_style
  )
  return(mizr_plot)
}

mizr_scatter_effect_plot <- function(df, x_var, response_var,
                                     comp_func = mean, ...) {
  #' Produces a scatter plot that does a main effect of x_var by response_var
  #' @param df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param response_var the response variable to plot on the y axis
  #' @return mizr_plot the plot object

  mizr_df <- scatter_effect_compute_df(df, x_var, response_var, comp_func, ...)
  mizr_plot <- scatter_effect_produce_plot(
    mizr_df, x_var, response_var,
    comp_func, ...
  )
  return(mizr_plot)
}

mizr_scatter_treatment_plot <- function(df, x_var, treatment_fac, response_var,
                                        shape_style = "point") {
  #' Produces a scatter plot that colors and shapes by the treatment factor
  #' @param df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param treatment_fac the treatment factor
  #' @param response_var the response variable to plot on the y axis
  #' @param shape_style a string indicating if the shape should be points ('point') or textual
  #' symbols 'txt'. Defaults to point.
  #' @return mizr_plot the plot object

  mizr_df <- scatter_treatment_compute_df(df, x_var, treatment_fac, response_var)
  mizr_plot <- scatter_treatment_produce_plot(
    mizr_df, x_var, treatment_fac, response_var,
    shape_style
  )
  return(mizr_plot)
}

mizr_tile_table_plot <- function(df, x_fac_vec, y_fac_vec, response_var,
                                 low_color = "#FF0000",
                                 mid_color = "#FFFFFF",
                                 high_color = "#0074FF",
                                 midpoint = 0, digits = 2,
                                 use_margins = FALSE, tile_text_size = rel(2.0)) {
  #' Produce a Colored Table of all factors
  #' @param df the data frame
  #' @param x_fac_vec the vector of factors to go on the x-axis.
  #' @param y_fac_vec the vector of factors to go on the y-axis.
  #' @param response_var the name of the response variable.
  #' @param low_color the color for the lowest value.
  #' @param mid_color the color for the midpoint value.
  #' @param high_color the color for the highest value.
  #' @param midpoint the numerical value of the middle point to color mid_color
  #' @param digits the number of digits to round numbers in the plot to. Defaults to 2.
  #' @param use_margins if TRUE, provide row and column margin aggregates. FALSE by
  #' default.
  #' @param tile_text_size the size of the text in tiles.
  #' can be a number or a rel(1.0) for relative sizes.
  #' @return mizr_plot the plot object
  mizr_df <- tile_table_compute_df(
    df, x_fac_vec, y_fac_vec, response_var, digits,
    use_margins
  )
  mizr_plot <- tile_table_produce_plot(
    mizr_df, x_fac_vec, y_fac_vec, response_var,
    low_color, mid_color, high_color, midpoint,
    use_margins, tile_text_size
  )
  return(mizr_plot)
}

mizr_tile_table_count_plot <- function(df, x_fac_vec, y_fac_vec, response_name = "count",
                                       low_color = "#FFFFFF",
                                       mid_color = "#EEEEEE",
                                       high_color = "#0074FF", midpoint = 1,
                                       digits = 2,
                                       use_margins = FALSE, tile_text_size = rel(1.6),
                                       remove_NA_factors = FALSE) {
  #' Produce a Colored Table of all counts of the number of attributes
  #' @param df the data frame
  #' @param x_fac_vec the vector of factors to go on the x-axis.
  #' @param y_fac_vec the vector of factors to go on the y-axis.
  #' @param response_var the name to display for the count response. Defaults to "count"
  #' @param low_color the color for the lowest value.
  #' @param mid_color the color for the midpoint value.
  #' @param high_color the color for the highest value.
  #' @param midpoint the numerical value of the middle point to color mid_color
  #' @param digits the number of digits to round numbers in the plot to. Defaults to 2.
  #' @param use_margins if TRUE, provide row and column margin aggregates. FALSE by
  #' default.
  #' @param tile_text_size the size of the text in tiles.
  #' can be a number or a rel(1.0) for relative sizes.
  #' @param remove_NA_factors if TRUE, remove all attributes with a NA factor. FALSE by
  #' default.
  #' @return mizr_plot the plot object
  mizr_df <- tile_table_count_df(
    df, x_fac_vec, y_fac_vec, response_name, digits,
    use_margins, remove_NA_factors
  )
  mizr_plot <- tile_table_count_produce_plot(
    mizr_df, x_fac_vec, y_fac_vec, response_name,
    low_color, mid_color, high_color, midpoint,
    use_margins, tile_text_size
  )
  return(mizr_plot)
}

mizr_tile_tf_count_plot <- function(df, fac_vec, response_name = "count",
                                    low_color = "#FFFFFF",
                                    mid_color = "#EEEEEE",
                                    high_color = "#0074FF", midpoint = 1,
                                    digits = 2,
                                    tile_text_size = rel(2.0),
                                    remove_NA_factors = FALSE) {
  #' Produce a Colored Table of all counts of all two-factor combinations
  #' @param df the data frame
  #' @param fac_vec the vector of factors to do two-factor counts of
  #' @param response_var the name to display for the count response. Defaults to "count"
  #' @param low_color the color for the lowest value.
  #' @param mid_color the color for the midpoint value.
  #' @param high_color the color for the highest value.
  #' @param midpoint the numerical value of the middle point to color mid_color
  #' @param digits the number of digits to round numbers in the plot to. Defaults to 2.
  #' @param use_margins if TRUE, provide row and column margin aggregates. FALSE by
  #' default.
  #' @param tile_text_size the size of the text in tiles.
  #' can be a number or a rel(1.0) for relative sizes.
  #' @param remove_NA_factors if TRUE, remove all attributes with a NA factor. FALSE by
  #' default.
  #' @return mizr_plot the plot object
  mizr_df <- tile_tf_count_df(
    df, fac_vec, response_name, digits,
    remove_NA_factors
  )
  mizr_plot <- tile_tf_count_produce_plot(
    mizr_df, fac_vec, response_name,
    low_color, mid_color, high_color, midpoint,
    tile_text_size
  )
  return(mizr_plot)
}

mizr_tile_tf_plot <- function(df, fac_vec, response_var,
                              low_color = "#FF0000",
                              mid_color = "#FFFFFF",
                              high_color = "#0074FF",
                              midpoint = 0,
                              digits = 2,
                              tile_text_size = rel(2.0),
                              remove_NA_factors = FALSE) {
  #' Produce a title table of all two-factor combinations. Used to provide
  #' information on two-term interactions
  #' @param df the data frame
  #' @param fac_vec the vector of factors to do two-factor counts of
  #' @param response_var the response variable column.
  #' @param low_color the color for the lowest value.
  #' @param mid_color the color for the midpoint value.
  #' @param high_color the color for the highest value.
  #' @param midpoint the numerical value of the middle point to color mid_color
  #' @param digits the number of digits to round numbers in the plot to. Defaults to 2.
  #' @param use_margins if TRUE, provide row and column margin aggregates. FALSE by
  #' default.
  #' @param tile_text_size the size of the text in tiles.
  #' can be a number or a rel(1.0) for relative sizes.
  #' @param remove_NA_factors if TRUE, remove all attributes with a NA factor. FALSE by
  #' default.
  #' @return mizr_plot the plot object
  mizr_df <- tile_tf_compute_df(
    df, fac_vec, response_var, digits,
    remove_NA_factors
  )
  mizr_plot <- tile_tf_produce_plot(
    mizr_df, fac_vec, response_var,
    low_color, mid_color, high_color, midpoint,
    tile_text_size
  )
  return(mizr_plot)
}
