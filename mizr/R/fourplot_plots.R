histogram_compute_df <- function(df, x_val, bins = NULL, binwidth = NULL, closed = "left") {
  count_string <- "..count.."
  basic_hist_plot <- ggplot(df) +
    geom_histogram(aes_string(x = x_val, y = count_string),
      bins = bins, binwidth = binwidth, closed = closed
    )
  hist_res_df <- ggplot_build(basic_hist_plot)$data[[1]]
  hist_res_df <- hist_res_df[, c("x", "xmin", "xmax", "count", "density")]
  names(hist_res_df)[names(hist_res_df) == "xmin"] <- paste0(x_val, "_min")
  names(hist_res_df)[names(hist_res_df) == "xmax"] <- paste0(x_val, "_max")
  return(hist_res_df)
}

# We are using the plot to compute the data for us, so we need not use the compute_histogram_df
histogram_produce_plot <- function(df, hist_df, x_val, bins = NULL, binwidth = NULL,
                                   closed = "left", add_text = TRUE) {
  count_string <- "..count.."
  basic_hist_plot <- ggplot(df) +
    geom_histogram(aes_string(x = x_val, y = count_string),
      color = "black", fill = NA, bins = bins,
      binwidth = binwidth, closed = closed
    )
  if (add_text) {
    basic_hist_plot <- basic_hist_plot +
      geom_text(aes_string(x = x_val, y = count_string, label = count_string),
        hjust = 0.5, vjust = -0.3, bins = bins, binwidth = binwidth, stat = "bin"
      )
  }
  basic_hist_plot <- basic_hist_plot +
    coord_cartesian(ylim = c(0, 1.07 * max(hist_df$count)), expand = 0) +
    ggtitle(paste0("Histogram of variable ", x_val))
  return(basic_hist_plot)
}


lag_compute_df <- function(df, response_var, group_fac_vec, run_var) {
  if (is.null(run_var)) {
    run_var <- "row_ind"
    # Use exact rownames rather than seq(1, nrow(df), 1) In case rownames have meaning
    df$row_ind <- rownames(df)
  }
  # Now add lagged statistics
  if (!is.null(group_fac_vec)) {
    group_sym <- syms(group_fac_vec)
    lag_df <- df %>%
      arrange(.data[[run_var]]) %>%
      group_by(!!!group_sym) %>%
      mutate(response_lag = lag(.data[[response_var]], n = 1)) %>%
      as.data.frame()
  } else {
    lag_df <- df %>%
      arrange(.data[[run_var]]) %>%
      group_by() %>%
      mutate(response_lag = lag(.data[[response_var]], n = 1)) %>%
      as.data.frame()
  }
  return(lag_df)
}

lag_produce_plot <- function(lag_df, response_var, group_fac_vec, run_var,
                             label_facets_with_variables) {
  lag_col <- "response_lag"
  plot_obj <- ggplot(data = lag_df)
  plot_obj <- plot_obj + geom_point(aes_string(x = lag_col, y = response_var))

  if (!is.null(group_fac_vec)) {
    facet_sym <- syms(group_fac_vec)
    if (label_facets_with_variables) {
      plot_obj <- plot_obj + facet_grid(
        cols = vars(!!!(facet_sym)), scales = "fixed",
        switch = "x", labeller = label_both
      )
    } else {
      plot_obj <- plot_obj + facet_grid(
        cols = vars(!!!(facet_sym)), scales = "fixed",
        switch = "x", labeller = "label_value"
      )
    }
  }
  plot_obj <- plot_obj + xlab(paste0(response_var, "_(i-1)")) + ylab(paste0(response_var, "_(i)"))
  if (run_var == "row_ind") {
    plot_obj <- plot_obj + ggtitle(paste0(
      "Lag Plot of Respose ", response_var,
      " using run variable Row Index"
    ))
  } else {
    plot_obj <- plot_obj + ggtitle(paste0(
      "Lag Plot of Respose ", response_var,
      " using run variable ", run_var
    ))
  }

  return(plot_obj)
}

normal_prob_compute_df <- function(df, y_var) {
  pplot <- ggplot(data = df) +
    geom_qq(aes_string(sample = y_var))
  ppdf <- ggplot_build(pplot)$data[[1]]
  return(ppdf[, c("sample", "theoretical")])
}

normal_prob_produce_plot <- function(df, y_var) {
  pplot <- ggplot(data = df) +
    geom_qq(aes_string(sample = y_var)) +
    stat_qq_line(aes_string(sample = y_var),
      size = 1,
      color = rgb(203, 203, 255, maxColorValue = 255), linetype = "dashed"
    ) +
    ggtitle(paste0("Normal Probability Plot for Variable ", y_var))
  return(pplot)
}

run_compute_df <- function(df, response_var, group_fac_vec, run_var) {
  if (is.null(run_var)) {
    run_var <- "row_ind"
    # Use seq 1:nrow() in case data frame row numbers have been renamed from a subset
    df$row_ind <- seq(1, nrow(df), 1)
  }
  return(df)
}

run_produce_plot <- function(run_df, response_var, group_fac_vec, run_var,
                             label_facets_with_variables) {
  plot_obj <- ggplot(data = run_df)
  plot_obj <- plot_obj + geom_point(aes_string(x = run_var, y = response_var))

  if (!is.null(group_fac_vec)) {
    facet_sym <- syms(group_fac_vec)
    if (label_facets_with_variables) {
      plot_obj <- plot_obj + facet_grid(
        cols = vars(!!!(facet_sym)), scales = "fixed",
        switch = "x", labeller = label_both
      )
    } else {
      plot_obj <- plot_obj + facet_grid(
        cols = vars(!!!(facet_sym)), scales = "fixed",
        switch = "x", labeller = "label_value"
      )
    }
  }
  if (run_var == "row_ind") {
    plot_obj <- plot_obj + xlab("Row Index (Run Number)")
    plot_obj <- plot_obj + ggtitle(paste0(
      "Run Plot of Respose ", response_var,
      " by run variable Row Index"
    ))
  } else {
    plot_obj <- plot_obj + xlab(paste0(run_var, " (Run Number)"))
    plot_obj <- plot_obj + ggtitle(paste0(
      "Run Plot of Respose ", response_var,
      " by run variable ", run_var
    ))
  }

  return(plot_obj)
}
