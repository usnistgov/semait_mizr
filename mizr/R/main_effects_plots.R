main_effects_compute_sd_df <- function(df, effects_var_vec, response_var,
                                       plot_NA_values = FALSE) {
  effects_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(effects_all_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    # We do not want to include NA Labels
    effect_df <- df[!is.na(df[[effect_var]]), ] %>%
      group_by_(effect_var) %>%
      summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    effect_rs_df <- melt(effect_df, id.var = c(response_var))
    if (!plot_NA_values) {
      effect_rs_df <- effect_rs_df[!is.na(effect_rs_df$value), ]
    }
    effect_rs_df$val_name <- paste(effect_var,
      as.character(effect_rs_df$value),
      sep = "_"
    )
    effects_all_df <- rbind(effects_all_df, effect_rs_df)
  }

  effects_all_df$value_fac <- factor(effects_all_df$val_name,
    levels = unique(effects_all_df$val_name)
  )

  # First, compute sd frame and augment effects_all_df
  sd_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(sd_all_df) <- c(response_var, "variable", "value")
  count_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(count_all_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(effects_var_vec))) {
    sd_var <- effects_var_vec[i]
    # We do not want to include NA Labels
    sd_df <- df[!is.na(df[[sd_var]]), ] %>%
      group_by_(sd_var) %>%
      summarize(response_var = sd(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    count_df <- df[!is.na(df[[sd_var]]), ] %>%
      group_by_(sd_var) %>%
      summarize(response_var = (length(.data[[response_var]]))) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()

    sd_rs_df <- melt(sd_df, id.var = c(response_var))
    sd_all_df <- rbind(sd_all_df, sd_rs_df)
    count_rs_df <- melt(count_df, id.var = c(response_var))
    count_all_df <- rbind(count_all_df, count_rs_df)
  }
  effects_all_df["sd_upper"] <- effects_all_df[[response_var]] + sd_all_df[[response_var]]
  effects_all_df["sd_lower"] <- effects_all_df[[response_var]] - sd_all_df[[response_var]]
  effects_all_df["sd"] <- sd_all_df[[response_var]]
  return(effects_all_df)
}

main_effects_compute_df <- function(df, effects_var_vec, response_var,
                                    plot_NA_values = FALSE,
                                    bar_ci = FALSE, alpha = 0.05) {
  effects_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(effects_all_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    # We do not want to include NA Labels
    effect_df <- df[!is.na(df[[effect_var]]), ] %>%
      group_by_(effect_var) %>%
      summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    effect_rs_df <- melt(effect_df, id.var = c(response_var))
    if (!plot_NA_values) {
      effect_rs_df <- effect_rs_df[!is.na(effect_rs_df$value), ]
    }
    effect_rs_df$val_name <- paste(effect_var,
      as.character(effect_rs_df$value),
      sep = "_"
    )
    effects_all_df <- rbind(effects_all_df, effect_rs_df)
  }

  effects_all_df$value_fac <- factor(effects_all_df$val_name,
    levels = unique(effects_all_df$val_name)
  )

  if (bar_ci) {
    # First, compute se frame and augment effects_all_df
    sd_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(sd_all_df) <- c(response_var, "variable", "value")
    count_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(count_all_df) <- c(response_var, "variable", "value")
    for (i in seq_len(length(effects_var_vec))) {
      sd_var <- effects_var_vec[i]
      # We do not want to include NA Labels
      sd_df <- df[!is.na(df[[sd_var]]), ] %>%
        group_by_(sd_var) %>%
        summarize(response_var = sd(.data[[response_var]], na.rm = TRUE)) %>%
        rename(!!(response_var) := response_var) %>%
        as.data.frame()
      count_df <- df[!is.na(df[[sd_var]]), ] %>%
        group_by_(sd_var) %>%
        summarize(response_var = (length(.data[[response_var]]))) %>%
        rename(!!(response_var) := response_var) %>%
        as.data.frame()

      sd_rs_df <- melt(sd_df, id.var = c(response_var))
      sd_all_df <- rbind(sd_all_df, sd_rs_df)
      count_rs_df <- melt(count_df, id.var = c(response_var))
      count_all_df <- rbind(count_all_df, count_rs_df)
    }
    ci_all_df <- qnorm(1 - (alpha / 2)) * (sd_all_df[[response_var]] /
      sqrt(count_all_df[[response_var]]))
    effects_all_df["upper"] <- effects_all_df[[response_var]] + ci_all_df
    effects_all_df["lower"] <- effects_all_df[[response_var]] - ci_all_df
    effects_all_df["ci_error"] <- ci_all_df
  }
  return(effects_all_df)
}

main_effects_compute_labels <- function(df, effects_var_vec, response_var,
                                        plot_NA_values = FALSE, bar_ci = FALSE, alpha = 0.05) {
  labels_vec <- c()
  effects_all_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(effects_all_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    effect_df <- df[!is.na(df[[effect_var]]), ] %>%
      group_by_(effect_var) %>%
      summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    effect_rs_df <- melt(effect_df, id.var = c(response_var))
    if (!plot_NA_values) {
      effect_rs_df <- effect_rs_df[!is.na(effect_rs_df$value), ]
    }
    effect_rs_df$val_name <- paste(effect_var,
      as.character(effect_rs_df$value),
      sep = "_"
    )
    labels_vec <- c(labels_vec, unique(effect_rs_df$value))
    effects_all_df <- rbind(effects_all_df, effect_rs_df)
  }

  return(labels_vec)
}

compute_grand_mean <- function(df, effects_var_vec, response_var, weighted = TRUE) {
  #' Computes the grand mean of a data frame
  #'
  #' The grand mean is an average of each of the groups averages, so we
  #' compute the grand mean according to the grouping specified by effects_var_vec
  #' @param df the data frame.
  #' @param effects_var_vec the vector of factors to group by for the average.
  #' @param response_var the response variable.
  #' @param weighted book a boolean to determine if the grand mean average is weighted by effect
  #' or computed unweighted.
  #' If [], this function will just return the average of all data points. This is one of
  #' the only functions that can take an empty [] effects_var_vec.
  #'
  #' Missing values are removed from the averaging, making it necessary to average all of the groups
  #'

  # If we have a length 0 effects_var_vec, we just take the global mean
  mean_val <- mean(df[[response_var]], na.rm = TRUE)
  if (length(effects_var_vec >= 1) && weighted == TRUE) {
    mean_df <- compute_mean_df(df, effects_var_vec, response_var)
    mean_val <- mean(mean_df[[response_var]], na.rm = TRUE)
  }
  return(mean_val)
}

main_effects_produce_plot <- function(effects_all_df, labels_vec, mean_val,
                                      effects_var_vec, response_var, sd_df = NULL,
                                      plot_NA_values = FALSE,
                                      bar_ci = FALSE, alpha = 0.05, bar_sd = FALSE) {
  #' Produces a main effects plot
  #' @param effects_all_df the data frame with the data
  #' @param labels_vec the vector of all of the lables
  #' @param mean_val the grand mean.
  #' @param effects_var_vec the list of column names of the effect factors, specified in order
  #' @param response_var the column name of the response variable
  #' @param plot_NA_values FALSE by default. If true, plot the main effects that are missing.
  #' @param bar_ci If TRUE, plot bars that are the standard error.
  #' @param alpha the confidence level to produce a 1 - alpha confidence interval
  #' @param bar_sd FALSE by default. If TRUE, plot the standard deviation bars.
  #' FALSE by default. Here sdn is computed similar to a standard error on the mean.
  #' @return plot_obj a plot object

  upper_var <- "upper"
  lower_var <- "lower"
  val_fac_var <- "value_fac"
  sd_upper_var <- "sd_upper"
  sd_lower_var <- "sd_lower"

  y_unit <- ""

  plot_obj <- ggplot(data = effects_all_df) +
    geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = 1)

  if (bar_ci) {
    # now add the bars using geom_errorbar
    plot_obj <- plot_obj + geom_errorbar(
      aes_string(
        x = val_fac_var, ymin = lower_var,
        ymax = upper_var
      ),
      group = 1, width = 0.60, size = 1
    )
  }
  if (!(is.null(sd_df)) && bar_sd) {
    plot_obj <- plot_obj +
      geom_errorbar(
        data = sd_df, aes_string(
          x = val_fac_var, ymin = sd_lower_var,
          ymax = sd_upper_var, group = 1
        ),
        width = 0.40, size = 0.5, color = rgb(0, 0, 255, maxColorValue = 255),
        linetype = "longdash"
      )
  }

  plot_obj <- plot_obj + geom_point(
    data = effects_all_df,
    aes_string(x = val_fac_var, y = response_var, group = 1),
    size = 4
  ) +
    geom_path(aes_string(x = val_fac_var, y = response_var, group = 1), linetype = "dashed") +
    scale_x_discrete(breaks = unique(effects_all_df$value_fac), labels = labels_vec) +
    facet_grid(. ~ variable, scales = "free_x", switch = "x") +
    scale_color_discrete(guide = FALSE) + xlab("Factor") + ylab(response_var) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (bar_ci && bar_sd) {
    plot_obj <- plot_obj + ggtitle(paste0(
      "Main Effects Plot,\n", 1 - alpha,
      "%-CI error bars in solid black,",
      "with Standard Deviation bar in dashed blue"
    ))
  } else {
    plot_obj <- plot_obj + ggtitle("Main Effects Plot")
  }


  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }

  return(plot_obj)
}


main_effects_hsd_compute_df <- function(df, effects_var_vec, response_var,
                                        plot_NA_values = FALSE, alpha = 0.95) {
  # Construct formula from vectors with no interaction
  formula <- mizr_make_formula(effects_var_vec, response_var, interaction_depth = 1)

  curr_mod <- lm(formula, data = df, contrasts = c("contr.sum", "contr.poly"))
  curr_aov_I <- aov(curr_mod)
  curr_HSD <- TukeyHSD(curr_aov_I, conf.level = 1 - alpha)
  # Now extract data frame with CI
  hsd_df <- data.frame(matrix(ncol = 6, nrow = 0))
  hsd_names <- c("value", "diff", "lower", "upper", "p_adjust", "variable")
  colnames(hsd_df) <- hsd_names
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    j <- match(effect_var, names(curr_HSD))
    if (is.na(j)) {
      print(paste0(
        "Response Factor ", effect_var, " not in ANOVA because it provides no",
        " additional variation. Continuing without adding into data frame."
      ))
      next
    }
    curr_df <- as.data.frame(curr_HSD[j]) %>%
      rownames_to_column("variable") %>%
      as.data.frame()
    names(curr_df) <- c("value", "diff", "lower", "upper", "p_adjust")
    # Fix dash names: want 1 - -1, not 1--1, and likewise spaced subtraction
    curr_df$variable <- names(curr_HSD[j])
    curr_df$val_name <- paste(names(curr_HSD[j]), as.character(curr_df$value), sep = "_")
    # Only sub first occurrence so second dash does not become a problem
    curr_df$value <- sub("([A-Z]*[0-9]*)-([A-Z]*[0-9]*)", "\\1 - \\2", curr_df$value)
    curr_df$value <- sub("-([0-9.]+)", "(-\\1)", curr_df$value)
    hsd_df <- rbind(hsd_df, curr_df)
  }
  hsd_df$variable <- factor(hsd_df$variable, levels = unique(hsd_df$variable))
  hsd_df$value_fac <- factor(hsd_df$val_name, levels = unique(hsd_df$val_name))

  return(hsd_df)
}

main_effects_hsd_labels <- function(df, effects_var_vec, response_var,
                                    plot_NA_values, alpha) {
  labels_vec <- c()
  # Construct formula from vectors
  formula_str <- paste0(response_var, " ~ ")
  first_var <- TRUE
  for (effect_var in effects_var_vec) {
    if (first_var) {
      formula_str <- paste0(formula_str, effect_var)
      first_var <- FALSE
    } else {
      formula_str <- paste0(formula_str, " + ", effect_var)
    }
  }
  curr_mod <- lm(formula_str, data = df, contrasts = c("contr.sum", "contr.poly"))
  curr_aov_I <- aov(curr_mod)
  curr_HSD <- TukeyHSD(curr_aov_I, conf.level = 1 - alpha)
  # Now extract data frame with CI
  hsd_df <- data.frame(matrix(ncol = 6, nrow = 0))
  hsd_names <- c("value", "diff", "lower", "upper", "p_adjust", "variable")
  colnames(hsd_df) <- hsd_names
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    j <- match(effect_var, names(curr_HSD))
    if (is.na(j)) {
      next
    }
    curr_df <- as.data.frame(curr_HSD[j]) %>%
      rownames_to_column("variable") %>%
      as.data.frame()
    names(curr_df) <- c("value", "diff", "lower", "upper", "p_adjust")
    # Fix dash names: want 1 - -1, not 1--1, and likewise spaced subtraction
    curr_df$variable <- names(curr_HSD[j])
    curr_df$val_name <- paste(names(curr_HSD[j]), as.character(curr_df$value), sep = "_")
    curr_df$value <- sub("([A-Z]*[0-9]*)-([A-Z]*[0-9]*)", "\\1 - \\2", curr_df$value)
    curr_df$value <- sub("-([0-9.]+)", "(-\\1)", curr_df$value)
    labels_vec <- c(labels_vec, unique(curr_df$value))
    hsd_df <- rbind(hsd_df, curr_df)
  }
  hsd_df$value_fac <- factor(hsd_df$val_name, levels = unique(hsd_df$val_name))
  return(labels_vec)
}

main_effects_hsd_produce_plot <- function(mizr_df, labels_vec, effects_var_vec, response_var,
                                          plot_NA_values, alpha) {
  upper_var <- "upper"
  lower_var <- "lower"
  val_fac_var <- "value_fac"
  diff_var <- "diff"

  y_unit <- ""

  plot_obj <- ggplot(data = mizr_df)

  # now add the bars using geom_errorbar
  plot_obj <- plot_obj + geom_errorbar(aes_string(
    x = val_fac_var, ymin = lower_var,
    ymax = upper_var,
    group = 1,
    width = 0.30
  ))

  # Mow add the points and the intercept
  plot_obj <- plot_obj + geom_point(aes_string(x = val_fac_var, y = diff_var, group = 1),
    size = 4
  ) +
    scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
    facet_grid(. ~ variable, scales = "free_x", switch = "x") +
    geom_hline(yintercept = 0, lwd = 0.6, color = "gray50", linetype = 1) +
    xlab("Factor") + ylab("Difference") +
    ggtitle(paste0(
      "Tukey Honest Significance Difference (HSD) Plot \non Effects, alpha = ",
      alpha
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }

  return(plot_obj)
}

main_effects_mar_compute_df <- function(df, variable_fac, value_fac, response_var) {
  x_form_var <- paste(variable_fac, value_fac, sep = " + ")
  y_form_var <- response_var
  effects_all_df <- aggregate(
    formula = reformulate(x_form_var, y_form_var), data = df,
    FUN = mean, na.rm = TRUE
  )
  return(effects_all_df)
}

main_effects_mar_labels <- function(effects_all_df, variable_fac, value_fac,
                                    response_var) {
  labels_vec <- effects_all_df$value_fac
  return(labels_vec)
}

main_effects_mar_produce_plot <- function(effects_all_df, labels_vec, variable_fac,
                                          value_fac, response_var) {
  #' Produces a main effects plot with a pre-computed intermediary data frame
  #' @param effects_all_df the data frame with the data, already aggregated
  #' @param label_vec the column names of the labels.
  #' @param variable_fac the column name representing the column variables
  #' @param value_fac the column name representing the values of the factors in
  #' column `variable_fac`
  #' @param response_var the column name of the response variable
  #' @return plot_obj a plot object

  y_unit <- ""
  dot_var <- "."
  plot_obj <- ggplot(data = effects_all_df) +
    geom_point(aes_string(x = value_fac, y = response_var, group = 1, color = variable_fac)) +
    geom_path(aes_string(x = value_fac, y = response_var, group = 1, color = variable_fac)) +
    facet_grid(reformulate(variable_fac, dot_var), scales = "free_x", switch = "x") +
    scale_color_discrete(guide = FALSE) +
    xlab("Factor (Facet) and Level Difference (Ticks)") +
    ylab(response_var) +
    ggtitle("Main Effects Plot") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }

  return(plot_obj)
}
