one_term_level_avg_df <- function(df, effects_var_vec, response_var,
                                  plot_NA_values = FALSE, alpha = 0.05) {
  #' Compute main effect category averages
  #'
  #' Table format: ("Factor", "Level", "<response_var>")
  #' where the "<response_var>" is the average (arithmetic mean) for that category

  ef_df <- main_effects_compute_df(df, effects_var_vec, response_var,
    plot_NA_values = plot_NA_values, alpha = alpha
  )
  names(ef_df)[names(ef_df) == "variable"] <- "Factor"
  names(ef_df)[names(ef_df) == "value"] <- "Level"

  return(ef_df[, c("Factor", "Level", response_var)])
}


effects_one_term_df <- function(df, effects_var_vec, response_var,
                                max_value_only = TRUE,
                                plot_NA_values = FALSE, alpha = 0.05) {
  #' Compute effect differences of main effects in effects df table format
  #'
  #' Table format: ("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")

  avg_df <- one_term_level_avg_df(df, effects_var_vec, response_var,
    plot_NA_values = plot_NA_values, alpha = alpha
  )
  effects_df <- data.frame(
    Factors = character(),
    "Effect" = numeric(),
    "abs(Effect)" = numeric(),
    Max_Level = character(),
    Min_Level = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(length(effects_var_vec))) {
    ef <- effects_var_vec[i]
    me_df <- avg_df[avg_df$Factor == ef, ]
    levels_list <- unique(me_df$Level)
    level_comb_list <- t(combn(levels_list, 2))
    # If there is one row, this gives the two values in the combination
    se_fac <- ef
    max_se_eff <- 0
    max_se_abs <- 0
    max_se <- ""
    min_se <- ""
    for (r in seq_len(nrow(level_comb_list))) {
      v1 <- level_comb_list[r, 1]
      v2 <- level_comb_list[r, 2]
      me_v1 <- me_df[me_df$Level == v1, ]
      me_v2 <- me_df[me_df$Level == v2, ]
      value_1 <- me_df[me_df$Level == v1, response_var]
      value_2 <- me_df[me_df$Level == v2, response_var]
      max_eff <- me_v1[, response_var]
      min_eff <- me_v2[, response_var]
      if (is.na(min_eff) || is.na(max_eff)) {
        se_max <- me_v1$Level
        se_min <- me_v2$Level
        se_eff <- NA
      } else if (min_eff > max_eff) {
        se_max <- me_v2$Level
        se_min <- me_v1$Level
        se_eff <- value_2 - value_1
      } else {
        se_max <- me_v1$Level
        se_min <- me_v2$Level
        se_eff <- value_1 - value_2
      }
      se_abs <- abs(se_eff)
      if (se_abs > max_se_abs) {
        max_se_abs <- se_abs
        max_se_eff <- se_eff
        max_se <- se_max
        min_se <- se_min
      }
      if (max_value_only == FALSE) {
        curr_ef_row <- c(se_fac, se_eff, se_abs, se_max, se_min)
        effects_df <- rbind(effects_df, curr_ef_row)
        names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
        effects_df$Factors <- as.character(effects_df$Factors)
        effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
        effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
        effects_df$Max_Level <- as.character(effects_df$Max_Level)
        effects_df$Min_Level <- as.character(effects_df$Min_Level)
      }
    }
    if (max_value_only == TRUE) {
      curr_ef_row <- c(se_fac, max_se_eff, max_se_abs, max_se, min_se)
      effects_df <- rbind(effects_df, curr_ef_row)
      names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
      effects_df$Factors <- as.character(effects_df$Factors)
      effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
      effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
      effects_df$Max_Level <- as.character(effects_df$Max_Level)
      effects_df$Min_Level <- as.character(effects_df$Min_Level)
    }
  }
  return(effects_df)
}

effects_two_term_level_df <- function(df, effect_var_1, effect_var_2, response_var,
                                      max_value_only = FALSE) {
  #' Compute all of the effect sizes for all of the different pairings of one combination
  #' of Factor variables
  #' @param df the data frame to compute effects on
  #' @param effect_var_1 the list of the main effect variables
  #' @param effect_var_1 the list of the two-term effect interactions
  #' @param response_var the response variable
  #' @param max_value_only if TRUE, only give the maximum value for each interaction
  #' If FALSE, compute all values
  #' Use main_effects_hsd_compute_df() to compute the effect sizes for one-term level
  #' changes
  effects_df <- data.frame(
    Factors = character(),
    "Effect" = numeric(),
    "abs(Effect)" = numeric(),
    Max_Level = character(),
    Min_Level = character(),
    stringsAsFactors = FALSE
  )
  # The averages of each two-level combo is computed with our tile_table method
  # Get extra precise here
  int_means_df <- tile_table_compute_df(df, effect_var_1, effect_var_2,
    response_var,
    use_margins = FALSE,
    digits = 11
  )
  ef <- paste(effect_var_1, effect_var_2, sep = "*")
  # Enumerate all combinations of levels. If a mean is missing, put NA for the Effect
  # picking an arbitrary one of the two as Maximum.
  # Design choice: Use unique options rather than levels
  var_1_levels <- unique(df[[effect_var_1]])
  var_2_levels <- unique(df[[effect_var_2]])
  curr_row_options <- t(combn(var_1_levels, 2))
  curr_col_options <- t(combn(var_2_levels, 2))
  min_level <- ""
  max_level <- ""
  max_effect <- 0
  max_abs_effect <- 0

  for (r in seq_len(nrow(curr_row_options))) {
    for (c in seq_len(nrow(curr_col_options))) {
      cc_row_m <- curr_row_options[r, 1]
      cc_row_p <- curr_row_options[r, 2]
      cc_col_m <- curr_col_options[c, 1]
      cc_col_p <- curr_col_options[c, 2]
      # Check for any missing row
      mean_m_m <- int_means_df[int_means_df[[effect_var_1]] == cc_row_m &
        int_means_df[[effect_var_2]] == cc_col_m, response_var]
      mean_m_p <- int_means_df[int_means_df[[effect_var_1]] == cc_row_m &
        int_means_df[[effect_var_2]] == cc_col_p, response_var]
      mean_p_m <- int_means_df[int_means_df[[effect_var_1]] == cc_row_p &
        int_means_df[[effect_var_2]] == cc_col_m, response_var]
      mean_p_p <- int_means_df[int_means_df[[effect_var_1]] == cc_row_p &
        int_means_df[[effect_var_2]] == cc_col_p, response_var]
      # We have a missing value if a length is 0 and not 1
      curr_effect <- NA
      curr_abs_effect <- NA
      v1 <- paste(curr_row_options[r, 1], curr_row_options[r, 2], sep = ";")
      v2 <- paste(curr_col_options[c, 1], curr_col_options[c, 2], sep = ";")
      v12 <- paste(v1, v2, sep = "|")
      low_level <- paste0(v12, "|-")
      high_level <- paste0(v12, "|+")
      # length==0 means we have a missing value, meaning we cannot compute
      # the effect
      if (length(mean_m_m) == 1 && length(mean_m_p) == 1 &&
        length(mean_p_m) == 1 && length(mean_p_p) == 1) {
        curr_effect <- (mean_m_m + mean_p_p) / 2 - (mean_p_m + mean_m_p) / 2
        curr_abs_effect <- abs(curr_effect)
        if (curr_effect < 0) {
          # Minus setting is higher
          low_level <- paste0(v12, "|-")
          high_level <- paste0(v12, "|+")
          # Condiser using: curr_effect <- -1*curr_effect
        } else {
          # Plus setting is higher
          low_level <- paste0(v12, "|+")
          high_level <- paste0(v12, "|-")
        }
        if (curr_abs_effect > max_abs_effect) {
          min_level <- low_level
          max_level <- high_level
          max_effect <- curr_effect
          max_abs_effect <- curr_abs_effect
        }
      }
      if (max_value_only == FALSE) {
        curr_ef_row <- c(ef, curr_effect, curr_abs_effect, high_level, low_level)
        effects_df <- rbind(effects_df, curr_ef_row)
        names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
        effects_df$Factors <- as.character(effects_df$Factors)
        effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
        effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
        effects_df$Max_Level <- as.character(effects_df$Max_Level)
        effects_df$Min_Level <- as.character(effects_df$Min_Level)
      }
    }
  }
  if (max_value_only == TRUE) {
    curr_ef_row <- c(ef, max_effect, max_abs_effect, max_level, min_level)
    effects_df <- rbind(effects_df, curr_ef_row)
    names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
    effects_df$Factors <- as.character(effects_df$Factors)
    effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
    effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
    effects_df$Max_Level <- as.character(effects_df$Max_Level)
    effects_df$Min_Level <- as.character(effects_df$Min_Level)
  }
  effects_df <- effects_df[order(effects_df$`abs(Effect)`, decreasing = TRUE), ]
  return(effects_df)
}

effects_two_term_df <- function(df, effects_var_vec, response_var,
                                max_value_only = TRUE,
                                plot_NA_values = FALSE, alpha = 0.05) {
  #' Compute effect differences of two_term interactionseffects in effects df table format
  #'
  #' Table format: ("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
  #' This assumes we have at least two factors
  effects_df <- data.frame(
    Factors = character(),
    "Effect" = numeric(),
    "abs(Effect)" = numeric(),
    Max_Level = character(),
    Min_Level = character(),
    stringsAsFactors = FALSE
  )

  effects_combn <- t(combn(effects_var_vec, 2))
  for (r in seq_len(nrow(effects_combn))) {
    ev1 <- effects_combn[r, 1]
    ev2 <- effects_combn[r, 2]
    e_int_df <- effects_two_term_level_df(df, ev1, ev2, response_var,
      max_value_only = max_value_only
    )
    effects_df <- rbind(effects_df, e_int_df)
    names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
    effects_df$Factors <- as.character(effects_df$Factors)
    effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
    effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
    effects_df$Max_Level <- as.character(effects_df$Max_Level)
    effects_df$Min_Level <- as.character(effects_df$Min_Level)
  }


  effects_df <- effects_df[order(effects_df$`abs(Effect)`, decreasing = TRUE), ]
  return(effects_df)
}

effects_compute_df <- function(df, effects_var_vec, response_var, interaction_depth = 2,
                               max_value_only = TRUE) {
  #' Produce a table of |effects| sizes for factors up to the specified interaction depth
  effects_df <- data.frame(
    Factors = character(),
    "Effect" = numeric(),
    "abs(Effect)" = numeric(),
    Max_Level = character(),
    Min_Level = character(),
    stringsAsFactors = FALSE
  )

  me_df <- effects_one_term_df(df, effects_var_vec, response_var, max_value_only = max_value_only)
  tt_df <- effects_two_term_df(df, effects_var_vec, response_var, max_value_only = max_value_only)
  effects_df <- rbind(effects_df, me_df)
  names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
  effects_df$Factors <- as.character(effects_df$Factors)
  effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
  effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
  effects_df$Max_Level <- as.character(effects_df$Max_Level)
  effects_df$Min_Level <- as.character(effects_df$Min_Level)
  effects_df <- rbind(effects_df, tt_df)
  names(effects_df) <- c("Factors", "Effect", "abs(Effect)", "Max_Level", "Min_Level")
  effects_df$Factors <- as.character(effects_df$Factors)
  effects_df$`Effect` <- as.numeric(as.character(effects_df$`Effect`))
  effects_df$`abs(Effect)` <- as.numeric(as.character(effects_df$`abs(Effect)`))
  effects_df$Max_Level <- as.character(effects_df$Max_Level)
  effects_df$Min_Level <- as.character(effects_df$Min_Level)

  effects_df <- effects_df[order(effects_df$`abs(Effect)`, decreasing = TRUE), ]
  return(effects_df)
}

effects_produce_plot <- function(mizr_df, effects_var_vec,
                                 grand_mean = NA) {
  #' Produce |effects| plot
  #'

  plot_df <- mizr_df
  # First order factors by absolute value
  plot_df <- plot_df[order(plot_df$`abs(Effect)`, decreasing = TRUE), ]
  # Our data frame is ordered by |Effect| so levels are in order
  plot_df$Factors <- factor(plot_df$Factors,
    levels = plot_df$Factors, ordered = FALSE
  )
  mizr_plot <- ggplot(data = plot_df) +
    geom_segment(aes(
      x = Factors, xend = Factors,
      y = 0, yend = `abs(Effect)`
    )) +
    geom_point(aes(x = Factors, y = `abs(Effect)`, fill = `abs(Effect)`),
      shape = 21, color = "black", size = 4
    ) +
    scale_fill_gradient(low = "#FFFFFF", high = "#0074FF", limits = c(0, NA)) +
    xlab("Factors (Main Effects or Interactions)") +
    ylab("abs(Effect)") +
    ggtitle(paste0(
      "|Effects| Plot for response ", response_var,
      "\nResponse Value of Grand Mean: ", round(grand_mean, 2)
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  return(mizr_plot)
}
