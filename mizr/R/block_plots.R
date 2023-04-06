compute_mean_df <- function(df, effects_var_vec, response_var) {
  #' Basic function to group by all factors and compute a mean
  #'
  #' This is called by other methods
  group_sym <- syms(effects_var_vec)
  mean_df <- df %>%
    group_by(!!!group_sym) %>%
    summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
    rename(!!(response_var) := response_var) %>%
    as.data.frame()
  return(mean_df)
}

block_compute_df <- function(df, x_var_vec, treatment_var, response_var,
                             comp_func = mean, ...) {
  # Produce the aggregated data frame
  group_sym <- syms(c(x_var_vec, treatment_var))

  if (length(list(...)) == 0) {
    block_df <- df %>%
      group_by(!!!group_sym) %>%
      summarize(response_var = comp_func(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
  } else {
    block_df <- df %>%
      group_by(!!!group_sym) %>%
      summarize(response_var = comp_func(.data[[response_var]], ...)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
  }
  # add default argument of na.rm = TRUE if no custom function or args are specified
  # To refer to the arguments, use  "arguments <- list(...)"
  return(block_df)
}

block_compute_box_df <- function(block_df, x_var_vec, treatment_var, response_var,
                                 comp_func = mean, ...) {
  # Produce the boxes
  block_group_sym <- syms(x_var_vec)
  block_min_df <- block_df %>%
    group_by(!!!block_group_sym) %>%
    summarize(response_min = min(.data[[response_var]], na.rm = TRUE)) %>%
    as.data.frame()
  block_max_df <- block_df %>%
    group_by(!!!block_group_sym) %>%
    summarize(response_max = max(.data[[response_var]], na.rm = TRUE)) %>%
    as.data.frame()
  block_box_df <- merge(block_min_df, block_max_df)
  block_box_df$response_mid <- (block_box_df$response_max + block_box_df$response_min) / 2
  # height_buffer is proportional to the height of the window
  height_buffer <- 0.05 * (max(block_box_df$response_max) - min(block_box_df$response_min))
  block_box_df$response_height <- (block_box_df$response_max - block_box_df$response_min) +
    height_buffer
  return(block_box_df)
}

block_produce_plot <- function(block_df, block_box_df, x_var_vec,
                               treatment_var, response_var,
                               value_text_size = rel(2.0)) {

  #' Produces a block plot
  #' @param block_df the data frame after computations
  #' @param block_box_df the data frame for each of the block rectangle coordinates.
  #' @param x_var_vec the vector of columns to display on the x-axis.
  #' Multiple factors can be combined to enumerate out the different values.
  #' @param treatment_var the blocking factor or effect to plot separately as different symbols
  #' @param response_var the response column to plot on the y axis.
  #' @param value_text_size the size of the text within the plot. Often specified
  #' as rel(Y) or a relative text size
  #' @return plot_obj a plot object with this plot to display

  short_var <- "short_label"
  response_mid_var <- "response_mid"
  response_height_var <- "response_height"
  full_var <- "full_label"

  y_unit <- ""

  # Works for factors and continuous variables
  var_key <- sort(unique(block_df[[treatment_var]]))
  geom_text_object <- get_custom_text_object(var_key,
    cols = scales::hue_pal()(length(var_key))
  )

  facet_sym <- syms(x_var_vec)

  if (length(x_var_vec) > 1) {
    block_df$x_var <- as.factor(do.call(paste, c(block_df[x_var_vec], sep = " | ")))
    block_box_df$x_var <- as.factor(do.call(paste, c(block_box_df[x_var_vec], sep = " | ")))
    x_var_str <- paste(x_var_vec, collapse = " | ")
  } else {
    block_df$x_var <- block_df[[x_var_vec]]
    block_box_df$x_var <- block_box_df[[x_var_vec]]
    x_var_str <- x_var_vec
  }
  x_var_col <- "x_var"

  # For now, since the text label is the entire label, the legend does not give any information.
  # The legend will be useful when the plotted text is an abbreviation and
  # the legend is the full name.
  plot_obj <- ggplot(data = block_df)

  # No metadata, use original values as labels
  plot_obj <- plot_obj + geom_text_custom(
    aes_string(
      x = x_var_col, y = response_var,
      label = treatment_var, color = treatment_var
    ),
    size = value_text_size,
    geom = geom_text_object
  )


  plot_obj <- plot_obj + geom_tile(
    data = block_box_df, aes_string(
      x = x_var_col,
      y = response_mid_var,
      height = response_height_var,
      width = 0.8
    ), fill = NA,
    color = "black", size = 0.5
  ) +
    ggtitle(paste0("Block Plot with Blocking Factor ", treatment_var)) +
    xlab("Factors") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }

  return(plot_obj)
}

binom_p_value_br <- function(sign_test_df_row_part, alpha = 0.05) {
  bin_conf_level <- 1 - alpha
  temp_out <- binom.test(sign_test_df_row_part[1],
    sign_test_df_row_part[2],
    p = 0.5, alternative = "two.sided",
    conf.level = bin_conf_level
  )
  p_val <- temp_out$p.value
  return(p_val)
}

sign_test_compute_df <- function(df, x_var_vec, treatment_var, response_var,
                                 higher_is_better = TRUE, use_margins = TRUE, alpha = 0.05,
                                 p_adj = "bonferonni",
                                 comp_func = mean, ...) {
  #' Computes a sign test rank df from the block plot
  #'
  #'
  #'
  #'

  # This uses the block_df computed from block_compute_df()
  block_df <- block_compute_df(
    df, x_var_vec, treatment_var, response_var,
    comp_func, ...
  )

  sign_test_df <- data.frame(
    treatment_1 = character(),
    treatment_2 = character(),
    num_comparisons = numeric(),
    num_1_better_or_equal_than_2 = numeric(),
    num_1_equal_to_2 = numeric(),
    stringsAsFactors = FALSE
  )

  # Enumerate all combinations of levels. If a mean is missing, put NA for the Effect
  # picking an arbitrary one of the two as Maximum.
  # Design choice: Use unique options rather than levels
  var_levels <- unique(df[[treatment_var]])
  curr_options <- t(combn(var_levels, 2))
  num_comparisons <- 0
  num_1_better_or_equal_than_2 <- 0

  treatment_var_1 <- paste(treatment_var, "1", sep = "_")
  treatment_var_2 <- paste(treatment_var, "2", sep = "_")
  response_var_1 <- paste(response_var, "1", sep = "_")
  response_var_2 <- paste(response_var, "2", sep = "_")

  for (r in seq_len(nrow(curr_options))) {
    treatment_1 <- curr_options[r, 1]
    treatment_2 <- curr_options[r, 2]
    t1_df <- block_df[block_df[[treatment_var]] == treatment_1, ]
    t2_df <- block_df[block_df[[treatment_var]] == treatment_2, ]
    colnames(t1_df)[which(names(t1_df) == treatment_var)] <- treatment_var_1
    colnames(t2_df)[which(names(t2_df) == treatment_var)] <- treatment_var_2
    colnames(t1_df)[which(names(t1_df) == response_var)] <- response_var_1
    colnames(t2_df)[which(names(t2_df) == response_var)] <- response_var_2
    t_df <- merge(t1_df, t2_df)
    t_df$diff <- t_df[[response_var_1]] - t_df[[response_var_2]]
    num_comparisons <- nrow(t_df)
    num_ge_comparisons <- nrow(t_df[t_df$diff >= 0, ])
    num_le_comparisons <- nrow(t_df[t_df$diff <= 0, ])
    num_eq_comparisons <- nrow(t_df[t_df$diff == 0, ])
    num_1_better_or_equal_than_2 <- nrow(t_df[])
    # Now construct two rows from this computation, switching these axes
    curr_1_row <- c(
      as.character(treatment_1), as.character(treatment_2),
      num_comparisons, num_ge_comparisons, num_eq_comparisons
    )
    curr_2_row <- c(
      as.character(treatment_2), as.character(treatment_1),
      num_comparisons, num_le_comparisons, num_eq_comparisons
    )
    if (!higher_is_better) {
      curr_1_row <- c(
        as.character(treatment_1), as.character(treatment_2),
        num_comparisons, num_le_comparisons, num_eq_comparisons
      )
      curr_2_row <- c(
        as.character(treatment_2), as.character(treatment_1),
        num_comparisons, num_ge_comparisons, num_eq_comparisons
      )
    }

    sign_test_df <- rbind(sign_test_df, curr_1_row)
    names(sign_test_df) <- c(
      "treatment_1", "treatment_2",
      "num_comparisons", "num_1_better_or_equal_than_2",
      "num_1_equal_to_2"
    )
    sign_test_df <- rbind(sign_test_df, curr_2_row)
    names(sign_test_df) <- c(
      "treatment_1", "treatment_2",
      "num_comparisons", "num_1_better_or_equal_than_2",
      "num_1_equal_to_2"
    )
  }
  sign_test_df$treatment_1 <- factor(sign_test_df$treatment_1,
    levels = levels(df[[treatment_var]])
  )
  sign_test_df$treatment_2 <- factor(sign_test_df$treatment_2,
    levels = levels(df[[treatment_var]])
  )
  sign_test_df$num_comparisons <- as.numeric(sign_test_df$num_comparisons)
  sign_test_df$num_1_better_or_equal_than_2 <-
    as.numeric(sign_test_df$num_1_better_or_equal_than_2)
  sign_test_df$num_1_equal_to_2 <- as.numeric(sign_test_df$num_1_equal_to_2)

  # Get levels for the treatment in descending order of total number of geq comparisons
  block_sum_df <- aggregate(num_1_better_or_equal_than_2 ~ treatment_1,
    data = sign_test_df, FUN = sum, na.rm = TRUE
  )
  block_sum_df <- block_sum_df[order(-block_sum_df$num_1_better_or_equal_than_2), ]
  treatment_levels <- rev(unique(block_sum_df$treatment_1))
  sign_test_df$treatment_1 <- factor(sign_test_df$treatment_1,
    levels = treatment_levels
  )
  sign_test_df$treatment_2 <- factor(sign_test_df$treatment_2,
    levels = treatment_levels
  )

  if (use_margins) {
    block_sum_df <- aggregate(
      cbind(
        num_1_better_or_equal_than_2, num_1_equal_to_2,
        num_comparisons
      ) ~ treatment_1,
      data = sign_test_df, FUN = sum, na.rm = TRUE
    )
    block_sum_df <- block_sum_df[order(-block_sum_df$num_1_better_or_equal_than_2), ]
    block_sum_df$treatment_2 <- "(all)"
    treatment_levels <- c("(all)", rev(unique(as.character(block_sum_df$treatment_1))))
    # Append the sum to sign test rank
    block_sum_df <- block_sum_df[, names(sign_test_df)]
    sign_test_df <- rbind(sign_test_df, block_sum_df)
    sign_test_df$treatment_1 <- factor(as.character(sign_test_df$treatment_1),
      levels = treatment_levels
    )
    sign_test_df$treatment_2 <- factor(as.character(sign_test_df$treatment_2),
      levels = treatment_levels
    )
  }
  sign_test_df$frac_1_better_or_equal_than_2 <-
    sign_test_df$num_1_better_or_equal_than_2 / sign_test_df$num_comparisons
  sign_test_df$frac_1_equal_to_2 <-
    sign_test_df$num_1_equal_to_2 / sign_test_df$num_comparisons
  sign_test_df$num_1_better_than_2 <-
    sign_test_df$num_1_better_or_equal_than_2 - sign_test_df$num_1_equal_to_2
  sign_test_df$frac_1_better_than_2 <-
    sign_test_df$num_1_better_than_2 / sign_test_df$num_comparisons
  # Get p-values
  sign_test_df$p_values <- apply(
    sign_test_df[, c(
      "num_1_better_or_equal_than_2",
      "num_comparisons"
    )],
    MARGIN = 1, FUN = binom_p_value_br, alpha = alpha
  )
  sign_test_df$p_values_str <- apply(sign_test_df[, c("num_1_better_than_2", "num_comparisons")],
    MARGIN = 1, FUN = binom_p_value_br, alpha = alpha
  )
  sign_test_df$p_values_bf_adj <- NA
  sign_test_df[sign_test_df$treatment_2 != "(all)", ]$p_values_bf_adj <-
    p.adjust(sign_test_df[sign_test_df$treatment_2 != "(all)", ]$p_values, method = "bonferroni")
  sign_test_df$p_values_str_bf_adj <- NA
  sign_test_df[sign_test_df$treatment_2 != "(all)", ]$p_values_str_bf_adj <-
    p.adjust(sign_test_df[sign_test_df$treatment_2 != "(all)", ]$p_values_str,
      method = "bonferroni"
    )
  if (use_margins) {
    sign_test_df[sign_test_df$treatment_2 == "(all)", ]$p_values <- NA
    sign_test_df[sign_test_df$treatment_2 == "(all)", ]$p_values_bf_adj <- NA
  }
  return(sign_test_df)
}

sign_test_produce_plot <- function(sign_test_df, x_var_vec, treatment_var, response_var,
                                   use_margins = FALSE,
                                   include_equal_comparisons = TRUE, alpha = 0.05,
                                   p_adj = "bonferonni",
                                   low_color = "#FF0000",
                                   mid_color = "#FFFFFF",
                                   high_color = "#0074FF",
                                   tile_text_size = rel(2.0)) {
  #' Produces a sign test rank plot
  # We don't want the 0 numbers to appear since they are blank

  min_val <- 0
  max_val <- 1
  mid_val <- 0.5
  # color by proportion value rather than p-value significance for now
  num_comparisons <- min(sign_test_df$num_comparisons)
  num_treatment_levels <-
    length(unique(sign_test_df[sign_test_df$treatment_1 != "(all)", ]$treatment_1))
  num_treatment_comparisons <- num_treatment_levels * (num_treatment_levels - 1) / 2
  block_sum_df <- aggregate(
    cbind(
      num_1_better_or_equal_than_2, num_1_equal_to_2,
      num_comparisons
    ) ~ treatment_1,
    data = sign_test_df, FUN = sum, na.rm = TRUE
  )
  block_sum_df <- block_sum_df[order(-block_sum_df$num_1_better_or_equal_than_2), ]
  treatment_1_levels <- c("(all)", rev(unique(as.character(block_sum_df$treatment_1))))
  treatment_2_levels <- c(unique(as.character(block_sum_df$treatment_1)), "(all)")
  sign_test_df$treatment_1 <- factor(as.character(sign_test_df$treatment_1),
    levels = treatment_1_levels
  )
  sign_test_df$treatment_2 <- factor(as.character(sign_test_df$treatment_2),
    levels = treatment_2_levels
  )

  sign_test_df$fill_col <- sign_test_df$frac_1_better_or_equal_than_2


  sign_test_df$p_value_str <- paste0(
    "p = ",
    as.character(round(sign_test_df$p_values, 3))
  )
  if (p_adj == "bonferroni") {
    sign_test_df$p_value_str <- paste0(
      "p = ",
      as.character(round(sign_test_df$p_values_bf_adj, 3))
    )
  }
  sign_test_df$text_label <-
    paste0(
      as.character(round(sign_test_df$frac_1_better_or_equal_than_2, digits = 2)), "; (",
      as.character(sign_test_df$num_1_better_or_equal_than_2), "/",
      as.character(sign_test_df$num_comparisons), ")", "\n",
      as.character(sign_test_df$p_value_str)
    )
  sign_test_df$margin_text_label <-
    paste0(
      as.character(round(sign_test_df$frac_1_better_or_equal_than_2, digits = 2)), "; (",
      as.character(sign_test_df$num_1_better_or_equal_than_2), "/",
      as.character(sign_test_df$num_comparisons), ")"
    )

  color_name_str <- "Fraction of comparisons \nrow is better than or \nequal to the column"
  title_str <-
    paste0(
      "Sign Test Comparison Table of Treatment ", treatment_var, " over Response ", response_var,
      "\nWith ", num_comparisons, " comparisons between each of ", num_treatment_comparisons,
      " treatment pairs, \np_adj: ", p_adj
    )
  if (!include_equal_comparisons) {
    sign_test_df$fill_col <- sign_test_df$frac_1_better_than_2
    sign_test_df$text_label <-
      paste0(
        as.character(round(sign_test_df$frac_1_better_than_2, digits = 2)), "\n(",
        as.character(sign_test_df$num_1_better_than_2), "/",
        as.character(sign_test_df$num_comparisons), ")"
      )
    color_name_str <- "Fraction of comparisons \nrow is strictly better than \nthe column"
    title_str <-
      paste0(
        "Sign Test Comparison Table of Treatment ", treatment_var, " over Response ", response_var,
        "\nWith ", num_comparisons,
        " comparisons between each of ", num_treatment_comparisons,
        " treatment pairs excluding equal comparisons, \np_adj: ", p_adj
      )
    block_sum_df <- aggregate(
      cbind(
        num_1_better_than_2, num_1_equal_to_2,
        num_comparisons
      ) ~ treatment_1,
      data = sign_test_df, FUN = sum, na.rm = TRUE
    )
    block_sum_df <- block_sum_df[order(-block_sum_df$num_1_better_than_2), ]
    treatment_1_levels <- c("(all)", rev(unique(as.character(block_sum_df$treatment_1))))
    treatment_2_levels <- c(unique(as.character(block_sum_df$treatment_1)), "(all)")
    sign_test_df$treatment_1 <- factor(as.character(sign_test_df$treatment_1),
      levels = treatment_1_levels
    )
    sign_test_df$treatment_2 <- factor(as.character(sign_test_df$treatment_2),
      levels = treatment_2_levels
    )
  }
  # Add a margin for block_sum
  plot_df <- sign_test_df[sign_test_df$treatment_2 != "(all)", ]

  mizr_plot <- ggplot(data = sign_test_df, aes(x = treatment_2, y = treatment_1)) +
    geom_blank() +
    geom_tile(
      data = plot_df, aes(
        x = treatment_2, y = treatment_1,
        fill = fill_col
      ),
      color = rgb(0, 0, 0)
    ) +
    geom_text(
      data = plot_df,
      aes(x = treatment_2, y = treatment_1, label = text_label),
      size = tile_text_size
    )

  if (use_margins) {
    # Normalize the fill to account for the number of comparisons
    margin_df <- sign_test_df[sign_test_df$treatment_2 == "(all)", ]

    mizr_plot <- mizr_plot +
      geom_tile(
        data = margin_df,
        aes(
          x = treatment_2, y = treatment_1,
          fill = fill_col
        ),
        color = rgb(255, 255, 255, maxColorValue = 255)
      ) +
      geom_text(
        data = margin_df,
        aes(x = treatment_2, y = treatment_1, label = margin_text_label),
        size = 0.85 * tile_text_size
      )
  }

  col_name_str <- "Fraction of comparisons \nrow is better than or \nequal to the column"

  if (min_val < max_val) {
    # There is currently an issue where just having a midpoint involves 0.5 (the midpoint)
    # being colored slightly red.
    mizr_plot <- mizr_plot +
      scale_fill_gradientn(
        colors = c(low_color, mid_color, high_color),
        values = scales::rescale(c(min_val, mid_val, max_val)),
        name = color_name_str
      )
  } else {
    mizr_plot <- mizr_plot +
      scale_fill_gradientn(
        colors = c(low_color, mid_color, high_color),
        name = color_name_str
      )
  }
  mizr_plot <- mizr_plot +
    xlab(paste0("Treatment 2 Factor (Column)\n", treatment_var)) +
    ylab(paste0("Treatment 1 Factor (Row)\n", treatment_var)) +
    scale_x_discrete(position = "top") +
    ggtitle(title_str) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(mizr_plot)
}
