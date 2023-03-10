interact_compare_compute_df <- function(df, effects_var_vec, treatment_var,
                                        response_var,
                                        bar_ci, plot_NA_values = FALSE,
                                        comp_func = mean, ...) {
  labels_vec <- c()
  effects_all_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(effects_all_df) <- c(response_var, treatment_var, "variable", "value")
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    if (length(list(...)) == 0) {
      effect_df <- df %>%
        group_by_(effect_var, treatment_var) %>%
        summarize(response_var = comp_func(.data[[response_var]], na.rm = TRUE)) %>%
        rename(!!(response_var) := response_var) %>%
        as.data.frame()
    } else {
      effect_df <- df %>%
        group_by_(effect_var, treatment_var) %>%
        summarize(response_var = comp_func(.data[[response_var]], ...)) %>%
        rename(!!(response_var) := response_var) %>%
        as.data.frame()
    }
    effect_rs_df <- melt(effect_df, id.var = c(response_var, treatment_var))
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

  effects_all_df$value_fac <- factor(effects_all_df$val_name,
    levels = unique(effects_all_df$val_name)
  )

  return(effects_all_df)
}

interact_compare_labels <- function(df, effects_var_vec, treatment_var,
                                    response_var, bar_ci,
                                    plot_NA_values = FALSE, comp_func = mean, ...) {
  labels_vec <- c()
  effects_all_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(effects_all_df) <- c(response_var, treatment_var, "variable", "value")
  for (i in seq_len(length(effects_var_vec))) {
    effect_var <- effects_var_vec[i]
    if (length(list(...)) == 0) {
      effect_df <- df %>%
        group_by_(effect_var, treatment_var) %>%
        summarize(response_var = comp_func(.data[[response_var]], na.rm = TRUE)) %>%
        rename(!!(response_var) := response_var) %>%
        as.data.frame()
    } else {
      effect_df <- df %>%
        group_by_(effect_var, treatment_var) %>%
        summarize(response_var = comp_func(.data[[response_var]], ...)) %>%
        rename(!!(response_var) := response_var) %>%
        as.data.frame()
    }
    effect_rs_df <- melt(effect_df, id.var = c(response_var, treatment_var))
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

  effects_all_df$value_fac <- factor(effects_all_df$val_name,
    levels = unique(effects_all_df$val_name)
  )

  return(labels_vec)
}

interact_compare_produce_plot <- function(effects_all_df, labels_vec,
                                          effects_var_vec, treatment_var, response_var,
                                          plot_NA_values = FALSE,
                                          comp_func = mean, ...) {
  #' Produces a main effects plot
  #' @param effects_all_df the data frame with the data and all of the effects
  #' @param labels_vec the vector of the labels
  #' @param effects_var_vec the list of colum names of the effect factors, specified in order
  #' @param treatment_var the treatment col to compare in all of these plots
  #' @param response_var the column name of the response variable
  #' @param plot_NA_values if TRUE, plot missing values. FALSE by default.
  #' @param comp_func mean by default. The computation function used to aggregate the multiple
  #' values into a single value.
  #' @return plot_obj a plot object

  y_unit <- ""

  val_fac_var <- "value_fac"
  # Save variable_var <- "variable"
  plot_obj <- ggplot(data = effects_all_df) +
    geom_point(
      aes_string(
        x = val_fac_var,
        y = response_var,
        group = treatment_var,
        color = treatment_var,
        shape = treatment_var
      ),
      size = 4
    ) +
    geom_path(aes_string(
      x = val_fac_var, y = response_var,
      group = treatment_var, color = treatment_var
    )) +
    scale_x_discrete(breaks = unique(effects_all_df$value_fac), labels = labels_vec) +
    facet_grid(. ~ variable, scales = "free_x", switch = "x") +
    xlab("Factor") +
    ylab(response_var) +
    ggtitle(paste0(
      "Two-Term Interaction Plots ",
      "\nComparing Interactions with Factor ", treatment_var
    )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }

  return(plot_obj)
}
