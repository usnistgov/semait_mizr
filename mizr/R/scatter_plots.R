order_compute_df <- function(df, fac_vec, response_var, digits = 2,
                             remove_NA_factors = FALSE) {
  treatment_fac <- fac_vec[1]
  block_vec <- c()
  if (length(fac_vec) >= 2) {
    block_vec <- fac_vec[2:length(fac_vec)]
  }
  tile_df <- block_compute_df(df, block_vec, treatment_fac, response_var,
    comp_func = mean,
    na.rm = TRUE
  )
  tile_df[[response_var]] <- round(tile_df[[response_var]], digits = digits)
  tile_df <- tile_df[order(tile_df[[response_var]]), ]

  if (remove_NA_factors) {
    tile_df <- na.omit(tile_df)
  }
  return(tile_df)
}

order_produce_plot <- function(mizr_df, fac_vec, response_var) {
  # Data should already be ordered
  # unneeded line mizr_df[order(mizr_df[[response_var]]),]

  facet_sym <- syms(fac_vec)

  if (length(fac_vec) > 1) {
    mizr_df$x_var <- as.factor(do.call(paste, c(mizr_df[fac_vec], sep = " | ")))
    x_var_str <- paste(fac_vec, collapse = " | ")
  } else {
    mizr_df$x_var <- mizr_df[[fac_vec]]
    x_var_str <- fac_vac
  }
  x_var_col <- "x_var"

  mizr_df$x_var <- factor(mizr_df$x_var, levels = unique(mizr_df$x_var))

  plot_obj <- ggplot(data = mizr_df) +
    geom_point(aes_string(x = x_var_col, y = response_var)) +
    xlab(paste0("Factors\n", x_var_str)) +
    ylab(paste0(response_var)) +
    ggtitle(paste0("Order Plot over Response ", response_var)) +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4)
    )
  return(plot_obj)
}


scatter_compute_df <- function(df, x_var, response_var) {
  return(df)
}

scatter_produce_plot <- function(df, x_var, response_var,
                                 shape_style = "point") {
  #' Produces a basic scatter plot that colors and shapes by the treatment factor
  #' @param df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param response_var the response variable to plot on the y axis
  #' @param shape_style a string indicating if the shape should be points ('point') or textual
  #' symbols 'txt'. Defaults to point.
  #' @return plot_obj the plot object

  x_unit <- ""
  y_unit <- ""

  plot_obj <- ggplot(data = df)

  plot_obj <- plot_obj + geom_point(aes_string(x = x_var, y = response_var))

  plot_obj <- plot_obj + ggtitle(paste0(
    "Scatter plot of x: ", x_var, ", to y: ",
    response_var
  )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (x_unit != "") {
    plot_obj <- plot_obj + xlab(paste0(x_var, " (", x_unit, ")"))
  }
  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }
  return(plot_obj)
}

scatter_doe_compute_sd_df <- function(df, fac_vec, response_var, treatment_fac = NULL,
                                      plot_NA_values = FALSE) {
  mizr_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(mizr_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(c(fac_vec)))) {
    effect_var <- fac_vec[i]
    # We do not want to include NA Labels
    effect_df <- df[!is.na(df[[effect_var]]), c(effect_var, treatment_fac, response_var)]
    effect_rs_df <- melt(effect_df, id.var = c(response_var, treatment_fac))
    if ((!is.null(treatment_fac)) && (effect_var == treatment_fac)) {
      effect_rs_df$variable <- gsub(".1", "", effect_rs_df$variable)
    }
    if (!plot_NA_values) {
      effect_rs_df <- effect_rs_df[!is.na(effect_rs_df$value), ]
    }
    effect_rs_df$val_name <- paste(effect_var,
      as.character(effect_rs_df$value),
      sep = "_"
    )
    mizr_df <- rbind(mizr_df, effect_rs_df)
  }
  mizr_df$value_fac <- factor(mizr_df$val_name,
    levels = unique(mizr_df$val_name)
  )

  # Add columns "mean", "sd", "lower", and "upper" for lines and error bars
  group_sym <- syms(c("variable", "value_fac"))
  if (!(is.null(treatment_fac))) {
    group_sym <- syms(c("variable", "value_fac", treatment_fac))
  }
  sd_df <- mizr_df %>%
    group_by(!!!group_sym) %>%
    summarize(
      mean = mean(.data[[response_var]], na.rm = TRUE),
      sd = sd(.data[[response_var]], na.rm = TRUE)
    ) %>%
    as.data.frame()
  sd_df$lower <- sd_df$mean - sd_df$sd
  sd_df$upper <- sd_df$mean + sd_df$sd

  return(sd_df)
}

scatter_doe_compute_df <- function(df, fac_vec, response_var, treatment_fac = NULL,
                                   plot_NA_values = FALSE) {
  mizr_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(mizr_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(c(fac_vec)))) {
    effect_var <- fac_vec[i]
    # We do not want to include NA Labels
    effect_df <- df[!is.na(df[[effect_var]]), c(effect_var, treatment_fac, response_var)]
    effect_rs_df <- melt(effect_df, id.var = c(response_var, treatment_fac))
    if ((!is.null(treatment_fac)) && (effect_var == treatment_fac)) {
      effect_rs_df$variable <- gsub(".1", "", effect_rs_df$variable)
    }
    if (!plot_NA_values) {
      effect_rs_df <- effect_rs_df[!is.na(effect_rs_df$value), ]
    }
    effect_rs_df$val_name <- paste(effect_var,
      as.character(effect_rs_df$value),
      sep = "_"
    )
    mizr_df <- rbind(mizr_df, effect_rs_df)
  }
  mizr_df$value_fac <- factor(mizr_df$val_name,
    levels = unique(mizr_df$val_name)
  )

  return(mizr_df)
}

scatter_doe_compute_labels <- function(df, fac_vec, response_var, treatment_fac = NULL,
                                       plot_NA_values = FALSE) {
  labels_vec <- c()
  mizr_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(mizr_df) <- c(response_var, "variable", "value")
  for (i in seq_len(length(fac_vec))) {
    effect_var <- fac_vec[i]
    # We do not want to include NA Labels
    effect_df <- df[!is.na(df[[effect_var]]), c(effect_var, treatment_fac, response_var)]
    effect_rs_df <- melt(effect_df, id.var = c(response_var, treatment_fac))
    if ((!is.null(treatment_fac)) && (effect_var == treatment_fac)) {
      effect_rs_df$variable <- gsub(".1", "", effect_rs_df$variable)
    }
    if (!plot_NA_values) {
      effect_rs_df <- effect_rs_df[!is.na(effect_rs_df$value), ]
    }
    effect_rs_df$val_name <- paste(effect_var,
      as.character(effect_rs_df$value),
      sep = "_"
    )
    mizr_df <- rbind(mizr_df, effect_rs_df)
    labels_vec <- c(labels_vec, unique(effect_rs_df$value))
    mizr_df <- rbind(mizr_df, effect_rs_df)
  }

  return(labels_vec)
}

scatter_doe_compute_mean <- function(df, response_var) {
  mean_val <- mean(df[[response_var]], na.rm = TRUE)
  return(mean_val)
}

scatter_doe_produce_plot <- function(mizr_df, labels_vec, fac_vec, response_var, mean_val,
                                     sd_df = NULL,
                                     treatment_fac = NULL,
                                     doe_plot_style = "enhanced") {
  #' Produces a scatter plot that colors and shapes by the treatment factor
  #' @param mizr_df the data frame
  #' @param labels_vec the vector names of the labels
  #' @param fac_vec the factors to plot on the x column
  #' @param response_var the response variable to plot on the y axis
  #' @param mean_val the mean value
  #' @param treatment_fac the treatment factor. NULL if none.
  #' @param doe_plot_style Specifies the Plot Style. Use "plain" to mimic the DOE Scatter Plot
  #' in dataplot, and "enhanced" to use an enhanced version, and
  #' "boxplot" to place the boxplot in front of the points, and "sd" to replace the boxplot
  #' To produce a mean and standard deviation rather than quantiles. Defaults to "enhanced".
  #' @return plot_obj the plot object

  val_fac_var <- "value_fac"
  val_var <- "value"
  mean_var <- "mean"
  upper_var <- "upper"
  lower_var <- "lower"

  y_unit <- ""

  # x-axis jutter for easier viewing of multiple points
  jitter_pos <- position_jitter(width = 0.0, height = 0)
  jitter_dodge_pos <- position_jitterdodge(
    jitter.width = 0.0, jitter.height = 0,
    dodge.width = 0.9
  )
  boxplot_dodge_pos <- position_dodge(width = 0.9)

  plot_obj <- ggplot(data = mizr_df)

  if (doe_plot_style == "plain") {
    # Treatment factor is not used in the plain dataplot style
    plot_obj <- plot_obj +
      geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "dashed") +
      geom_point(aes_string(x = val_fac_var, y = response_var, group = 1),
        size = 4
      ) +
      geom_path(aes_string(x = val_fac_var, y = response_var, group = val_var),
        linetype = "solid"
      ) +
      scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
      facet_grid(. ~ variable, scales = "free_x", switch = "x") +
      scale_color_discrete(guide = FALSE) + xlab("Factor") + ylab(response_var) +
      ggtitle("DOE Scatter Plot (Dataplot Style)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  } else if (doe_plot_style == "boxplot") {
    if (is.null(treatment_fac)) {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_boxplot(aes_string(x = val_fac_var, y = response_var, group = val_var),
          width = 0.6,
          alpha = 0.8
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        scale_color_discrete(guide = FALSE) + xlab("Factor") + ylab(response_var) +
        ggtitle("DOE-Style Scatter Plot (Boxplot at Front)") +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    } else {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_boxplot(
          aes_string(
            x = val_fac_var, y = response_var,
            color = treatment_fac
          ),
          width = 0.6, alpha = 0.8,
          outlier.shape = treatment_fac, position = boxplot_dodge_pos
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        xlab("Factor") + ylab(response_var) +
        ggtitle(paste0(
          "DOE-Style Scatter Plot (Boxplot at Front),\nTreatment Factor ",
          treatment_fac
        )) +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
  } else if (doe_plot_style == "boxplot_points") {
    if (is.null(treatment_fac)) {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "solid") +
        geom_point(aes_string(x = val_fac_var, y = response_var, group = 1),
          size = 4, position = jitter_pos, alpha = 0.8
        ) +
        geom_boxplot(aes_string(x = val_fac_var, y = response_var, group = val_var),
          width = 0.6,
          outlier.shape = NA, alpha = 0.8
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        scale_color_discrete(guide = FALSE) + xlab("Factor") + ylab(response_var) +
        ggtitle("DOE-Style Scatter Plot (Boxplot at Front)") +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    } else {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "solid") +
        geom_point(
          aes_string(
            x = val_fac_var, y = response_var, group = treatment_fac,
            shape = treatment_fac, alpha = 0.8
          ),
          size = 4, position = jitter_dodge_pos
        ) +
        geom_boxplot(
          aes_string(
            x = val_fac_var, y = response_var,
            color = treatment_fac
          ),
          width = 0.6, alpha = 0.8,
          outlier.shape = NA, position = boxplot_dodge_pos
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        xlab("Factor") + ylab(response_var) +
        ggtitle(paste0(
          "DOE-Style Scatter Plot (Boxplot at Front),\nTreatment Factor ",
          treatment_fac
        )) +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
  } else if (doe_plot_style == "sd") {
    if (is.null(treatment_fac)) {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "solid") +
        geom_point(aes_string(x = val_fac_var, y = response_var, group = 1),
          size = 4, position = jitter_pos, alpha = 0.8
        ) +
        geom_errorbar(
          data = sd_df, aes_string(
            x = val_fac_var, ymin = lower_var,
            ymax = upper_var, group = 1
          ),
          show.legend = FALSE, width = 0.40, size = 0.5,
          color = rgb(0, 0, 255, maxColorValue = 255),
          linetype = "longdash"
        ) +
        geom_point(
          data = sd_df, aes_string(x = val_fac_var, y = mean_var, group = 1),
          size = 4, shape = 3, show.legend = FALSE, stroke = 4,
          color = rgb(0, 0, 255, maxColorValue = 255)
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        scale_color_discrete(guide = FALSE) + xlab("Factor") + ylab(response_var) +
        ggtitle("DOE-Style Scatter Plot with Standard Deviation and Mean") +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    } else {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "solid") +
        geom_point(
          aes_string(
            x = val_fac_var, y = response_var, group = treatment_fac,
            shape = treatment_fac, alpha = 0.8
          ),
          size = 4, position = jitter_dodge_pos
        ) +
        geom_errorbar(
          data = sd_df, aes_string(
            x = val_fac_var, ymin = lower_var,
            ymax = upper_var, group = treatment_fac,
            color = treatment_fac
          ),
          position = boxplot_dodge_pos, width = 0.40, size = 0.5,
          linetype = "longdash"
        ) +
        geom_point(
          data = sd_df, aes_string(
            x = val_fac_var, y = mean_var,
            group = treatment_fac,
            color = treatment_fac
          ),
          size = 4, shape = 3, show.legend = FALSE, position = boxplot_dodge_pos,
          stroke = 4
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        xlab("Factor") + ylab(response_var) +
        ggtitle(paste0(
          "DOE-Style Scatter Plot with Standard Deviation and Mean,",
          "\nTreatment Factor ",
          treatment_fac
        )) +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
  } else {
    if (is.null(treatment_fac)) {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_boxplot(aes_string(x = val_fac_var, y = response_var, group = val_var),
          width = 0.6,
          outlier.shape = NA
        ) +
        geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "solid") +
        geom_point(aes_string(x = val_fac_var, y = response_var, group = 1),
          size = 4, position = jitter_pos, alpha = 0.8
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        scale_color_discrete(guide = FALSE) + xlab("Factor") + ylab(response_var) +
        ggtitle("DOE-Style Scatter Plot") +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    } else {
      plot_obj <- plot_obj +
        geom_vline(
          xintercept = seq(1.5, length(unique(mizr_df$val_name)) - 0.5, 1), lwd = 0.4,
          color = "gray50", linetype = 2
        ) +
        geom_boxplot(
          aes_string(
            x = val_fac_var, y = response_var,
            color = treatment_fac
          ),
          width = 0.6,
          outlier.shape = NA, position = boxplot_dodge_pos
        ) +
        geom_hline(yintercept = mean_val, lwd = 0.6, color = "gray50", linetype = "solid") +
        geom_point(
          aes_string(
            x = val_fac_var, y = response_var, group = treatment_fac,
            shape = treatment_fac
          ),
          size = 4, position = jitter_dodge_pos, alpha = 0.8
        ) +
        scale_x_discrete(breaks = unique(mizr_df$value_fac), labels = labels_vec) +
        facet_grid(. ~ variable, scales = "free_x", switch = "x") +
        xlab("Factor") + ylab(response_var) +
        ggtitle(paste0("DOE-Style Scatter Plot,\nTreatment Factor ", treatment_fac)) +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
  }

  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }
  return(plot_obj)
}


scatter_effect_compute_df <- function(df, x_var, response_var,
                                      comp_func = mean, ...) {
  if (length(list(...)) == 0) {
    effect_df <- aggregate(reformulate(x_var, response_var), df, FUN = comp_func, na.rm = TRUE)
  } else {
    effect_df <- aggregate(reformulate(x_var, response_var), df, FUN = comp_func, ...)
  }
  return(effect_df)
}
scatter_effect_produce_plot <- function(effect_df, x_var, response_var,
                                        comp_func = mean, ...) {
  #' Produces a scatter plot that does a main effect of x_var by response_var
  #' @param effect_df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param response_var the response variable to plot on the y axis
  #' @param comp_func mean by defalut. The computation function used to aggregate multiple values
  #' into a single value
  #' @return plot_obj the plot object

  x_unit <- ""
  y_unit <- ""

  plot_obj <- ggplot(data = effect_df) +
    geom_point(aes_string(x = x_var, y = response_var),
      group = 1
    ) +
    geom_line(aes_string(x = x_var, y = response_var), group = 1) +
    ggtitle(paste0("Margins Effect Plot of \nx: ", x_var, ", y: ", response_var)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


  if (x_unit != "") {
    plot_obj <- plot_obj + xlab(paste0(x_var, " (", x_unit, ")"))
  }
  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }
  return(plot_obj)
}


scatter_treatment_compute_df <- function(df, x_var, treatment_fac, response_var) {
  return(df)
}

scatter_treatment_produce_plot <- function(df, x_var, treatment_fac, response_var,
                                           shape_style = "point") {
  #' Produces a scatter plot that colors and shapes by the treatment factor
  #' @param df the data frame
  #' @param x_var the factor to plot on the x column
  #' @param treatment_fac the treatment factor
  #' @param response_var the response variable to plot on the y axis
  #' @param shape_style a string indicating if the shape should be points ('point') or textual
  #' symbols 'text'. Defaults to point.
  #' @return plot_obj the plot object

  x_unit <- ""
  y_unit <- ""

  full_var <- "full_label"

  var_key <- unique(df[[treatment_fac]])
  geom_text_object <- get_custom_text_object(var_key,
    cols = scales::hue_pal()(length(var_key))
  )
  plot_obj <- ggplot(data = df)
  if (shape_style == "text") {
    # No metadata, use original values as labels
    plot_obj <- plot_obj + geom_text_custom(
      aes_string(
        x = x_var, y = response_var,
        color = treatment_fac, label = treatment_fac
      ),
      geom = geom_text_object
    )
  } else {
    plot_obj <- plot_obj + geom_point(aes_string(
      x = x_var, y = response_var,
      color = treatment_fac
    ))
  }
  plot_obj <- plot_obj + ggtitle(paste0(
    "Scatter plot of x: ", x_var, ", to y: ",
    response_var, "\nwith treatment: ", treatment_fac
  )) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (x_unit != "") {
    plot_obj <- plot_obj + xlab(paste0(x_var, " (", x_unit, ")"))
  }
  if (y_unit != "") {
    plot_obj <- plot_obj + ylab(paste0(response_var, " (", y_unit, ")"))
  }
  return(plot_obj)
}
