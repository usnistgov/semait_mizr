tile_table_count_df <- function(df, x_fac_vec, y_fac_vec, response_name = "count",
                                digits = 2, use_margins = FALSE, remove_NA_factors = FALSE) {
  treatment_fac <- x_fac_vec[1]
  fac_vec <- y_fac_vec
  if (length(x_fac_vec) >= 2) {
    fac_vec <- c(x_fac_vec[2:length(x_fac_vec)], y_fac_vec)
  }
  group_sym <- syms(c(fac_vec, treatment_fac))
  count_df <- df %>%
    dplyr::count(!!!group_sym, .drop = FALSE) %>%
    as.data.frame()
  names(count_df)[names(count_df) == "n"] <- response_name
  if (remove_NA_factors) {
    count_df <- na.omit(count_df)
  }

  # Add x-axis and y-axis margins if desired
  # For ease of display, we add the margins to the data frame with an (all) variable
  if (use_margins) {
    # Produce the aggregated data frame
    global_mean <- mean(count_df[[response_name]])
    group_x_sym <- syms(c(x_fac_vec))
    x_mar_df <- count_df %>%
      group_by(!!!group_x_sym) %>%
      dplyr::summarize(response_name = mean(.data[[response_name]], na.rm = TRUE)) %>%
      rename(!!(response_name) := response_name) %>%
      as.data.frame()
    group_y_sym <- syms(c(y_fac_vec))
    y_mar_df <- count_df %>%
      group_by(!!!group_y_sym) %>%
      dplyr::summarize(response_name = mean(.data[[response_name]], na.rm = TRUE)) %>%
      rename(!!(response_name) := response_name) %>%
      as.data.frame()
    mar_df <- plyr::rbind.fill(count_df, x_mar_df, y_mar_df)
    for (x_fac in x_fac_vec) {
      prev_levels <- levels(mar_df[[x_fac]])
      new_levels <- c(prev_levels, "(all)")
      mar_df[[x_fac]] <- as.character(mar_df[[x_fac]])
      mar_df[[x_fac]][is.na(mar_df[[x_fac]])] <- "(all)"
      mar_df[[x_fac]] <- factor(mar_df[[x_fac]], levels = new_levels)
    }
    for (y_fac in y_fac_vec) {
      prev_levels <- levels(mar_df[[y_fac]])
      new_levels <- c(prev_levels, "(all)")
      mar_df[[y_fac]] <- as.character(mar_df[[y_fac]])
      mar_df[[y_fac]][is.na(mar_df[[y_fac]])] <- "(all)"
      mar_df[[y_fac]] <- factor(mar_df[[y_fac]], levels = new_levels)
    }
    global_vec <- c(rep("(all)", length(x_fac_vec) + length(y_fac_vec)), global_mean)
    mar_df <- rbind(mar_df, global_vec)
    mar_df[[response_name]] <- as.numeric(mar_df[[response_name]])
    count_df <- mar_df
  }
  count_df[[response_name]] <- round(count_df[[response_name]], digits = digits)
  return(count_df)
}

tile_tf_count_df <- function(df, fac_vec, response_name = "count",
                             digits = 2, remove_NA_factors = FALSE) {
  tf_df <- data.frame(
    x_fac = character(), y_fac = character(),
    x_scale = character(), y_scale = character(),
    count = integer(), stringsAsFactors = FALSE
  )
  # First do uni-factor counts
  for (fac in fac_vec) {
    group_sym <- syms(c(fac))
    count_df <- df %>%
      dplyr::count(!!!group_sym, .drop = FALSE) %>%
      as.data.frame()
    names(count_df)[names(count_df) == "n"] <- response_name
    count_df$x_fac <- fac
    count_df$y_fac <- fac
    colnames(count_df)[which(names(count_df) == fac)] <- "x_scale"
    count_df$x_scale <- as.character(count_df$x_scale)
    count_df$y_scale <- as.character(count_df$x_scale)
    count_df <- count_df[, c("x_fac", "y_fac", "x_scale", "y_scale", "count")]
    tf_df <- rbind(tf_df, count_df)
  }
  # Now do two-level interaction counts
  for (fac_comb in combn(fac_vec, 2, simplify = FALSE)) {
    group_sym <- syms(c(fac_comb))
    count_df <- df %>%
      dplyr::count(!!!group_sym, .drop = FALSE) %>%
      as.data.frame()
    names(count_df)[names(count_df) == "n"] <- response_name
    count_b_df <- count_df
    # One direction of the combination
    x_fac <- fac_comb[1]
    y_fac <- fac_comb[2]
    count_df$x_fac <- x_fac
    count_df$y_fac <- y_fac
    colnames(count_df)[which(names(count_df) == x_fac)] <- "x_scale"
    colnames(count_df)[which(names(count_df) == y_fac)] <- "y_scale"
    count_df$x_scale <- as.character(count_df$x_scale)
    count_df$y_scale <- as.character(count_df$y_scale)
    count_df <- count_df[, c("x_fac", "y_fac", "x_scale", "y_scale", "count")]
    tf_df <- rbind(tf_df, count_df)
    # Other direction of the combination
    count_b_df$x_fac <- y_fac
    count_b_df$y_fac <- x_fac
    colnames(count_b_df)[which(names(count_b_df) == y_fac)] <- "x_scale"
    colnames(count_b_df)[which(names(count_b_df) == x_fac)] <- "y_scale"
    count_b_df$x_scale <- as.character(count_b_df$x_scale)
    count_b_df$y_scale <- as.character(count_b_df$y_scale)
    count_b_df <- count_b_df[, c("x_fac", "y_fac", "x_scale", "y_scale", "count")]
    tf_df <- rbind(tf_df, count_b_df)
  }
  tf_df$x_fac <- as.factor(tf_df$x_fac)
  tf_df$y_fac <- as.factor(tf_df$y_fac)
  tf_df$x_scale <- as.factor(tf_df$x_scale)
  tf_df$y_scale <- as.factor(tf_df$y_scale)

  if (remove_NA_factors) {
    tf_df <- na.omit(tf_df)
  }
  tf_df[[response_name]] <- round(tf_df[[response_name]], digits = digits)
  return(tf_df)
}


tile_tf_compute_df <- function(df, fac_vec, response_var,
                               digits = 2, remove_NA_factors = FALSE) {
  tf_df <- data.frame(
    x_fac = character(), y_fac = character(),
    x_scale = character(), y_scale = character(),
    response = integer(), stringsAsFactors = FALSE
  )
  names(tf_df)[names(tf_df) == "response"] <- response_var

  # First do uni-factor counts
  for (fac in fac_vec) {
    group_sym <- syms(c(fac))
    resp_df <- df %>%
      group_by(!!!group_sym) %>%
      dplyr::summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    resp_df$x_fac <- fac
    resp_df$y_fac <- fac
    colnames(resp_df)[which(names(resp_df) == fac)] <- "x_scale"
    resp_df$x_scale <- as.character(resp_df$x_scale)
    resp_df$y_scale <- as.character(resp_df$x_scale)
    resp_df <- resp_df[, c("x_fac", "y_fac", "x_scale", "y_scale", response_var)]
    tf_df <- rbind(tf_df, resp_df)
  }
  # Now do two-level interaction counts
  for (fac_comb in combn(fac_vec, 2, simplify = FALSE)) {
    group_sym <- syms(c(fac_comb))
    resp_df <- df %>%
      group_by(!!!group_sym) %>%
      dplyr::summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    resp_b_df <- resp_df
    # One direction of the combination
    x_fac <- fac_comb[1]
    y_fac <- fac_comb[2]
    resp_df$x_fac <- x_fac
    resp_df$y_fac <- y_fac
    colnames(resp_df)[which(names(resp_df) == x_fac)] <- "x_scale"
    colnames(resp_df)[which(names(resp_df) == y_fac)] <- "y_scale"
    resp_df$x_scale <- as.character(resp_df$x_scale)
    resp_df$y_scale <- as.character(resp_df$y_scale)
    resp_df <- resp_df[, c("x_fac", "y_fac", "x_scale", "y_scale", response_var)]
    tf_df <- rbind(tf_df, resp_df)
    # Other direction of the combination
    resp_b_df$x_fac <- y_fac
    resp_b_df$y_fac <- x_fac
    colnames(resp_b_df)[which(names(resp_b_df) == y_fac)] <- "x_scale"
    colnames(resp_b_df)[which(names(resp_b_df) == x_fac)] <- "y_scale"
    resp_b_df$x_scale <- as.character(resp_b_df$x_scale)
    resp_b_df$y_scale <- as.character(resp_b_df$y_scale)
    resp_b_df <- resp_b_df[, c("x_fac", "y_fac", "x_scale", "y_scale", response_var)]
    tf_df <- rbind(tf_df, resp_b_df)
  }
  tf_df$x_fac <- as.factor(tf_df$x_fac)
  tf_df$y_fac <- as.factor(tf_df$y_fac)
  tf_df$x_scale <- as.factor(tf_df$x_scale)
  tf_df$y_scale <- as.factor(tf_df$y_scale)

  if (remove_NA_factors) {
    tf_df <- na.omit(tf_df)
  }
  tf_df[[response_var]] <- round(tf_df[[response_var]], digits = digits)
  return(tf_df)
}

tile_table_compute_df <- function(df, x_fac_vec, y_fac_vec, response_var,
                                  digits = 2, use_margins = FALSE) {
  treatment_fac <- x_fac_vec[1]
  fac_vec <- y_fac_vec
  if (length(x_fac_vec) >= 2) {
    fac_vec <- c(x_fac_vec[2:length(x_fac_vec)], y_fac_vec)
  }
  tile_df <- block_compute_df(df, fac_vec, treatment_fac, response_var,
    comp_func = mean,
    na.rm = TRUE
  )
  # Add x-axis and y-axis margins if desired
  # For ease of display, we add the margins to the data frame with an (all) variable
  if (use_margins) {
    # Produce the aggregated data frame
    # We use our method to compute the grand_mean rather than equivalently taking the
    # mean of mean(tile_df[[response_var]], na.rm = TRUE)
    global_mean <- compute_grand_mean(df, c(treatment_fac, fac_vec), response_var)
    group_x_sym <- syms(c(x_fac_vec))
    x_mar_df <- tile_df %>%
      group_by(!!!group_x_sym) %>%
      dplyr::summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    group_y_sym <- syms(c(y_fac_vec))
    y_mar_df <- tile_df %>%
      group_by(!!!group_y_sym) %>%
      dplyr::summarize(response_var = mean(.data[[response_var]], na.rm = TRUE)) %>%
      rename(!!(response_var) := response_var) %>%
      as.data.frame()
    mar_df <- plyr::rbind.fill(tile_df, x_mar_df, y_mar_df)
    for (x_fac in x_fac_vec) {
      prev_levels <- levels(mar_df[[x_fac]])
      new_levels <- c(prev_levels, "(all)")
      mar_df[[x_fac]] <- as.character(mar_df[[x_fac]])
      mar_df[[x_fac]][is.na(mar_df[[x_fac]])] <- "(all)"
      mar_df[[x_fac]] <- factor(mar_df[[x_fac]], levels = new_levels)
    }
    for (y_fac in y_fac_vec) {
      prev_levels <- levels(mar_df[[y_fac]])
      new_levels <- c(prev_levels, "(all)")
      mar_df[[y_fac]] <- as.character(mar_df[[y_fac]])
      mar_df[[y_fac]][is.na(mar_df[[y_fac]])] <- "(all)"
      mar_df[[y_fac]] <- factor(mar_df[[y_fac]], levels = new_levels)
    }
    # compute global mean
    global_vec <- c(rep("(all)", length(x_fac_vec) + length(y_fac_vec)), global_mean)
    mar_df <- rbind(mar_df, global_vec)
    mar_df[[response_var]] <- as.numeric(mar_df[[response_var]])
    tile_df <- mar_df
  }
  tile_df[[response_var]] <- round(tile_df[[response_var]], digits = digits)
  return(tile_df)
}

tile_table_produce_plot <- function(mizr_df, x_fac_vec, y_fac_vec, response_var,
                                    low_color = "#FF0000",
                                    mid_color = "#FFFFFF",
                                    high_color = "#0074FF",
                                    midpoint = 0,
                                    use_margins = FALSE, tile_text_size = rel(2.0)) {
  facet_row_sym <- syms(y_fac_vec)
  facet_col_sym <- syms(x_fac_vec)

  if (length(x_fac_vec) > 1) {
    mizr_df$x_fac <- as.factor(do.call(paste, c(mizr_df[x_fac_vec], sep = " | ")))
    x_fac_str <- paste(x_fac_vec, collapse = " | ")
  } else {
    mizr_df$x_fac <- mizr_df[[x_fac_vec]]
    x_fac_str <- x_fac_vec
  }
  if (length(y_fac_vec) > 1) {
    mizr_df$y_fac <- as.factor(do.call(paste, c(mizr_df[y_fac_vec], sep = " | ")))
    y_fac_str <- paste(y_fac_vec, collapse = " | ")
  } else {
    mizr_df$y_fac <- mizr_df[[y_fac_vec]]
    y_fac_str <- y_fac_vec
  }

  # Change the level arrangement
  # Can sort for future levels
  x_fac_levels <-
    c(
      unique(as.character(mizr_df[!grepl("\\(all\\)", mizr_df$x_fac), ]$x_fac)),
      unique(as.character(mizr_df[grep("\\(all\\)", mizr_df$x_fac), ]$x_fac))
    )
  y_fac_levels <-
    c(
      unique(as.character(mizr_df[grepl("\\(all\\)", mizr_df$y_fac), ]$y_fac)),
      rev(unique(as.character(mizr_df[!grepl("\\(all\\)", mizr_df$y_fac), ]$y_fac)))
    )

  mizr_df$x_fac <- factor(as.character(mizr_df$x_fac), levels = x_fac_levels)
  mizr_df$y_fac <- factor(as.character(mizr_df$y_fac), levels = y_fac_levels)

  x_fac_col <- "x_fac"
  y_fac_col <- "y_fac"
  min_val <- min(mizr_df[[response_var]], na.rm = TRUE)
  max_val <- max(mizr_df[[response_var]], na.rm = TRUE)

  mizr_plot <- ggplot(data = mizr_df, aes(x = x_fac, y = y_fac)) +
    geom_blank()
  mizr_plot <- mizr_plot +
    geom_tile(
      data = mizr_df[!apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
      aes_string(x = x_fac_col, y = y_fac_col, fill = response_var),
      color = rgb(0, 0, 0)
    ) +
    geom_text(
      data = mizr_df[!apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
      aes_string(x = x_fac_col, y = y_fac_col, label = response_var),
      size = tile_text_size
    )

  if (use_margins) {
    mizr_plot <- mizr_plot +
      geom_tile(
        data = mizr_df[apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
        aes_string(x = x_fac_col, y = y_fac_col, fill = response_var),
        color = rgb(255, 255, 255, maxColorValue = 255)
      ) +
      geom_text(
        data = mizr_df[apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
        aes_string(x = x_fac_col, y = y_fac_col, label = response_var),
        size = 0.85 * tile_text_size
      )
  }

  if (min_val < max_val) {
    mizr_plot <- mizr_plot +
      scale_fill_gradientn(
        colors = c(low_color, mid_color, high_color),
        values = scales::rescale(c(min_val, midpoint, max_val))
      )
  } else {
    mizr_plot <- mizr_plot +
      scale_fill_gradientn(colors = c(low_color, mid_color, high_color))
  }
  mizr_plot <- mizr_plot +
    xlab(paste0("X-Axis Factors\n", x_fac_str)) +
    ylab(paste0("Y-Axis Factors\n", y_fac_str)) +
    scale_x_discrete(position = "top") +
    ggtitle(paste0(
      "Colored Tile Table of Response Variable ", response_var,
      "\nOver Different Factors"
    )) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(mizr_plot)
}

tile_table_count_produce_plot <- function(mizr_df, x_fac_vec, y_fac_vec, response_name = "count",
                                          low_color = "#FFFFFF",
                                          mid_color = "#EEEEEE",
                                          high_color = "#0074FF",
                                          midpoint = 1,
                                          use_margins = FALSE, tile_text_size = rel(2.0)) {
  # We don't want the 0 numbers to appear since they are blank
  mizr_df$text_response <- mizr_df[[response_name]]
  if (nrow(mizr_df[mizr_df$text_response == 0, ]) > 0) {
    mizr_df[mizr_df$text_response == 0, ]$text_response <- ""
  }
  text_label_col <- "text_response"
  facet_row_sym <- syms(y_fac_vec)
  facet_col_sym <- syms(x_fac_vec)

  if (length(x_fac_vec) > 1) {
    mizr_df$x_fac <- as.factor(do.call(paste, c(mizr_df[x_fac_vec], sep = " | ")))
    x_fac_str <- paste(x_fac_vec, collapse = " | ")
  } else {
    mizr_df$x_fac <- mizr_df[[x_fac_vec]]
    x_fac_str <- x_fac_vec
  }
  if (length(y_fac_vec) > 1) {
    mizr_df$y_fac <- as.factor(do.call(paste, c(mizr_df[y_fac_vec], sep = " | ")))
    y_fac_str <- paste(y_fac_vec, collapse = " | ")
  } else {
    mizr_df$y_fac <- mizr_df[[y_fac_vec]]
    y_fac_str <- y_fac_vec
  }

  # Change the level arrangement
  # Can sort for future levels
  x_fac_levels <-
    c(
      unique(as.character(mizr_df[!grepl("\\(all\\)", mizr_df$x_fac), ]$x_fac)),
      unique(as.character(mizr_df[grep("\\(all\\)", mizr_df$x_fac), ]$x_fac))
    )
  y_fac_levels <-
    c(
      unique(as.character(mizr_df[grepl("\\(all\\)", mizr_df$y_fac), ]$y_fac)),
      rev(unique(as.character(mizr_df[!grepl("\\(all\\)", mizr_df$y_fac), ]$y_fac)))
    )

  mizr_df$x_fac <- factor(as.character(mizr_df$x_fac), levels = x_fac_levels)
  mizr_df$y_fac <- factor(as.character(mizr_df$y_fac), levels = y_fac_levels)

  x_fac_col <- "x_fac"
  y_fac_col <- "y_fac"
  min_val <- min(mizr_df[[response_name]], na.rm = TRUE)
  max_val <- max(mizr_df[[response_name]], na.rm = TRUE)

  mizr_plot <- ggplot(data = mizr_df, aes(x = x_fac, y = y_fac)) +
    geom_blank()
  mizr_plot <- mizr_plot +
    geom_tile(
      data = mizr_df[!apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
      aes_string(x = x_fac_col, y = y_fac_col, fill = response_name),
      color = rgb(0, 0, 0)
    ) +
    geom_text(
      data = mizr_df[!apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
      aes_string(x = x_fac_col, y = y_fac_col, label = text_label_col),
      size = tile_text_size
    )

  if (use_margins) {
    mizr_plot <- mizr_plot +
      geom_tile(
        data = mizr_df[apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
        aes_string(x = x_fac_col, y = y_fac_col, fill = response_name),
        color = rgb(255, 255, 255, maxColorValue = 255)
      ) +
      geom_text(
        data = mizr_df[apply(mizr_df, 1, function(r) any(r %in% c("(all)"))), ],
        aes_string(x = x_fac_col, y = y_fac_col, label = text_label_col),
        size = 0.85 * tile_text_size
      )
  }

  if (min_val < max_val) {
    mizr_plot <- mizr_plot +
      # scale_fill_gradient(low = low_color, high = high_color, space = "Lab") +
      scale_fill_gradientn(
        colors = c(low_color, mid_color, high_color),
        values = scales::rescale(c(min_val, midpoint, max_val))
      )
  } else {
    mizr_plot <- mizr_plot +
      scale_fill_gradientn(colors = c(low_color, mid_color, high_color))
  }
  mizr_plot <- mizr_plot +
    xlab(paste0("X-Axis Factors\n", x_fac_str)) +
    ylab(paste0("Y-Axis Factors\n", y_fac_str)) +
    scale_x_discrete(position = "top") +
    ggtitle(paste0(
      "Title Table Count of Samples with Response Variable ", response_name,
      "\nOver Different Factors"
    )) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(mizr_plot)
}

tile_tf_count_produce_plot <- function(mizr_df, fac_vec, response_name = "count",
                                       low_color = "#FFFFFF",
                                       mid_color = "#EEEEEE",
                                       high_color = "#0074FF",
                                       midpoint = 1,
                                       tile_text_size = rel(2.0)) {
  # We don't want the 0 numbers to appear since they are blank
  mizr_df$text_response <- mizr_df[[response_name]]
  if (nrow(mizr_df[mizr_df$text_response == 0, ]) > 0) {
    mizr_df[mizr_df$text_response == 0, ]$text_response <- ""
  }
  text_label_col <- "text_response"

  x_fac_col <- "x_fac"
  y_fac_col <- "y_fac"
  x_scale_col <- "x_scale"
  y_scale_col <- "y_scale"

  facet_row_sym <- syms(unique(as.character(mizr_df$x_fac)))
  facet_col_sym <- syms(unique(as.character(mizr_df$y_fac)))

  min_val <- min(mizr_df[[response_name]], na.rm = TRUE)
  max_val <- max(mizr_df[[response_name]], na.rm = TRUE)

  mizr_plot <- ggplot(data = mizr_df) +
    geom_tile(aes_string(x = x_scale_col, y = y_scale_col, fill = response_name),
      color = rgb(0, 0, 0)
    ) +
    geom_text(
      data = mizr_df,
      aes_string(x = x_scale_col, y = y_scale_col, label = text_label_col),
      size = tile_text_size
    ) +
    facet_grid(
      rows = vars(y_fac), cols = vars(x_fac), scale = "free", drop = FALSE,
      switch = "both"
    ) +
    scale_fill_gradientn(
      colors = c(low_color, mid_color, high_color),
      values = scales::rescale(c(min_val, midpoint, max_val))
    ) +
    xlab("X Factor") +
    ylab("Y Factor") +
    ggtitle("Two-Factor Tile Table Count of Samples") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(mizr_plot)
}


tile_tf_produce_plot <- function(mizr_df, fac_vec, response_var,
                                 low_color = "#FF0000",
                                 mid_color = "#FFFFFF",
                                 high_color = "#0074FF",
                                 midpoint = 0,
                                 tile_text_size = rel(2.0)) {
  x_fac_col <- "x_fac"
  y_fac_col <- "y_fac"
  x_scale_col <- "x_scale"
  y_scale_col <- "y_scale"

  facet_row_sym <- syms(unique(as.character(mizr_df$x_fac)))
  facet_col_sym <- syms(unique(as.character(mizr_df$y_fac)))

  min_val <- min(mizr_df[[response_var]], na.rm = TRUE)
  max_val <- max(mizr_df[[response_var]], na.rm = TRUE)

  mizr_plot <- ggplot(data = mizr_df) +
    geom_tile(aes_string(x = x_scale_col, y = y_scale_col, fill = response_var),
      color = rgb(0, 0, 0)
    ) +
    geom_text(
      data = mizr_df,
      aes_string(x = x_scale_col, y = y_scale_col, label = response_var),
      size = tile_text_size
    ) +
    facet_grid(
      rows = vars(y_fac), cols = vars(x_fac), scale = "free", drop = FALSE,
      switch = "both"
    ) +
    scale_fill_gradientn(
      colors = c(low_color, mid_color, high_color),
      values = scales::rescale(c(min_val, midpoint, max_val))
    ) +
    xlab("X Factor") +
    ylab("Y Factor") +
    ggtitle(paste0(
      "Two-Factor Tile Table Averages with Response Variable ", response_var
    )) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(mizr_plot)
}
