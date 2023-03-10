penalize_missing <- function(df, score_col, key_col, key_values, dec_col) {
  pen_df <- df
  neg_key_value <- key_values[1]
  pos_key_value <- key_values[2]
  # Add decision of `t` and score of 9999 for nontarget trials
  if (nrow(pen_df[(is.na(pen_df[[score_col]])) & (pen_df[[key_col]] == neg_key_value), ]) > 0) {
    if (!is.null(dec_col)) {
      pen_df[(is.na(pen_df[[score_col]])) &
        (pen_df[[key_col]] == neg_key_value), ][[dec_col]] <- pos_key_value
    }
    pen_df[(is.na(pen_df[[score_col]])) &
      (pen_df[[key_col]] == neg_key_value), ][[score_col]] <- 9999
  }

  # Add decision of `f` and score of -9999 for target trials
  if (nrow(pen_df[(is.na(pen_df[[score_col]])) & (pen_df[[key_col]] == pos_key_value), ]) > 0) {
    if (!is.null(dec_col)) {
      pen_df[(is.na(pen_df[[score_col]])) &
        (pen_df[[key_col]] == pos_key_value), ][[dec_col]] <- neg_key_value
    }
    pen_df[(is.na(pen_df[[score_col]])) &
      (pen_df[[key_col]] == pos_key_value), ][[score_col]] <- -9999
  }
  return(pen_df)
}

get_decision_thresholds <- function(df, score_col, key_col, key_values, dec_col) {
  neg_key_value <- key_values[1]
  pos_key_value <- key_values[2]
  # max_t is not used so it is not computed
  min_f <- min(df[df[[dec_col]] == neg_key_value, score_col])
  min_t <- min(df[df[[dec_col]] == pos_key_value, score_col])
  max_f <- max(df[df[[dec_col]] == neg_key_value, score_col])
  dec_thresh <- 0
  if (is.na(min_t)) {
    print(paste0(
      "All points are ", neg_key_value,
      ".\nHence, the paragraph with thresholds may be inconsistent."
    ))
    dec_thresh <- max_f
  } else if (is.na(min_f)) {
    print(paste0(
      "All points are ", pos_key_value,
      ".\nHence, the paragraph with thresholds may be inconsistent."
    ))
    dec_thresh <- min_t
  } else if (min_t < max_f) {
    print(paste0(
      "The decision is invalid, \nbecause it is not equivalent to picking a threshold.",
      "\nHence, the paragraph with thresholds is inconsistent."
    ))
    dec_thresh <- (min_t + max_f) / 2
  } else {
    dec_thresh <- (min_t + max_f) / 2
  }
  return(dec_thresh)
}

get_dcf <- function(miss_rate, fa_rate, c_miss = 1, c_fa = 1, p_hkv = 0.5) {
  value <- (c_miss * miss_rate * p_hkv) + (c_fa * fa_rate * (1 - p_hkv))
  return(value)
}

compute_dcf_for_det_df <- function(det_df_row, c_miss = 1, c_fa = 1, p_hkv = 0.5) {
  miss_rate <- det_df_row[2]
  fa_rate <- det_df_row[3]
  dcf_val <- get_dcf(miss_rate, fa_rate, c_miss, c_fa, p_hkv)
  return(dcf_val)
}

tdeviate_trans <- function() {
  trans_new("tdeviate", qnorm, pnorm)
}


# reduce keydf to score, targettype
det_compute_df <- function(df, score_col, key_col, key_values = c(0, 1), dec_col = NULL) {
  neg_key_value <- key_values[1]
  pos_key_value <- key_values[2]
  pen_df <- penalize_missing(df, score_col, key_col, key_values, dec_col)
  score_df <- pen_df[, c(score_col, key_col)]
  names(score_df) <- c("score", "key")
  threshs <- as.data.frame(table(score_df$score))
  target_threshs <- as.data.frame(table(score_df[score_df$key == pos_key_value, ]$score))
  nontarget_threshs <- as.data.frame(table(score_df[score_df$key == neg_key_value, ]$score))
  score_df_DET <- merge(merge(threshs, target_threshs, by = "Var1", all = TRUE),
    nontarget_threshs,
    by = "Var1", all = TRUE
  )
  score_df_DET[is.na(score_df_DET)] <- 0
  names(score_df_DET) <- c("score", "new_points", "new_ms", "new_tn")
  score_df_DET$score <- as.numeric(levels(score_df_DET$score))[score_df_DET$score]

  # add first row with 0 new points and score of -Inf
  score_df_DET <- rbind(c(-Inf, 0, 0, 0), score_df_DET)

  ### First, compute the total number of targets and the total number of points
  num_targets <- sum(score_df_DET$new_ms)
  num_non_targets <- sum(score_df_DET$new_tn)
  num_points <- num_targets + num_non_targets

  ### Now produce the DET Curve
  score_df_DET$num_ms <- cumsum(score_df_DET$new_ms)
  score_df_DET$num_tn <- cumsum(score_df_DET$new_tn)
  score_df_DET$num_condneg <- score_df_DET$num_ms + score_df_DET$num_tn
  score_df_DET$num_condpos <- num_points - score_df_DET$num_condneg
  score_df_DET$num_tp <- num_targets - score_df_DET$num_ms
  score_df_DET$num_fa <- score_df_DET$num_condpos - score_df_DET$num_tp
  score_df_DET$total_points <- score_df_DET$num_fa + score_df_DET$num_tn + score_df_DET$num_tp +
    score_df_DET$num_ms
  # now compute det values
  score_df_DET$miss_rate <- score_df_DET$num_ms / num_targets
  score_df_DET$fa_rate <- score_df_DET$num_fa / num_non_targets
  return(score_df_DET[, c("score", "miss_rate", "fa_rate")])
}

det_compute_costs_df <- function(df, det_df, score_col, key_col, key_values, dec_col,
                                 cp_miss, cp_fa, pp_hkv,
                                 cs_miss, cs_fa, ps_hkv) {
  neg_key_value <- key_values[1]
  pos_key_value <- key_values[2]
  det_df$dcf_p <- apply(det_df, 1, FUN = compute_dcf_for_det_df, cp_miss, cp_fa, pp_hkv)
  det_df$dcf_s <- apply(det_df, 1, FUN = compute_dcf_for_det_df, cs_miss, cs_fa, ps_hkv)
  min_dcf_p <- min(det_df$dcf_p)
  min_dcf_row_p <- det_df[det_df$dcf_p == min(det_df$dcf_p), ]
  min_miss_rate_p <- min_dcf_row_p[, "miss_rate"]
  min_fa_rate_p <- min_dcf_row_p[, "fa_rate"]
  min_thresh_p <- min_dcf_row_p[, "score"]
  min_dcf_s <- min(det_df$dcf_s)
  min_dcf_row_s <- det_df[det_df$dcf_s == min(det_df$dcf_s), ]
  min_miss_rate_s <- min_dcf_row_s[, "miss_rate"]
  min_fa_rate_s <- min_dcf_row_s[, "fa_rate"]
  min_thresh_s <- min_dcf_row_s[, "score"]
  # Make costs_df with min info
  act_dcf_p <- NA
  act_dcf_s <- NA
  act_miss_rate <- NA
  act_fa_rate <- NA
  act_thresh <- NA
  # get decision statistics if dec_col is not NULL
  if (!is.null(dec_col)) {
    dec_thresh <- get_decision_thresholds(df, score_col, key_col, key_values, dec_col)
    # We select a threshold between a decisions, so the actual threshold is not a row in
    # the det_df
    # Compute actual DCF
    act_num_tp <- nrow(df[(df[[key_col]] == pos_key_value) & (df[[dec_col]] == pos_key_value), ])
    act_num_fa <- nrow(df[(df[[key_col]] == neg_key_value) & (df[[dec_col]] == pos_key_value), ])
    act_num_ms <- nrow(df[(df[[key_col]] == pos_key_value) & (df[[dec_col]] == neg_key_value), ])
    act_num_tn <- nrow(df[(df[[key_col]] == neg_key_value) & (df[[dec_col]] == neg_key_value), ])
    act_miss_rate <- act_num_ms / (act_num_ms + act_num_tp)
    act_fa_rate <- act_num_fa / (act_num_fa + act_num_tn)
    act_dcf_p <- get_dcf(act_miss_rate, act_fa_rate, cp_miss, cp_fa, pp_hkv)
    act_dcf_s <- get_dcf(act_miss_rate, act_fa_rate, cs_miss, cs_fa, ps_hkv)
    act_thresh <- dec_thresh
  }
  costs_df <- data.frame(
    act_thresh = c(act_thresh), act_miss_rate = c(act_miss_rate),
    act_fa_rate = c(act_fa_rate), act_dcf_p = c(act_dcf_p),
    act_dcf_s = c(act_dcf_s),
    min_thresh_p = c(min_thresh_p),
    min_miss_rate_p = c(min_miss_rate_p), min_fa_rate_p = c(min_fa_rate_p),
    min_dcf_p = c(min_dcf_p), min_thresh_s = c(min_thresh_s),
    min_miss_rate_s = c(min_miss_rate_s), min_fa_rate_s = c(min_fa_rate_s),
    min_dcf_s = c(min_dcf_s)
  )
  return(costs_df)
}


det_produce_plot <- function(det_df, costs_df, score_col, key_col, key_values,
                             dec_col, cp_miss, cp_fa, pp_hkv,
                             cs_miss, cs_fa, ps_hkv, sys_name = "") {
  miss_labels <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 40, 80)
  fa_labels <- c(0.001, 0.01, 0.1, 0.2, 0.5, 1, 2, 10, 20, 40)
  miss_breaks <- miss_labels / 100
  fa_breaks <- fa_labels / 100
  det_blue <- rgb(0, 0, 255, maxColorValue = 255)
  det_red <- rgb(255, 0, 0, maxColorValue = 255)
  det_dkgreen <- rgb(0, 102, 0, maxColorValue = 255)
  det_black <- rgb(0, 0, 0, maxColorValue = 255)
  det_gray <- rgb(128, 128, 128, maxColorValue = 255)

  sys_label <- "System DET"
  min_label_p <- paste("min_p cost: ", round(costs_df$min_dcf_p, 3), sep = "")
  min_label_s <- paste("min_s cost: ", round(costs_df$min_dcf_s, 3), sep = "")
  act_label_pa <- paste("act_p cost: ", round(costs_df$act_dcf_p, 3), sep = "")
  act_label_pb <- paste("\nact_s cost: ", round(costs_df$act_dcf_s, 3), sep = "")
  act_label <- paste(act_label_pa, act_label_pb, sep = "")
  legend_col_order <- c("System DET", "min p", "min s", "act")

  det_plot <- ggplot() +
    geom_point(data = det_df, aes(
      x = fa_rate, y = miss_rate,
      color = "System DET", fill = "System DET",
      shape = "System DET"
    ), size = 2)

  if (!is.null(dec_col)) {
    # act costs
    det_plot <- det_plot + geom_point(data = costs_df, aes(
      x = act_fa_rate, y = act_miss_rate,
      color = "act", fill = "act",
      shape = "act"
    ), size = 6)
  }
  # min p
  det_plot <- det_plot + geom_point(data = costs_df, aes(
    x = min_fa_rate_p, y = min_miss_rate_p,
    color = "min p", fill = "min p",
    shape = "min p"
  ), size = 4)
  # min s
  det_plot <- det_plot + geom_point(data = costs_df, aes(
    x = min_fa_rate_s, y = min_miss_rate_s,
    color = "min s", fill = "min s",
    shape = "min s"
  ), size = 4)
  # legend
  det_plot <- det_plot +
    scale_color_manual(
      name = "legend", values = c(
        "System DET" = det_gray, "min p" = det_black,
        "min s" = det_black, "act" = det_blue
      ),
      breaks = legend_col_order, labels = c(
        sys_label, min_label_p, min_label_s,
        act_label
      )
    ) +
    scale_fill_manual(
      name = "legend", values = c(
        "System DET" = det_gray, "min p" = det_red,
        "min s" = det_dkgreen, "act" = det_blue
      ),
      breaks = legend_col_order, labels = c(
        sys_label, min_label_p, min_label_s,
        act_label
      )
    ) +
    scale_shape_manual(
      name = "legend", values = c(
        "System DET" = 20, "min p" = 21, "min s" = 21,
        "act" = 3
      ), breaks = legend_col_order,
      labels = c(sys_label, min_label_p, min_label_s, act_label)
    ) +
    guides(fill = guide_legend(override.aes = list(size = 3)))

  # scaling
  det_plot <- det_plot +
    scale_x_continuous(trans = "tdeviate", breaks = fa_breaks, labels = fa_labels) +
    scale_y_continuous(trans = "tdeviate", breaks = miss_breaks, labels = miss_labels) +
    coord_cartesian(xlim = c(0.0009, 90) / 100, ylim = c(0.09, 90) / 100)


  title_string <- paste0("Detection Error Tradeoff (DET) Curve for System ", sys_name)
  title_string <- paste0(
    title_string, "\nPrimary DCF (cp_miss, cp_fa, pp_hkv): (",
    cp_miss, ", ", cp_fa, ", ", pp_hkv, ")"
  )
  title_string <- paste0(
    title_string, "\nSecondary DCF (cs_miss, cs_fa, ps_hkv): (",
    cs_miss, ", ", cs_fa, ", ", ps_hkv, ")"
  )

  det_plot <- det_plot + ggtitle(title_string) +
    xlab("False Alarm Rate (%)") + ylab("Miss Rate (%)")
  det_plot <- det_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # These are components of the theme for a DET curve that should be maintained regardless of
  # choice of default theme.
  det_plot <- det_plot + theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )
  return(det_plot)
}
