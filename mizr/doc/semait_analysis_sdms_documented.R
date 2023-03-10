## ----setup, include=FALSE, echo=FALSE, warning=FALSE--------------------------
knitr::opts_chunk$set(fig.height = 9, fig.width = 14)

## ----filepaths----------------------------------------------------------------
# If the environment variable is set, this command can be unchanged.
# Else, replace with the path to the "data" directory
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
sdms_fpath <- normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"))

## ----libraries, message=FALSE-------------------------------------------------
library(knitr)
library(ggplot2)
library(stringr)
library(reshape2)
library(Hmisc)
library(plyr)
library(dplyr)
library(mizr)
library(grid)
library(gridExtra)
old_theme <- mizr_set_theme(base_size = 18)

## ----load_data----------------------------------------------------------------
sdms_df <- read.csv(sdms_fpath, stringsAsFactors = TRUE)
sdms_df$dataset_metric <- as.factor(paste(as.character(sdms_df$dataset),
                                          as.character(sdms_df$metric), sep = "_"))
sdms_df$metric_dataset <- as.factor(paste(as.character(sdms_df$metric),
                                          as.character(sdms_df$dataset), sep = "_"))
factor_cols <- c("metric", "dataset", "system")
response_var <- "score"

## ----ten_step_block, fig.height=16, fig.width=22------------------------------
metric_list <- c("acc", "auc", "dpr", "f1", "precision", "recall")

accuracy_df <- sdms_df[sdms_df$metric == "acc", ]
accuracy_plot <- mizr_block_plot(accuracy_df, c("dataset"), "system", response_var) +
  coord_cartesian(ylim = c(0.0, 1.0))
accuracy_plot <- accuracy_plot +
  guides(color = "none") + xlab("dataset") +
  ggtitle("Block Plot of Systems vs. Datasets on \nMetric 'acc'")

auc_df <- sdms_df[sdms_df$metric == "auc", ]
auc_plot <- mizr_block_plot(auc_df, c("dataset"), "system", response_var) +
  coord_cartesian(ylim = c(0.0, 1.0))
auc_plot <- auc_plot +
  guides(color = "none") + xlab("dataset") +
  ggtitle("Block Plot of Systems vs. Datasets on \nMetric 'auc'")


dpr_df <- sdms_df[sdms_df$metric == "dpr", ]
dpr_plot <- mizr_block_plot(dpr_df, c("dataset"), "system", response_var) +
  coord_cartesian(ylim = c(0.0, 1.0))
dpr_plot <- dpr_plot +
  guides(color = "none") + xlab("dataset") +
  ggtitle("Block Plot of Systems vs. Datasets on \nMetric 'dpr'")

f1_df <- sdms_df[sdms_df$metric == "f1", ]
f1_plot <- mizr_block_plot(f1_df, c("dataset"), "system", response_var) +
  coord_cartesian(ylim = c(0.0, 1.0))
f1_plot <- f1_plot +
  guides(color = "none") + xlab("dataset") +
  ggtitle("Block Plot of Systems vs. Datasets on \nMetric 'f1'")

precision_df <- sdms_df[sdms_df$metric == "precision", ]
precision_plot <- mizr_block_plot(precision_df, c("dataset"), "system", response_var) +
  coord_cartesian(ylim = c(0.0, 1.0))
precision_plot <- precision_plot +
  guides(color = "none") + xlab("dataset") +
  ggtitle("Block Plot of Systems vs. Datasets on \nMetric 'precision'")

recall_df <- sdms_df[sdms_df$metric == "recall", ]
recall_plot <- mizr_block_plot(recall_df, c("dataset"), "system", response_var) +
  coord_cartesian(ylim = c(0.0, 1.0))
recall_plot <- recall_plot +
  guides(color = "none") + xlab("dataset") +
  ggtitle("Block Plot of Systems vs. Datasets on \nMetric 'recall'")


# Now use a grid or a wrap to place all 7 plots on one page
grid.arrange(accuracy_plot, auc_plot, dpr_plot, f1_plot,
  precision_plot, recall_plot,
  nrow = 2
)

## ----sign_test_plot-----------------------------------------------------------
block_df <- block_compute_df(sdms_df, c("dataset", "metric"), "system", response_var)
sign_test_df <- sign_test_compute_df(sdms_df, c("dataset", "metric"), "system", response_var,
  higher_is_better = TRUE, use_margins = TRUE, alpha = 0.05
)
brm_plot <- mizr_sign_test_plot(sdms_df, c("dataset", "metric"), "system", response_var,
  higher_is_better = TRUE, use_margins = TRUE,
  include_equal_comparisons = TRUE, alpha = 0.05, p_adj = "bonferroni",
  tile_text_size = rel(3.6)
)
brm_plot

brmn_plot <- mizr_sign_test_plot(sdms_df, c("dataset", "metric"), "system", response_var,
  higher_is_better = TRUE, use_margins = TRUE,
  include_equal_comparisons = TRUE, alpha = 0.05, p_adj = "none",
  tile_text_size = rel(3.6)
)
brmn_plot

## ----bc_rank_table, results="asis"--------------------------------------------
bc_df <- sign_test_compute_df(sdms_df, c("dataset", "metric"), "system", response_var,
  higher_is_better = TRUE, use_margins = TRUE,
  include_equal_comparisons = TRUE, alpha = 0.05, p_adj = "bonferroni"
)
bc_mar_df <- bc_df[bc_df$treatment_2 == "(all)", ]
bc_mar_df <- bc_mar_df[order(-bc_mar_df$frac_1_better_or_equal_than_2), ]
kable(bc_mar_df[, c("treatment_1", "frac_1_better_or_equal_than_2")],
      caption = "Block Comparison Ranking", digits = 4)

## ----main_eff-----------------------------------------------------------------
mizr_main_effects_plot(sdms_df, c("system", "dataset", "metric"), "score")
mizr_main_effects_hsd_plot(sdms_df, c("system", "dataset", "metric"), "score")

## ----main_eff_rank, results='asis'--------------------------------------------
main_eff_df <- main_effects_compute_df(sdms_df, c("system", "dataset", "metric"), "score")
main_mod_df <- main_eff_df[main_eff_df$variable == "system", ]
main_mod_df <- main_mod_df[order(-main_mod_df$score), ]
kable(main_mod_df[, c("value", "score")], caption = "Main Effects Ranking", digits = 4)

## ----make_rank_df-------------------------------------------------------------
system_rank_df <- sdms_df %>%
  arrange(dataset, metric, score) %>%
  group_by(dataset, metric) %>%
  mutate(rank = rank(-score)) %>%
  as.data.frame()

metric_rank_df <- sdms_df %>%
  arrange(dataset, system, score) %>%
  group_by(dataset, system) %>%
  mutate(rank = rank(-score)) %>%
  as.data.frame()

dataset_rank_df <- sdms_df %>%
  arrange(system, metric, score) %>%
  group_by(system, metric) %>%
  mutate(rank = rank(-score)) %>%
  as.data.frame()

## ----rank_plot_system, fig.height=16, fig.width=19----------------------------
mizr_tile_table_plot(system_rank_df, c("dataset", "metric"), "system", "rank",
                     use_margins = TRUE, tile_text_size = rel(3.6))

dataset_list <- c("adult", "hmda", "titanic")

adult_df <- system_rank_df[system_rank_df$dataset == "adult", ]
adult_plot <- mizr_tile_table_plot(adult_df, "metric", "system", "rank",
                                   use_margins = TRUE, tile_text_size = rel(3.6))
adult_plot <- adult_plot + ggtitle("system Rank for each Metric of \nadult Dataset")

hmda_df <- system_rank_df[system_rank_df$dataset == "hmda", ]
hmda_plot <- mizr_tile_table_plot(hmda_df, "metric", "system", "rank",
                                  use_margins = TRUE, tile_text_size = rel(3.6))
hmda_plot <- hmda_plot + ggtitle("system Rank for each Metric of \nhmda Dataset")

titanic_df <- system_rank_df[system_rank_df$dataset == "titanic", ]
titanic_plot <- mizr_tile_table_plot(titanic_df, "metric", "system", "rank",
                                     use_margins = TRUE, tile_text_size = rel(3.6))
titanic_plot <- titanic_plot + ggtitle("system Rank for each Metric of \ntitanic Dataset")

# Now use a grid or a wrap to place all 7 plots on one page
grid.arrange(adult_plot, hmda_plot, titanic_plot, nrow = 1)

## ----system_rank_analysis, results='asis'-------------------------------------
mrank_mar_df <- tile_table_compute_df(system_rank_df, c("dataset", "metric"),
                                      "system", "rank", use_margins = TRUE)
mr_df <- mrank_mar_df[mrank_mar_df$metric == "(all)" &
                        mrank_mar_df$dataset == "(all)" & mrank_mar_df$system != "(all)", ]
mr_df <- mr_df[order(mr_df$rank), ]
kable(mr_df[, c("system", "rank")],
      caption = "Ranking by Mean Rank on Metrics and Datasets", digits = 4)

## ----rank_plot_metrics, fig.height=16, fig.width=19---------------------------
mizr_tile_table_plot(metric_rank_df, c("dataset", "system"), "metric", "rank",
                     use_margins = TRUE, tile_text_size = rel(3.6))

## ----rank_metrics-------------------------------------------------------------
sign_test_df <- sign_test_compute_df(sdms_df, c("system", "dataset"), "metric", response_var,
  higher_is_better = TRUE, use_margins = TRUE, alpha = 0.05
)
brm_plot <- mizr_sign_test_plot(sdms_df, c("system", "dataset"), "metric", response_var,
  higher_is_better = TRUE, use_margins = TRUE,
  include_equal_comparisons = TRUE, alpha = 0.05, p_adj = "bonferroni",
  tile_text_size = rel(3.6)
)
brm_plot

## ----rank_datasets------------------------------------------------------------
sign_test_df <- sign_test_compute_df(sdms_df, c("system", "metric"), "dataset", response_var,
  higher_is_better = TRUE, use_margins = TRUE, alpha = 0.05
)
brm_plot <- mizr_sign_test_plot(sdms_df, c("system", "metric"), "dataset", response_var,
  higher_is_better = TRUE, use_margins = TRUE,
  include_equal_comparisons = TRUE, alpha = 0.05, p_adj = "bonferroni",
  tile_text_size = rel(3.6)
)
brm_plot

## ----rank_plot_datasets, fig.height=16, fig.width=19--------------------------
mizr_tile_table_plot(dataset_rank_df, c("system", "metric"), "dataset", "rank",
                     use_margins = TRUE, tile_text_size = rel(3.6))

## ----interactions-------------------------------------------------------------
mizr_interact_compare_plot(sdms_df, c("dataset", "metric"), "system", "score") +
  scale_shape_manual(values = c(1, 2, 16, 17, 18, 19, 3, 4, 5))
mizr_interact_compare_plot(sdms_df, c("metric_dataset"), "system", "score") +
  scale_shape_manual(values = c(1, 2, 16, 17, 18, 19, 3, 4, 5))

## ----histogram----------------------------------------------------------------
# Implement a double histogram
basic_hist_plot <- ggplot(data = sdms_df) +
  geom_histogram(aes(x = score, y = ..count..),
    color = "black", fill = "#DDDDDD",
    binwidth = 0.1, stat = "bin", boundary = 0, closed = "left", pad = TRUE
  ) +
  geom_text(aes(x = score, y = ..count.., label = ..count..),
    vjust = -0.3, binwidth = 0.1, stat = "bin", boundary = 0, closed = "left"
  ) +
  geom_histogram(aes(x = score, y = ..count..),
    color = "black", fill = "#FFFFFF",
    binwidth = 0.01, stat = "bin", boundary = 0, closed = "left", pad = TRUE
  ) +
  # geom_text(aes(x = score, y = ..count.., label = ..count..),
  #  vjust = -0.3, binwidth = 0.01, stat="bin", boundary=0, closed="left") +
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_y_continuous(expand = expansion(add = c(0, 10))) +
  ggtitle("Double Histogram of Counts of Score Values")

basic_hist_plot

