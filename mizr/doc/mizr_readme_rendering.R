## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 11)

## ----library_loading----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(mizr)

## ----install_check_code-------------------------------------------------------
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
old_theme <- mizr_set_theme(base_size = 12)
sdms_fpath <- normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"), mustWork = TRUE)
sdms_df <- read.csv(sdms_fpath, stringsAsFactors = TRUE)

mizr_tile_table_count_plot(sdms_df, c("dataset", "metric"), c("system"))
mizr_block_plot(sdms_df, c("dataset", "metric"), "system", "score") %>%
  eh_title("Block Plot of Systems over different Datasets and Metrics")

## ----quick_run----------------------------------------------------------------
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
old_theme <- mizr_set_theme(base_size = 12)
sdms_fpath <- normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"), mustWork = TRUE)
sdms_df <- read.csv(sdms_fpath, stringsAsFactors = TRUE)

mizr_tile_table_count_plot(sdms_df, c("dataset", "metric"), c("system"))

# As mizr_..._plot() methods return ggplot objects, we can augment them like ggplots.
mizr_histogram_plot(sdms_df, "score", binwidth = 0.05)
mizr_histogram_plot(sdms_df, "score", binwidth = 0.05) +
  coord_cartesian(xlim = c(0, 1), clip = "off")

# We can also pipe them to some of our mizr methods in enhance_plots.R
mizr_block_plot(sdms_df, c("dataset", "metric"), "system", "score",
                value_text_size = rel(3.2)) %>%
  eh_title("Block Plot of Systems over different \nDatasets and Metrics")

mizr_tile_table_plot(sdms_df, c("dataset", "metric"), "system", "score", use_margins = TRUE,
                     tile_text_size = rel(3.2))

mizr_box_plot(sdms_df, c("system"), "score")
mizr_order_plot(sdms_df, c("dataset", "metric", "system"), "score")
mizr_scatter_doe_plot(sdms_df, "system", "score", doe_plot_style = "enhanced")

