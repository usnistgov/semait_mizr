context("Testing Plots on SEMAIT Sample Data")

test_that("SEMAIT Data, Scatter Plot Title", {
  sdms_fpath <- normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"),
    mustWork = TRUE
  )
  sdms_df <- read.csv(sdms_fpath, stringsAsFactors = TRUE)
  curr_plot <- mizr_block_plot(sdms_df, c("metric", "dataset"), "system", "score")
  block_title <- get_title(curr_plot)
  expected_title <- "Block Plot with Blocking Factor system"
  expect_identical(block_title, expected_title)
  new_expected_title <- "Block Plot of Systems over various datasets and metrics"
  rev_plot <- eh_title(
    curr_plot,
    "Block Plot of Systems over various datasets and metrics"
  )
  new_block_title <- get_title(rev_plot)
  expect_identical(new_block_title, new_expected_title)
  new_block_title_pipe <- rev_plot %>% get_title()
  expect_identical(new_block_title_pipe, new_expected_title)
})
