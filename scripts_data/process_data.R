# Run download_data.sh and then call this file
# data_directory is pointed to the environment variable `MIG_ANALYZER_DATA_DIR`
library(foreign)

# Inputs for user to change
data_dir <- Sys.getenv("MIG_ANALYZER_INTERNAL_DATA_DIR")

# OpenML Titanic Data
titanic_dir <- normalizePath(file.path(data_dir, "raw", "titanic_openml_40945"))
titanic_proc_dir <- normalizePath(file.path(data_dir, "processed", "titanic_openml_40945"))
titanic_fp <- normalizePath(file.path(titanic_dir, "titanic_40945_data.arff"))
titanic_df <- read.arff(titanic_fp)

# We need an id column, so we add record_id with the basic id of the row_number
# As this might have information, we will not provide this field to the training and test systems
titanic_df$record_id <- 1:nrow(titanic_df)
write.csv(titanic_df,
          normalizePath(file.path(titanic_proc_dir, "titanic_40945_data_all.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
# For ease of comparisons, we split these datasets into 75% training and 25% test set
set.seed(3715)
train_rows = sort(sample(nrow(titanic_df), nrow(titanic_df)*.75))
titanic_train_df <- titanic_df[train_rows, ]
titanic_test_df <- titanic_df[-train_rows, ]
write.csv(titanic_train_df,
          normalizePath(file.path(titanic_proc_dir, "titanic_40945_data_train.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
write.csv(titanic_test_df,
          normalizePath(file.path(titanic_proc_dir, "titanic_40945_data_test.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)

# UCI Adult Dataset
adult_dir <- normalizePath(file.path(data_dir, "raw", "adult_uci"))
adult_proc_dir <- normalizePath(file.path(data_dir, "processed", "adult_uci"))
adult_fp <- normalizePath(file.path(adult_dir, "adult_data.csv"))
adult_header_cols <- c("age", "workclass", "fnlwgt", "education", "education_num",
                       "marital_status", "occupation", "relationship", "race", "gender",
                       "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
adult_df <- read.csv(adult_fp, header = FALSE, col.names = adult_header_cols)
# We need an id column, so we add record_id with the basic id of the row_number
# As this might have information, we will not provide this field to the training and test systems
adult_df$record_id <- 1:nrow(adult_df)
write.csv(adult_df,
          normalizePath(file.path(adult_proc_dir, "adult_data_all.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
# For ease of comparisons, we split these datasets into 75% training and 25% test set
set.seed(4216)
train_rows = sort(sample(nrow(adult_df), nrow(adult_df)*.75))
adult_train_df <- adult_df[train_rows, ]
adult_test_df <- adult_df[-train_rows, ]
write.csv(adult_train_df,
          normalizePath(file.path(adult_proc_dir, "adult_data_train.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
write.csv(adult_test_df,
          normalizePath(file.path(adult_proc_dir, "adult_data_test.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)


# HMDA Dataset
hmda_dir <- normalizePath(file.path(data_dir, "raw", "hmda_2021"))
hmda_proc_dir <- normalizePath(file.path(data_dir, "processed", "hmda_2021"))
hmda_fp <- normalizePath(file.path(hmda_dir, "hmda_2021_county_24031_subset.csv"))
hmda_df <- read.csv(hmda_fp, header = TRUE)
# We need an id column, so we add record_id with the basic id of the row_number
# As this might have information, we will not provide this field to the training and test systems
hmda_df$record_id <- 1:nrow(hmda_df)
# We clean up the interest rate by removing the "Exempt" and the "NA" and converting it to a number
hmda_df <- hmda_df[(!is.na(hmda_df$interest_rate)) & (hmda_df$interest_rate != "Exempt"), ]
hmda_df$interest_rate <- as.numeric(hmda_df$interest_rate)
# Median interest rate is 2.99%, so we split at the 3% interval for a classification problem
hmda_df$interest_ge_3 <- hmda_df$interest_rate >= 3
write.csv(hmda_df,
          normalizePath(file.path(hmda_proc_dir, "hmda_subset_all.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
# For ease of comparisons, we split these datasets into 75% training and 25% test set
set.seed(5384)
train_rows = sort(sample(nrow(hmda_df), nrow(hmda_df)*.75))
hmda_train_df <- hmda_df[train_rows, ]
hmda_test_df <- hmda_df[-train_rows, ]
write.csv(hmda_train_df,
          normalizePath(file.path(hmda_proc_dir, "hmda_data_train.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
write.csv(hmda_test_df,
          normalizePath(file.path(hmda_proc_dir, "hmda_data_test.csv"), 
                        mustWork = FALSE),
          row.names = FALSE)
