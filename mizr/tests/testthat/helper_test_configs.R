#' This directory contains all of the configuration needed for testing
#' Here this points to the data directory for testing

# Directory that points to where the data is
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
## If you wish to change the hardcode for your implemention, you can use:
# data_dir <- <path_to_data_dir>
## However, the CI uses the environment variable to point to the data dir for testing
