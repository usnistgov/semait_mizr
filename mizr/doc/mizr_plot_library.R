## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 5)

## ----load_dependencies--------------------------------------------------------
library(ggplot2)
library(dplyr)
library(knitr) # For displaying in html
library(yaml) # for reading yaml files in R
library(reshape2)

## ----load_package-------------------------------------------------------------
library(mizr)

## ----input_paths--------------------------------------------------------------
# If the environment variable is set, this command can be unchanged.
# Else, replace with the path to the "data" directory
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")

## -----------------------------------------------------------------------------
args(view_plot)
args(save_plot)

