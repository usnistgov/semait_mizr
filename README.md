# SEMAIT_mizr Analysis Software Library

Our work towards a Structured Evaluation Methodology for Artificial Intelligence Technology (SEMAIT) aims
to provide plots, tools, methods, and strategies to extract insights out of various machine learning (ML)
and Artificial Intelligence (AI) data.

Included in this software is the  MIg analyZeR (mizr) R software package that produces various plots. 
It was initially developed within the Multimodal Information Group (MIG) at the 
National Institute of Standards and Technology (NIST).

A quickstart is below. Full documentations (docs and vignettes) are pre-build and the index html page is available in this branch at [mizr/docs/index.html](mizr/docs/index.html)

**For ease of updating, all of the paths and inputs required are at the top of each R file.**

This repository contains the package mizr, as well as the template script for our work towards a Structured Evaluation Methodology for Artificial Intelligence Technology (SEMAIT). 

This software is also used for our work on Towards a Structured Evaluation Methodology for Artificial Intelligence Technology (SEMAIT). The [R code to run the SEMAIT analysis on a sample dataset](scripts_semait/semait_analysis_sdms_script_code.R)
is provided in `scripts_semait/semait_analysis_sdms_script_code.R` of this repository. A [documented R Markdown of the same script](mizr/vignettes/semait_analysis_sdms_documented.Rmd) is available as the vignette semait_analysis_sdms_documented.Rmd

Please see our SEMAIT paper for more information (paper coming soon), and please cite our paper with the citation (citation coming soon)

This version of mizr is currently built and tested on version 4.2.2 of R with ggplot 3.4.1 and will work on any OS
and configuration that supports R 4.2.2 and ggplot 3.4.1. It has been tested both on Mac OS and Linux. As it has
been implemented over different versions of R and ggplot, some functions may have depreciation warnings.

## Contact

Please contact mizr@nist.gov with questions, comments, feedback, or issues.

## Contributors

Contributors to this code repository:

* Peter Fontana (NIST)



# Quick Start

Here is a quick start to get up and running with the mizr tool. Right now mizr is an R package without a CLI. A CLI is to come shortly

## Installing mizr

If using a terminal, mizr can be installed by going into the root directory of this repository and running

```bash
R CMD INSTALL --no-multiarch --with-keep.source mizr
```

Another way to install it is via RStudio. In RStudio, make a new project (or open a project) within the package folder mizr. That means that it is the `<repository_directory>/mizr` directory. After making a project, use the menu `Build -> Install and Restart` option. This will install the package `mizr`.


## Installation Check

First, load the R package in an R environment with

```R
library(mizr)
```

Then, if you wish to run a few plots to check, you can use one of the examples in the `data` directory. The code below loads an example set, processes it, and runs a few plots. To run this example, please change data_dir to be the location of the `data` directory of this repository. Although this example retrieves the directory from an environment variable, a direct path will work.

```R
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
```

Then load the supporting libraries

Then run the code below

```R 
library(mizr)
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
old_theme <- mizr_set_theme()
sdms_fpath <- normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"), mustWork = TRUE)
sdms_df <- read.csv(sdms_fpath, stringsAsFactors = TRUE)

mizr_tile_table_count_plot(sdms_df, c("dataset", "metric"), c("system"))
mizr_block_plot(sdms_df, c("dataset", "metric"), "system", "score") %>%
  eh_title("Block Plot of Systems over different Datasets and Metrics")
```

It takes the sdms, which is a data frame in experimental data format, loads it, the accompanying metadata file, and calls a selection of the plotting methods. This gives a quick check of
the installation. The next subsection gives a quick view of
some additional plots. The output of the Installation Check is provided in the Vignette *MIg analyZeR (mizr) README Rendering*.

## Quick Run of Select Plots

This block of code takes the sdms_df, which is a data frame in experimental data format, loads it, and provides a few plots. This gives an overview of some of the plotting functions currently implemented. The output of the Quick Run is provided in the Vignette *MIg analyZeR (mizr) README Rendering*.

This code uses the previously assigned directory `data_dir`.

```R 
library(ggplot2)
library(dplyr)
library(mizr)
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
old_theme <- mizr_set_theme()
sdms_fpath <- normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"), mustWork = TRUE)
sdms_df <- read.csv(sdms_fpath, stringsAsFactors = TRUE)

mizr_tile_table_count_plot(sdms_df, c("dataset", "metric"), c("system"))

# As mizr_..._plot() methods return ggplot objects, we can augment them like ggplots.
mizr_histogram_plot(sdms_df, "score", binwidth = 0.05)
mizr_histogram_plot(sdms_df, "score", binwidth = 0.05) +
  coord_cartesian(xlim = c(0, 1), clip = "off")

# We can also pipe them to some of our mizr methods in enhance_plots.R
mizr_block_plot(sdms_df, c("dataset", "metric"), "system", "score") %>%
  eh_title("Block Plot of Systems over different \nDatasets and Metrics")

mizr_tile_table_plot(sdms_df, c("dataset", "metric"), "system", "score", use_margins = TRUE)

mizr_box_plot(sdms_df, c("system"), "score")
mizr_order_plot(sdms_df, c("dataset", "metric", "system"), "score")
mizr_scatter_doe_plot(sdms_df, "system", "score", doe_plot_style = "enhanced")
```


## mizr Package vignettes

Rendered vignettes can be found in the "Articles" section of the rendered documentation displable on the [MIg analyZeR gitlab repository via gitlab pages](https://pcf.ipages.nist.gov/mig_analyzer). The version rendered is the version on master branch, and the package has its version number. Included is the vignette which provides the SEMAIT code to run the software as well as a documented analysis on that code for an example dataset.


Rendered vignettes can be produced with the R command

```
devtools::build_vignettes()
```

and vignettes will be in `/mizr/inst/docs` which will have the html files.  

## Additional Design Documentation

The package `mizr` has a custom ggplot theme that is used. Calling `mizr_set_theme` will set the
mizr theme as the default them in the document for all plots. To use that theme, run

```
old_theme <- mizr_set_theme()
```

Otherwise your default ggplot theme will be used to produce the mizr plots.

All of the main plot methods start with `mizr` and can be found in the file `mizr_plots.R`. By 
design, each `mizr_method` is split into two submethods: one that produces the computed data frame
and one that produces the plots. This way, if you wish to get the data frame with the numbers
of any plot, simply run the first submethod that calls the data frame.

* Requirements specification of the input metadata YAML (.yml) file is in [Analyzer Input Metadata Requirements](design_doc/Analyzer_Input_Metadata_Requirements.md)


# Tests

There is a test suite each inside test directories inside the folders.

To test, please set the environment variable `MIG_ANALYZER_DATA_DIR` to point to the location of the data in this repository. For testing and some production, this is the `data` directory of your clone of this repository. This environment variable will be loaded by the R test suite to find the test data. To load that environment value in R, add it to your `.Renviron` file, typically found in `~/.Renviron`

To run all of the tests, run the file

```
./test_R_package.sh
```

in the root directory. This will run the R tests and coverage metrics. The commands that are being run to do the testing and coverage are below for those that do not wish to run a shell script.


## R: Automated Tests

Currently the tests are only a stub. However, to run the R current tests, go into the `./mizr` directory and run in an terminal

```
Rscript -e 'library(testthat);devtools::test(reporter="progress")'
```

If one wishes to test it in an R terminal, one can use this

```
library(testthat)
devtools::test(reporter="progress")
```

to test the package, or the command below to test

```
test_dir("./tests/testthat", reporter="progress")
```

the directory of tests.

## R: Code Coverage of Tests

To get code coverage, go into the `./mizr` directory and run in an terminal

```
Rscript -e 'library(covr);package_coverage(combined_types=FALSE)'
```


In R, the commands are

```
library(covr)
package_coverage(combined_types=FALSE)
```

A report with

```
library(covr)
report()
```

gives an interactive report for in-depth coverage and can be saved with

```
library(covr)
report(package_coverage(combined_types=FALSE),file="R_coverage_report.html",browse=FALSE)
```

# R: Code Quality

This package conforms to the `lintr` package, excluding the documented vignettes (because `knitr` produces blocks of commented code, which fail the `lintr`). To run the code cheks, within the `mizr` directory, in an R terminal run

```
Rscript -e "library(lintr);lintr::lint_package()"
```

This `lintr` uses the configurations defined in `mizr/.lintr`

This code conforms to the automated code styling package `styler`. To run the styler, run 


```
Rscript -e "library(styler);styler:::style_pkg()"
```

# R: Package Documentation

Package documentation is partially complete as documentation is in progress. However, there is
a tutorial vignette available.

R documentation is generated to .Rd files through `roxygen2` with,

```
devtools::document()
```



and is converted to an .html page with

```
library(pkgdown)
pkgdown::build_site()
```

To embed the README.md into the home page of the site, copy the README.md to the mizr directory with

```
cp README.md mizr/README.md
```

Since the vignettes require the `mizr` package to be installed, please install the package prior to
building the vignettes. To build the vignettes, run in R.

```
devtools::build_vignettes()
```

.Rd files are in `/mizr/man` and `index.html` is in `/mizr/docs`, and vignettes will be in `/mizr/inst/docs` which will have the html files.


# LICENSE

The license is documented in the [LICENSE file](LICENSE.md) and on the [NIST website](https://www.nist.gov/topics/data/public-access-nist-research/copyright-fair-use-and-licensing-statements-srd-data-and).

# Disclaimer

Certain commercial entities, equipment, or materials may be identified in this document in order to describe an experimental
procedure or concept adequately. Such identification is not intended to imply recommendation or endorsement by the National
Institute of Standards and Technology, nor is it intended to imply that the entities, materials, or equipment mentioned are
necessarily the best available for the purpose. All copyrights and trademarks are properties of their respective owners.





