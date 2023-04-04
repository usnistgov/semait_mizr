# SEMAIT_mizr Analysis Software Library

Our work towards a Structured Evaluation Methodology for Artificial Intelligence Technology (SEMAIT) aims
to provide plots, tools, methods, and strategies to extract insights out of various machine learning (ML)
and Artificial Intelligence (AI) data.

Included in this software is the  MIg analyZeR (mizr) R software package that produces various plots. 
It was initially developed within the Multimodal Information Group (MIG) at the 
National Institute of Standards and Technology (NIST).

A quickstart is below. Full documentations (docs and vignettes) are available on [semait_mizr Gitlab pages](https://semait.ipages.nist.gov/semait_mizr)

**For ease of updating, all of the paths and inputs required are at the top of each R or Rmd file.**

The [R code to run the SEMAIT analysis on a sample dataset](scripts_semait/semait_analysis_sdms_script_code.R)
is provided in `scripts_semait/semait_analysis_sdms_script_code.R` of this repository. A [documented R Markdown of the same script](mizr/vignettes/semait_analysis_sdms_documented.Rmd) is available as the vignette semait_analysis_sdms_documented.Rmd

Please see our SEMAIT paper for more information (paper coming soon), and please cite our paper with the citation (citation coming soon)

This version of mizr is currently built and tested on version 4.2.2 of R with ggplot 3.4.1 and will work on any OS
and configuration that supports R 4.2.2 and ggplot 3.4.1. It has been tested both on Mac OS and Linux. As it has
been partially implemented on earlier versions of R and ggplot, some functions may have depreciation warnings.

The main mizr plotting methods are named `mizr_YYY_plot()`.
Every `mizr_YYY_plot()` plot method produces a plot as a ggplot object and **returns that ggplot object**. The benefit of this design decision is that many changes (including theme changes) can be customized by altering or augmenting the returned ggplot object (such as `<plot_object> + theme(...)`) without having to examine the mizr method source. A second design feature is that every ``mizr_YYY_plot()` is designed to call two sub-methods in sequence: A `zzz_compute_df()` method that comuptes the data frame; and a `zzz_produce_plot()` method that takes that computed dataframe and renders the plot. This design feature provides a way to get the raw data frame of any plot's data. More details are in the [MIg analyZeR (mizr) Plot Library Requirements vignette](mizr/vignettes/mizr_plot_library.Rmd).

## Contact

Please contact mizr@nist.gov with questions, comments, feedback, or issues.

## Contributors

Contributors to this code repository:

* Peter Fontana (NIST)



# Quick Start

Here is a quick start to get up and running with the mizr software package.

## Installing mizr

If using a terminal, mizr can be installed by going into the root directory of this repository and running

```bash
R CMD INSTALL --no-multiarch --with-keep.source mizr
```

Another way to install it is via RStudio. In RStudio, make a new project (or open a project) within the package folder mizr. That means that it is the `<repository_directory>/mizr` directory. After making a project, use the menu `Build -> Install and Restart` option. This will install the package `mizr`.

This installs the minimum packages to run mizr. To run the test suite and documentation code, you will need additional packages. To install these packages, run in a shell 

```bash
Rscript -e 'install.packages(c("covr", "DT", "devtools", "foreign", "htmltools", "knitr", "lintr", "pkgdown", "purrr", "rmarkdown", "roxygen2", "styler", "testthat", "tools"),dependencies=TRUE,repos="http://cran.rstudio.com", quiet=TRUE)'
```

or in an R console

```R
install.packages(c("covr", "DT", "devtools", "foreign", "htmltools", "knitr", "lintr", "pkgdown", "purrr", "rmarkdown", "roxygen2", "styler", "testthat", "tools"),dependencies=TRUE,repos="http://cran.rstudio.com", quiet=TRUE)
```

## Installation Check

First, load the R package in an R environment with

```R
library(mizr)
```

Then, if you wish to run a few plots to check, you can use one of the examples in the `data` directory. The code below loads an example set, processes it, and runs a few plots. To run this example, please change data_dir to be the location of the `data` directory of this repository. In this case the data dir is stored in a shell environment variable `MIG_ANALYZER_DATA_DIR`,  Although this example retrieves the directory from an environment variable, a direct path will work.

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
some additional plots. The output of the Installation Check is provided in the Vignette [MIg analyZeR (mizr) README Rendering](mizr/vignettes/mizr_readme_rendering.Rmd).

## Quick Run of Select Plots

This block of code takes the sdms_df, which is a data frame in experimental data format, loads it, and provides a few plots. This gives an overview of some of the plotting functions currently implemented. The output of the Quick Run is provided in the Vignette [MIg analyZeR (mizr) README Rendering](mizr/vignettes/mizr_readme_rendering.Rmd).

This code uses the previously assigned directory `data_dir`. O

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


## Full RMarkdown Examples (mizr Package vignettes)

To make fully-worked through examples more accessible with the package documentation, we have placed the R Markdown source scripts in `mizr/vignettes`, calling these examples _vignettes_. The making and placing of these vignettes allows these complete examples to be bundled with the package documentation and can be referred to in the "Articles" menu of the rendered documentation. 

Rendered vignettes can be found in the "Articles" section of the rendered documentation displayable on the [MIg analyZeR gitlab repository via gitlab pages](https://semait.ipages.nist.gov/). The version rendered is the version on master branch, and the package has its version number. Included is the vignette which provides the SEMAIT code to run the software as well as a documented analysis on that code for an example data. 

The [example data](data/raw/semait_sdms_1.csv) is a self-constructed set of tuples where we ran baseline machine learning systems on publicly-available datasets scored on a variety of metrics. The scripts to produce this data are in the `scripts_data` folder for those who wish to have the source code for this data (code is a combination of bash and R scripts).


Rendered vignettes can be produced with the R command:

```
devtools::build_vignettes()
```

and those rendered vignettes (and html files) will be in `/mizr/inst/docs`.  

## Additional Design Documentation

The package `mizr` has a custom ggplot theme that is used. Calling `mizr_set_theme()` will set the
mizr theme as the default them in the document for all plots. To use that theme, run

```
old_theme <- mizr_set_theme()
```

Otherwise your default ggplot theme will be used to produce the mizr plots.

All of the main plot methods start with `mizr` and can be found in the file `mizr_plots.R`. By 
design, each `mizr_method` is split into two submethods: one that produces the computed data frame
and one that produces the plots. This way, if you wish to get the data frame with the numbers
of any plot, simply run the first submethod that calls the data frame.


# Tests

There is a test suite each inside test directories inside the folders.

To test, please set the environment variable `MIG_ANALYZER_DATA_DIR` to point to the location of the data in this repository. For testing and some production, this is the `data` directory of your clone of this repository. This environment variable will be loaded by the R test suite to find the test data. To load that environment value in R, add it to your `.Renviron` file, typically found in `~/.Renviron`

To run all of the tests, run the file

```
./test_R_packages.sh
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

To embed the README.md into the home page of the site, copy the README.md to the mizr directory with the command below and then update the site links.

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

Certain equipment, instruments, software, or materials are identified in this paper in order to specify the experimental procedure adequately.  Such identification is not intended to imply recommendation or endorsement of any product or service by NIST, nor is it intended to imply that the materials or equipment identified are necessarily the best available for the purpose.



