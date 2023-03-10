# Make a docker container that has R and python3 installed

# Start from an RStudio image with latex
FROM --platform=linux/x86_64 rocker/verse:4.2.2
RUN apt-get update -y && apt-get upgrade -y -qq && apt-get dist-upgrade -y -qq&& apt-get update -y -qq && apt-get install -y -qq emacs tmux libudunits2-dev libharfbuzz-dev libfribidi-dev libmagick++-dev cargo gdal-bin
RUN apt-get install -y -qq python3-pip python3-dev
RUN Rscript -e 'install.packages(c("dplyr","ggplot2","grid", "reshape2","scales", "tibble", "yaml"),dependencies=TRUE,repos="http://cran.rstudio.com", quiet=TRUE)'
RUN Rscript -e 'install.packages(c("covr", "DT", "devtools", "foreign", "htmltools", "knitr", "lintr", "pkgdown", "purrr", "rmarkdown", "roxygen2", "styler", "testthat", "tools"),dependencies=TRUE,repos="http://cran.rstudio.com", quiet=TRUE)'
RUN apt-get install -y -qq pandoc
