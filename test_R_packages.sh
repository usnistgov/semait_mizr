#!/bin/bash

CURR_DIR=$(pwd)

# change to R directory
echo ""
echo ""
echo "===================================="
echo "Testing R package mizr"
echo "===================================="
cd ./mizr
echo ""
echo "# Unit Tests"
Rscript -e 'library(testthat);devtools::test(reporter="progress")'

echo ""
echo "# Code Coverage Report"
Rscript -e 'library(covr);package_coverage(line_exclusions = "R/zzz.R", combined_types=FALSE)'
# Produce report
echo ""
Rscript test_R_coverage_report_commands.R
test -d test_coverage || mkdir -p test_coverage
rm -rf test_coverage/lib
mv lib test_coverage/lib
mv R_coverage_report.html test_coverage/

echo ""
echo ""
echo "Load 'test_coverage/R_coverage_report.html' to get more interactive information on code coverage, including a line-by-line report of which lines were not covered"

cd $CURR_DIR
