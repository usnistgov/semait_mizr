#!/bin/bash
cd $MIG_ANALYZER_INTERNAL_DATA_DIR
mkdir -p raw
mkdir -p processed
mkdir -p processed/adult_uci
mkdir -p processed/titanic_openml_40945
mkdir -p processed/hmda_2021
cd raw
mkdir -p adult_uci
# We do not use the adult.test in this example
curl https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data -o adult_uci/adult_data.csv
curl https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names -o adult_uci/adult_names.txt
mkdir -p titanic_openml_40945
curl https://api.openml.org/data/download/16826755/phpMYEkMl -o titanic_openml_40945/titanic_40945_data.arff
curl https://www.openml.org/api/v1/json/data/40945 -o titanic_openml_40945/titanic_40945_description.json
mkdir -p hmda_2021
#curl "https://ffiec.cfpb.gov/v2/data-browser-api/view/csv?counties=24031&years=2021&actions_taken=1,6&loan_purposes=1" -o hmda_2021/hmda_2021_county_24031_subset.csv
# For us, the above commented-out API query gave this URL below
curl "https://cfpb-hmda-public.s3.amazonaws.com/prod/data-browser/2021/filtered-queries/snapshot/38b05a97b0073a6000f91e78dc1af8f7.csv" -o hmda_2021/hmda_2021_county_24031_subset.csv
