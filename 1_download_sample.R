library(tidyverse)
# library(devtools)
# install_github('bgcarlisle/cthist')
# the latest download (2022-04-12) was done with a development version of cthist
library(cthist)
library(testthat)

## download the complete IntoValue 1 and 2 datasets from GitHub
## (last downloaded and saved on 24 January 2022)
dat_IV <- read_csv('https://raw.githubusercontent.com/maia-sh/intovalue-data/main/data/processed/trials.csv')

## apply our predefined inclusion criteria, including removing
## duplicates that are present in both IntoValue versions
dat_IV_included <- dat_IV %>%
  filter(
    iv_interventional == TRUE &
    is_randomized == TRUE &
    (is_publication_2y == TRUE | is_publication_5y == TRUE)
  ) %>%
  filter(!(is_dupe == TRUE & iv_version == 1)) 

## test the number of rows
test_that(
  'Test whether the dataset has the expected number of rows',
  expect_equal(nrow(dat_IV_included), 1746)
)

## for export to the cthist package, select the IDs of all
## included trials
included_ids <- dat_IV_included %>%
  select(id)

## split the entries from ClinicalTrials.gov and DRKS
included_ids_ct <- included_ids %>%
  filter(str_detect(id, 'NCT')) %>%
  unlist() %>%
  as.vector()
included_ids_DRKS <- included_ids %>%
  filter(str_detect(id, 'DRKS')) %>%
  unlist() %>%
  as.vector()

## download the historical versions from ClinicalTrials.gov and DRKS separately
clinicaltrials_gov_download(included_ids_ct, 'data/historical_versions_ct.csv')
# ClinicalTrials.gov data last downloaded 2022-04-12,
# using most recent development version of cthist
# (an older version of the file, downloaded on 2022-01-17 and 2022-01-24,
# has also been saved in the repo under data/historical_versions_ct_2022-01-24
# for reference and comparison)
drks_de_download(included_ids_DRKS, 'data/historical_versions_DRKS.csv')
# DRKS data last downloaded on 2022-01-17 and 2022-01-24,
# using cthist version 0.1.4

## save the included IntoValue data, too
dat_IV_included %>%
  write_csv('data/data_IntoValue_included.csv')
