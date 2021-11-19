library(tidyverse)

set.seed(946)

## download the complete IntoValue 1 and 2 datasets from GitHub
## downloaded on 10-11-2021
data <- read_csv('https://raw.githubusercontent.com/maia-sh/intovalue-data/main/data/processed/trials.csv')

## apply our predefined inclusion criteria and count the rows
included_data <- data %>%
  filter(iv_interventional == TRUE & is_randomized == TRUE & is_publication_5y == TRUE) # talk to Maia

nrow(included_data)
# 1800

## draw a sample, at first for piloting
sample <- included_data %>%
  select(id) %>%
  sample_n(25) # pilot sample

## save the entries from ClinicalTrials.gov and DRKS separately
sample %>%
  rename(nctid = 'id') %>%
  filter(registry == "ClinicalTrials.gov") %>%
  write_csv('nctids.csv')
sample %>%
  rename(drksid = 'id') %>%
  filter(registry == "DRKS")  %>%
  write_csv('drksids.csv')

## The full historical versions of trial registry entries in this
## sample must be downloaded using the clinicaltrials.gov.R and the
## DRKS.de.R scripts found here:
## https://codeberg.org/bgcarlisle/ClinicalTrialsHistoryScraper
## When finished, save the resulting CSV files to this directory as:
## historical_versions_ct.csv
## historical_versions_DRKS.csv
