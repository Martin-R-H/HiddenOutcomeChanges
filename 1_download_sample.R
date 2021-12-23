library(tidyverse)
library(cthist)

set.seed(946)

## download the complete IntoValue 1 and 2 datasets from GitHub
## (insert last download date here, maybe)
dat_IV <- read_csv('https://raw.githubusercontent.com/maia-sh/intovalue-data/main/data/processed/trials.csv')

## apply our predefined inclusion criteria and count the rows
dat_IV_included <- dat_IV %>%
  filter(iv_interventional == TRUE & is_randomized == TRUE & (is_publication_2y == TRUE | is_publication_5y == TRUE))

nrow(dat_IV_included)
# 1800

## draw a sample, at first for piloting
sample <- dat_IV_included %>%
  select(id) %>%
  sample_n(25) # pilot sample

## save the entries from ClinicalTrials.gov and DRKS separately
sample_ct <- sample %>%
  filter(str_detect(id, 'NCT')) %>%
  unlist() %>%
  as.vector()
sample_DRKS <- sample %>%
  filter(str_detect(id, 'DRKS')) %>%
  unlist() %>%
  as.vector()

## download the historical versions from ClinicalTrials.gov and DRKS separately
## (insert last download date here, maybe)
clinicaltrials_gov_download(sample_ct, 'historical_versions_ct.csv')
drks_de_download(sample_DRKS, 'historical_versions_DRKS.csv')

## save the IntoValue sample, too
dat_IV_included_sample <- dat_IV_included %>%
  filter(id %in% sample$id) %>%
  filter(!(is_dupe == TRUE & iv_version == 1)) # remove duplicates that are present in both IntoValue versions
dat_IV_included_sample %>%
  write_csv('included_data_IntoValue.csv')
