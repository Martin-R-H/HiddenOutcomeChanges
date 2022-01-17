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



## ---- OLD STRATEGY: draw a sample of 25 ----

## draw a sample, at first for piloting
sample_nctids <- dat_IV_included %>%
  select(id) %>%
  sample_n(25) # pilot sample

## split the entries from ClinicalTrials.gov and DRKS
sample_nctids_ct <- sample_nctids %>%
  filter(str_detect(id, 'NCT')) %>%
  unlist() %>%
  as.vector()
sample_nctids_DRKS <- sample_nctids %>%
  filter(str_detect(id, 'DRKS')) %>%
  unlist() %>%
  as.vector()

## download the historical versions from ClinicalTrials.gov and DRKS separately
## (insert last download date here, maybe)
clinicaltrials_gov_download(sample_nctids_ct, 'data/historical_versions_ct.csv')
drks_de_download(sample_nctids_DRKS, 'data/historical_versions_DRKS.csv')

## save the IntoValue sample, too
dat_IV_sample <- dat_IV_included %>%
  filter(id %in% sample_nctids$id) %>%
  filter(!(is_dupe == TRUE & iv_version == 1)) # remove duplicates that are present in both IntoValue versions
dat_IV_sample %>%
  write_csv('data/sample_IntoValue.csv')



## ---- NEW STRATEGY: run history scraper on all included trials ----

## select IDs
ids <- dat_IV_included %>%
  select(id)

## split the entries from ClinicalTrials.gov and DRKS
ids_ct <- ids %>%
  filter(str_detect(id, 'NCT')) %>%
  unlist() %>%
  as.vector()
ids_DRKS <- ids %>%
  filter(str_detect(id, 'DRKS')) %>%
  unlist() %>%
  as.vector()

## download the historical versions from ClinicalTrials.gov and DRKS separately
## (insert last download date here, maybe)
clinicaltrials_gov_download(ids_ct, 'data/historical_versions_ct_all.csv')
drks_de_download(ids_DRKS, 'data/historical_versions_DRKS_all.csv')

## save the IntoValue sample, too
dat_IV_no_dupes <- dat_IV_included %>%
  filter(!(is_dupe == TRUE & iv_version == 1)) # remove duplicates that are present in both IntoValue versions
dat_IV_no_dupes %>%
  write_csv('data/data_IntoValue_no_dupes.csv')

## save the sample nctids for possible later use
## to extract PMIDs (deprecated)
# sample_nctids %>%
#   rename(nctid = id) %>%
#   write_csv('data/nctids.csv')