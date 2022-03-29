library(tidyverse)
# library(devtools)
# remove.packages('cthist')
# install_github('bgcarlisle/cthist')
## this ensures that for this branch, version 1.0 of cthist is downloaded (on 2022-03-21),
## instead of version 0.1.4 that was used for the first download of the data
library(cthist)

set.seed(946)

## download the complete IntoValue 1 and 2 datasets from GitHub
## (last downloaded on 21 March 2022)
dat_IV <- read_csv('https://raw.githubusercontent.com/maia-sh/intovalue-data/main/data/processed/trials.csv')

dat_2 <- sample_n(dat_IV, 20)



## apply our predefined inclusion criteria, including removing
## duplicates that are present in both IntoValue versions
dat_IV_included <- dat_IV %>%
  filter(iv_interventional == TRUE & is_randomized == TRUE & (is_publication_2y == TRUE | is_publication_5y == TRUE)) %>%
  filter(!(is_dupe == TRUE & iv_version == 1)) 

## count the rows
nrow(dat_IV_included)
# 1746

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

# (NEW)
# shuffle order to get a more random sample
included_ids_ct2 <- sample(included_ids_ct, 140)

## download the historical versions from ClinicalTrials.gov and DRKS separately
## (last downloaded 21 March 2022, as a completely new dataset using cthist version 1.0)
clinicaltrials_gov_download(included_ids_ct2, 'data/historical_versions_ct.csv')
drks_de_download(included_ids_DRKS, 'data/historical_versions_DRKS.csv')

## save the included IntoValue data, too
dat_IV_included %>%
  write_csv('data/data_IntoValue_included.csv')
