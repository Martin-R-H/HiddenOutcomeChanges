library(tidyverse)

## read the csv with the PMIDs for each trial
dat_pmid <- read_csv('nctid-pmid.csv')

## read in our sample of the IntoValue data
dat_IV_included <- read_csv('included_data_IntoValue.csv')

## add publication information to the dataset
