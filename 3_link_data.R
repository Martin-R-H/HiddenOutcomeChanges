library(tidyverse)

## read in the included Into_Value trials
IV_data <- read_csv('included_data_IntoValue.csv') %>%
  select(c(id, registry, title, main_sponsor, study_type, intervention_type, phase, recruitment_status, allocation, start_date, primary_completion_date, doi, pmid, url, publication_date, pub_title, is_publication_2y, is_publication_5y))
