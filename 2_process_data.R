library(tidyverse)
library(jsonlite)

## Read in the files that contain all historical versions of trials
## in our sample
## downloaded on 02-12-2021 (piloting)
data_ct <- read_csv("historical_versions_ct.csv")
data_drks <- read_csv("historical_versions_DRKS.csv")

## separate for each dataset (to ease merging them), drop all variables
## that we do not need and mutate others
data_ctxx <- data_ct %>%
  select(!c(enrolment, enrolment_type, min_age, max_age, sex, gender_based, accepts_healthy_volunteers, contacts, sponsor_collaborators)) %>%
  separate(col = outcome_measures, into = c(primary_outcomes, secondary_outcomes), sep = ',{"section":"Secondary Outcome Measures:"', extra = 'merge', ) # works?
## some other code chunks
## mutate(primary_outcomes = str_split_fixed(outcome_measures, 'Secondary Outcome Measures', n = 2))
## mutate(str_split_fixed(outcome_measures, 'Secondary Outcome Measures', n = 2)) ## does not work
data_drks <- data_drks %>%
  select(!c(enrolment, enrolment_type, min_age, max_age, gender, contacts)) %>%
  mutate(criteria = paste('INCLUSION CRITERIA:', inclusion_criteria, 'EXCLUSION_CRITERIA:', exclusion_criteria)) %>%
  relocate(criteria, .before = inclusion_criteria) %>%
  select(!c(inclusion_criteria, exclusion_criteria))

## just to check
data_ctxx %>% write_csv('testcwt.csv')
data_ctxxxxx <- prettify(data_ct)


## TO DO:
## rename overall_status (ct) and recruitment_status (DRKS) to status
## (DRKS) start_date to study_start_date
## primary_completion_date (ct) and closing_date (DRKS) to completion_date
## primary_completion_date_type - there need to be NAs in DRKS, as it does not have that variable