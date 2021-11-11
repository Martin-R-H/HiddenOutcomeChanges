library(tidyverse)

## Read in the files that contain all historical versions of trials
## in our sample
## downloaded on 10-11-2021 (piloting)
data_ct <- read_csv("historical_versions_ct.csv")
data_drks <- read_csv("historical_versions_DRKS.csv")

## separate for each dataset (to ease merging them), drop all variables
## that we do not need and mutate others
data_ctxx <- data_ct %>%
  select(!c(enrolment, enrolment_type, min_age, max_age, sex, gender_based, accepts_healthy_volunteers, contacts)) %>%
  mutate(str_split_fixed(outcome_measures, 'Secondary Outcome Measures', n = 2)) ## does not work
## not yet sure how to do this
data_drks <- data_drks %>%
  select(!c(enrolment, enrolment_type, min_age, max_age, gender, contacts)) %>%
  mutate(criteria = paste('INCLUSION CRITERIA:', inclusion_criteria, 'EXCLUSION_CRITERIA:', exclusion_criteria)) %>%
  relocate(criteria, .before = inclusion_criteria) %>%
  select(!c(inclusion_criteria, exclusion_criteria))

## just to check
data_ctxx %>% write_csv('testcwt.csv')
data_drks %>% write_csv('testdrks.csv')
