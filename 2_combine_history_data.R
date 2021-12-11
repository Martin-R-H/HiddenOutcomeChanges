library(tidyverse)
library(jsonlite)
library(tidyjson)

## Read in the files that contain all historical versions of trials
## in our sample
data_ct <- read_csv("historical_versions_ct.csv")
data_drks <- read_csv("historical_versions_DRKS.csv")

## separate for each dataset (to ease merging them), drop all variables
## that we do not need and mutate others
data_ct <- data_ct %>%
  select(!c(enrolment, enrolment_type, min_age, max_age, sex, gender_based, accepts_healthy_volunteers, contacts, sponsor_collaborators)) %>%
  # separate(col = outcome_measures, into = c(primary_outcomes, secondary_outcomes), sep = ',{"section":"Secondary Outcome Measures:"', extra = 'merge', ) # does not work?
  rename(primary_outcomes = outcome_measures) %>% add_column(secondary_outcomes = NA) %>% # workaround to create equal columns
  rename(id = nctid, status = overall_status, completion_date = primary_completion_date, completion_date_type = primary_completion_date_type)
## some other code chunks to split primnary and secondary outcomes
## mutate(primary_outcomes = str_split_fixed(outcome_measures, 'Secondary Outcome Measures', n = 2))
## mutate(str_split_fixed(outcome_measures, 'Secondary Outcome Measures', n = 2)) ## does not work
data_drks <- data_drks %>%
  mutate(criteria = paste('INCLUSION CRITERIA:', inclusion_criteria, 'EXCLUSION_CRITERIA:', exclusion_criteria)) %>%
  relocate(criteria, .before = inclusion_criteria) %>%
  select(!c(enrolment, enrolment_type, min_age, max_age, gender, contacts, inclusion_criteria, exclusion_criteria)) %>%
  add_column(completion_date_type = NA) %>%
  rename(id = drksid, status = recruitment_status, study_start_date = start_date, completion_date = closing_date)

## combine the datasets
data <- bind_rows(data_ct, data_drks)

## save the dataset
data %>%
  write_csv('combined_history_data.csv')


## some desperate code bits to solve the outcomes problem above

# sss <- data_ct %>%
#   select(nctid, outcome_measures)
# sss2 <- toJSON(data_ct$outcome_measures)
# str(sss)
# sss3 <- as.data.frame(jsonlite::fromJSON(sss2)) %>%
#   flatten()
# 
# read_json(data_ct$outcome_measures)
# 
# yyy <- as.data.frame(data_ct$outcome_measures) %>% tbl_json()
# 
# xxx <- tbl_json(sss, json.list = sss$outcome_measures)
# 
# xxx2 <- spread_values(xxx$outcome_measures)
# 
# xxx3 <- unnest_longer(xxx2, ..JSON)
