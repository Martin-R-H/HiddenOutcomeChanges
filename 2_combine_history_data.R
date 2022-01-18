library(tidyverse)
library(jsonlite)



## ---- OLD STRATEGY: sample of 25 ----

## read in the files that contain all historical versions of trials
## in our sample
dat_ct <- read_csv('data/historical_versions_ct.csv')
dat_drks <- read_csv('data/historical_versions_DRKS.csv')

## in a first step, restructure the outcomes data from the ClinicalTrials.gov
## sample
dat_ct_outcomes <- fromJSON(as.character(dat_ct[1, 'outcome_measures']))
p_o <- dat_ct_outcomes %>%
  filter(section == 'Primary Outcome Measures:') %>%
  toJSON() %>%
  as.list.data.frame()
s_o <- dat_ct_outcomes %>%
  filter(section == 'Secondary Outcome Measures:') %>%
  toJSON() %>%
  as.list.data.frame()
o_o <- dat_ct_outcomes %>%
  filter(section == 'Other Outcome Measures:') %>%
  toJSON() %>%
  as.list.data.frame()
dat_ct_outcomes <- tibble(primary_outcomes = p_o, secondary_outcomes = s_o, other_outcomes = o_o)
for (z in 2:nrow(dat_ct)) {
  dat_o <- fromJSON(as.character(dat_ct[z, 'outcome_measures']))
  if (is.data.frame(dat_o) == TRUE) {
    p_o <- dat_o %>%
      filter(section == 'Primary Outcome Measures:') %>%
      toJSON() %>%
      as.list.data.frame()
    s_o <- dat_o %>%
      filter(section == 'Secondary Outcome Measures:') %>%
      toJSON() %>%
      as.list.data.frame()
    o_o <- dat_o %>%
      filter(section == 'Other Outcome Measures:') %>%
      toJSON() %>%
      as.list.data.frame()
    dat_o <- tibble(primary_outcomes = p_o, secondary_outcomes = s_o, other_outcomes = o_o)
    }
  else {
    dat_o <- tibble(primary_outcomes = NA, secondary_outcomes = NA, other_outcomes = NA)
    }
  dat_ct_outcomes <- bind_rows(dat_ct_outcomes, dat_o)
}

## separately for each dataset (to ease merging them), drop all variables
## that we do not need and mutate others
dat_ct <- dat_ct %>%
  select(!c(
    enrolment,
    enrolment_type,
    min_age,
    max_age,
    sex,
    gender_based,
    accepts_healthy_volunteers,
    outcome_measures,
    contacts,
    sponsor_collaborators
    )) %>%
  bind_cols(dat_ct_outcomes) %>%
  rename(
    id = nctid,
    status = overall_status,
    completion_date = primary_completion_date,
    completion_date_type = primary_completion_date_type
    )
dat_drks <- dat_drks %>%
  mutate(criteria = paste('INCLUSION CRITERIA:', inclusion_criteria, 'EXCLUSION_CRITERIA:', exclusion_criteria)) %>%
  select(!c(
    enrolment,
    enrolment_type,
    min_age,
    max_age,
    gender,
    contacts,
    inclusion_criteria,
    exclusion_criteria
    )) %>%
  add_column(completion_date_type = NA, other_outcomes = NA) %>%
  relocate(criteria, .before = primary_outcomes) %>%
  relocate(completion_date_type, .after = closing_date) %>%
  relocate(other_outcomes, .after = secondary_outcomes) %>%
  rename(
    id = drksid,
    status = recruitment_status,
    study_start_date = start_date,
    completion_date = closing_date
    )

## combine the datasets
dat <- bind_rows(dat_ct, dat_drks)

## save the dataset
dat %>%
  write_csv('data/combined_history_data.csv')



## ---- NEW STRATEGY: run history scraper on all included trials ----

## read in the files that contain all historical versions of trials
## in our sample
dat_ct_all <- read_csv('data/historical_versions_ct_all.csv')
dat_drks_all <- read_csv('data/historical_versions_DRKS_all.csv')

## in a first step, restructure the outcomes data from the ClinicalTrials.gov
## sample
dat_ct_outcomes_all <- tibble(
  primary_outcomes = character(),
  secondary_outcomes = character(),
  other_outcomes = character(),
  primary_outcomes_number = numeric()
  )
for (z in 1:nrow(dat_ct_all)) {
  if (validate(as.character(dat_ct_all[z, 'outcome_measures'])) == TRUE) {
    dat_o <- fromJSON(as.character(dat_ct_all[z, 'outcome_measures']))
    if (is.data.frame(dat_o) == TRUE) {
      p_o <- dat_o %>%
        filter(section == 'Primary Outcome Measures:') %>%
        toJSON() %>%
        as.list.data.frame()
      s_o <- dat_o %>%
        filter(section == 'Secondary Outcome Measures:') %>%
        toJSON() %>%
        as.list.data.frame()
      o_o <- dat_o %>%
        filter(section == 'Other Outcome Measures:') %>%
        toJSON() %>%
        as.list.data.frame()
      nrow_p_o <- dat_o %>%
        filter(section == 'Primary Outcome Measures:') %>%
        nrow()
      dat_o <- tibble(primary_outcomes = p_o, secondary_outcomes = s_o, other_outcomes = o_o, primary_outcomes_number = nrow_p_o)
    } else {
      dat_o <- tibble(primary_outcomes = as.character(dat_ct_all[z, 'outcome_measures']), secondary_outcomes = NA, other_outcomes = NA, primary_outcomes_number = 0)
    }
  } else {
    dat_o <- tibble(primary_outcomes = as.character(dat_ct_all[z, 'outcome_measures']), secondary_outcomes = NA, other_outcomes = NA, primary_outcomes_number = NA)
  }
  dat_ct_outcomes_all <- bind_rows(dat_ct_outcomes_all, dat_o)
}

## separately for each dataset (to ease merging them), drop all variables
## that we do not need and mutate others
dat_ct_all <- dat_ct_all %>%
  select(!c(
    enrolment,
    enrolment_type,
    min_age,
    max_age,
    sex,
    gender_based,
    accepts_healthy_volunteers,
    outcome_measures,
    contacts,
    sponsor_collaborators
  )) %>%
  bind_cols(dat_ct_outcomes_all) %>%
  rename(
    id = nctid,
    status = overall_status,
    completion_date = primary_completion_date,
    completion_date_type = primary_completion_date_type
  )
dat_drks_all <- dat_drks_all %>%
  mutate(criteria = paste('INCLUSION CRITERIA:', inclusion_criteria, 'EXCLUSION_CRITERIA:', exclusion_criteria)) %>%
  select(!c(
    enrolment,
    enrolment_type,
    min_age,
    max_age,
    gender,
    contacts,
    inclusion_criteria,
    exclusion_criteria
  )) %>%
  add_column(completion_date_type = NA, other_outcomes = NA, primary_outcomes_number = NA) %>%
  relocate(criteria, .before = primary_outcomes) %>%
  relocate(completion_date_type, .after = closing_date) %>%
  relocate(other_outcomes, .after = secondary_outcomes) %>%
  rename(
    id = drksid,
    status = recruitment_status,
    study_start_date = start_date,
    completion_date = closing_date
  )

## combine the datasets
dat_all <- bind_rows(dat_ct_all, dat_drks_all)

## save the dataset
dat_all %>%
  write_csv('data/combined_history_data_all.csv')
