library(tidyverse)
library(jsonlite)

## read in the files that contain all historical versions of the included trials
dat_ct <- read_csv('data/historical_versions_ct.csv')
# compared to the 2022-01-24 version, this dataset has a few more rows, which
# is due to the fact that some history versions were added
dat_drks <- read_csv('data/historical_versions_DRKS.csv')

## in a first step, restructure the outcomes data from the ClinicalTrials.gov
## sample
## create an empty dataframe
dat_ct_outcomes <- tibble(
  nctid = character(),
  version_number = double(),
  primary_outcomes = character(),
  secondary_outcomes = character(),
  other_outcomes = character(),
  points_to_results = character(),
  primary_outcomes_number = numeric()
)
## for the ClinicalTrials.gov data, store primary, secondary, and other outcomes
## in separate columns
for (z in 1:nrow(dat_ct)) {
  # if outcomes data are stored in json format and resolve to a data frame, we
  # can turn them into a separate dataframe row by row, and split them up
  if (validate(as.character(dat_ct[z, 'outcome_measures'])) == TRUE) {
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
      p_res <- dat_o %>%
        filter(section == '[See Results Section.]') %>%
        toJSON() %>%
        as.list.data.frame()
      nrow_p_o <- dat_o %>%
        filter(section == 'Primary Outcome Measures:') %>%
        nrow()
      dat_q <- tibble(
        nctid = as.character(dat_ct[z, 'nctid']),
        version_number = as.double(dat_ct[z, 'version_number']),
        primary_outcomes = p_o,
        secondary_outcomes = s_o,
        other_outcomes = o_o,
        points_to_results = p_res,
        primary_outcomes_number = nrow_p_o
      )
    rm(dat_o, p_o, s_o, o_o, p_res, nrow_p_o)
    } else {
      # if outcomes data are stored in json format, but do not resolve to a data
      # frame, we assume there are no outcomes, but we will store the whole
      # variable in the primary_outcomes column for safety
      dat_q <- tibble(
        nctid = as.character(dat_ct[z, 'nctid']),
        version_number = as.double(dat_ct[z, 'version_number']),
        primary_outcomes = as.character(dat_ct[z, 'outcome_measures']),
        secondary_outcomes = NA,
        other_outcomes = NA,
        points_to_results = 'Does not point to results.',
        primary_outcomes_number = 0
      )
    }
  } else {
    # if outcomes data are not stored in json format, we assume there are no
    # outcomes, but we will store the whole variable in the primary_outcomes
    # column for safety
    dat_q <- tibble(
      nctid = as.character(dat_ct[z, 'nctid']),
      version_number = as.double(dat_ct[z, 'version_number']),
      primary_outcomes = as.character(dat_ct[z, 'outcome_measures']),
      secondary_outcomes = NA,
      other_outcomes = NA,
      points_to_results = 'Does not point to results.',
      primary_outcomes_number = NA
    )
  }
  # for each trial history version, take the split outcomes dataset and merge it
  dat_ct_outcomes <- bind_rows(dat_ct_outcomes, dat_q)
}

## separately for each dataset (to ease merging them), drop all variables
## that we do not need and mutate or rename others
## start with the ClinicalTrials.gov dataset
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
  left_join(dat_ct_outcomes, by = c('nctid', 'version_number')) %>%
  rename(
    id = nctid,
    status = overall_status,
    completion_date = primary_completion_date,
    completion_date_precision = primary_completion_date_precision,
    completion_date_type = primary_completion_date_type
  )
## do the same with the DRKS dataset
dat_drks <- dat_drks %>%
  mutate(
    criteria = paste(
      'INCLUSION CRITERIA:', inclusion_criteria, 'EXCLUSION_CRITERIA:', exclusion_criteria
    )
  ) %>%
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
  add_column(
    study_start_date_precision = NA,
    completion_date_precision = NA,
    completion_date_type = NA,
    other_outcomes = NA,
    points_to_results = NA,
    primary_outcomes_number = NA
  ) %>%
  relocate(study_start_date_precision, .after = start_date) %>%
  relocate(criteria, .before = primary_outcomes) %>%
  relocate(completion_date_precision, completion_date_type, .after = closing_date) %>%
  relocate(other_outcomes, .after = secondary_outcomes) %>%
  rename(
    id = drksid,
    status = recruitment_status,
    study_start_date = start_date,
    completion_date = closing_date
  )

## combine the datasets
dat_combined <- bind_rows(dat_ct, dat_drks) %>%
  mutate(
    points_to_results = if_else(
      points_to_results == '[{"section":"[See Results Section.]"}]',
      TRUE,
      FALSE
    )
  )

## save the dataset
dat_combined %>%
  write_csv('data/combined_history_data.csv')
