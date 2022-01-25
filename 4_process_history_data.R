library(tidyverse)

set.seed(946)

## read in the file
dat <- read_csv('data/combined_history_data.csv')

## create a 'date of first registration' variable
dat <- dat %>%
  group_by(id) %>%
  mutate(first_reg_date = min(version_date))

## create a 'final status' variable (since we are looking at published trials, most should be 'completed')
dat <- dat %>%
  group_by(id) %>%
  mutate(final_status = status[which.max(version_date)])

## create two temporary variables to determine the critical timepoints
dat$postlaunch_temp <- ifelse(
  dat$status == "Recruiting" |
  dat$status == "Enrolling by invitation" |
  dat$status == "Active, not recruiting" |
  dat$status == "Completed" |
  dat$status == "Terminated" |
  dat$status == "Recruiting ongoing" |
  dat$status == "Recruiting complete, follow-up complete" |
  dat$status == "Recruiting stopped after recruiting started" |
  dat$status == "Recruiting suspended on temporary hold",
  dat$version_date,
  NA
)
dat$postcompletion_temp <- ifelse(
  dat$status == "Active, not recruiting" |
  dat$status == "Completed" |
  dat$status == "Terminated" |
  dat$status == "Recruiting stopped after recruiting started" |
  dat$status == "Recruiting complete, follow-up complete",
  dat$version_date,
  NA
)

## create a variable that represents the first 'launch' date, i.e.
## the first date where the trial registry entry that has a status of
## Recruiting, Enrolling by invitation, Active, not recruiting, Completed,
## Terminated (ClinicalTrials.gov terminology), Recruiting ongoing, Recruiting
## complete, follow-up complete, Recruiting stopped after recruiting started, or
## Recruiting suspended on temporary hold (DRKS terminology) - before this point,
## the trials were not yet recruiting)

## first, we have to create a temporary dataframe of those trials that have no 
## 'postlaunch' value, i.e., they apparently never start recruiting
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(postlaunch_temp))) %>%
  mutate(original_start_date = NA)

## then create the variable
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(postlaunch_temp))) %>%
  mutate(original_start_date = study_start_date[which.min(postlaunch_temp)])

## bind the two datasets together
dat <- bind_rows(dat_temp1, dat_temp2)

## create a variable that represents the first 'completion' date, i.e.
## the first date where the trial registry entry that has a status of
## Active, not recruiting, Completed, Terminated (ClinicalTrials.gov
## terminology), Recruiting complete, follow-up complete, or Recruiting
## stopped after recruiting started (DRKS terminology) - this is the 
## original completion date

## first, we have to create a temporary dataframe of those trials that have no 
## 'postcompletion' value, i.e., they apparently are never completed
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(postcompletion_temp))) %>%
  mutate(original_completion_date = NA)

## then create the variable
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(postcompletion_temp))) %>%
  mutate(original_completion_date = completion_date[which.min(postcompletion_temp)])
# old code from Murph (why did he do it like this?):
#   mutate(original_completion_date = postcompletion_temp[which.min(postcompletion_temp)])
# dat_temp2$original_completion_date <- as.Date(dat_temp2$original_completion_date, origin="1970-01-01")

## bind the two datasets together
dat <- bind_rows(dat_temp1, dat_temp2)

## drop the intermediate variables
dat <- dat %>%
  select(!c(postlaunch_temp, postcompletion_temp))

## retrieve the publication dates from the IntoValue dataset
dat_IV_extended_pb <- read_csv('data/data_IntoValue_extended.csv') %>%
  select(id, publication_date)
dat <- dat %>%
  left_join(dat_IV_extended_pb, by = 'id')

## create a variable that indicates the point the study is currently at
dat <- dat %>%
  ungroup() %>%
  mutate(
    trial_phase = case_when(
      version_date < original_start_date ~ 'pre-recruitment',
      version_date >= original_start_date & version_date < original_completion_date ~ 'recruitment',
      version_date >= original_completion_date & version_date < publication_date ~ 'post-completion',
      version_date >= publication_date ~ 'post-publication',
      TRUE ~ 'unknown'
    )
  )

## determine those versions with changes to their outcomes and mark them as such (logical vector)
## Step 1: Identify "run" lengths for outcomes within a trial
outcome_runs <- rle(paste(dat$id, dat$primary_outcomes))
## Step  2: Make an `outcome_run` column that assigns a number to each
## "run" of outcomes
dat <- dat %>%
  ungroup() %>%
  mutate(
    outcome_run = rep(
      seq_along(outcome_runs$lengths),
      outcome_runs$lengths
    )
  )
## Step 3: Create a logical vector that indicates whether an
## outcome has been changed or not - this is done by grouping
## by "runs" of outcomes and selecting only the first of each
dat <- dat %>%
  group_by(outcome_run) %>%
  mutate(temp = min(version_number)) %>%
  mutate(primary_outcome_changed = ifelse(
    version_number == temp,
    TRUE,
    FALSE
  )
  ) %>%
  ungroup() %>%
  select(!c(temp, outcome_run))

## currently, the first history version of each trial reports
## outcome changes as 'True' - we create a new variable that 
## corrects this
dat <- dat %>%
  mutate(
    primary_outcome_changed_x = if_else(
      version_number == 1, FALSE, primary_outcome_changed
    )
  ) %>%
  select(!primary_outcome_changed) %>%
  rename(primary_outcome_changed = primary_outcome_changed_x)

## create variables that indicate whether outcomes have been changed
dat <- dat %>%
  group_by(id, trial_phase) %>%
  mutate(
    outcome_changed_prerecruitment = if_else(
      !all(primary_outcome_changed == FALSE) & trial_phase == 'pre-recruitment', TRUE, FALSE
    )
  )
dat <- dat %>%
  group_by(id, trial_phase) %>%
  mutate(
    outcome_changed_recruitment = if_else(
      !all(primary_outcome_changed == FALSE) & trial_phase == 'recruitment', TRUE, FALSE
    )
  )
dat <- dat %>%
  group_by(id, trial_phase) %>%
  mutate(
    outcome_changed_postcompletion = if_else(
      !all(primary_outcome_changed == FALSE) & trial_phase == 'post-completion', TRUE, FALSE
    )
  )
dat <- dat %>%
  group_by(id, trial_phase) %>%
  mutate(
    outcome_changed_postpublication = if_else(
      !all(primary_outcome_changed == FALSE) & trial_phase == 'post-publication', TRUE, FALSE
    )
  )

## these logical vectors just indicate outcome changes in their respective
## groups - we now make it so that the indicate for all history versions of
## the same trial id
dat <- dat %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    outcome_changed_prerecruitment = as.logical(max(outcome_changed_prerecruitment)),
    outcome_changed_recruitment = as.logical(max(outcome_changed_recruitment)),
    outcome_changed_postcompletion = as.logical(max(outcome_changed_postcompletion)),
    outcome_changed_postpublication = as.logical(max(outcome_changed_postpublication))
  )

## save the long version of the historical data
dat %>%
  write_csv('data/processed_history_data_long.csv')

## then extract the outcomes at the four different timepoints
dat_short <- dat %>%
  group_by(id) %>%
  mutate(
    outcome_start = primary_outcomes[which.min(version_number) ]
  )
# & which(trial_phase == 'recruitment')

## save the 'short' version, in which each line is just one trial,
## after combining the data with the IntoValue dataset

# save a short version with slice_head and combine with IntoValue Data
dat_IV_extended <- read_csv('data/data_IntoValue_extended.csv') %>%
  select(c(id, registry, title, main_sponsor, study_type, intervention_type, phase, recruitment_status, allocation, start_date, primary_completion_date, doi, pmid, url, publication_date, pub_title, is_publication_2y, is_publication_5y))
dat_short <- dat_short %>%
  left_join(dat_IV_extended, by = 'id')

dat_short %>%
  write_csv('data/processed_history_data_short.csv')

dat_short %>%
  select() %>% # drop the unnecessary stuff
  write_csv('data/processed_history_data_Numbat.csv')


#### ---- PILOT 1 ----

## for a first piloting, save 5 of our IntoValue trials in a separate file
## (files for pilot first saved on xxxxxxx)

## create a new sample first
pilot_sample_1 <- sample(unique(dat_IV_sample$id), 5)

dat_IV_sample %>%
  filter(id %in% pilot_sample_1) %>%
  write_csv('data/PILOT_5_IV.csv')

dat %>%
  filter(id %in% pilot_sample_1) %>%
  write_csv('data/PILOT_5_HISTORICAL.csv')
