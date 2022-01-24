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

## exclude those history versions before study start (i.e., version_date before original_start_date),
## but keep those versions with NA as original_start_date
dat <- dat %>%
  ungroup() %>%
  filter(version_date >= original_start_date)
# what about special cases, like the start date being NA first?

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



#### CONTINUE CINTINUE CONTINUE ###



## see about changes to outcomes at which timepoints
## (1)
## first, create logical vectors recruitment_phase (version_date < completion date)
## completion_phase (version_date >= completion date AND version_date < publication_date)
## postpublication_phase (version_date >= publication_date)
## (2)
## then extract four outcomes
# ifelse loop
# if outcome_changed == TRUE and version_date before completion_date
# create varialbe outcome_changed_precompletion == TRUE
# else create varialbe outcome_changed_precompletion == FALSE
# if outcome_changed == TRUE and version_date after completion_date but before publication_date
# create varialbe outcome_changed_precompletion == TRUE

## save a version with all historical versions
# dat %>%
#   write_csv('data/processed_history_data.csv')

## create a dataset with only those versions that have changes to the outcome, and add
## the IntoValue data
dat_IV_sample <- read_csv('data/sample_IntoValue.csv') %>%
  select(c(id, registry, title, main_sponsor, study_type, intervention_type, phase, recruitment_status, allocation, start_date, primary_completion_date, doi, pmid, url, publication_date, pub_title, is_publication_2y, is_publication_5y))
# dat_Numbat <- dat %>%
#   filter(primary_outcome_changed == TRUE) %>%
#   left_join(dat_IV, by = 'id')

##  for Numbat, save a file with only those versions that have changes to the outcome
# dat_Numbat %>%
#   write_csv('data/export_to_Numbat.csv')


#### ---- PILOT 1 ----

## for a first piloting, save 5 of our IntoValue trials in a separate file

## create a new sample first
pilot_sample_1 <- sample(unique(dat_IV_sample$id), 5)

dat_IV_sample %>%
  filter(id %in% pilot_sample_1) %>%
  write_csv('data/PILOT_5_IV.csv')

dat %>%
  filter(id %in% pilot_sample_1) %>%
  write_csv('data/PILOT_5_HISTORICAL.csv')
