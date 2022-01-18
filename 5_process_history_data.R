library(tidyverse)

set.seed(946)



## ---- OLD STRATEGY: draw a sample of 25 ----

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
dat2 <- dat %>%
  coalesce(postlaunch_temp, 0) %>%
  # filter(!(id == c('NCT01601769'))) %>%
  group_by(id) %>%
  mutate(original_start_date = study_start_date[which.min(postlaunch_temp)])
# not working due to NAs in the postlaunch_temp variable (there are groups in which there are only NAs)
# dat<- dat %>%
#   group_by(id) %>%
#   ifelse(
#     is.na(min(postlaunch_temp)),
#     mutate(original_start_date = study_start_date[which.min(postlaunch_temp)]),
#     mutate(original_start_date = NA)
#   )
# 
# dat<- dat %>%
#   group_by(id) %>%
#   ifelse(
#     is.na(cumsum(postlaunch_temp)),
#     mutate(original_start_date = NA),
#     mutate(original_start_date = study_start_date[which.min(postlaunch_temp)])
#   )


## create a variable that represents the first 'completion' date, i.e.
## the first date where the trial registry entry that has a status of
## Active, not recruiting, Completed, Terminated (ClinicalTrials.gov
## terminology), Recruiting complete, follow-up complete, or Recruiting
## stopped after recruiting started (DRKS terminology) - this is the 
## original completion date
dat3 <- dat %>%
  filter(!(id == c('NCT00111345', 'NCT01601769'))) %>%
  group_by(id) %>%
  mutate(original_completion_date = postcompletion_temp[which.min(postcompletion_temp)])
dat$original_completion_date <- as.Date(dat$original_completion_date, origin="1970-01-01")

## create a last completion date
# TO DO

## drop the intermediate variables
dat <- dat %>%
  select(!c(postlaunch_temp, postcompletion_temp))

## filter for those history versions before study start (i.e., version_date before original_start_date)

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


#### ---- for a first piloting, save 5 of our IntoValue trials in a separate file ----

## create a new sample first
pilot_sample_1 <- sample(unique(dat_IV_sample$id), 5)

dat_IV_sample %>%
  filter(id %in% pilot_sample_1) %>%
  write_csv('data/PILOT_5_IV.csv')

dat %>%
  filter(id %in% pilot_sample_1) %>%
  write_csv('data/PILOT_5_HISTORICAL.csv')



## ---- NEW STRATEGY: run history scraper on all included trials ----


## read in the file
dat_all <- read_csv('data/combined_history_data_all.csv')

## create a 'date of first registration' variable
dat_all <- dat_all %>%
  group_by(id) %>%
  mutate(first_reg_date = min(version_date))

## determine those versions with changes to their outcomes and mark them as such (logical vector)
## Step 1: Identify "run" lengths for outcomes within a trial
outcome_runs <- rle(paste(dat_all$id, dat_all$primary_outcomes))
## Step  2: Make an `outcome_run` column that assigns a number to each
## "run" of outcomes
dat_all <- dat_all %>%
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
dat_all <- dat_all %>%
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

## save the file
dat_all %>%
  write_csv('data/combined_history_data_all.csv')
