library(tidyverse)

## read in the file
d <- read_csv('combined_history_data.csv')

## create a 'date of first registration' variable
d <- d %>%
  group_by(id) %>%
  mutate(first_reg_date = min(version_date))

## create a 'final status' variable (since we are looking at published trials, most should be 'completed')
d <- d %>%
  group_by(id) %>%
  mutate(final_status = status[which.max(version_date)])

## create two temporary variables to determine the critical timepoints
d$postlaunch_temp <- ifelse(
  d$status == "Recruiting" |
  d$status == "Enrolling by invitation" |
  d$status == "Active, not recruiting" |
  d$status == "Completed" |
  d$status == "Terminated" |
  d$status == "Recruiting ongoing" |
  d$status == "Recruiting complete, follow-up complete" |
  d$status == "Recruiting stopped after recruiting started" |
  d$status == "Recruiting suspended on temporary hold",
  d$version_date,
  NA
)
d$postcompletion_temp <- ifelse(
  d$status == "Active, not recruiting" |
  d$status == "Completed" |
  d$status == "Terminated" |
  d$status == "Recruiting stopped after recruiting started" |
  d$status == "Recruiting complete, follow-up complete",
  d$version_date,
  NA
)

## create a variable that represents the first 'launch' date, i.e.
## the first date where the trial registry entry that has a status of
## Recruiting, Enrolling by invitation, Active, not recruiting, Completed,
## Terminated (ClinicalTrials.gov terminologxy), Recruiting ongoing, Recruiting
## complete, follow-up complete, Recruiting stopped after recruiting started, or
## Recruiting suspended on temporary hold (DRKS terminology) - before this point,
## the trials were not yet recruiting)
d <- d %>%
  group_by(id) %>%
  mutate(original_start_date = study_start_date[which.min(postlaunch_temp)])
# code has a problems with NAs

## create a variable that represents the first 'completion' date, i.e.
## the first date where the trial registry entry that has a status of
## Active, not recruiting, Completed, Terminated (ClinicalTrials.gov
## terminology), Recruiting complete, follow-up complete, or Recruiting
## stopped after recruiting started (DRKS terminology) - this is the 
## original completion date
d <- d %>%
  group_by(id) %>%
  mutate(original_completion_date = postcompletion_temp[which.min(postcompletion_temp)])
d$original_completion_date <- as.Date(d$original_completion_date, origin="1970-01-01")
# code has a problems with NAs

## create a last completion date
# TO DO

## drop the intermediate variables
d <- d %>%
  select(!c(postlaunch_temp, postcompletion_temp))

## filter for those history versions before study start (i.e., version_date before original_start_date)

## determine those versions with changes to their outcomes and mark them as such (logical vector)
## Step 1: Identify "run" lengths for outcomes within a trial
outcome_runs <- rle(paste(d$id, d$primary_outcomes))
## Step  2: Make an `outcome_run` column that assigns a number to each
## "run" of outcomes
d <- d %>%
  mutate(
    outcome_run = rep(
      seq_along(outcome_runs$lengths),
      outcome_runs$lengths
    )
  )
# USED TO WORK ON MY WINDOWS PC. FUR UNKNOWN REASONS, IT DOES NOT ON MAC
## Step 3: Create a logical vector that indicates whether an
## outcome has been changed or not - this is done by grouping
## by "runs" of outcomes and selecting only the first of each
d <- d %>%
  group_by(outcome_run) %>%
  mutate(temp = min(version_number)) %>%
  mutate(primary_outcome_changed = ifelse(
    version_number == temp,
    TRUE,
    FALSE
  )
  ) %>%
  ungroup() %>%
  select(!c(temp_outcome_run))

## save a version with all historial versions
d %>%
  write_csv('processed_history_data.csv')

## create a dataset with only those versions that have changes to the outcome, and add
## the IntoValue data
d_IV <- read_csv('included_data_IntoValue.csv') %>%
  select(c(id, registry, title, main_sponsor, study_type, intervention_type, phase, recruitment_status, allocation, start_date, primary_completion_date, doi, pmid, url, publication_date, pub_title, is_publication_2y, is_publication_5y))
d_Numbat <- d %>%
  filter(primary_outcome_changed == TRUE) %>%
  left_join(d_IV, by = 'id')

##  for Numbat, save a file with only those versions that have changes to the outcome
d_Numbat %>%
  write_csv('export_to_Numbat.csv')
