library(tidyverse)
library(testthat)
set.seed(1025)

## this script prepares the export of another dataset to Numbat for ratings of
## within-registry outcome changes
## this is because the older cthist version did not properly read in some
## outcomes when there was a reference to results in the description

## before we created this file, we made some extensive checks, which can be
## found in the tests/T1_assess_additional_rating_file.R script 

## read in the data, filter those that we expect to have changes (either those
## that point to results and thus were subject to the bug, or those that have
## new versions later than the original download date), and filter for relevant
## variables
dat_export <-  read_csv('data/processed_history_data_short.csv') %>%
  filter(
    results_posted == TRUE |
    latest_version_date >= '2022-01-24'
  ) %>%
  select(
    c(
      id,
      # we need the dates to see at which phase results are posted
      first_reg_date,
      original_start_date,
      original_completion_date,
      publication_date,
      latest_version_date,
      results_posted,
      results_posted_date,
      # phases will be important when we use outcomes from earlier phases
      has_recruitment_phase, 
      has_post_completion_phase,
      has_post_publication_phase,
      # these vectors allow us to automatically filter trials that had no change
      # in that phase
      p_outcome_changed_recruitment,
      p_outcome_changed_postcompletion,
      p_outcome_changed_postpublication,
      p_outcome_start,
      p_outcome_last_recruitment,
      p_outcome_last_postcompletion,
      p_outcome_last_postpublication,
      s_outcome_start,
      s_outcome_last_recruitment,
      s_outcome_last_postcompletion,
      s_outcome_last_postpublication
    )
  )

## create a longer version where each row is a trial phase
dat_export_longer <- dat_export %>%
  pivot_longer(
    cols = c(
      p_outcome_changed_recruitment,
      p_outcome_changed_postcompletion,
      p_outcome_changed_postpublication
    ),
    names_to = 'trial_phase',
    values_to = 'p_outcome_changed',
    names_prefix = 'p_outcome_changed_'
  ) %>%
  relocate(c(trial_phase, p_outcome_changed), .after = results_posted_date)

## extract the relevant rows and create new outcome variables, in which only
## the relevant outcomes for that respective rating are extracted
dat_export_longer1 <- dat_export_longer %>%
  filter(trial_phase == 'recruitment') %>%
  filter(
    p_outcome_changed == TRUE &
    (results_posted_date >= original_start_date & 
       results_posted_date < original_completion_date)
  ) %>%
  mutate(P_Outcome_A = p_outcome_start) %>%
  mutate(P_Outcome_B = p_outcome_last_recruitment) %>%
  mutate(S_Outcome_A = s_outcome_start) %>%
  mutate(S_Outcome_B = s_outcome_last_recruitment)
dat_export_longer2 <- dat_export_longer %>%
  filter(trial_phase == 'postcompletion') %>%
  filter(
    p_outcome_changed == TRUE &
    ((results_posted_date >= original_start_date &
        results_posted_date < original_completion_date) |
        (results_posted_date >= original_completion_date &
        results_posted_date < publication_date))
  ) %>%
  mutate(
    P_Outcome_A = if_else(
      has_recruitment_phase == TRUE,
      p_outcome_last_recruitment,
      p_outcome_start
    )
  ) %>%
  mutate(P_Outcome_B = p_outcome_last_postcompletion) %>%
  mutate(
    S_Outcome_A = if_else(
      has_recruitment_phase == TRUE,
      s_outcome_last_recruitment,
      s_outcome_start
    )
  ) %>%
  mutate(S_Outcome_B = s_outcome_last_postcompletion)
dat_export_longer3 <- dat_export_longer %>%
  filter(trial_phase == 'postpublication') %>%
  filter(
    p_outcome_changed == TRUE &
    results_posted == TRUE
  ) %>%
  mutate(
    P_Outcome_A = case_when(
      has_post_completion_phase == TRUE ~ p_outcome_last_postcompletion,
      has_post_completion_phase == FALSE & has_recruitment_phase == TRUE ~ p_outcome_last_recruitment,
      TRUE ~ p_outcome_start
    )
  ) %>%
  mutate(P_Outcome_B = p_outcome_last_postpublication) %>%
  mutate(
    S_Outcome_A = case_when(
      has_post_completion_phase == TRUE ~ s_outcome_last_postcompletion,
      has_post_completion_phase == FALSE & has_recruitment_phase == TRUE ~ s_outcome_last_recruitment,
      TRUE ~ s_outcome_start
    )
  ) %>%
  mutate(S_Outcome_B = s_outcome_last_postpublication)

## bind the three datasets together and drop some variables we do not need
dat_export <- bind_rows(dat_export_longer1, dat_export_longer2, dat_export_longer3) %>%
  rename(trial_id = id) %>%
  arrange(trial_id, trial_phase) %>%
  select(
    !c(
      first_reg_date,
      original_start_date,
      original_completion_date,
      publication_date,
      latest_version_date,
      results_posted,
      results_posted_date,
      has_recruitment_phase,
      has_post_completion_phase,
      has_post_publication_phase,
      p_outcome_start,
      p_outcome_last_recruitment,
      p_outcome_last_postcompletion,
      p_outcome_last_postpublication,
      s_outcome_start,
      s_outcome_last_recruitment,
      s_outcome_last_postcompletion,
      s_outcome_last_postpublication
    )
  )

## save the dataset
dat_export %>%
  write_tsv('data/processed_history_data_Numbat_b.tsv')
