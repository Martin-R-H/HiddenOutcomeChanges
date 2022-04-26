library(tidyverse)
library(testthat)

set.seed(946)



## ---- LONG VERSION: create a 'long' version of the history data ----

## read in the file
dat <- read_csv('data/combined_history_data.csv')
nrow_beginning <- nrow(dat)



## ---- LONG VERSION: create a few new variables ----

## create a 'date of first registration' variable
dat <- dat %>%
  group_by(id) %>%
  mutate(first_reg_date = min(version_date)) %>%
  ungroup()

## create a 'first status' variable (it indicates the status the trial has at
## registration)
dat <- dat %>%
  group_by(id) %>%
  mutate(first_status = status[which.min(version_date)]) %>%
  ungroup()

## create a 'final status' variable (since we are looking at published trials,
## most should be 'completed')
dat <- dat %>%
  group_by(id) %>%
  mutate(final_status = status[which.max(version_date)]) %>%
  ungroup()



## ---- LONG VERSION: determine critical timepoints ----

## create three temporary variables to determine the critical timepoints
dat$recruitment_temp <- ifelse(
  dat$status == "Recruiting" |
  dat$status == "Enrolling by invitation" |
  dat$status == "Recruiting ongoing",
  dat$version_date,
  NA
)
dat$postlaunch_temp <- ifelse(
  dat$status == "Recruiting" |
  dat$status == "Enrolling by invitation" |
  dat$status == "Recruiting ongoing" |
  dat$status == "Active, not recruiting" |
  dat$status == "Recruiting suspended on temporary hold" |
  # dat$status == "Recruiting complete, follow-up continuing" |
  dat$status == "Recruiting complete, follow-up complete" |
  dat$status == "Recruiting stopped after recruiting started" |
  dat$status == "Completed" |
  dat$status == "Terminated",
  dat$version_date,
  NA
)
dat$postcompletion_temp <- ifelse(
  dat$status == "Recruiting complete, follow-up complete" |
  dat$status == "Recruiting stopped after recruiting started" |
  dat$status == "Completed" |
  dat$status == "Terminated",
  dat$version_date,
  NA
)

## create a variable that represents the 'original' study start date, that is,
## the first start date where the trial registry entry that has a status of
## Recruiting, Enrolling by invitation, Active, not recruiting, Completed,
## Terminated (ClinicalTrials.gov terminology), Recruiting ongoing, Recruiting
## complete, follow-up complete, Recruiting stopped after recruiting started, or
## Recruiting suspended on temporary hold (DRKS terminology) - before this point,
## the trials were not yet recruiting

## first, we have to create a temporary dataframe of those trials that have no 
## 'postlaunch' value, i.e., they apparently never start recruiting or stay 'unknown'
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(postlaunch_temp))) %>%
  mutate(original_start_date = NA) %>%
  mutate(original_start_date_precision = NA) %>%
  ungroup()

## for the trials that do report a 'postlaunch' variable, create another temporary
## dataframe and create a variable for the 'original' study start date
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(postlaunch_temp))) %>%
  mutate(original_start_date = study_start_date[which.min(postlaunch_temp)]) %>%
  mutate(original_start_date_precision = study_start_date_precision[which.min(postlaunch_temp)]) %>%
  ungroup()

## bind the two datasets together
test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

## original start date - alternative 1:
## in some cases, our first option to determine the 'original' study start date
## fails - for example, when a trial gets set to recruiting, completed, etc. at 
## some point, but reports no study start date at that point in time - in this
## case, we take the first reported study start date (even if it is added after
## recruitment has started)
dat <- dat %>%
  group_by(id) %>%
  mutate(
    original_start_date_alt1 = if_else(
      is.na(original_start_date),
      first(na.omit(study_start_date), order_by = version_number),
      original_start_date
    )
  ) %>%
  mutate(
    original_start_date_precision = if_else(
      is.na(original_start_date_precision),
      first(na.omit(study_start_date_precision), order_by = version_number),
      original_start_date_precision
    )
  ) %>%
  ungroup()

## original start date - alternative 2:
## if the first and second options do not work (which would be because a trial
## never reports a study start date at all), we will just take the date at which
## the trial was set to 'recruiting'
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(recruitment_temp))) %>%
  mutate(original_start_date_alt2 = original_start_date_alt1) %>%
  mutate(original_start_date_precision = original_start_date_precision) %>%
  ungroup()
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(recruitment_temp))) %>%
  mutate(
    original_start_date_alt2 = if_else(
      is.na(original_start_date_alt1),
      version_date[which.min(recruitment_temp)],
      original_start_date_alt1
    )
  ) %>%
  mutate(
    original_start_date_precision = if_else(
      is.na(original_start_date_alt1) & !is.na(original_start_date_alt2),
      'day', # because the version date is always precise to the day
      original_start_date_precision
    )
  ) %>%
  ungroup()
test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

## also, create a variable that indicates by which way the original start
## date was determined
dat <- dat %>%
  group_by(id) %>%
  mutate(
    original_start_date_type = case_when(
      !is.na(original_start_date) ~ 'start date at launch or later',
      (is.na(original_start_date) & !is.na(original_start_date_alt1)) == TRUE ~ 'first reported start date (Alternative 1)',
      (is.na(original_start_date_alt1) & !is.na(original_start_date_alt2)) == TRUE ~ 'date at which trial gets set to recruiting (Alternative 2)'
    )
  ) %>%
  ungroup()

## and then rename the other variables and drop some
dat <- dat %>%
  select(
    !c(
      original_start_date,
      original_start_date_alt1
    )
  ) %>%
  rename(original_start_date = original_start_date_alt2)

test_that(
  'test whether all trials now have a start date',
  expect_equal(sum(is.na(dat$original_start_date)), 0)
)

## create a variable that represents the first 'completion' date, i.e.
## the first date where the trial registry entry that has a status of
## Completed, Terminated (ClinicalTrials.gov terminology), Recruiting
## complete, follow-up complete, or Recruiting stopped after recruiting
## started (DRKS terminology) - this is the original completion date

## first, we have to create a temporary dataframe of those trials that have no 
## 'postcompletion' value, i.e., they apparently are never completed
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(postcompletion_temp))) %>%
  mutate(original_completion_date = NA) %>%
  mutate(original_completion_date_precision = NA) %>%
  ungroup()

## for the trials that do report a 'postcompletion' value, create another temporary
## dataframe and create a variable for the 'original' completion date
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(postcompletion_temp))) %>%
  mutate(original_completion_date = completion_date[which.min(postcompletion_temp)]) %>%
  mutate(original_completion_date_precision = completion_date_precision[which.min(postcompletion_temp)]) %>%
  ungroup()

## bind the two datasets together
test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

## original completion date - alternative 1:
## if our first option to determine the original completion start date fails (for
## example, because a trial reports no completion date at completion), we take
## the first reported completion date (even if it is added after the study has 
## been completed)
dat <- dat %>%
  group_by(id) %>%
  mutate(
    original_completion_date_alt1 = if_else(
      is.na(original_completion_date),
      first(na.omit(completion_date), order_by = version_number),
      original_completion_date
    )
  ) %>% 
  mutate(
    original_completion_date_precision = if_else(
      is.na(original_completion_date_precision),
      first(na.omit(completion_date_precision), order_by = version_number),
      original_completion_date_precision
    )
  ) %>%
  ungroup()

## original completion date - alternative 2:
## if the first and second options do not work (which would be because a trial
## never reports a completion date at all), we will just take the date at
## which the trial was set to 'completed'
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(postcompletion_temp))) %>%
  mutate(original_completion_date_alt2 = original_completion_date_alt1) %>%
  mutate(original_completion_date_precision = original_completion_date_precision) %>%
  ungroup()
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(postcompletion_temp))) %>%
  mutate(
    original_completion_date_alt2 = if_else(
      is.na(original_completion_date_alt1),
      version_date[which.min(postcompletion_temp)],
      original_completion_date_alt1
    )
  ) %>%
  mutate(
    original_completion_date_precision = if_else(
      is.na(original_completion_date_alt1) & !is.na(original_completion_date_alt2),
      'day', # because the version date is always precise to the day
      original_completion_date_precision
    )
  ) %>%
  ungroup()
test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

## also, create a variable that indicates by which way the original completion
## date was determined
dat <- dat %>%
  group_by(id) %>%
  mutate(
    original_completion_date_type = case_when(
      !is.na(original_completion_date) ~ 'completion date at completion or later',
      (is.na(original_completion_date) & !is.na(original_completion_date_alt1)) == TRUE ~ 'first reported completion date (Alternative 1)',
      (is.na(original_completion_date_alt1) & !is.na(original_completion_date_alt2)) == TRUE ~ 'date at which trial gets set to completed (Alternative 2)'
    )
  ) %>%
  ungroup()

## and then rename the other variables and drop some, including the
## intermediate variables from the beginning
dat <- dat %>%
  select(
    !c(
      original_completion_date,
      original_completion_date_alt1,
      recruitment_temp,
      postlaunch_temp,
      postcompletion_temp
    )
  ) %>%
  rename(original_completion_date = original_completion_date_alt2)

## retrieve the publication dates from the IntoValue dataset
dat_IV_extended_pb <- read_csv('data/data_IntoValue_extended.csv') %>%
  select(id, publication_date)

## check whether both datasets have the same number of unique IDs
length(unique(dat_IV_extended_pb$id))
# 1746, as was expected (see also the script 1_download_sample.R for that)
length(unique(dat$id))
# 1746 again, which is the expected outcome
# (in an earlier version of this script, we had 6 additional cases, probably
# because our original download was based on an older version of the IV dataset,
# but those were dropped when we re-downloaded the dataset)

## combine the datasets
dat <- dat %>%
  left_join(dat_IV_extended_pb, by = 'id')
test_that(
  'test the new dataset has the same number of rows as in the beginning (i.e., nothing was lost)',
  expect_equal(nrow(dat), nrow_beginning)
)

## create a new variable that indicates the date of the latest version
dat <- dat %>%
  group_by(id) %>%
  mutate(latest_version_date = max(version_date)) %>%
  ungroup()
  
## here, create a new variable that indicates whether a trial's outcomes point
## to results at some point (this only applies to ClinicalTrials.gov trials)
dat <- dat %>%
  group_by(id) %>%
  mutate(
    results_posted = if_else(
      any(points_to_results == TRUE),
      TRUE,
      FALSE
    )
  ) %>%
  ungroup()

## add new variable indicating the date of the first historical version that
## points to results
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(any(points_to_results == TRUE)) %>%
  mutate(
    results_posted_date = version_date[detect_index(points_to_results, isTRUE)]
  ) %>%
  ungroup()
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!any(points_to_results == TRUE) | any(is.na(points_to_results))) %>%
  mutate(results_posted_date = NA) %>%
  ungroup()
test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

## for each historical version, create a variable that indicates the point the
## study is currently at
dat <- dat %>%
  mutate(
    trial_phase = case_when(
      version_date < original_start_date ~ 'pre_recruitment',
      version_date >= original_start_date & version_date < original_completion_date ~ 'recruitment',
      version_date >= original_completion_date & version_date < publication_date ~ 'post_completion',
      version_date >= publication_date ~ 'post_publication',
      TRUE ~ 'unknown'
    )
  ) %>%
  arrange(id, version_number) # to avoid errors in the next step

## determine those versions with changes to their outcomes and mark them as such (logical vector)
## Step 1: Identify "run" lengths for outcomes within a trial
outcome_runs <- rle(paste(dat$id, dat$primary_outcomes))
## Step  2: Make an `outcome_run` column that assigns a number to each
## "run" of outcomes
dat <- dat %>%
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
  mutate(
    primary_outcome_changed = ifelse(
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
    primary_outcome_changed_temp = if_else(
      version_number == 1,
      FALSE,
      primary_outcome_changed
    )
  ) %>%
  select(!primary_outcome_changed) %>%
  rename(primary_outcome_changed = primary_outcome_changed_temp)

## create boolean vectors that indicate whether a trial actually has
## each of the phases
dat <- dat %>%
  group_by(id) %>%
  mutate(
    has_pre_recruitment_phase = if_else(
      any(trial_phase == 'pre_recruitment'),
      TRUE,
      FALSE
    ),
    has_recruitment_phase = if_else(
      any(trial_phase == 'recruitment'),
      TRUE,
      FALSE
    ),
    has_post_completion_phase = if_else(
      any(trial_phase == 'post_completion'),
      TRUE,
      FALSE
    ),
    has_post_publication_phase = if_else(
      any(trial_phase == 'post_publication'),
      TRUE,
      FALSE
    ),
    has_unknown_phase = if_else(
      any(trial_phase == 'unknown'),
      TRUE,
      FALSE
    )
  ) %>%
  ungroup()

## create variables that indicate whether outcomes have been changed
## and that turn into NA if that phase does not exist
dat <- dat %>%
  group_by(id, trial_phase) %>%
  mutate(
    p_outcome_changed_prerecruitment = case_when(
      has_pre_recruitment_phase == FALSE ~ NA,
      any(primary_outcome_changed == TRUE) & trial_phase == 'pre_recruitment' ~ TRUE,
      TRUE ~ FALSE
    ),
    p_outcome_changed_recruitment = case_when(
      has_recruitment_phase == FALSE ~ NA,
      any(primary_outcome_changed == TRUE) & trial_phase == 'recruitment' ~ TRUE,
      TRUE ~ FALSE
    ),
    p_outcome_changed_postcompletion = case_when(
      has_post_completion_phase == FALSE ~ NA,
      any(primary_outcome_changed == TRUE) & trial_phase == 'post_completion' ~ TRUE,
      TRUE ~ FALSE
    ),
    p_outcome_changed_postpublication = case_when(
      has_post_publication_phase == FALSE ~ FALSE,
      # this is different from the other phases, where we set the value to NA if
      ## the trial does not have that phase - however, if a trial has no 'post-
      # publication' phase, we can probably safely assume that the outcomes were
      # left untouched after the trial has been published
      any(primary_outcome_changed == TRUE) & trial_phase == 'post_publication' ~ TRUE,
      TRUE ~ FALSE
    ),
    p_outcome_changed_unknown = case_when(
      has_unknown_phase == FALSE ~ NA,
      any(primary_outcome_changed == TRUE) & trial_phase == 'unknown' ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()

## the logical vectors above may just indicate outcome changes in their respective
## groups - we now make it so that the indicate for all history versions of
## the same trial id
dat <- dat %>%
  group_by(id) %>%
  mutate(
    p_outcome_changed_prerecruitment = as.logical(max(p_outcome_changed_prerecruitment)),
    p_outcome_changed_recruitment = as.logical(max(p_outcome_changed_recruitment)),
    p_outcome_changed_postcompletion = as.logical(max(p_outcome_changed_postcompletion)),
    p_outcome_changed_postpublication = as.logical(max(p_outcome_changed_postpublication)),
    p_outcome_changed_unknown = as.logical(max(p_outcome_changed_unknown))
  ) %>%
  ungroup()



## ---- LONG VERSION: extract primary outcomes for each phase ----

## extract the primary outcome at the beginning
dat_temp1 <- dat %>%
  filter(!trial_phase == 'pre_recruitment') %>%
  group_by(id) %>%
  mutate(
    trial_phase_start = trial_phase[which.min(version_number)]
  ) %>% mutate(
    p_outcome_start = primary_outcomes[which.min(version_number)]
  ) %>%
  ungroup()

dat_temp2 <- dat %>%
  filter(trial_phase == 'pre_recruitment') %>%
  group_by(id) %>%
  mutate(
    trial_phase_start = NA
  ) %>%
    mutate(
      p_outcome_start = NA
  ) %>%
  ungroup()

test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

dat <- dat %>%
  arrange(id, version_number) %>%
  group_by(id) %>%
  fill(trial_phase_start, p_outcome_start, .direction = 'downup') %>%
  ungroup()

## extract the primary outcomes at the end of each phase
dat_temp1 <- dat %>%
  filter(trial_phase == 'recruitment') %>%
  group_by(id) %>%
  mutate(
    p_outcome_last_recruitment_temp = primary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    p_outcome_last_recruitment = if_else(
      is.na(p_outcome_last_recruitment_temp),
      'No outcome specified; field was empty in the registry.',
      p_outcome_last_recruitment_temp
    )
  ) %>%
  ungroup()
dat_temp2 <- dat %>%
  filter(trial_phase == 'post_completion') %>%
  group_by(id) %>%
  mutate(
    p_outcome_last_postcompletion_temp = primary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    p_outcome_last_postcompletion = if_else(
      is.na(p_outcome_last_postcompletion_temp),
      'No outcome specified; field was empty in the registry.',
      p_outcome_last_postcompletion_temp
    )
  ) %>%
  ungroup()
dat_temp3 <- dat %>%
  filter(trial_phase == 'post_publication') %>%
  group_by(id) %>%
  mutate(
    p_outcome_last_postpublication_temp = primary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    p_outcome_last_postpublication = if_else(
      is.na(p_outcome_last_postpublication_temp),
      'No outcome specified; field was empty in the registry.',
      p_outcome_last_postpublication_temp
    )
  ) %>%
  ungroup()
dat_temp4 <- dat %>%
  filter(trial_phase == 'unknown') %>%
  group_by(id) %>%
  mutate(
    p_outcome_last_unknown_temp = primary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    p_outcome_last_unknown = if_else(
      is.na(p_outcome_last_unknown_temp),
      'No outcome specified; field was empty in the registry.',
      p_outcome_last_unknown_temp
    )
  ) %>%
  ungroup()
dat_temp5 <- dat %>%
  filter(trial_phase == 'pre_recruitment')

test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2) + nrow(dat_temp3) + nrow(dat_temp4) + nrow(dat_temp5)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2, dat_temp3, dat_temp4, dat_temp5) %>%
  select(
    !c(
      p_outcome_last_recruitment_temp,
      p_outcome_last_postcompletion_temp,
      p_outcome_last_postpublication_temp,
      p_outcome_last_unknown_temp
    )
  )

dat <- dat %>%
  group_by(id) %>%
  fill(
    p_outcome_last_recruitment,
    p_outcome_last_postcompletion,
    p_outcome_last_postpublication,
    p_outcome_last_unknown,
    .direction = 'downup'
  ) %>%
  ungroup() %>%
  arrange(id, version_number)

## mutate the respective variables to indicate where a trial phase simply
## does not exist
dat <- dat %>%
  group_by(id) %>%
  mutate(
    p_outcome_last_recruitment = if_else(
      is.na(p_outcome_last_recruitment),
      'This trial had no recruitment phase according to our definition!',
      p_outcome_last_recruitment
    )
  ) %>%
  mutate(
    p_outcome_last_postcompletion = if_else(
      is.na(p_outcome_last_postcompletion),
      'This trial had no post-completion phase according to our definition!',
      p_outcome_last_postcompletion
    )
  ) %>%
  mutate(
    p_outcome_last_postpublication = if_else(
      is.na(p_outcome_last_postpublication),
      'This trial had no post-publication phase according to our definition!',
      p_outcome_last_postpublication
    )
  ) %>%
  mutate(
    p_outcome_last_unknown = if_else(
      is.na(p_outcome_last_unknown),
      'This trial had no *unknown* phase according to our definition!',
      p_outcome_last_unknown
    )
  ) %>%
  ungroup()

## create a variable that indicates the trial phase at the most recent
## historical version
dat <- dat %>%
  group_by(id) %>%
  mutate(
    trial_phase_final = trial_phase[which.max(version_number)]
  ) %>%
  ungroup() %>%
  relocate(trial_phase_final, .after = trial_phase_start)

## for later export to Numbat, extract the last primary outcomes section
dat <- dat %>%
  group_by(id) %>%
  mutate(
    p_outcome_final = case_when(
      (trial_phase_final == 'pre_recruitment') ~ 'Latest outcome determined before recruitment started!',
      (trial_phase_final == 'recruitment') ~ p_outcome_last_recruitment,
      (trial_phase_final == 'post_completion') ~ p_outcome_last_postcompletion,
      (trial_phase_final == 'post_publication') ~ p_outcome_last_postpublication,
      (trial_phase_final == 'unknown') ~ p_outcome_last_unknown
    )
  ) %>%
  ungroup()



## ---- LONG VERSION: extract secondary outcomes for each phase ----

## extract the secondary outcome at the beginning
dat_temp1 <- dat %>%
  filter(!trial_phase == 'pre_recruitment') %>%
  group_by(id) %>%
  mutate(
    s_outcome_start = secondary_outcomes[which.min(version_number)]
  ) %>%
  ungroup()
dat_temp2 <- dat %>%
  filter(trial_phase == 'pre_recruitment') %>%
  group_by(id) %>%
  mutate(
    s_outcome_start = NA
  ) %>%
  ungroup()
test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2)

dat <- dat %>%
  arrange(id, version_number) %>%
  group_by(id) %>%
  fill(s_outcome_start, .direction = 'downup') %>%
  ungroup()

## extract the secondary outcomes at the end of each phase
dat_temp1 <- dat %>%
  filter(trial_phase == 'recruitment') %>%
  group_by(id) %>%
  mutate(
    s_outcome_last_recruitment_temp = secondary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    s_outcome_last_recruitment = if_else(
      is.na(s_outcome_last_recruitment_temp),
      'No outcome specified; field was empty in the registry.',
      s_outcome_last_recruitment_temp
    )
  ) %>%
  ungroup()
dat_temp2 <- dat %>%
  filter(trial_phase == 'post_completion') %>%
  group_by(id) %>%
  mutate(
    s_outcome_last_postcompletion_temp = secondary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    s_outcome_last_postcompletion = if_else(
      is.na(s_outcome_last_postcompletion_temp),
      'No outcome specified; field was empty in the registry.',
      s_outcome_last_postcompletion_temp
    )
  ) %>%
  ungroup()
dat_temp3 <- dat %>%
  filter(trial_phase == 'post_publication') %>%
  group_by(id) %>%
  mutate(
    s_outcome_last_postpublication_temp = secondary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    s_outcome_last_postpublication = if_else(
      is.na(s_outcome_last_postpublication_temp),
      'No outcome specified; field was empty in the registry.',
      s_outcome_last_postpublication_temp
    )
  ) %>%
  ungroup()
dat_temp4 <- dat %>%
  filter(trial_phase == 'unknown') %>%
  group_by(id) %>%
  mutate(
    s_outcome_last_unknown_temp = secondary_outcomes[which.max(version_number)]
  ) %>%
  mutate(
    s_outcome_last_unknown = if_else(
      is.na(s_outcome_last_unknown_temp),
      'No outcome specified; field was empty in the registry.',
      s_outcome_last_unknown_temp
    )
  ) %>%
  ungroup()
dat_temp5 <- dat %>%
  filter(trial_phase == 'pre_recruitment')

test_that(
  'test that you haven’t lost any rows in the process of splitting the datasets',
  expect_equal((nrow(dat_temp1) + nrow(dat_temp2) + nrow(dat_temp3) + nrow(dat_temp4) + nrow(dat_temp5)), nrow(dat))
)
dat <- bind_rows(dat_temp1, dat_temp2, dat_temp3, dat_temp4, dat_temp5) %>%
  select(
    !c(
      s_outcome_last_recruitment_temp,
      s_outcome_last_postcompletion_temp,
      s_outcome_last_postpublication_temp,
      s_outcome_last_unknown_temp
    )
  )

dat <- dat %>%
  group_by(id) %>%
  fill(
    s_outcome_last_recruitment,
    s_outcome_last_postcompletion,
    s_outcome_last_postpublication,
    s_outcome_last_unknown,
    .direction = 'downup'
  ) %>%
  ungroup() %>%
  arrange(id, version_number)

## mutate the respective variables to indicate where a trial phase simply
## does not exist
dat <- dat %>%
  group_by(id) %>%
  mutate(
    s_outcome_last_recruitment = if_else(
      is.na(s_outcome_last_recruitment),
      'This trial had no recruitment phase according to our definition!',
      s_outcome_last_recruitment
    )
  ) %>%
  mutate(
    s_outcome_last_postcompletion = if_else(
      is.na(s_outcome_last_postcompletion),
      'This trial had no post-completion phase according to our definition!',
      s_outcome_last_postcompletion
    )
  ) %>%
  mutate(
    s_outcome_last_postpublication = if_else(
      is.na(s_outcome_last_postpublication),
      'This trial had no post-publication phase according to our definition!',
      s_outcome_last_postpublication
    )
  ) %>%
  mutate(
    s_outcome_last_unknown = if_else(
      is.na(s_outcome_last_unknown),
      'This trial had no *unknown* phase according to our definition!',
      s_outcome_last_unknown
    )
  ) %>%
  ungroup()

## for later export to Numbat, extract the last secondary outcomes section
dat <- dat %>%
  group_by(id) %>%
  mutate(
    s_outcome_final = case_when(
      (trial_phase_final == 'pre_recruitment') ~ 'Latest secondary outcome determined before recruitment started!',
      (trial_phase_final == 'recruitment') ~ s_outcome_last_recruitment,
      (trial_phase_final == 'post_completion') ~ s_outcome_last_postcompletion,
      (trial_phase_final == 'post_publication') ~ s_outcome_last_postpublication,
      (trial_phase_final == 'unknown') ~ s_outcome_last_unknown
    )
  ) %>%
  ungroup()

## save the "long" version of the historical data 
dat %>%
  write_csv('data/processed_history_data_long.csv')



## ---- SHORT VERSION: create a 'short' version ----

## save the 'short' version, in which each line is just one trial,
## after combining the data with the IntoValue dataset
dat_IV_extended <- read_csv('data/data_IntoValue_extended.csv') %>%
  select(c(
    id,
    registry,
    title,
    main_sponsor,
    study_type,
    intervention_type,
    phase,
    recruitment_status,
    masking,
    allocation,
    doi,
    pmid,
    url,
    pub_title,
    publication_type,
    journal_pubmed,
    journal_unpaywall,
    is_publication_2y,
    is_publication_5y)
  )
dat_short <- dat %>%
  group_by(id) %>%
  slice_head() %>%
  select(c(
    id,
    total_versions,
    first_reg_date,
    first_status,
    final_status,
    original_start_date,
    original_start_date_precision,
    original_start_date_type, 
    original_completion_date, 
    original_completion_date_precision, 
    original_completion_date_type,
    publication_date,
    latest_version_date,
    results_posted,
    results_posted_date,
    trial_phase_start,
    trial_phase_final,
    has_pre_recruitment_phase,
    has_recruitment_phase,
    has_post_completion_phase,
    has_post_publication_phase,
    has_unknown_phase,
    p_outcome_changed_prerecruitment,
    p_outcome_changed_recruitment,
    p_outcome_changed_postcompletion,
    p_outcome_changed_postpublication,
    p_outcome_changed_unknown,
    p_outcome_start,
    p_outcome_last_recruitment,
    p_outcome_last_postcompletion,
    p_outcome_last_postpublication,
    p_outcome_last_unknown,
    p_outcome_final,
    s_outcome_start,
    s_outcome_last_recruitment,
    s_outcome_last_postcompletion,
    s_outcome_last_postpublication,
    s_outcome_last_unknown,
    s_outcome_final
  )) %>%
  left_join(dat_IV_extended, by = 'id')

## save the data
dat_short %>% 
  write_csv('data/processed_history_data_short.csv')



## ---- SHORT VERSION: create a dataset for export to Numbat ----

## save the data for import into Numbat to rate the within-registry outcome
## changes (Numbat requires tab-separated values)
## also filter for those trials that have no within-history outcome switches
dat_Numbat <- dat_short %>% 
  select(
    c(
      id,
      title,
      pub_title,
      doi,
      url,
      total_versions,
      first_reg_date,
      first_status,
      final_status,
      original_start_date,
      original_start_date_precision,
      original_start_date_type,
      original_completion_date, 
      original_completion_date_precision,
      original_completion_date_type,
      publication_date,
      latest_version_date,
      results_posted,
      results_posted_date,
      trial_phase_start,
      trial_phase_final,
      has_pre_recruitment_phase,
      has_recruitment_phase,
      has_post_completion_phase,
      has_post_publication_phase,
      has_unknown_phase,
      p_outcome_changed_recruitment,
      p_outcome_changed_postcompletion,
      p_outcome_changed_postpublication,
      p_outcome_changed_unknown,
      p_outcome_start,
      p_outcome_last_recruitment,
      p_outcome_last_postcompletion,
      p_outcome_last_postpublication,
      p_outcome_last_unknown,
      s_outcome_start,
      s_outcome_last_recruitment,
      s_outcome_last_postcompletion,
      s_outcome_last_postpublication,
      s_outcome_last_unknown
    )
  ) %>%
  filter(
    p_outcome_changed_recruitment == TRUE |
    p_outcome_changed_postcompletion == TRUE |
    p_outcome_changed_postpublication == TRUE |
    p_outcome_changed_unknown == TRUE
  )

## save the data
dat_Numbat %>%
  write_tsv('data/processed_history_data_Numbat.tsv')



## ---- SHORT VERSION: create a second dataset for export to Numbat ----

## save the data for import into Numbat to rate the publications for presence
## of outcome-switching between registry and publication, as well as reporting
## of any changes to outcomes (Numbat requires tab-separated values)
dat_Numbat_2 <- dat_short %>% 
  select(
    c(
      id,
      registry,
      title,
      pub_title,
      doi,
      pmid,
      url,
      publication_type,
      journal_pubmed,
      journal_unpaywall,
      trial_phase_start,
      trial_phase_final,
      original_start_date,
      original_start_date_precision,
      original_start_date_type,
      original_completion_date,
      original_completion_date_precision,
      original_completion_date_type,
      publication_date,
      latest_version_date,
      results_posted,
      results_posted_date,
      has_pre_recruitment_phase,
      has_recruitment_phase,
      has_post_completion_phase,
      has_post_publication_phase,
      has_unknown_phase,
      p_outcome_start,
      p_outcome_final,
      s_outcome_final
    )
  )

## save the data
dat_Numbat_2 %>%
  write_tsv('data/processed_history_data_Numbat_2.tsv')

