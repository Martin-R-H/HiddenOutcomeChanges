library(tidyverse)

set.seed(946)


## ---- LONG VERSION ----
## create a 'long' version of the history data

## read in the file
dat <- read_csv('data/combined_history_data.csv')

## create a 'date of first registration' variable
dat <- dat %>%
  group_by(id) %>%
  mutate(first_reg_date = min(version_date))

## create a 'first status' variable (it indicates the status the trial has at registration)
dat <- dat %>%
  group_by(id) %>%
  mutate(first_status = status[which.min(version_date)])

## create a 'final status' variable (since we are looking at published trials, most should be 'completed')
dat <- dat %>%
  group_by(id) %>%
  mutate(final_status = status[which.max(version_date)])


## ---- LONG VERSION: determine critical timepoints ----

## create two temporary variables to determine the critical timepoints
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

## create a variable that represents the first study start date, i.e.,
## the first start date where the trial registry entry that has a status of
## Recruiting, Enrolling by invitation, Active, not recruiting, Completed,
## Terminated (ClinicalTrials.gov terminology), Recruiting ongoing, Recruiting
## complete, follow-up complete, Recruiting stopped after recruiting started, or
## Recruiting suspended on temporary hold (DRKS terminology) - before this point,
## the trials were not yet recruiting)

## first, we have to create a temporary dataframe of those trials that have no 
## 'postlaunch' value, i.e., they apparently never start recruiting or stay 'unknown'
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(postlaunch_temp))) %>%
  mutate(original_start_date = NA)

## for the trials that do report a 'postlaunch' variable, create another temporary
## dataframe and create a variable for the 'original' study start date
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(postlaunch_temp))) %>%
  mutate(original_start_date = study_start_date[which.min(postlaunch_temp)])

## bind the two datasets together
dat <- bind_rows(dat_temp1, dat_temp2)

## original start date - alternative 1:
## if our first option to determine the original study start date fails (for
## example, because a trial reports no study start date at launch), we take
## the first reported start date (even if it is added after recruitment has started)
dat <- dat %>%
  group_by(id) %>%
  mutate(
    original_start_date_alt1 = if_else(
      is.na(original_start_date),
      first(na.omit(study_start_date)),
      original_start_date
    )
  )

## original start date - alternative 2:
## if the first and second options fail (i.e., because a trial reports no study start
## date at all), we will just take the date at which the trial was set to 'recruiting'
dat_temp1 <- dat %>%
  group_by(id) %>%
  filter(all(is.na(recruitment_temp))) %>%
  mutate(original_start_date_alt2 = original_start_date_alt1)
dat_temp2 <- dat %>%
  group_by(id) %>%
  filter(!all(is.na(recruitment_temp))) %>%
  mutate(
    original_start_date_alt2 = if_else(
      is.na(original_start_date_alt1),
      version_date[which.min(recruitment_temp)],
      original_start_date_alt1
    )
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
  )

## and then rename the other variables and drop some
dat <- dat %>%
  select(
    !c(
      original_start_date,
      original_start_date_alt1
    )
  ) %>%
  rename(original_start_date = original_start_date_alt2)

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

## bind the two datasets together
dat <- bind_rows(dat_temp1, dat_temp2)

####



####

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
      version_date < original_start_date ~ 'pre_recruitment',
      version_date >= original_start_date & version_date < original_completion_date ~ 'recruitment',
      version_date >= original_completion_date & version_date < publication_date ~ 'post_completion',
      version_date >= publication_date ~ 'post_publication',
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
  ungroup() %>%
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
  )

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
      has_post_publication_phase == FALSE ~ NA,
      any(primary_outcome_changed == TRUE) & trial_phase == 'post_publication' ~ TRUE,
      TRUE ~ FALSE
    ),
    p_outcome_changed_unknown = case_when(
      has_unknown_phase == FALSE ~ NA,
      any(primary_outcome_changed == TRUE) & trial_phase == 'unknown' ~ TRUE,
      TRUE ~ FALSE
    )
  )

## the logical vectors above may just indicate outcome changes in their respective
## groups - we now make it so that the indicate for all history versions of
## the same trial id
dat <- dat %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    p_outcome_changed_prerecruitment = as.logical(max(p_outcome_changed_prerecruitment)),
    p_outcome_changed_recruitment = as.logical(max(p_outcome_changed_recruitment)),
    p_outcome_changed_postcompletion = as.logical(max(p_outcome_changed_postcompletion)),
    p_outcome_changed_postpublication = as.logical(max(p_outcome_changed_postpublication)),
    p_outcome_changed_unknown = as.logical(max(p_outcome_changed_unknown))
  )

## ---- LONG VERSION: extract primary outcomes for each phase ----
## extract the primary outcome at the beginning
dat_temp1 <- dat %>%
  filter(!trial_phase == 'pre_recruitment') %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    trial_phase_start = trial_phase[which.min(version_number)]
  ) %>% mutate(
    p_outcome_start = primary_outcomes[which.min(version_number)]
  )

dat_temp2 <- dat %>%
  filter(trial_phase == 'pre_recruitment') %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    trial_phase_start = NA
  ) %>%
    mutate(
      p_outcome_start = NA
  )

dat <- bind_rows(dat_temp1, dat_temp2)

dat <- dat %>%
  arrange(id, version_number) %>%
  ungroup() %>%
  group_by(id) %>%
  fill(trial_phase_start, p_outcome_start, .direction = 'downup')

## extract the primary outcomes at the end of each phase
dat_temp1 <- dat %>%
  ungroup() %>%
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
  )
dat_temp2 <- dat %>%
  ungroup() %>%
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
  )
dat_temp3 <- dat %>%
  ungroup() %>%
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
  )
dat_temp4 <- dat %>%
  ungroup() %>%
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
  )
dat_temp5 <- dat %>%
  ungroup() %>%
  filter(trial_phase == 'pre_recruitment')

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
  )



## ---- LONG VERSION: extract secondary outcomes for each phase ----
## extract the secondary outcome at the beginning
dat_temp1 <- dat %>%
  filter(!trial_phase == 'pre_recruitment') %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    s_outcome_start = secondary_outcomes[which.min(version_number)]
  )
dat_temp2 <- dat %>%
  filter(trial_phase == 'pre_recruitment') %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    s_outcome_start = NA
  )
dat <- bind_rows(dat_temp1, dat_temp2)

dat <- dat %>%
  arrange(id, version_number) %>%
  ungroup() %>%
  group_by(id) %>%
  fill(s_outcome_start, .direction = 'downup')

## extract the secondary outcomes at the end of each phase
dat_temp1 <- dat %>%
  ungroup() %>%
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
  )
dat_temp2 <- dat %>%
  ungroup() %>%
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
  )
dat_temp3 <- dat %>%
  ungroup() %>%
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
  )
dat_temp4 <- dat %>%
  ungroup() %>%
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
  )
dat_temp5 <- dat %>%
  ungroup() %>%
  filter(trial_phase == 'pre_recruitment')

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
  )

## save the "long" version of the historical data 
dat %>%
  write_csv('data/processed_history_data_long.csv')



## ---- SHORT VERSION ----
## create a 'short' version of the history data for Numbat

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
    allocation,
    start_date,
    primary_completion_date,
    doi,
    pmid,
    url,
    pub_title,
    is_publication_2y,
    is_publication_5y)
  )
dat_short <- dat %>%
  ungroup() %>%
  group_by(id) %>%
  slice_head() %>%
  select(c(
    id,
    total_versions,
    first_reg_date,
    first_status,
    final_status,
    original_start_date,
    original_completion_date, 
    publication_date,
    has_pre_recruitment_phase,
    has_recruitment_phase,
    has_post_completion_phase,
    has_post_publication_phase,
    has_unknown_phase,
    p_outcome_changed_recruitment,
    p_outcome_changed_postcompletion,
    p_outcome_changed_postpublication,
    p_outcome_changed_unknown,
    trial_phase_start,
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
  )) %>%
  left_join(dat_IV_extended, by = 'id') %>%
  relocate(p_outcome_start, .after = trial_phase_start)

## save the data
dat_short %>% 
  write_csv('data/processed_history_data_short.csv')

## save the data for import into Numbat to rate the outcome changes (Numbat requires tab-separated values)
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
      original_completion_date, 
      publication_date,
      has_pre_recruitment_phase,
      has_recruitment_phase,
      has_post_completion_phase,
      has_post_publication_phase,
      has_unknown_phase,
      p_outcome_changed_recruitment,
      p_outcome_changed_postcompletion,
      p_outcome_changed_postpublication,
      p_outcome_changed_unknown,
      trial_phase_start,
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
