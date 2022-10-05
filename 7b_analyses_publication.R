library(tidyverse)
library(lubridate)
library(binom)
library(ggupset)
library(brms)
library(testthat)

set.seed(638) # I asked Siri to generate a random number between 1 and 1000



## ---- READ IN DATA -----------------------------------------------------------

dat <- read_csv(
  'data/processed_history_data_analyses.csv',
  guess_max = 2000 # solves the parsing issue from script 7a
)



## ---- PREPARE DATA: CALCULATE OUTCOME CHANGE VARIABLES -----------------------

## first, create variables that indicate whether trials have any outcome change
## in each of the study phases
dat <- dat %>%
  mutate(
    p_o_change_rec = case_when(
      (change_a_i_new_primary == '1' |
         change_a_i_primary_from_secondary == '1' |
         change_a_i_change_measurement == '1' |
         change_a_i_change_aggregation == '1' |
         change_a_i_change_timing == '1' |
         change_a_i_added_measurement == '1' |
         change_a_i_added_aggregation == '1' |
         change_a_i_added_timing == '1' |
         change_a_i_omitted_measurement == '1' |
         change_a_i_omitted_aggregation == '1' |
         change_a_i_omitted_timing == '1' |
         change_a_i_primary_to_secondary == '1' |
         change_a_i_primary_omitted == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_postcomp = case_when(
      (change_i_p_new_primary == '1' |
         change_i_p_primary_from_secondary == '1' |
         change_i_p_change_measurement == '1' |
         change_i_p_change_aggregation == '1' |
         change_i_p_change_timing == '1' |
         change_i_p_added_measurement == '1' |
         change_i_p_added_aggregation == '1' |
         change_i_p_added_timing == '1' |
         change_i_p_omitted_measurement == '1' |
         change_i_p_omitted_aggregation == '1' |
         change_i_p_omitted_timing == '1' |
         change_i_p_primary_to_secondary == '1' |
         change_i_p_primary_omitted == '1') ~
      TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_postpub= case_when(
      (change_p_l_new_primary == '1' |
         change_p_l_primary_from_secondary == '1' |
         change_p_l_change_measurement == '1' |
         change_p_l_change_aggregation == '1' |
         change_p_l_change_timing == '1' |
         change_p_l_added_measurement == '1' |
         change_p_l_added_aggregation == '1' |
         change_p_l_added_timing == '1' |
         change_p_l_omitted_measurement == '1' |
         change_p_l_omitted_aggregation == '1' |
         change_p_l_omitted_timing == '1' |
         change_p_l_primary_to_secondary == '1' |
         change_p_l_primary_omitted == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_anywithin = if_else(
      (p_o_change_rec == TRUE |
         p_o_change_postcomp == TRUE |
         p_o_change_postpub == TRUE),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_reg_pub = case_when(
      has_publication_rating == TRUE &
        (pub_outcome_change_new_primary == '1' |
        pub_outcome_change_primary_from_secondary == '1' |
        pub_outcome_change_change_measurement == '1' |
        pub_outcome_change_change_aggregation == '1' |
        pub_outcome_change_change_timing == '1' |
        pub_outcome_change_added_measurement == '1' |
        pub_outcome_change_added_aggregation == '1' |
        pub_outcome_change_added_timing == '1' |
        pub_outcome_change_omitted_measurement == '1' |
        pub_outcome_change_omitted_aggregation == '1' |
        pub_outcome_change_omitted_timing == '1' |
        pub_outcome_change_primary_to_secondary == '1' |
        pub_outcome_change_primary_omitted == '1') ~
        TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
  )

## second, create variables that indicate whether trials have 'severe' outcome
## changes (i.e., adding or omitting primary outcomes, moving primary outcomes
## to secondary, or the other way round) in each of the study phases
dat <- dat %>%
  mutate(
    p_o_change_severe_rec = case_when(
      (change_a_i_new_primary == '1' |
         change_a_i_primary_omitted == '1' |
         change_a_i_primary_from_secondary == '1' |
         change_a_i_primary_to_secondary == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_severe_postcomp = case_when(
      (change_i_p_new_primary == '1' |
         change_i_p_primary_omitted == '1' |
         change_i_p_primary_from_secondary == '1' |
         change_i_p_primary_to_secondary == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_severe_postpub = case_when(
      (change_p_l_new_primary == '1' |
         change_p_l_primary_omitted == '1' |
         change_p_l_primary_from_secondary == '1' |
         change_p_l_primary_to_secondary == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_severe_anywithin = if_else(
      p_o_change_severe_rec == TRUE |
        p_o_change_severe_postcomp == TRUE |
        p_o_change_severe_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_severe_reg_pub = case_when(
      has_publication_rating == TRUE &
        (pub_outcome_change_new_primary == '1' |
        pub_outcome_change_primary_omitted == '1' |
        pub_outcome_change_primary_from_secondary == '1' |
        pub_outcome_change_primary_to_secondary == '1') ~
        TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
  )


## third, create variables that indicate whether trials have 'non-severe'
## outcome changes (i.e., adding or omitting primary outcomes, moving primary
## outcomes to secondary, or the other way round) in each of the study phases

## 'non-severe' changes: changes to measurement, metric or method of
## aggregation, or timing
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_rec = case_when(
      (change_a_i_change_measurement == '1' |
         change_a_i_change_aggregation == '1' |
         change_a_i_change_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_postcomp = case_when(
      (change_i_p_change_measurement == '1' |
         change_i_p_change_aggregation == '1' |
         change_i_p_change_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_postpub = case_when(
      (change_p_l_change_measurement == '1' |
         change_p_l_change_aggregation == '1' |
         change_p_l_change_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_anywithin = if_else(
      p_o_change_nonsevere_c_rec == TRUE |
        p_o_change_nonsevere_c_postcomp == TRUE |
        p_o_change_nonsevere_c_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_reg_pub = case_when(
      has_publication_rating == TRUE &
        (pub_outcome_change_change_measurement == '1' |
        pub_outcome_change_change_aggregation == '1' |
        pub_outcome_change_change_timing == '1') ~
        TRUE,
      has_publication_rating == TRUE ~FALSE,
      TRUE ~ NA
    )
  )

## 'non-severe' changes: additions or omissions to measurement, metric or method
## of aggregation, or timing
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_ao_rec = case_when(
      (change_a_i_added_measurement == '1' |
         change_a_i_added_aggregation == '1'|
         change_a_i_added_timing == '1' |
         change_a_i_omitted_measurement == '1' |
         change_a_i_omitted_aggregation == '1' |
         change_a_i_omitted_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_ao_postcomp = case_when(
      (change_i_p_added_measurement == '1' |
         change_i_p_added_aggregation == '1'|
         change_i_p_added_timing == '1' |
         change_i_p_omitted_measurement == '1' |
         change_i_p_omitted_aggregation == '1' |
         change_i_p_omitted_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_ao_postpub = case_when(
      (change_p_l_added_measurement == '1' |
         change_p_l_added_aggregation == '1'|
         change_p_l_added_timing == '1' |
         change_p_l_omitted_measurement == '1' |
         change_p_l_omitted_aggregation == '1' |
         change_p_l_omitted_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_ao_anywithin = if_else(
      p_o_change_nonsevere_ao_rec == TRUE |
        p_o_change_nonsevere_ao_postcomp == TRUE |
        p_o_change_nonsevere_ao_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_ao_reg_pub = case_when(
      has_publication_rating == TRUE &
        (pub_outcome_change_added_measurement == '1' |
        pub_outcome_change_added_aggregation == '1'|
        pub_outcome_change_added_timing == '1' |
        pub_outcome_change_omitted_measurement == '1' |
        pub_outcome_change_omitted_aggregation == '1' |
        pub_outcome_change_omitted_timing == '1') ~
        TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
  )

## 'non-severe' changes: any
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_rec = if_else(
      p_o_change_nonsevere_c_rec == TRUE | p_o_change_nonsevere_ao_rec == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_postcomp = if_else(
      p_o_change_nonsevere_c_postcomp == TRUE | p_o_change_nonsevere_ao_postcomp == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_postpub = if_else(
      p_o_change_nonsevere_c_postpub == TRUE | p_o_change_nonsevere_ao_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_anywithin = if_else(
      p_o_change_nonsevere_rec == TRUE |
        p_o_change_nonsevere_postcomp == TRUE |
        p_o_change_nonsevere_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_reg_pub = case_when(
      has_publication_rating == TRUE &
        (p_o_change_nonsevere_c_reg_pub == TRUE |
        p_o_change_nonsevere_ao_reg_pub == TRUE) ~
        TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
  )

## fourth, create a variable that indicates whether the respective study phase
## did not exist in a trial
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)
dat <- dat %>%
  mutate(
    no_rec_phase = case_when(
      has_recruitment_phase == FALSE | change_a_i_no_phase == '1' ~ TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_postcomp_phase = case_when(
      has_post_completion_phase == FALSE | change_i_p_no_phase == '1' ~ TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_postpub_phase = case_when(
      has_post_publication_phase == FALSE | change_p_l_no_phase == '1' ~ TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_anywithin_phase = if_else(
      no_rec_phase == TRUE & no_postcomp_phase == TRUE & no_postpub_phase == TRUE,
      TRUE,
      FALSE
    )
  )

## fifth, create a variable that indicates whether a trial had no change in the
## respective study phase
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)
dat <- dat %>%
  mutate(
    no_change_rec = case_when(
      p_outcome_changed_recruitment == FALSE | change_a_i_no_change == '1' ~ TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_postcomp = case_when(
      p_outcome_changed_postcompletion == FALSE | change_i_p_no_change == '1' ~ TRUE,
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_postpub = case_when(
      (has_post_publication_phase == TRUE & p_outcome_changed_postpublication == FALSE) |
        change_p_l_no_change == '1' ~ TRUE,
        # this is a bit of a different definition. why? because by default, in
        # script 4, we defined it as "no outcome change" if the post-publication
        # phase did not exist - but then, the categories were not mutually
        # exclusive anymore, which is why we make this to make it so again
      TRUE ~ FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_anywithin = if_else(
      no_change_rec == TRUE & no_change_postcomp == TRUE & no_change_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_reg_pub = case_when(
      has_publication_rating == TRUE & pub_outcome_change_no_change == '1' ~ TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
  )



## ---- PREPARE DATA: CALCULATE REGRESSION VARIABLES ---------------------------

## recode the phase variable
dat <- dat %>%
  mutate(
    phase_recoded = case_when(
      phase == 'Early Phase 1' | phase ==  'I' | phase == 'Phase 1' ~ 'Phase 1',
      phase == 'II' | phase ==  'IIa' | phase ==  'IIb' | phase ==  'Phase 1/Phase 2' | phase ==  'Phase 2' ~ 'Phase 2',
      phase == 'II-III' | phase ==  'III' | phase ==  'IIIb' | phase == 'Phase 2/Phase 3' | phase ==  'Phase 3' ~ 'Phase 3',
      phase == 'IV' | phase ==  'Phase 4' ~ 'Phase 3',
      TRUE ~ 'No phase'
    )
  ) %>%
  relocate(phase_recoded, .after = phase)

## recode the year variables
dat <- dat %>%
  mutate(registration_year = year(first_reg_date)) %>%
  mutate(publication_year = year(publication_date)) %>%
  relocate(registration_year, .after = first_reg_date) %>%
  relocate(publication_year, .after = publication_date)

## recode the intervention variable
dat <- dat %>%
  mutate(
    intervention_type_recoded = case_when(
      intervention_type == 'Drug' | intervention_type ==  'Biological' ~ 'Drug or Biological',
      intervention_type == 'Device' ~ 'Device',
      TRUE ~ 'Other'
    )
  ) %>%
  relocate(intervention_type_recoded, .after = intervention_type)

## add the multicenter variable
dat_IV_add <- read_csv('data/data_IntoValue_included.csv') %>%
  select(
    c(id, enrollment, is_multicentric)
  )
dat <- dat %>%
  left_join(
    dat_IV_add, by = 'id'
  ) %>%
  relocate(enrollment, .after = phase) %>%
  relocate(is_multicentric, .after = allocation)
rm(dat_IV_add)

## add the medical fields variable
## extract journal fields based on the a copy of the Scimago Journal & Country
## Rank database (https://www.scimagojr.com/journalrank.php), which provides
## this classification
dat_fields <- read_delim('scimagojr 2021.csv', delim = ';') %>%
  rename(medical_field = 'Categories') %>%
  select(
    c(Title, medical_field)
  ) %>%
  mutate(Title = tolower(Title)) %>%
  mutate(
    Title = str_replace(Title, 'jama - journal of the american medical association', 'jama journal of the american medical association')
  ) # to make sure they journal names properly match

## prepare matching
dat <- dat %>%
  mutate(journal_name_matching = tolower(journal_unpaywall)) %>%
  mutate(
    journal_name_matching = str_replace(journal_name_matching, 'the lancet', 'lancet')
  ) %>%
  mutate(
    journal_name_matching = if_else(
      str_detect(journal_name_matching, 'lancet'),
      str_c(journal_name_matching, ', the'),
      journal_name_matching
    )
  ) %>%
  mutate(
    journal_name_matching = str_replace(journal_name_matching, 'bmj(?![:blank:])', 'bmj, the'),
    journal_name_matching = str_replace(journal_name_matching, 'jama(?![:blank:]\\w)', 'jama journal of the american medical association')#,
    # journal_name_matching = str_replace(journal_name_matching, 'jama(?![:blank:]\\w)', 'jama journal of the american medical association'),
  ) # to make sure they journal names properly match

## join the data
dat <- dat %>%
  left_join(dat_fields, by = c('journal_name_matching' = 'Title')) %>%
  mutate(has_medical_field = !is.na(medical_field))

## the merging creates a few duplicates, which now need to be removed
dat_duplicates <- dat[which(duplicated(dat$id)), ]
rm(dat_duplicates)
dat <- dat %>%
  group_by(id) %>%
  slice_head() %>%
  ungroup()

## how many trials get matched with each of the databases?
sum(dat$has_medical_field)
dat_sample_checks <- dat %>%
  filter(has_medical_field == FALSE)
rm(dat_sample_checks)

source('fun/recode_fields.R') # to keep the algorithm out of this script

## recode the medical fields variable into 'General' and 'Specialty'
dat <- dat %>%
  categorisefields_binary()

## recode the medical fields variable into a more fine-grained terminology
dat <- dat %>%
  categorisefields()


## ---- RESEARCH QUESTION 1 ----------------------------------------------------

## Based on all registry changes in all RCTs in the dataset:
## We will determine the proportion of ‘within-registry’ outcome changes
## in published clinical trials across key trial phases (active, inactive,
## published).

## calculate the relevant numbers and proportions

## any changes in any within-registry phase
n_any_any <- sum(dat$p_o_change_anywithin)
p_any_any <- sum(dat$p_o_change_anywithin)/nrow(dat)*100
## severe changes in any phase
n_severe_any <- sum(dat$p_o_change_severe_anywithin)
p_severe_any <- sum(dat$p_o_change_severe_anywithin)/nrow(dat)*100
## non-severe changes in any phase
n_nonsevere_any <- sum(dat$p_o_change_nonsevere_anywithin)
p_nonsevere_any <- sum(dat$p_o_change_nonsevere_anywithin)/nrow(dat)*100
## non-severe changes (changes) in any phase
n_nonsevere_c_any <- sum(dat$p_o_change_nonsevere_c_anywithin)
p_nonsevere_c_any <- sum(dat$p_o_change_nonsevere_c_anywithin)/nrow(dat)*100
## non-severe changes (additions or omissions) in any phase
n_nonsevere_ao_any <- sum(dat$p_o_change_nonsevere_ao_anywithin)
p_nonsevere_ao_any <- sum(dat$p_o_change_nonsevere_ao_anywithin)/nrow(dat)*100
## phase does not exist - any phase
n_no_phase_any <- sum(dat$no_anywithin_phase)
p_no_phase_any <- sum(dat$no_anywithin_phase)/nrow(dat)*100
## no changes in any phase
n_no_change_any <- sum(dat$no_change_anywithin)
p_no_change_any <- sum(dat$no_change_anywithin)/nrow(dat)*100

## any changes in recruitment phase
n_any_rec <- sum(dat$p_o_change_rec)
p_any_rec <- sum(dat$p_o_change_rec)/nrow(dat)*100
## severe changes in recruitment phase
n_severe_rec <- sum(dat$p_o_change_severe_rec)
p_severe_rec <- sum(dat$p_o_change_severe_rec)/nrow(dat)*100
## non-severe changes in recruitment phase
n_nonsevere_rec <- sum(dat$p_o_change_nonsevere_rec)
p_nonsevere_rec <- sum(dat$p_o_change_nonsevere_rec)/nrow(dat)*100
## non-severe changes (changes) in recruitment phase
n_nonsevere_c_rec <- sum(dat$p_o_change_nonsevere_c_rec)
p_nonsevere_c_rec <- sum(dat$p_o_change_nonsevere_c_rec)/nrow(dat)*100
## non-severe changes (additions or omissions) in recruitment phase
n_nonsevere_ao_rec <- sum(dat$p_o_change_nonsevere_ao_rec)
p_nonsevere_ao_rec <- sum(dat$p_o_change_nonsevere_ao_rec)/nrow(dat)*100
## phase does not exist - recruitment phase
n_no_phase_rec <- sum(dat$no_rec_phase)
p_no_phase_rec <- sum(dat$no_rec_phase)/nrow(dat)*100
## no changes in recruitment phase
n_no_change_rec <- sum(dat$no_change_rec)
p_no_change_rec <- sum(dat$no_change_rec)/nrow(dat)*100

## any changes in post-completion phase
n_any_postcomp <- sum(dat$p_o_change_postcomp)
p_any_postcomp <- sum(dat$p_o_change_postcomp)/nrow(dat)*100
## severe changes in post-completion phase
n_severe_postcomp <- sum(dat$p_o_change_severe_postcomp)
p_severe_postcomp <- sum(dat$p_o_change_severe_postcomp)/nrow(dat)*100
## non-severe changes in post-completion phase
n_nonsevere_postcomp <- sum(dat$p_o_change_nonsevere_postcomp)
p_nonsevere_postcomp <- sum(dat$p_o_change_nonsevere_postcomp)/nrow(dat)*100
## non-severe changes (changes) in post-completion phase
n_nonsevere_c_postcomp <- sum(dat$p_o_change_nonsevere_c_postcomp)
p_nonsevere_c_postcomp <- sum(dat$p_o_change_nonsevere_c_postcomp)/nrow(dat)*100
## non-severe changes (additions or omissions) in post-completion phase
n_nonsevere_ao_postcomp <- sum(dat$p_o_change_nonsevere_ao_postcomp)
p_nonsevere_ao_postcomp <- sum(dat$p_o_change_nonsevere_ao_postcomp)/nrow(dat)*100
## phase does not exist - post-completion phase
n_no_phase_postcomp <- sum(dat$no_postcomp_phase)
p_no_phase_postcomp <- sum(dat$no_postcomp_phase)/nrow(dat)*100
## no changes in post-completion phase
n_no_change_postcomp <- sum(dat$no_change_postcomp)
p_no_change_postcomp <- sum(dat$no_change_postcomp)/nrow(dat)*100

## any changes in post-publication phase
n_any_postpub <- sum(dat$p_o_change_postpub)
p_any_postpub <- sum(dat$p_o_change_postpub)/nrow(dat)*100
## severe changes in post-publication phase
n_severe_postpub <- sum(dat$p_o_change_severe_postpub)
p_severe_postpub <- sum(dat$p_o_change_severe_postpub)/nrow(dat)*100
## non-severe changes in post-publication phase
n_nonsevere_postpub <- sum(dat$p_o_change_nonsevere_postpub)
p_nonsevere_postpub <- sum(dat$p_o_change_nonsevere_postpub)/nrow(dat)*100
## non-severe changes (changes) in post-publication phase
n_nonsevere_c_postpub <- sum(dat$p_o_change_nonsevere_c_postpub)
p_nonsevere_c_postpub <- sum(dat$p_o_change_nonsevere_c_postpub)/nrow(dat)*100
## non-severe changes (additions or omissions) in post-publication phase
n_nonsevere_ao_postpub <- sum(dat$p_o_change_nonsevere_ao_postpub)
p_nonsevere_ao_postpub <- sum(dat$p_o_change_nonsevere_ao_postpub)/nrow(dat)*100
## phase does not exist - post-publication phase
n_no_phase_postpub <- sum(dat$no_postpub_phase)
p_no_phase_postpub <- sum(dat$no_postpub_phase)/nrow(dat)*100
## no changes in post-publication phase
n_no_change_postpub <- sum(dat$no_change_postpub)
p_no_change_postpub <- sum(dat$no_change_postpub)/nrow(dat)*100

## run some tests
# test_that(
#   'In the recruitment phase, do trials with changes, with no changes and where the phase does not exist add up?',
#   expect_equal(n_any_rec + n_no_phase_rec + n_no_change_rec, nrow(dat))
# )
test_that(
  'In the post-completion phase, do trials with changes, with no changes and where the phase does not exist add up?',
  expect_equal(n_any_postcomp + n_no_phase_postcomp + n_no_change_postcomp, nrow(dat))
)
test_that(
  'In the post-publication phase, do trials with changes, with no changes and where the phase does not exist add up?',
  expect_equal(n_any_postpub + n_no_phase_postpub + n_no_change_postpub, nrow(dat))
)
## the test fails for the recruitment phase, there is one too many! it is
## apparently categorised in more than one of the above categories - find it:
dat_test_rec <- dat %>%
  filter(
    (no_change_rec == TRUE & no_rec_phase == TRUE) | (p_o_change_rec == TRUE & no_rec_phase == TRUE) | (no_change_rec == TRUE & p_o_change_rec == TRUE)
  )
## the trial has no outcome change during recruitment, but is accidentally rated
## as such, which is why it is both in the 'any change' and in the 'no change'
## categories
rm(dat_test_rec)


## ---- RESEARCH QUESTION 1 (Figure 2) -----------------------------------------

## Figure 2:
## What are the changes that happen within the registry across study phases?
## To show this more concisely, we do a stacked bar chart for change types
## across time points.

dat_Figure2 <- tribble(

  ~phase,             ~type,                  ~percentage,

  'recruitment',      'severe change',        p_severe_rec,
  'recruitment',      'non-severe change',    p_nonsevere_c_rec,
  'recruitment',      'non-severe add/omm',   p_nonsevere_ao_rec,
  'recruitment',      'phase does not exist', p_no_phase_rec,
# 'recruitment',      'no change',            p_no_change_rec,

  'post-completion',  'severe change',        p_severe_postcomp,
  'post-completion',  'non-severe change',    p_nonsevere_c_postcomp,
  'post-completion',  'non-severe add/omm',   p_nonsevere_ao_postcomp,
  'post-completion',  'phase does not exist', p_no_phase_postcomp,
# 'post-completion',  'no change',            p_no_change_postcomp,

  'post-publication', 'severe change',        p_severe_postpub,
  'post-publication', 'non-severe change',    p_nonsevere_c_postpub,
  'post-publication', 'non-severe add/omm',   p_nonsevere_ao_postpub,
  'post-publication', 'phase does not exist', p_no_phase_postpub #,
# 'post-publication', 'no change',            p_no_change_postpub

) %>%
  mutate(phase = factor(phase)) %>%
  mutate(
    phase = fct_relevel(phase, 'post-publication', 'post-completion', 'recruitment')
  ) %>%
  mutate(
    type = factor(type)
  ) %>%
  mutate(
    type = fct_relevel(
      type,
      'severe change',
      'non-severe change',
      'non-severe add/omm'
    )
  )

Figure2 <- ggplot(dat_Figure2) +
  aes(
    x = phase,
    y = percentage,
    fill = type
  ) +
  geom_bar(position = 'stack', stat = 'identity') +
  coord_flip() + # we  use this to make it horizontal
  scale_fill_manual(values = c('#994455', '#997700', '#EECC66', '#CCCCCC'))
Figure2

ggsave("Figure2.pdf",
       Figure2,
       scale = 1.25,
       width = 7,
       height = 5
)



## ---- RESEARCH QUESTION 2 ----------------------------------------------------

## Based on all registry changes in all RCTs in the dataset:
## We will assess the association of ‘within-registry’ outcome changes with key
## candidate predictors (listed below).
## Candidate predictors to be used in the exploratory logistic regression
## analysis include study phase, industry sponsorship, publication year, medical
## specialty, registry, multicenter trial. We will carefully justify the
## selection of variables and give precise definitions before starting our
## analysis. We will use descriptive statistics where appropriate.

## the model will be estimated with a generalised linear model (logistic
## regression)

## bayesian model
model_RQ2 <- brm(
  as.numeric(p_o_change_anywithin) ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    # medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat,
  cores = getOption('mc.cores', 2),
  seed = 227 # I again asked Siri for a number between 1 and 999
)
summary(model_RQ2)

## see what priors the model used
get_prior(
  as.numeric(p_o_change_anywithin) ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    # medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat
)

## frequentist model
model_RQ2_freq <- glm(
  p_o_change_anywithin ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    # medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat
)
summary(model_RQ2_freq)



## ---- RESEARCH QUESTION 3 ----------------------------------------------------

## Based on publications:
## We will determine the proportion of ‘classical’ outcome switching (i.e., if
## the primary outcome reported in the publication deviates from the one
## reported in the latest version of the preregistration before publication).

## calculate the respective proportions

## any changes between latest registry entry and publication
n_any_reg_pub <- sum(dat$p_o_change_reg_pub, na.rm = T)
p_any_reg_pub <- sum(dat$p_o_change_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_any_reg_pub <-
  binom.bayes(n_any_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_any_reg_pub_freq <-
  binom.test(n_any_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## severe changes between latest registry entry and publication
n_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = T)
p_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_severe_reg_pub <-
  binom.bayes(n_severe_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_severe_reg_pub_freq <-
  binom.test(n_severe_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes between latest registry entry and publication
n_nonsevere_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = T)
p_nonsevere_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_nonsevere_reg_pub <-
  binom.bayes(n_nonsevere_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_nonsevere_reg_pub_freq <-
  binom.test(n_nonsevere_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes (changes) between latest registry entry and publication
n_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = T)
p_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_nonsevere_c_reg_pub <-
  binom.bayes(n_nonsevere_c_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_nonsevere_c_reg_pub_freq <-
  binom.test(n_nonsevere_c_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes (additions or omissions) between latest registry entry and publication
n_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = T)
p_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_nonsevere_ao_reg_pub <-
  binom.bayes(n_nonsevere_ao_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_nonsevere_ao_reg_pub_freq <-
  binom.test(n_nonsevere_ao_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## no changes between latest registry entry and publication
n_no_change_reg_pub <- sum(dat$no_change_reg_pub, na.rm = T)
p_no_change_reg_pub<- sum(dat$no_change_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_no_change_reg_pub <-
  binom.bayes(n_no_change_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_no_change_reg_pub_freq <-
  binom.test(n_no_change_reg_pub, sum(dat$has_publication_rating))$conf.int*100



## ---- RESEARCH QUESTION 3 (Figures 3, S1 and S2) -----------------------------

## Figure 3:
## What are the pathways of changes that happen within the registry across study
## phases?
## As a more comprehensive assessment of when changes are made - especially
## keeping in mind the possibility of changes at multiple time points -, we do
## an Upset Plot for changes for the published trials.
## OPEN QUESTION:
## See Figure 1 from the protocol - maybe we need a more thorough assessment
## of pathways throughout a study when doing the Upset plot?

## transform links into list column of intersection sets
dat_pub <- dat %>%
  filter(has_publication_rating == TRUE)

## Figure 3: UpSet plot for any changes in the sample of 300
## transform links into list column of intersection sets
links <-  dat_pub %>%
  select(
    id,
    p_o_change_rec,
    p_o_change_postcomp,
    p_o_change_postpub,
    p_o_change_reg_pub
  ) %>%
  rename(
    "Start Date - Completion Date" = p_o_change_rec,
    "Completion Date - Publication Date" = p_o_change_postcomp,
    "Publication Date - Latest Entry" = p_o_change_postpub,
    "Latest Entry - Paper" = p_o_change_reg_pub
  ) %>%
  pivot_longer(cols = -id, names_to = "link") %>%
  filter(value == TRUE) %>%
  group_by(id) %>%
  mutate(links = list(link)) %>%
  ungroup() %>%
  select(-value, -link) %>%
  distinct()

# prepare trials without links
# create dummy links list column
no_links <- dat_pub %>%
  filter(!(id %in% links$id)) %>%
  select(id) %>%
  mutate(links = list(NULL))

dat_Figure3 <- bind_rows(links, no_links)
rm(links, no_links)

Figure3 <- dat_Figure3 %>%
  ggplot(aes(x=links, colour = NULL)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_upset(
    sets = c(
      'Latest Entry - Paper',
      'Publication Date - Latest Entry',
      'Completion Date - Publication Date',
      'Start Date - Completion Date'
    )
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) +
  theme(panel.background=element_rect(fill = "white", colour = "lightgrey"),
        panel.grid.major=element_line(colour="lightgrey", linetype = "dotted"),
        panel.grid.minor=element_line(colour="lightgrey", linetype = "dotted")) +
  # theme_combmatrix(
  #   combmatrix.panel.point.color.fill = c('#994455', '#997700', '#EECC66', '#CCCCCC')
  # ) +
  # I do not think it is actually possible to assign different colours to different sets
  ylab("Proportion of trials") +
  xlab('Any outcome changes across multiple time points') # +
# ggtitle(label = '"Less severe" primary outcome changes', subtitle = 'Sample of 300 registry entries and trial results publications')
Figure3

ggsave('Figure3.pdf',
       Figure3,
       scale = 1.25,
       width = 7,
       height = 5
)

## Figure S1: severe changes in the sample of 300
links <- dat_pub %>%
  select(
    id,
    p_o_change_severe_rec,
    p_o_change_severe_postcomp,
    p_o_change_severe_postpub,
    p_o_change_severe_reg_pub
  ) %>%
  rename(
    "Start - Completion" = p_o_change_severe_rec,
    "Completion - Publication" = p_o_change_severe_postcomp,
    "Publication - Latest Entry" = p_o_change_severe_postpub,
    "Latest Entry - Paper" = p_o_change_severe_reg_pub
  ) %>%
  pivot_longer(cols = -id, names_to = "link") %>%
  filter(value == TRUE) %>%
  group_by(id) %>%
  mutate(links = list(link)) %>%
  ungroup() %>%
  select(-value, -link) %>%
  distinct()

# Prepare trials without links
# Create dummy links list column
no_links <- dat_pub %>%
  filter(!(id %in% links$id)) %>%
  select(id) %>%
  mutate(links = list(NULL))

dat_FigureS1 <- bind_rows(links, no_links)
rm(links, no_links)

FigureS1 <- dat_FigureS1 %>%
  ggplot(aes(x=links)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_upset(
    sets = c(
      'Latest Entry - Paper',
      'Publication - Latest Entry',
      'Completion - Publication',
      'Start - Completion'
    )
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) +
  theme(panel.background=element_rect(fill = "white", colour = "lightgrey"),
        panel.grid.major=element_line(colour="lightgrey", linetype = "dotted"),
        panel.grid.minor=element_line(colour="lightgrey", linetype = "dotted")) +
  ylab("Proportion of trials") +
  xlab('"Severe" outcome changes across multiple time points') #+
# ggtitle(label = "'Severe' primary outcome switches", subtitle = 'Sample of 300 registry entries and trial results publications')
FigureS1

ggsave("FigureS1.pdf",
       FigureS1,
       scale = 1.25,
       width = 7,
       height = 5
)

## Figure S2: UpSet plot for non-severe changes in the sample of 300
# Transform links into list column of intersection sets
links <- dat_pub %>%
  select(
    id,
    p_o_change_nonsevere_rec,
    p_o_change_nonsevere_postcomp,
    p_o_change_nonsevere_postpub,
    p_o_change_nonsevere_reg_pub
  ) %>%
  rename(
    "Start - Completion" = p_o_change_nonsevere_rec,
    "Completion - Publication" = p_o_change_nonsevere_postcomp,
    "Publication - Latest Entry" = p_o_change_nonsevere_postpub,
    "Latest Entry - Paper" = p_o_change_nonsevere_reg_pub
  ) %>%
  pivot_longer(cols = -id, names_to = "link") %>%
  filter(value == TRUE) %>%
  group_by(id) %>%
  mutate(links = list(link)) %>%
  ungroup() %>%
  select(-value, -link) %>%
  distinct()

# Prepare trials without links
# Create dummy links list column
no_links <-
  dat_pub %>%
  filter(!(id %in% links$id)) %>%
  select(id) %>%
  mutate(links = list(NULL))

dat_FigureS2 <- bind_rows(links, no_links)
rm(links, no_links)

FigureS2 <- dat_FigureS2 %>%
  ggplot(aes(x=links, colour = NULL)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_upset(
    sets = c(
      'Latest Entry - Paper',
      'Publication - Latest Entry',
      'Completion - Publication',
      'Start - Completion'
    )
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) +
  theme(panel.background=element_rect(fill = "white", colour = "lightgrey"),
        panel.grid.major=element_line(colour="lightgrey", linetype = "dotted"),
        panel.grid.minor=element_line(colour="lightgrey", linetype = "dotted")) +
  ylab("Proportion of trials") +
  xlab('"Less severe" outcome changes across multiple time points') # +
# ggtitle(label = '"Less severe" primary outcome changes', subtitle = 'Sample of 300 registry entries and trial results publications')
FigureS2

ggsave("FigureS2.pdf",
       FigureS2,
       scale = 1.25,
       width = 7,
       height = 5
)



## ---- RESEARCH QUESTION 4 ----------------------------------------------------

## Based on publications:
## We will assess the association of ‘classical’ outcome switching with key
## candidate predictors (listed below).
## Listed below is: Candidate predictors to be used in the exploratory logistic
## regression analysis include study phase, industry sponsorship, publication
## year, medical specialty, registry, multicenter trial. We will carefully
## justify the selection of variables and give precise definitions before
## starting our analysis. We will use descriptive statistics where appropriate.

## bayesian model
library(brms)
model_RQ4 <- brm(
  as.numeric(p_o_change_reg_pub) ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    # medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat_pub,
  cores = getOption('mc.cores', 2),
  seed = 754 # I again asked Siri for a number between 1 and 999
)
summary(model_RQ4)

## see what priors the model used
get_prior(
  as.numeric(p_o_change_anywithin) ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    # medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat_pub
)

## frequentist model
model_RQ4_freq <- glm(
  p_o_change_reg_pub ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    # medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat_pub
)
sumary(model_RQ4_freq)



## ---- RESEARCH QUESTION 5 ----------------------------------------------------

## Based on publications:
## We will assess the association between ‘within-registry’ outcome switching
## and ‘classical’ outcome switching.

# ## bayesian model
model_RQ5 <- brm(
  as.numeric(p_o_change_reg_pub) ~ p_o_change_anywithin,
  family = 'binomial',
  data = dat_pub,
  cores = getOption('mc.cores', 2),
  seed = 827 # I again asked Siri for a number between 1 and 999
)
summary(model_RQ5)

## frequentist model
model_RQ5_freq <- glm(
  p_o_change_reg_pub ~ p_o_change_anywithin,
  family = 'binomial',
  data = dat_pub
)
summary(model_RQ5_freq)



## ---- RESEARCH QUESTION 6 ----------------------------------------------------

## Based on publications:
## We will determine the proportion of trials with transparent reporting of
## “within-registry” changes in the publication.

## How many trials with severe within-registry changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_severe_anywithin = if_else(
      p_o_change_severe_anywithin == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )
n_reporting_severe_anywithin <- sum(dat_pub$reporting_severe_anywithin, na.rm = TRUE)
n_severe_anywithin_sample <- sum(dat_pub$p_o_change_severe_anywithin, na.rm = TRUE)
p_reporting_severe_anywithin <- n_reporting_severe_anywithin / n_severe_anywithin_sample * 100
CI_reporting_severe_anywithin <-
  binom.bayes(n_reporting_severe_anywithin, n_severe_anywithin_sample) # multiply with 100
CI_reporting_severe_anywithin_freq <-
  binom.test(n_reporting_severe_anywithin, n_severe_anywithin_sample)$conf.int*100

## How many trials with *any* within-registry changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_any_anywithin = if_else(
      p_o_change_anywithin == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )
n_reporting_any_anywithin <- sum(dat_pub$reporting_any_anywithin, na.rm = TRUE)
n_any_anywithin_sample <- sum(dat_pub$p_o_change_anywithin, na.rm = TRUE)
p_reporting_any_anywithin <- n_reporting_any_anywithin / n_any_anywithin_sample * 100
CI_reporting_any_anywithin <-
  binom.bayes(n_reporting_any_anywithin, n_any_anywithin_sample) # multiply with 100
CI_reporting_any_anywithin_freq <-
  binom.test(n_reporting_any_anywithin, n_any_anywithin_sample)$conf.int*100



## ---- RESEARCH QUESTION 7 ----------------------------------------------------

## Based on publications:
## We will determine the proportion of trials with transparent reporting of
## “classical” outcome changes in the publication.

## How many trials with severe registry-publication changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_severe_reg_pub = if_else(
      p_o_change_severe_reg_pub == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )
n_reporting_severe_reg_pub <- sum(dat_pub$reporting_severe_reg_pub, na.rm = TRUE)
n_severe_reg_pub_sample <- sum(dat_pub$p_o_change_severe_reg_pub, na.rm = TRUE)
p_reporting_severe_reg_pub <- n_reporting_severe_reg_pub / n_severe_reg_pub_sample * 100
CI_reporting_severe_reg_pub <-
  binom.bayes(n_reporting_severe_reg_pub, n_severe_reg_pub_sample) # multiply with 100
CI_reporting_severe_reg_pub_freq <-
  binom.test(n_reporting_severe_reg_pub, n_severe_reg_pub_sample)$conf.int*100

## How many trials with any registry-publication changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_any_reg_pub = if_else(
      p_o_change_reg_pub == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )
n_reporting_any_reg_pub <- sum(dat_pub$reporting_any_reg_pub, na.rm = TRUE)
n_any_reg_pub_sample <- sum(dat_pub$p_o_change_reg_pub, na.rm = TRUE)
p_reporting_any_reg_pub <- n_reporting_any_reg_pub / n_any_reg_pub_sample * 100
CI_reporting_any_reg_pub  <-
  binom.bayes(n_reporting_any_reg_pub, n_any_reg_pub_sample) # multiply with 100
CI_reporting_any_reg_pub_freq <-
  binom.test(n_reporting_any_reg_pub, n_any_reg_pub_sample)$conf.int*100
