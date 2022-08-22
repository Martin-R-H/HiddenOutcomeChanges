library(tidyverse)
library(testthat)



## ---- Preprocessing and tests ----

## read in the 'short' dataset of history data
dat_short <- read_csv('data/processed_history_data_short.csv')

## Numbat data intermediate 22-03-29
dat_Numbat1 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-03-29_023303-form_2-refset_9-final.tsv'
) %>%
  select(
    c(
      referenceid,
      doi,
      title,
      pub_title,
      url, # we are selecting the first five because there is no id as unique identifier
      rater_comment,
      change_a_i_new_primary,
      change_a_i_primary_from_secondary,
      change_a_i_change_measurement,
      change_a_i_change_aggregation,
      change_a_i_change_timing,
      change_a_i_added_measurement,
      change_a_i_added_aggregation,
      change_a_i_added_timing,
      change_a_i_omitted_measurement,
      change_a_i_omitted_aggregation,
      change_a_i_omitted_timing,
      change_a_i_primary_to_secondary,
      change_a_i_primary_omitted,
      change_a_i_points_to_results,
      change_a_i_no_change,
      change_a_i_no_phase,
      change_i_p_new_primary,
      change_i_p_primary_from_secondary,
      change_i_p_change_measurement,
      change_i_p_change_aggregation,
      change_i_p_change_timing,
      change_i_p_added_measurement,
      change_i_p_added_aggregation,
      change_i_p_added_timing,
      change_i_p_omitted_measurement,
      change_i_p_omitted_aggregation,
      change_i_p_omitted_timing,
      change_i_p_primary_to_secondary,
      change_i_p_primary_omitted,
      change_i_p_points_to_results,
      change_i_p_no_change,
      change_i_p_no_phase,
      change_p_l_new_primary,
      change_p_l_primary_from_secondary,
      change_p_l_change_measurement,
      change_p_l_change_aggregation,
      change_p_l_change_timing,
      change_p_l_added_measurement,
      change_p_l_added_aggregation,
      change_p_l_added_timing,
      change_p_l_omitted_measurement,
      change_p_l_omitted_aggregation,
      change_p_l_omitted_timing,
      change_p_l_primary_to_secondary,
      change_p_l_primary_omitted,
      change_p_l_points_to_results,
      change_p_l_no_change,
      change_p_l_no_phase
    )
  )

## before we join the data, let's check whether we have any missing values for the 
## four 'identifier' variables
sum(is.na(dat_Numbat1$doi))
sum(is.na(dat_Numbat1$title))
sum(is.na(dat_Numbat1$pub_title))
sum(is.na(dat_Numbat1$url))
## the title column seems to have no NAs - let's see whether all titles are unique
test_that(
  'this tests whether all titles are unique',
  expect_equal(length(unique(dat_Numbat1$title)), nrow(dat_Numbat1))
)

## to get a better overview whether this is still a problem, let's read in the IV
## data and see whether the 'title' identifier would cause trouble there
test_dat_IV <- read_csv('data/data_IntoValue_extended.csv')
sum(is.na(test_dat_IV$title))
test_that(
  'this tests whether all titles are unique',
  expect_equal(length(unique(test_dat_IV$title)), nrow(test_dat_IV))
)
## Test failed! There seems to be one title that is double. But not too much of a problem.

## merge
dat <- inner_join(dat_short, dat_Numbat1, by = 'title')
nrow(dat)
# 154
nrow(dat_Numbat1)
# 165 - so apparently, eleven rows from Numbat are not merged

## some more tests to determine whether special characters are the problem
test3 <- dat_Numbat1 %>%
  filter(!(referenceid %in% dat$referenceid))
test4 <- test3 %>%
  inner_join(dat_short, by = 'doi') %>%
  select(title.x, title.y)
# visual inspect revals that special characters prevent titles from matching



## ---- Analyses ----

## we first want to find out how many trials actually have changes
## i.e., exclude those that, in all 3 phases, only have 'no change',
## 'points to results', or 'phase does not exist'
dat <- dat %>%
  mutate(
    within_outcome_switch = if_else(
      (change_a_i_no_change == '1' | change_a_i_points_to_results == '1'  | change_a_i_no_phase == '1') & # no changes in the 'recruitment' phase
      (change_i_p_no_change == '1' | change_i_p_points_to_results == '1'  | change_i_p_no_phase == '1') & # no changes in the postcompletion phase
      (change_p_l_no_change == '1' | change_p_l_points_to_results == '1'  | change_p_l_no_phase == '1'), # no changes in the post-publication phase
      FALSE,
      TRUE
    )
  )

## let's also create variables that indicate whether trials have any switch for each study phase
dat <- dat %>%
  mutate(
    within_outcome_switch_rec = if_else(
      (
        change_a_i_new_primary == '1' |
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
        change_a_i_primary_omitted == '1'
      ),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    within_outcome_switch_postcomp= if_else(
      (
        change_a_i_new_primary == '1' |
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
          change_i_p_primary_omitted == '1'
      ),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    within_outcome_switch_postpub= if_else(
      (
        change_a_i_new_primary == '1' |
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
          change_p_l_primary_omitted == '1'
      ),
      TRUE,
      FALSE
    )
  )

## take a look at how many trials point to results at some point
dat <- dat %>%
  mutate(
    points_to_results = if_else(
      change_a_i_points_to_results == '1' |
      change_i_p_points_to_results == '1' |
      change_p_l_points_to_results == '1' ,
      TRUE,
      FALSE
    )
  )


## let's take a look at some numbers
sum(dat$within_outcome_switch)
# 60 trials with changes, 94 trials without changes - plus the ones we never exported to numbat of course

sum(dat$within_outcome_switch_rec)
# of the 60 trials with changes, 40 have changes in the recruitment phase
sum(dat$within_outcome_switch_postcomp)
# of the 60 trials with changes, 22 have changes in the post-completion phase
sum(dat$within_outcome_switch_postpub)
# of the 60 trials with changes, 18 have changes in the post-publication phase

## let's see how many trials point to results at some point
sum(dat$points_to_results)
# of 154 trials, 81 point to results at some point (about half of them)

# let's include some sanity checks, e.g.
# (a) if the trial has no recriutment phase, this should be reflected in the Numbat ratings
test_that(
  'when has_recruitment_phase = TRUE then change_a_i_no_phase = 1 or NULL and when has_recruitment_phase = FALSE then change_a_i_no_phase = 0 or NULL',
  expect
)
# (b) the same applies to all other phases,


# RQ2:
# Candidate predictors to be used in the exploratory logistic regression analysis include 
# study phase, industry sponsorship, publication year, medical specialty, registry,
# multicentre trial. We will carefully justify the selection of variables and give precise
# definitions before starting our analysis.We will use descriptive statistics where appropriate.



##########

# FROM 6_pilot_analyses.R script!

##########



## read in data
dat_history <- read_csv('data/processed_history_data_long.csv')
dat_IV_extended <- read_csv('data/data_IntoValue_extended.csv')

## assess frequency of outcome changes
dat_history <- dat_history %>%
  filter(!trial_phase == 'pre-recruitment') %>%
  group_by(id) %>%
  mutate(number_outcome_changes = sum(primary_outcome_changed, na.rm = TRUE))

## assess number of primary outcomes
dat_history <- dat_history %>%
  group_by(id) %>%
  mutate(max_number_primary = max(primary_outcomes_number))



## summarise the data
dat_history_short <- dat_history %>%
  group_by(id) %>%
  slice_head()

table(dat_history_short$max_number_primary)
summary(dat_history_short$max_number_primary)
ggplot(dat_history_short, aes(max_number_primary)) + geom_histogram(binwidth = 1)

table(dat_history_short$number_outcome_changes)
summary(dat_history_short$number_outcome_changes)
ggplot(dat_history_short, aes(number_outcome_changes)) + geom_histogram(binwidth = 1)


## assess enrollment
summary(dat_IV_extended$enrollment)
ggplot(dat_IV_extended, aes(enrollment)) + geom_histogram(binwidth = 25) + xlim(0, 1500)

# how many trials are below 50?
dat_IV__extended %>%
  nrow()
dat_IV_extended %>%
  filter(enrollment < 50) %>%
  nrow()

