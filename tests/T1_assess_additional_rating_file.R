library(tidyverse)
library(testthat)
set.seed(1025)

## this script prepares the export of another dataset to Numbat for additional
## ratings of within-registry outcome changes

## before, we run some tests to make sure everything went right with the newly
## downloaded file

## ---- READ IN DATA ----

## read in the data (short version)
dat <- read_csv('data/processed_history_data_short.csv') %>%
  arrange(id)

## read in old dataset for comparison (this is the original processed dataset,
## based on the download on 22 January 2022
dat_old <- read_csv('data/processed_history_data_short_2022-03-09.csv') %>%
  filter(id %in% dat$id) %>%
  # filter for the six obscure additional trials that did not belong there in
  # the original dataset
  arrange(id)

## check out a few vectors in the 'new' and the 'old' datasets, and check whether
## those we expect to be the same are indeed the same

## (1) final status
test_that(
  'test whether the final status variable is the same in the two datasets',
  expect_equal(dat$final_status, dat_old$final_status)
)
# test fails
diff_final_status <- which(dat$final_status != dat_old$final_status)
dat_diff_combined <- bind_rows(dat[diff_final_status, ], dat_old[diff_final_status, ])
# the difference is in NCT00354133, which is identified on ClinicalTrials.gov as
# Unknown with the comment 'Study has passed its completion date and status has
## not been verified in more than two years.' - so this is perfectly normal

## (2) original start date
test_that(
  'test whether the original start date variable is the same in the two datasets',
  expect_equal(dat$original_start_date, dat_old$original_start_date)
)
# test passes

## (3) original completion date
test_that(
  'test whether the original completion date variable is the same in the two datasets',
  expect_equal(dat$original_completion_date, dat_old$original_completion_date)
)
# test passes

## (4) trial phase at the beginning
test_that(
  'test whether the trial ohase at start is the same in the two datasets',
  expect_equal(dat$trial_phase_start, dat_old$trial_phase_start)
)
# test passes

## check out a few vectors in the 'new' and the 'old' datasets, and see where
## those we expect NOT to be the same differ from each other
## we expect two cases:
## (a) trials in the newer dataset have a higher total_versions number, as new
##     historical versions were uploaded in the meantime
##     --> in this case, we expect a couple of differences between 'old' and
##         'new' trials, like last outcome
## (b) trials that point to results have different outcomes in the newer dataset, 
##     because they were read in correctly

## first, we identify changes to total versions
chngs_versions <- which(
  dat$total_versions != dat_old$total_versions
)
dat_chngs_versions <- dat[chngs_versions, ] %>%
  select(
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
    p_outcome_last_unknown,
    s_outcome_start,
    s_outcome_last_recruitment,
    s_outcome_last_postcompletion,
    s_outcome_last_postpublication
  )
dat_old_chngs_versions <- dat_old[chngs_versions, ] %>%
  select(
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
    p_outcome_last_unknown,
    s_outcome_start,
    s_outcome_last_recruitment,
    s_outcome_last_postcompletion,
    s_outcome_last_postpublication
  )
## compare the two datasets and have a quick look where they differ
which(dat_chngs_versions[1, ] != dat_old_chngs_versions[1, ])
# second column, which is only the number of total versions (and thus expected)
which(dat_chngs_versions[2, ] != dat_old_chngs_versions[2, ])
# p_outcome_last_postcompletion and some secondary outcomes (in line with expectations)
which(dat_chngs_versions[3, ] != dat_old_chngs_versions[3, ])
# p_outcome_last_postcompletion and some secondary outcomes (in line with expectations)
which(dat_chngs_versions[4, ] != dat_old_chngs_versions[4, ])
# has post-publication phase and secondary outcomes (in line with expectations)
which(dat_chngs_versions[5, ] != dat_old_chngs_versions[5, ])
# second column, which is only the number of total versions (and thus expected)

## second, we identify changes to the outcomes
change_1 <- which(
  dat$p_outcome_start != dat_old$p_outcome_start
)
change_2 <- which(
  dat$p_outcome_last_recruitment != dat_old$p_outcome_last_recruitment
)
change_3 <- which(
  dat$p_outcome_last_postcompletion != dat_old$p_outcome_last_postcompletion
)
change_4 <- which(
  dat$p_outcome_last_postpublication != dat_old$p_outcome_last_postpublication
)
chng_outcomes <- unique(append(change_1, c(change_2, change_3, change_4)))
rm(change_1, change_2, change_3, change_4)

dat_chngs_outcomes <- dat[chng_outcomes, ] %>%
  arrange(id) %>%
  select(
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
    p_outcome_last_unknown,
    s_outcome_start,
    s_outcome_last_recruitment,
    s_outcome_last_postcompletion,
    s_outcome_last_postpublication
  )

dat_chngs_versions_outcomes <- bind_rows(dat_chngs_outcomes, dat_chngs_versions) %>%
  distinct() %>%
  arrange(id)

## now we will recreate the 'changes' dataset without having to read in the
## 'old' version
## we do this by filtering for those trials that have their results posted (they
## should have changed because of the web scraper issue) and those that have a
## latest version date on or after 22 January 22 (which is when the dataset was
## downloaded first)
dat_chngs_filter <- dat %>%
  filter(
    results_posted == TRUE |
      latest_version_date >= '2022-01-24'
  ) %>%
  arrange(id)

## check whether the two datasets contain the same trials
test_that(
  'test whether results_posted are the same as dat_chngs_outcomes',
  expect_equal(dat_chngs_filter$id, dat_chngs_versions_outcomes$id)
)
# test passes

## import Numbat ratings and check whether the trials that point to results
## according to our webscraper are also the ones that were manually rated as
## 'points to results'
dat_Numbat_results <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-04-26_031723-form_2-refset_9-final.tsv'
  # version from 26 April 22 (not all reconciliations done)
) %>%
  select(
    c(
      doi,
      title,
      pub_title,
      url, # we are selecting the first five because there is no id as unique identifier
      rater_comment,
      change_a_i_points_to_results,
      change_a_i_no_change,
      change_a_i_no_phase,
      change_i_p_points_to_results,
      change_i_p_no_change,
      change_i_p_no_phase,
      change_p_l_points_to_results,
      change_p_l_no_change,
      change_p_l_no_phase
    )
  ) %>%
  mutate(
    rating_points_to_results = if_else(
      change_a_i_points_to_results == '1'  |
        change_i_p_points_to_results == '1'  |
        change_p_l_points_to_results == '1',
      TRUE,
      FALSE
    )
  )

## merge the files
dat_chngs_filter_N <- left_join(dat_chngs_filter, dat_Numbat_results)

## check whether the results_posted (webscraper data) and the rating_points_to_results
## (manual rating) variables are the same (at least where Numbat has no NAs)
dat_chngs_filter_N <- dat_chngs_filter_N %>%
  filter(!is.na(rater_comment))
test_that(
  'test whether the results_posted and the rating_points_to_results variables are the same',
  expect_equal(dat_chngs_filter_N$results_posted, dat_chngs_filter_N$rating_points_to_results)
)
# this means that all trials that were automatically identified as pointing to
# results at some point are also hand-coded as that
