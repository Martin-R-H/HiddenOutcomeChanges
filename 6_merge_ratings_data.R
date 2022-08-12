library(tidyverse)
library(testthat)



## ---- read in data ----

## read in the dataframe with the processed history versions
dat <- read_csv('data/processed_history_data_short.csv')



## ---- read and merge within-registry ratings ----

## read in the dataframe from Numbat
dat_ratings_wh <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-08-12_050654-form_2-refset_9-final.tsv'
) %>%
  select(
    c(
      referenceid,
      doi,
      url,
      title,
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

## we want to  combine the 'short' dataset with our within-registry ratings - as
## our unique identifier is missing (the trial id was accidentally deleted from
## the dataset during import to Numbat), we have to match mainly via the doi and
## url variables, as they have only few missings, are in most cases unique, and
## have no special characters that complicate the merge (unlike the title and
## pub_title variables)

## Step 1: If a trial has a value for doi and url, we will match on both.
dat_1 <- dat %>%
  filter(!is.na(doi) & !is.na(url)) %>%
  filter(!id %in% c(
    'NCT01276639',
    'NCT01309737',
    'NCT01703819',
    'NCT01703832',
    'DRKS00004195'
    # why are we separately excluding these trials? because their url - doi
    # combinations are not unique (i.e., they are linked to the same publication
    # as other trials in the data), and thus were merged with the wrong Numbat
    # data (in some cases, they got matched with two sets of ratings data) - we
    # hand-merge them in Step 5
  )) %>% 
  left_join(dat_ratings_wh, by = c('doi', 'url'), keep = FALSE) %>%
  select(!c(title.y)) %>%
  rename(title = title.x)

## Step 2: If a trial just has a value for doi, we will match on that.
dat_2 <- dat %>%
  filter(!is.na(doi) & is.na(url)) %>%
  filter(!id %in% c(
    'NCT01900067',
    'NCT01282827'
    # why are we separately excluding these trials? because their doi is not
    # unique (i.e., they are linked to the same publication as other trials in
    # the data), and thus were merged with the wrong Numbat data - NCT01282827,
    # for example, has the same doi as NCT01270126 and was erroneously matched
    # to the dataset with referenceid=404 (I checked) - we hand-merge in Step 5
  )) %>% 
  left_join(dat_ratings_wh, by = c('doi'), keep = FALSE) %>%
  select(!c(title.y, url.y)) %>%
  rename(url = url.x, title = title.x)

## Step 3: If a trial just has a value for url, we will match on that.
dat_3 <- dat %>%
  filter(is.na(doi) & !is.na(url)) %>%
  left_join(dat_ratings_wh, by = c('url'), keep = FALSE) %>%
  select(!c(title.y, doi.y)) %>%
  rename(doi = doi.x, title = title.x)

## Step 4: If a trial has neither a url nor a doi, we have to match based on
## title. (Which luckily just concerns one trial.)
dat_4 <- dat %>%
  filter(is.na(doi) & is.na(url)) %>%
  left_join(dat_ratings_wh, by = 'title', keep = FALSE) %>%
  select(!c(url.y, doi.y)) %>%
  rename(url = url.x, doi = doi.x)

## Step 5: For those trials we had to exclude earlier (due to matching errors),
## we have to match them by hand - if they have a match, of course.
## (I hand-searched the full dataset to find these matches...)
dat_5 <- dat %>%
  filter(
    id %in% c(
      'NCT01276639',
      'NCT01309737',
      'NCT01703819',
      'NCT01703832',
      'NCT01900067',
      'NCT01282827',
      'DRKS00004195'
    )
  ) %>%
  mutate(
    referenceid = case_when(
      id == 'NCT01276639' ~ 406,
      id == 'NCT01309737' ~ 419,
      id == 'NCT01703819' ~ 496,
      id == 'NCT01703832' ~ 497,
      id == 'NCT01900067' ~ 525
    )
  ) %>%
  left_join(dat_ratings_wh, by = 'referenceid', keep = FALSE) %>%
  select(!c(url.y, doi.y, title.y)) %>%
  rename(url = url.x, doi = doi.x, title = title.x)

## merge the dataframes
dat_merged <- bind_rows(dat_1, dat_2, dat_3, dat_4, dat_5)
rm(dat_1, dat_2, dat_3, dat_4, dat_5)

## Run some tests to see whether merging worked correctly...
## Test 1:
test_that(
  'Has the new dataframe lost any rows?',
  expect_equal(nrow(dat), nrow(dat_merged))
)
## Test 2:
test_that(
  'Does the new dataframe still have 1746 unique Registry Numbers (i.e., no duplicates)?',
  expect_equal(nrow(dat), length(na.omit(unique(dat_merged$id))))
)
## Test 3:
## build a vector can identify the positions of duplicate values
duplicates <- which(duplicated(na.omit(dat_merged$referenceid)))
test_that(
  'Are there any duplicate referenceids, i.e., were ratings matched twice?',
  expect_length(duplicates, 0)
)
## Test 4:
## create a dataframe that contains all trials with an outcome switch which do
## not have an accompanying rating
dat_missings <- dat_merged %>%
  filter(
    (p_outcome_changed_recruitment == TRUE |
       p_outcome_changed_postcompletion == TRUE |
       p_outcome_changed_postpublication == TRUE |
       p_outcome_changed_unknown == TRUE) &
      is.na(referenceid)
  )
test_that(
  'Do all trials have a referenceid (i.e., a rating) that should have one?',
  expect_equal(nrow(dat_missings), 0)
)
## Test 5:
## identify those trials that were not matched
ref_id <- c(1:nrow(dat_ratings_wh))
ref_id_small <- unique(dat_merged$referenceid) %>%
  na.omit() %>%
  sort()
ref_id_intersect <- ref_id[!ref_id %in% ref_id_small]
test_that(
  'Have all the lines from the ratings file been matched?',
  expect_equal(length(ref_id_intersect), 0)
)
# Test fails! There are 3 rows in the 'ratings' dataframe are not matched yet.
# Let's take a look at them...
dat_ratings_wh_UNMATCHED <- dat_ratings_wh %>%
  filter(referenceid %in% ref_id_intersect)
# There are no identifying information for these vectors in terms of doi, url,
# or title. When hand-searching the trials based on their primary outcome, I did
# not find them either. Since the tests before worked fine, I will assume these
# are leftover data from the previous webscraper version.
# remove test datasets:
rm(duplicates, ref_id, ref_id_small, ref_id_intersect)
rm(dat, dat_missings,dat_ratings_wh, dat_ratings_wh_UNMATCHED)



## ---- read and merge within-registry ratings (addendum) ----

## read in the dataframe from Numbat
dat_ratings_wh_addendum <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-08-12_050830-form_4-refset_18-final.tsv'
) %>%
  select(
    trial_id,
    trial_phase,
    rater_comment, # those need to be renamed later
    multi_select_new_primary,
    multi_select_primary_from_secondary,
    multi_select_change_measurement,
    multi_select_change_aggregation,
    multi_select_change_timing,
    multi_select_added_measurement,
    multi_select_added_aggregation,
    multi_select_added_timing,
    multi_select_omitted_measurement,
    multi_select_omitted_aggregation,
    multi_select_omitted_timing,
    multi_select_primary_to_secondary,
    multi_select_primary_omitted,
    multi_select_points_to_results,
    multi_select_no_change,
    multi_select_16 # this must be the 'phase does not exist' rating
  ) %>%
  rename(id = trial_id, rater_comment_addendum = rater_comment)

# split the data and merge them based on the phase - part 1
dat_ratings_wh_addendum_1 <- dat_ratings_wh_addendum %>% 
  filter(trial_phase == 'recruitment')
dat_new_1 <- dat_merged %>% 
  left_join(dat_ratings_wh_addendum_1, by = 'id') %>%
  mutate(
    change_a_i_new_primary = case_when(
      trial_phase == 'recruitment' ~ multi_select_new_primary,
      TRUE ~ change_a_i_new_primary
    )
  ) %>%
  mutate(
    change_a_i_primary_from_secondary = case_when(
      trial_phase == 'recruitment' ~ multi_select_primary_from_secondary,
      TRUE ~ change_a_i_primary_from_secondary
    )
  ) %>%
  mutate(
    change_a_i_change_measurement = case_when(
      trial_phase == 'recruitment' ~ multi_select_change_measurement,
      TRUE ~ change_a_i_change_measurement
    )
  ) %>%
  mutate(
    change_a_i_change_aggregation = case_when(
      trial_phase == 'recruitment' ~ multi_select_change_aggregation,
      TRUE ~ change_a_i_change_aggregation
    )
  ) %>%
  mutate(
    change_a_i_change_timing  = case_when(
      trial_phase == 'recruitment' ~ multi_select_change_timing,
      TRUE ~ change_a_i_change_timing
    )
  ) %>%
  mutate(
    change_a_i_added_measurement = case_when(
      trial_phase == 'recruitment' ~ multi_select_added_measurement,
      TRUE ~ change_a_i_added_measurement
    )
  ) %>%
  mutate(
    change_a_i_added_aggregation = case_when(
      trial_phase == 'recruitment' ~ multi_select_added_aggregation,
      TRUE ~ change_a_i_added_aggregation
    )
  ) %>%
  mutate(
    change_a_i_added_timing = case_when(
      trial_phase == 'recruitment' ~ multi_select_added_timing,
      TRUE ~ change_a_i_added_timing
    )
  ) %>%
  mutate(
    change_a_i_omitted_measurement = case_when(
      trial_phase == 'recruitment' ~ multi_select_omitted_measurement,
      TRUE ~ change_a_i_omitted_measurement
    )
  ) %>%
  mutate(
    change_a_i_omitted_aggregation = case_when(
      trial_phase == 'recruitment' ~ multi_select_omitted_aggregation,
      TRUE ~ change_a_i_omitted_aggregation
    )
  ) %>%
  mutate(
    change_a_i_omitted_timing = case_when(
      trial_phase == 'recruitment' ~ multi_select_omitted_timing,
      TRUE ~ change_a_i_omitted_timing
    )
  ) %>%
  mutate(
    change_a_i_primary_to_secondary = case_when(
      trial_phase == 'recruitment' ~ multi_select_primary_to_secondary,
      TRUE ~ change_a_i_primary_to_secondary
    )
  ) %>%
  mutate(
    change_a_i_primary_omitted = case_when(
      trial_phase == 'recruitment' ~ multi_select_primary_omitted,
      TRUE ~ change_a_i_primary_omitted
    )
  ) %>%
  mutate(
    change_a_i_points_to_results = case_when(
      trial_phase == 'recruitment' ~ multi_select_points_to_results,
      TRUE ~ change_a_i_points_to_results
    )
  ) %>%
  mutate(
    change_a_i_no_change = case_when(
      trial_phase == 'recruitment' ~ multi_select_no_change,
      TRUE ~ change_a_i_no_change
    )
  ) %>%
  mutate(
    change_a_i_no_phase = case_when(
      trial_phase == 'recruitment' ~ multi_select_16,
      TRUE ~ change_a_i_no_phase
    )
  ) %>%
  rename(
    rater_comment_add_rec = rater_comment_addendum
  ) %>%
  select(
    !c(
      trial_phase,
      multi_select_new_primary:multi_select_16
    )
  )

# split the data and merge them based on the phase - part 2
dat_ratings_wh_addendum_2 <- dat_ratings_wh_addendum %>% 
  filter(trial_phase == 'postcompletion')
dat_new_2 <- dat_new_1 %>% 
  left_join(dat_ratings_wh_addendum_2, by = 'id') %>%
  mutate(
    change_i_p_new_primary = case_when(
      trial_phase == 'postcompletion' ~ multi_select_new_primary,
      TRUE ~ change_i_p_new_primary
    )
  ) %>%
  mutate(
    change_i_p_primary_from_secondary = case_when(
      trial_phase == 'postcompletion' ~ multi_select_primary_from_secondary,
      TRUE ~ change_i_p_primary_from_secondary
    )
  ) %>%
  mutate(
    change_i_p_change_measurement = case_when(
      trial_phase == 'postcompletion' ~ multi_select_change_measurement,
      TRUE ~ change_i_p_change_measurement
    )
  ) %>%
  mutate(
    change_i_p_change_aggregation = case_when(
      trial_phase == 'postcompletion' ~ multi_select_change_aggregation,
      TRUE ~ change_i_p_change_aggregation
    )
  ) %>%
  mutate(
    change_i_p_change_timing  = case_when(
      trial_phase == 'postcompletion' ~ multi_select_change_timing,
      TRUE ~ change_i_p_change_timing
    )
  ) %>%
  mutate(
    change_i_p_added_measurement = case_when(
      trial_phase == 'postcompletion' ~ multi_select_added_measurement,
      TRUE ~ change_i_p_added_measurement
    )
  ) %>%
  mutate(
    change_i_p_added_aggregation = case_when(
      trial_phase == 'postcompletion' ~ multi_select_added_aggregation,
      TRUE ~ change_i_p_added_aggregation
    )
  ) %>%
  mutate(
    change_i_p_added_timing = case_when(
      trial_phase == 'postcompletion' ~ multi_select_added_timing,
      TRUE ~ change_i_p_added_timing
    )
  ) %>%
  mutate(
    change_i_p_omitted_measurement = case_when(
      trial_phase == 'postcompletion' ~ multi_select_omitted_measurement,
      TRUE ~ change_i_p_omitted_measurement
    )
  ) %>%
  mutate(
    change_i_p_omitted_aggregation = case_when(
      trial_phase == 'postcompletion' ~ multi_select_omitted_aggregation,
      TRUE ~ change_i_p_omitted_aggregation
    )
  ) %>%
  mutate(
    change_i_p_omitted_timing = case_when(
      trial_phase == 'postcompletion' ~ multi_select_omitted_timing,
      TRUE ~ change_i_p_omitted_timing
    )
  ) %>%
  mutate(
    change_i_p_primary_to_secondary = case_when(
      trial_phase == 'postcompletion' ~ multi_select_primary_to_secondary,
      TRUE ~ change_i_p_primary_to_secondary
    )
  ) %>%
  mutate(
    change_i_p_primary_omitted = case_when(
      trial_phase == 'postcompletion' ~ multi_select_primary_omitted,
      TRUE ~ change_i_p_primary_omitted
    )
  ) %>%
  mutate(
    change_i_p_points_to_results = case_when(
      trial_phase == 'postcompletion' ~ multi_select_points_to_results,
      TRUE ~ change_i_p_points_to_results
    )
  ) %>%
  mutate(
    change_i_p_no_change = case_when(
      trial_phase == 'postcompletion' ~ multi_select_no_change,
      TRUE ~ change_i_p_no_change
    )
  ) %>%
  mutate(
    change_i_p_no_phase = case_when(
      trial_phase == 'postcompletion' ~ multi_select_16,
      TRUE ~ change_i_p_no_phase
    )
  ) %>%
  rename(
    rater_comment_add_postcom = rater_comment_addendum
  ) %>%
  select(
    !c(
      trial_phase,
      multi_select_new_primary:multi_select_16
    )
  )

## split the data and merge them based on the phase - part 3
dat_ratings_wh_addendum_3 <- dat_ratings_wh_addendum %>% 
  filter(trial_phase == 'postpublication')
dat_new_3 <- dat_new_2 %>% 
  left_join(dat_ratings_wh_addendum_3, by = 'id') %>%
  mutate(
    change_p_l_new_primary = case_when(
      trial_phase == 'postpublication' ~ multi_select_new_primary,
      TRUE ~ change_p_l_new_primary
    )
  ) %>%
  mutate(
    change_p_l_primary_from_secondary = case_when(
      trial_phase == 'postpublication' ~ multi_select_primary_from_secondary,
      TRUE ~ change_p_l_primary_from_secondary
    )
  ) %>%
  mutate(
    change_p_l_change_measurement = case_when(
      trial_phase == 'postpublication' ~ multi_select_change_measurement,
      TRUE ~ change_p_l_change_measurement
    )
  ) %>%
  mutate(
    change_p_l_change_aggregation = case_when(
      trial_phase == 'postpublication' ~ multi_select_change_aggregation,
      TRUE ~ change_p_l_change_aggregation
    )
  ) %>%
  mutate(
    change_p_l_change_timing  = case_when(
      trial_phase == 'postpublication' ~ multi_select_change_timing,
      TRUE ~ change_p_l_change_timing
    )
  ) %>%
  mutate(
    change_p_l_added_measurement = case_when(
      trial_phase == 'postpublication' ~ multi_select_added_measurement,
      TRUE ~ change_p_l_added_measurement
    )
  ) %>%
  mutate(
    change_p_l_added_aggregation = case_when(
      trial_phase == 'postpublication' ~ multi_select_added_aggregation,
      TRUE ~ change_p_l_added_aggregation
    )
  ) %>%
  mutate(
    change_p_l_added_timing = case_when(
      trial_phase == 'postpublication' ~ multi_select_added_timing,
      TRUE ~ change_p_l_added_timing
    )
  ) %>%
  mutate(
    change_p_l_omitted_measurement = case_when(
      trial_phase == 'postpublication' ~ multi_select_omitted_measurement,
      TRUE ~ change_p_l_omitted_measurement
    )
  ) %>%
  mutate(
    change_p_l_omitted_aggregation = case_when(
      trial_phase == 'postpublication' ~ multi_select_omitted_aggregation,
      TRUE ~ change_p_l_omitted_aggregation
    )
  ) %>%
  mutate(
    change_p_l_omitted_timing = case_when(
      trial_phase == 'postpublication' ~ multi_select_omitted_timing,
      TRUE ~ change_p_l_omitted_timing
    )
  ) %>%
  mutate(
    change_p_l_primary_to_secondary = case_when(
      trial_phase == 'postpublication' ~ multi_select_primary_to_secondary,
      TRUE ~ change_p_l_primary_to_secondary
    )
  ) %>%
  mutate(
    change_p_l_primary_omitted = case_when(
      trial_phase == 'postpublication' ~ multi_select_primary_omitted,
      TRUE ~ change_p_l_primary_omitted
    )
  ) %>%
  mutate(
    change_p_l_points_to_results = case_when(
      trial_phase == 'postpublication' ~ multi_select_points_to_results,
      TRUE ~ change_p_l_points_to_results
    )
  ) %>%
  mutate(
    change_p_l_no_change = case_when(
      trial_phase == 'postpublication' ~ multi_select_no_change,
      TRUE ~ change_p_l_no_change
    )
  ) %>%
  mutate(
    change_p_l_no_phase = case_when(
      trial_phase == 'postpublication' ~ multi_select_16,
      TRUE ~ change_p_l_no_phase
    )
  ) %>%
  rename(
    rater_comment_add_postpub = rater_comment_addendum
  ) %>%
  select(
    !c(
      trial_phase,
      multi_select_new_primary:multi_select_16
    )
  )
dat_merged_full <- dat_new_3

## run some tests
test_that(
  'Do the three split addendum datasets actually add up to the whole?',
  expect_equal(
    nrow(dat_ratings_wh_addendum), nrow(dat_ratings_wh_addendum_1) + nrow(dat_ratings_wh_addendum_2) + nrow(dat_ratings_wh_addendum_3)
  )
)
test_results <- dat_merged_full %>%
  filter(
    change_a_i_points_to_results == '1' |
    change_i_p_points_to_results == '1' |
    change_p_l_points_to_results == '1'
  )
test_that(
  'Are there any trials that are rated as pointing to results now?',
  expect_equal(nrow(test_results), 0)
)
rm(dat_merged, dat_new_1, dat_new_2, dat_new_3, test_results)
rm(
  dat_ratings_wh_addendum,
  dat_ratings_wh_addendum_1, dat_ratings_wh_addendum_2, dat_ratings_wh_addendum_3
)

## ---- read and merge registry-publication ratings ----

## read in the dataframes from Numbat
dat_ratings_pub_25 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-08-12_051025-form_3-refset_16-final.tsv'
)
dat_ratings_pub_275 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-08-12_051103-form_3-refset_19-final.tsv'
) %>% 
  mutate(
    pub_outcome_change_no_change = as.character(pub_outcome_change_no_change),
    pub_outcome_reference_binary = as.character(pub_outcome_reference_binary),
    pub_outcome_time = as.character(pub_outcome_time)
  ) # might become obsolete later, will have to check dataframe structure
dat_ratings_pub <- bind_rows(dat_ratings_pub_25, dat_ratings_pub_275) %>%
  select(
    trial_id,
    reviewer_comment, # rename later
    pub_outcome_phrasing,
    outcome_determined,
    pub_outcome_change_new_primary,
    pub_outcome_change_primary_from_secondary,
    pub_outcome_change_change_measurement,
    pub_outcome_change_change_aggregation,
    pub_outcome_change_change_timing,
    pub_outcome_change_added_measurement,
    pub_outcome_change_added_aggregation,
    pub_outcome_change_added_timing,
    pub_outcome_change_omitted_measurement,
    pub_outcome_change_omitted_aggregation,
    pub_outcome_change_omitted_timing,
    pub_outcome_change_primary_to_secondary,
    pub_outcome_change_primary_omitted,
    pub_outcome_change_no_change,
    pub_outcome_reference_binary,
    pub_outcome_reference,
    pub_outcome_time,
    pub_sig_outcome,
    pub_sig_comment,
    pub_sig_pvalue 
  )
rm(dat_ratings_pub_25, dat_ratings_pub_275)

dat_merged_full <- dat_merged_full %>%
  left_join(dat_ratings_pub, by = c('id' = 'trial_id')) %>%
  mutate(
    has_publication_rating = if_else(
      id %in% dat_ratings_pub$trial_id,
      TRUE,
      FALSE
    )
  ) %>%
  relocate(
    has_publication_rating, .before = reviewer_comment
  )
rm(dat_ratings_pub)


## ---- save the analysis file ----
dat_merged_full %>%
  write_csv('data/processed_history_data_analyses.csv')
