library(tidyverse)
library(glue)
library(irr)



## ---- within-history ratings: read data ----

## original ratings dataset
irr_wh <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-10-10_024057-form_2-refset_9-extractions.tsv'
) %>%
  arrange(referenceid)


## ---- within-history ratings: merge data with trial ids ----

## this is necessary because in the first rating dataset, the trial id had
## accidentally not been exported to Numbat (see script 6_merge_ratings_data.R)
## for the same problem

## to match, first read in the dataframe with the processed history versions
dat <- read_csv('data/processed_history_data_short.csv')

## then, do essentially the same steps as in 6_merge_ratings_data.R
dat_1 <- dat %>%
  filter(!is.na(doi) & !is.na(url)) %>%
  filter(!id %in% c(
    'NCT01276639',
    'NCT01309737',
    'NCT01703819',
    'NCT01703832',
    'DRKS00004195'
  )) %>%
  select(id, doi, url)
dat_2 <- dat %>%
  filter(!is.na(doi) & is.na(url)) %>%
  filter(!id %in% c(
    'NCT01900067',
    'NCT01282827'
  )) %>%
  select(id, doi)
dat_3 <- dat %>%
  filter(is.na(doi) & !is.na(url)) %>%
  select(id, url)
dat_4 <- dat %>%
  filter(is.na(doi) & is.na(url)) %>%
  select(id, title)
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
  select(id, referenceid)
irr_wh <- irr_wh %>%
  left_join(dat_1, by = c('doi', 'url'), keep = FALSE) %>%
  left_join(dat_2, by = 'doi', keep = FALSE) %>%
  left_join(dat_3, by = 'url', keep = FALSE) %>%
  left_join(dat_4, by = 'title', keep = FALSE) %>%
  left_join(dat_5, by = 'referenceid', keep = FALSE) %>%
  mutate(
    trial_id = case_when(
      !is.na(id.x) ~ id.x,
      !is.na(id.y) ~ id.y, 
      !is.na(id.x.x) ~ id.x.x,
      !is.na(id.y.y) ~ id.y.y,
      !is.na(id) ~id
    )
  ) %>%
  relocate(trial_id, .after = 'username') %>%
  select(-id.x, -id.y, -id.x.x, -id.y.y, -id)
rm(dat, dat_1, dat_2, dat_3, dat_4, dat_5)


## now, integrate the new ratings, as in 6_merge_ratings_data.R

## dataset for the corrections where the webscraper had picked up 'points to
## results'
irr_wh_add <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-10-10_024140-form_4-refset_18-extractions.tsv'
) %>%
  select(
    c(
      trial_id,
      username,
      trial_phase,
      starts_with('multi_select_')
    )
  )

# split the data and merge them based on the phase - part 1
irr_wh_add_1 <- irr_wh_add %>% 
  filter(trial_phase == 'recruitment')
irr_wh <- irr_wh %>% 
  left_join(irr_wh_add_1, by = c('trial_id', 'username'), keep = FALSE) %>%
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
  select(
    !c(
      trial_phase,
      starts_with('multi_select_')
    )
  )

# split the data and merge them based on the phase - part 2
irr_wh_add_2 <- irr_wh_add %>% 
  filter(trial_phase == 'postcompletion')
irr_wh <- irr_wh %>% 
  left_join(irr_wh_add_2, by = c('trial_id', 'username'), keep = FALSE) %>%
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
  select(
    !c(
      trial_phase,
      starts_with('multi_select_')
    )
  )

## split the data and merge them based on the phase - part 3
irr_wh_add_3 <- irr_wh_add %>% 
  filter(trial_phase == 'postpublication')
irr_wh <- irr_wh %>% 
  left_join(irr_wh_add_3, by = c('trial_id', 'username'), keep = FALSE) %>%
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
  select(
    !c(
      trial_phase,
      starts_with('multi_select_')
    )
  )
rm(irr_wh_add, irr_wh_add_1, irr_wh_add_2, irr_wh_add_3)


## ---- within-history ratings: arrange data ----

## first, re-arrange the original ratings set
temp1 <- irr_wh %>%
  select(
    trial_id,
    username,
    starts_with('change_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('change_'))
temp2 <- irr_wh %>%
  select(
    trial_id,
    username,
    starts_with('change_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('change_'))
irr_wh_comb <- temp1 %>%
  left_join(
    temp2,
    by = c('trial_id', 'name')
  ) %>%
  rename(value_MRH = value.x, value_MJH = value.y) %>%
  select(-starts_with('username'))
rm(temp1, temp2, irr_wh)

## remove NULLs from the data
irr_wh_comb  <- irr_wh_comb %>%
  mutate(value_MRH = if_else(
    value_MRH == 'NULL',
    '0',
    value_MRH
  )) %>%
  mutate(value_MJH = if_else(
    value_MJH == 'NULL',
    '0',
    value_MJH
  )) %>%
  mutate(value_MRH = as.numeric(value_MRH)) %>%
  mutate(value_MJH = as.numeric(value_MJH))

## create a variable for study phase
irr_wh_comb <- irr_wh_comb %>%
  mutate(study_phase = case_when(
    str_detect(name, 'change_a_i_') ~ 'recruitment',
    str_detect(name, 'change_i_p_') ~ 'post-completion',
    str_detect(name, 'change_p_l_') ~ 'post-publication'
  )) %>%
  relocate(study_phase, .after = trial_id)


## ---- within-history ratings: calculate Kappa ----

## to calculate interrater reliability, let's calculate Cohen's kappa
irr_wh <- kappa2(irr_wh_comb[, 4:5])
irr_wh

## the code below would allow us to calculate some other interrater reliability
## measures, for sensitivity checks (I tried, with not much different results)
# cor.test(
#   as.vector(irr_wh_comb$value_MRH),
#   as.vector(irr_wh_comb$value_MRH),
#   method = "kendall"
# )
# meanrho(irr_wh_comb[, 3:4], fisher = TRUE)
# kripp.alpha(t(as.matrix(irr_wh_comb[, 3:4])), 'ordinal')


## ---- within-history ratings: calculate Kappa on timepoint-wise basis ----
irr_wh_comb_t <- irr_wh_comb %>%
  group_by(trial_id, study_phase) %>%
  mutate(value_MRH_collapsed = glue_collapse(value_MRH)) %>%
  mutate(value_MJH_collapsed = glue_collapse(value_MJH)) %>%
  slice_head() %>%
  ungroup() %>%
  select(-name, -value_MRH, -value_MJH)

# calculate Kappa, once again
irr_wh_timepoints <- kappa2(irr_wh_comb_t[, 3:4])
irr_wh_timepoints


## ---- registry-publication ratings: read data ----

## pilot sample (25 registry-publication pairs)
irr_regpub_1 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-10-10_064128-form_3-refset_16-extractions.tsv'
) %>%
  arrange(referenceid)

## full sample (additional 275 registry-publication pairs, rated by three
## different raters)
irr_regpub_2 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-10-10_064146-form_3-refset_19-extractions.tsv'
) %>%
  arrange(referenceid)


## ---- registry-publication ratings: arrange data ----

## first, re-arrange the pilot dataset
temp1 <- irr_regpub_1 %>%
  select(
    trial_id,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
temp2 <- irr_regpub_1 %>%
  select(
    trial_id,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
irr_regpub_1_comb <- temp1 %>%
  left_join(
    temp2,
    by = c('trial_id', 'name')
  ) %>%
  rename(value_Rater1 = value.x, value_Rater2 = value.y) %>%
  select(-starts_with('username'))
rm(temp1, temp2)

## do the same for the full sample of registry-publication ratings
## here, we have to keep in mind that MRJ and MJH rated half of the set each,
## and SY rated all of them as the second rater
temp1 <- irr_regpub_2 %>%
  select(
    trial_id,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
temp2 <- irr_regpub_2 %>%
  select(
    trial_id,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
temp12 <- bind_rows(temp1, temp2)
rm(temp1, temp2)

temp3 <- irr_regpub_2 %>%
  select(
    trial_id,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'samruddhi') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
irr_regpub_2_comb <- temp12 %>%
  left_join(
    temp3,
    by = c('trial_id', 'name')
  ) %>%
  rename(value_Rater1 = value.x, value_Rater2 = value.y) %>%
  select(-starts_with('username'))
rm(temp12, temp3)

## combine the datasets
irr_regpub_comb <- bind_rows(irr_regpub_1_comb, irr_regpub_2_comb)
rm(irr_regpub_1_comb, irr_regpub_2_comb, irr_regpub_1, irr_regpub_2)

## remove NULLs from the data
irr_regpub_comb  <- irr_regpub_comb %>%
  mutate(value_Rater1 = if_else(
    value_Rater1 == 'NULL',
    '0',
    value_Rater1
  )) %>%
  mutate(value_Rater2 = if_else(
    value_Rater2 == 'NULL',
    '0',
    value_Rater2
  )) %>%
  mutate(value_Rater1 = as.numeric(value_Rater1)) %>%
  mutate(value_Rater2 = as.numeric(value_Rater2))


## ---- registry-publication ratings: calculate Kappa ----

## to calculate interrater reliability, let's calculate Cohen's kappa
irr_regpub <- kappa2(irr_regpub_comb[, 3:4])
irr_regpub

## the code below would allow us to calculate some other interrater reliability
## measures, for sensitivity checks (I tried, with not much different results)
# cor.test(
#   as.vector(irr_wh_comb$value_MRH),
#   as.vector(irr_wh_comb$value_MRH),
#   method = "kendall"
# )
# meanrho(irr_wh_comb[, 3:4], fisher = TRUE)
# kripp.alpha(t(as.matrix(irr_wh_comb[, 3:4])), 'ordinal')


## ---- registry-publication ratings: calculate Kappa on timepoint-wise basis ----
irr_regpub_comb_c <- irr_regpub_comb %>%
  group_by(trial_id) %>%
  mutate(value_Rater1_collapsed = glue_collapse(value_Rater1)) %>%
  mutate(value_Rater2_collapsed = glue_collapse(value_Rater2)) %>%
  slice_head() %>%
  ungroup() %>%
  select(-name, -value_Rater1, -value_Rater2)

# calculate Kappa, once again
irr_regpub_cases <- kappa2(irr_regpub_comb_c[, 2:3])
irr_regpub_cases
