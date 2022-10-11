library(tidyverse)
library(irr)



## ---- within-history ratings: read data ----

## original ratings dataset
irr_wh_1 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-10-10_024057-form_2-refset_9-extractions.tsv'
) %>%
  arrange(referenceid)


## dataset for the corrections where the webscraper had picked up 'points to results'
irr_wh_2 <- read_tsv(
  'http://numbat.bgcarlisle.com/fmetrics/export/2022-10-10_024140-form_4-refset_18-extractions.tsv'
) %>%
  arrange(referenceid)


## ---- within-history ratings: arrange data ----

## first, re-arrange the original ratings set
temp1 <- irr_wh_1 %>%
  select(
    referenceid,
    username,
    starts_with('change_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('change_'))
temp2 <- irr_wh_1 %>%
  select(
    referenceid,
    username,
    starts_with('change_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('change_'))
irr_wh_1_comb <- temp1 %>%
  left_join(
    temp2,
    by = c('referenceid', 'name')
  ) %>%
  rename(value_MRH = value.x, value_MJH = value.y) %>%
  select(-starts_with('username'))
rm(temp1, temp2, irr_wh_1)

## second, re-arrange the dataset for the corrections
temp1 <- irr_wh_2 %>%
  select(
    referenceid,
    username,
    starts_with('multi_select_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('multi_select_'))
temp2 <- irr_wh_2 %>%
  select(
    referenceid,
    username,
    starts_with('multi_select_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('multi_select_'))
irr_wh_2_comb <- temp1 %>%
  left_join(
    temp2,
    by = c('referenceid', 'name')
  ) %>%
  rename(value_MRH = value.x, value_MJH = value.y) %>%
  select(-starts_with('username'))
rm(temp1, temp2, irr_wh_2)

## combine the datasets
irr_wh_comb <- bind_rows(irr_wh_1_comb, irr_wh_2_comb)
rm(irr_wh_1_comb, irr_wh_2_comb)

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


## ---- within-history ratings: calculate Kappa ----

## to calculate interrater reliability, let's calculate Cohen's kappa
irr_wh <- kappa2(irr_wh_comb[, 3:4])
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
    referenceid,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
temp2 <- irr_regpub_1 %>%
  select(
    referenceid,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
irr_regpub_1_comb <- temp1 %>%
  left_join(
    temp2,
    by = c('referenceid', 'name')
  ) %>%
  rename(value_Rater1 = value.x, value_Rater2 = value.y) %>%
  select(-starts_with('username'))
rm(temp1, temp2)

## do the same for the full sample of registry-publication ratings
## here, we have to keep in mind that MRJ and MJH rated half of the set each,
## and SY rated all of them as the second rater
temp1 <- irr_regpub_2 %>%
  select(
    referenceid,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'martin_holst_FM') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
temp2 <- irr_regpub_2 %>%
  select(
    referenceid,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'haslberm') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
temp12 <- bind_rows(temp1, temp2)
rm(temp1, temp2)

temp3 <- irr_regpub_2 %>%
  select(
    referenceid,
    username,
    starts_with('pub_outcome_change_')
  ) %>%
  filter(username == 'samruddhi') %>%
  pivot_longer(starts_with('pub_outcome_change_'))
irr_regpub_2_comb <- temp12 %>%
  left_join(
    temp3,
    by = c('referenceid', 'name')
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
