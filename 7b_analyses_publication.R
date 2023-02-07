library(tidyverse)
library(lubridate)
library(binom)
library(ggupset)
library(networkD3) # for Sankey plot
library(brms)
library(finalfit)
library(glmtoolbox)
library(testthat)

set.seed(638) # I asked Siri to generate a random number between 1 and 1000



## ---- READ IN DATA -----------------------------------------------------------

dat <- read_csv(
  'data/processed_history_data_analyses.csv',
  guess_max = 2000 # solves the parsing issue from script 7a
)



## ---- PREPARE DATA: Calculate Outcome Change Variables -----------------------

source('fun/recode_outcome_changes.R')

## first, create variables that indicate whether trials have any outcome change
## in each of the study phases
dat <- dat %>%
  recode_outcomes_any()

## second, create variables that indicate whether trials have 'severe' outcome
## changes (i.e., adding or omitting primary outcomes, moving primary outcomes
## to secondary, or the other way round) in each of the study phases
dat <- dat %>%
  recode_outcomes_severe()

## third, create variables that indicate whether trials have 'non-severe'
## outcome changes (i.e., adding or omitting primary outcomes, moving primary
## outcomes to secondary, or the other way round) in each of the study phases

## 'non-severe' changes: changes to measurement, metric or method of
## aggregation, or timing
dat <- dat %>%
  recode_outcomes_nonsevere_1()

## 'non-severe' changes: additions or omissions to measurement, metric or method
## of aggregation, or timing
dat <- dat %>%
  recode_outcomes_nonsevere_2()

## 'non-severe' changes: any
dat <- dat %>%
  recode_outcomes_nonsevere_3()

## fourth, create a variable that indicates whether the respective study phase
## did not exist in a trial
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)
dat <- dat %>%
  recode_outcomes_notexist()

## fifth, create a variable that indicates whether a trial had no change in the
## respective study phase
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)
dat <- dat %>%
  recode_outcomes_nophase()



## ---- PREPARE DATA: Calculate Regression Variables ---------------------------

## recode the phase variable
dat <- dat %>%
  mutate(
    phase_recoded = case_when(
      phase == 'Early Phase 1' | phase ==  'I' | phase == 'Phase 1' ~ 'Phase 1',
      phase == 'II' | phase ==  'IIa' | phase ==  'IIb' | phase ==  'Phase 1/Phase 2' | phase ==  'Phase 2' ~ 'Phase 2',
      phase == 'II-III' | phase ==  'III' | phase ==  'IIIb' | phase == 'Phase 2/Phase 3' | phase ==  'Phase 3' ~ 'Phase 3',
      phase == 'IV' | phase ==  'Phase 4' ~ 'Phase 4',
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

## add the enrollment and multicenter variables
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
    journal_name_matching = str_replace(journal_name_matching, 'jama(?![:blank:]\\w)', 'jama journal of the american medical association')
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

source('fun/recode_medical_fields.R') # to keep the algorithm out of this script

## recode the medical fields variable into 'General' and 'Specialty'
dat <- dat %>%
  categorisefields_binary()

## recode the medical fields variable into a more fine-grained terminology
dat <- dat %>%
  categorisefields()


## ---- OBJECTIVE 1: Within-Registry Discrepancies -----------------------------

## Based on all registry changes in all RCTs in the dataset:
## How often are changes of primary outcomes of clinical trials reported in
## clinical trial registries, across all available registry entry versions
## (“within-registry discrepancies”)? We will determine the proportions across
## key trial phases.

## (Equal to Research Question 1 from the preregistered protocol.)

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
test_that(
  'In the recruitment phase, do trials with changes, with no changes and where the phase does not exist add up?',
  expect_equal(n_any_rec + n_no_phase_rec + n_no_change_rec, nrow(dat))
)
test_that(
  'In the post-completion phase, do trials with changes, with no changes and where the phase does not exist add up?',
  expect_equal(n_any_postcomp + n_no_phase_postcomp + n_no_change_postcomp, nrow(dat))
)
test_that(
  'In the post-publication phase, do trials with changes, with no changes and where the phase does not exist add up?',
  expect_equal(n_any_postpub + n_no_phase_postpub + n_no_change_postpub, nrow(dat))
)
test_that(
  'For any of the phases, do trials with changes, with no changes and where the phase does not exist add up?',
  expect_equal(n_any_any + n_no_phase_any + n_no_change_any, nrow(dat))
)



## ---- OBJECTIVE 2: Registry-Publication Discrepancies ------------------------

## Based on publications:
## How often are there discrepancies between the latest registry entry and the
## results publication (registry-publication discrepancies, i.e., if the primary
## outcome reported in the publication deviates from the one reported in the
## latest version of the preregistration before publication).

## (Equal to Research Question 3 from the preregistered protocol.)

## calculate the respective proportions

## any changes between latest registry entry and publication
n_any_reg_pub <- sum(dat$p_o_change_reg_pub, na.rm = T)
p_any_reg_pub <- sum(dat$p_o_change_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
  # CI_any_reg_pub <-
  #   binom.bayes(n_any_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_any_reg_pub_freq <-
  binom.test(n_any_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## severe changes between latest registry entry and publication
n_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = T)
p_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
  # CI_severe_reg_pub <-
  #   binom.bayes(n_severe_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_severe_reg_pub_freq <-
  binom.test(n_severe_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes between latest registry entry and publication
n_nonsevere_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = T)
p_nonsevere_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
  # CI_nonsevere_reg_pub <-
  #   binom.bayes(n_nonsevere_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_nonsevere_reg_pub_freq <-
  binom.test(n_nonsevere_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes (changes) between latest registry entry and publication
n_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = T)
p_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
  # CI_nonsevere_c_reg_pub <-
  #   binom.bayes(n_nonsevere_c_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_nonsevere_c_reg_pub_freq <-
  binom.test(n_nonsevere_c_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes (additions or omissions) between latest registry entry and publication
n_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = T)
p_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
  # CI_nonsevere_ao_reg_pub <-
  #   binom.bayes(n_nonsevere_ao_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_nonsevere_ao_reg_pub_freq <-
  binom.test(n_nonsevere_ao_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## no changes between latest registry entry and publication
n_no_change_reg_pub <- sum(dat$no_change_reg_pub, na.rm = T)
p_no_change_reg_pub<- sum(dat$no_change_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
  # CI_no_change_reg_pub <-
  #   binom.bayes(n_no_change_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_no_change_reg_pub_freq <-
  binom.test(n_no_change_reg_pub, sum(dat$has_publication_rating))$conf.int*100

## run some tests
test_that(
  'Between registry and publication, do trials with changes and with no changes add up?',
  expect_equal(n_any_reg_pub + n_no_change_reg_pub, nrow(filter(dat, has_publication_rating == TRUE)))
)



## ---- OBJECTIVE 3: Hidden Changes --------------------------------------------

## Based on publications:
## How many changes are “hidden” behind the latest registry entry, i.e., are
## “within-registry”, but do not show up as “registry-publication discrepancies”
## and are therefore easily missed in review?

## changes ONLY within the registry, but NOT between latest registry entry and
## publication (i.e., trials with ONLY hidden changes)
n_hidden_changes <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_hidden_changes <- n_hidden_changes / sum(dat$has_publication_rating)*100
  # CI_hidden_changes <-
  #   binom.bayes(n_hidden_changes, sum(dat$has_publication_rating)) # multiply with 100
CI_hidden_changes_freq <-
  binom.test(n_hidden_changes, sum(dat$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_hidden_changes_severe <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_severe_anywithin == TRUE,
  na.rm = TRUE
)
n_hidden_changes_nonsevere <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_nonsevere_anywithin == TRUE,
  na.rm = TRUE
)

## changes ONLY between latest registry entry and publication, but NOT within
## the registry
n_only_reg_pub <- sum(
  dat$p_o_change_reg_pub == TRUE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_only_reg_pub <- n_only_reg_pub / sum(dat$has_publication_rating)*100
  # CI_only_reg_pub <-
  #   binom.bayes(n_only_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_only_reg_pub_freq <-
  binom.test(n_only_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_only_reg_pub_severe <- sum(
  dat$p_o_change_severe_reg_pub == TRUE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
n_only_reg_pub_nonsevere <- sum(
  dat$p_o_change_nonsevere_reg_pub == TRUE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)

## changes between latest registry entry and publication, AND additionally
## within the registry
n_within_and_reg_pub <- sum(
  dat$p_o_change_reg_pub == TRUE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_within_and_reg_pub <- n_within_and_reg_pub / sum(dat$has_publication_rating)*100
  # CI_within_and_reg_pub <-
  #   binom.bayes(n_within_and_reg_pub, sum(dat$has_publication_rating)) # multiply with 100
CI_within_and_reg_pub_freq <-
  binom.test(n_within_and_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_within_and_reg_pub_severe <- sum(
  dat$p_o_change_severe_reg_pub == TRUE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
n_within_and_reg_pub_nonsevere <- sum(
  dat$p_o_change_nonsevere_reg_pub == TRUE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)

## changes neither between latest registry entry and publication, nor within the
## registry
n_neither_nor <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_neither_nor <- n_neither_nor / sum(dat$has_publication_rating)*100

## run a test
test_that(
  'Does every trial with a rated publication fall in one of the categories?',
  expect_equal(n_hidden_changes + n_only_reg_pub + n_within_and_reg_pub + n_neither_nor, nrow(filter(dat, has_publication_rating == TRUE)))
)



## ---- OBJECTIVE 3: Figures 3, S1 and S2 --------------------------------------

## Figure 3:
## What are the pathways of changes that happen within the registry across study
## phases?
## As a more comprehensive assessment of when changes are made - especially
## keeping in mind the possibility of changes at multiple time points -, we do
## an Upset Plot for changes for the published trials.

dat_pub <- dat %>%
  filter(has_publication_rating == TRUE)

## Figure X: Sankey plot for changes in the sample of 300
dat_Sankey_nodes <- tribble(
  ~name, ~name_type,
  'Major (Start vs Completion)', 'major',      # 0
  'Minor (Start vs Completion)', 'minor',      # 1
  'No Changes (Start vs Completion)', 'no_change',              # 2
  'No Information', 'no_phase',                 # 3
  'Major (Completion vs Pub.)',  'major', # 4
  'Minor (Completion vs Pub.)', 'minor',  # 5
  'No Changes (Completion vs Pub.)', 'no_change',          # 6
  'No Information', 'no_phase',             # 7
  'Major (Pub. vs Latest)', 'major', # 8
  'Minor (Pub. vs Latest)', 'minor',# 9
  'No Changes (Pub. vs Latest)', 'no_change',         # 10
  'No Information', 'no_phase' #,            # 11
  # 'Major (Latest vs Paper)', 'major', # 12
  # 'Minor (Latest vs Paper)', 'minor', # 13
  # 'No Change (Latest vs Paper)', 'no_change'          # 14
)

dat_Sankey_links <- tribble(
  
  ~source, ~target, ~value,
  
   0,  4, nrow(filter(dat, p_o_change_severe_rec == TRUE & p_o_change_severe_postcomp == TRUE)),
   0,  5, nrow(filter(dat, p_o_change_severe_rec == TRUE & p_o_change_nonsevere_postcomp == TRUE)),
   0,  6, nrow(filter(dat, p_o_change_severe_rec == TRUE & no_change_postcomp == TRUE)),
   0,  7, nrow(filter(dat, p_o_change_severe_rec == TRUE & no_postcomp_phase == TRUE)),
   1,  4, nrow(filter(dat, p_o_change_nonsevere_rec == TRUE & p_o_change_severe_postcomp == TRUE)),
   1,  5, nrow(filter(dat, p_o_change_nonsevere_rec == TRUE & p_o_change_nonsevere_postcomp == TRUE)),
   1,  6, nrow(filter(dat, p_o_change_nonsevere_rec == TRUE & no_change_postcomp == TRUE)),
   1,  7, nrow(filter(dat, p_o_change_nonsevere_rec == TRUE & no_postcomp_phase == TRUE)),
   2,  4, nrow(filter(dat, no_change_rec == TRUE & p_o_change_severe_postcomp == TRUE)),
   2,  5, nrow(filter(dat, no_change_rec == TRUE & p_o_change_nonsevere_postcomp == TRUE)),
   2,  6, nrow(filter(dat, no_change_rec == TRUE & no_change_postcomp == TRUE)),
   2,  7, nrow(filter(dat, no_change_rec == TRUE & no_postcomp_phase == TRUE)),
   3,  4, nrow(filter(dat, no_rec_phase == TRUE & p_o_change_severe_postcomp == TRUE)),
   3,  5, nrow(filter(dat, no_rec_phase == TRUE & p_o_change_nonsevere_postcomp == TRUE)),
   3,  6, nrow(filter(dat, no_rec_phase == TRUE & no_change_postcomp == TRUE)),
   3,  7, nrow(filter(dat, no_rec_phase == TRUE & no_postcomp_phase == TRUE)),
  
   4,  8, nrow(filter(dat, p_o_change_severe_postcomp == TRUE & p_o_change_severe_postpub == TRUE)),
   4,  9, nrow(filter(dat, p_o_change_severe_postcomp == TRUE & p_o_change_nonsevere_postpub == TRUE)),
   4, 10, nrow(filter(dat, p_o_change_severe_postcomp == TRUE & no_change_postpub == TRUE)),
   4, 11, nrow(filter(dat, p_o_change_severe_postcomp == TRUE & no_postpub_phase == TRUE)),
   5,  8, nrow(filter(dat, p_o_change_nonsevere_postcomp == TRUE & p_o_change_severe_postpub == TRUE)),
   5,  9, nrow(filter(dat, p_o_change_nonsevere_postcomp == TRUE & p_o_change_nonsevere_postpub == TRUE)),
   5, 10, nrow(filter(dat, p_o_change_nonsevere_postcomp == TRUE & no_change_postpub == TRUE)),
   5, 11, nrow(filter(dat, p_o_change_nonsevere_postcomp == TRUE & no_postpub_phase == TRUE)),
   6,  8, nrow(filter(dat, no_change_postcomp == TRUE & p_o_change_severe_postpub == TRUE)),
   6,  9, nrow(filter(dat, no_change_postcomp == TRUE & p_o_change_nonsevere_postpub == TRUE)),
   6, 10, nrow(filter(dat, no_change_postcomp == TRUE & no_change_postpub == TRUE)),
   6, 11, nrow(filter(dat, no_change_postcomp == TRUE & no_postpub_phase == TRUE)),
   7,  8, nrow(filter(dat, no_postcomp_phase == TRUE & p_o_change_severe_postpub == TRUE)),
   7,  9, nrow(filter(dat, no_postcomp_phase == TRUE & p_o_change_nonsevere_postpub == TRUE)),
   7, 10, nrow(filter(dat, no_postcomp_phase == TRUE & no_change_postpub == TRUE)),
   7, 11, nrow(filter(dat, no_postcomp_phase == TRUE & no_postpub_phase == TRUE)) #,
  
  #  8, 12, nrow(filter(dat, p_o_change_severe_postpub == TRUE & p_o_change_severe_reg_pub == TRUE)),
  #  8, 13, nrow(filter(dat, p_o_change_severe_postpub == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  #  8, 14, nrow(filter(dat, p_o_change_severe_postpub == TRUE & no_change_reg_pub == TRUE)),
  #  9, 12, nrow(filter(dat, p_o_change_nonsevere_postpub == TRUE & p_o_change_severe_reg_pub == TRUE)),
  #  9, 13, nrow(filter(dat, p_o_change_nonsevere_postpub == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  #  9, 14, nrow(filter(dat, p_o_change_nonsevere_postpub == TRUE & no_change_reg_pub == TRUE)),
  # 10, 12, nrow(filter(dat, no_change_postpub == TRUE & p_o_change_severe_reg_pub == TRUE)),
  # 10, 13, nrow(filter(dat, no_change_postpub == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  # 10, 14, nrow(filter(dat, no_change_postpub == TRUE & no_change_reg_pub == TRUE)),
  # 11, 12, nrow(filter(dat, no_postpub_phase == TRUE & p_o_change_severe_reg_pub == TRUE)),
  # 11, 13, nrow(filter(dat, no_postpub_phase == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  # 11, 14, nrow(filter(dat, no_postpub_phase == TRUE & no_change_reg_pub == TRUE))
  
) %>%
  filter(value != 0)

Figure_X <- sankeyNetwork(
  Links = dat_Sankey_links,
  Nodes = dat_Sankey_nodes,
  Source = 'source',
  Target = 'target',
  Value = 'value',
  NodeID = 'name',
  NodeGroup = 'name_type',
  fontSize = 15,
  nodeWidth = 30
  # colourScale=ColourScal, nodeWidth=40, nodePadding=20
)
Figure_X

## Figure Y: Alternate Sankey plot for changes in the sample of 300
dat_SankeyY_nodes <- tribble(
  ~name, ~name_type,
  'Major (Within-Registry)', 'major',      # 0
  'Minor (Within-Registry)', 'minor',      # 1
  'No Changes (Within-Registry)', 'no_change',              # 2
  'No Information', 'no_phase',                 # 3
  'Major (Registry vs Publication)', 'major', # 4
  'Minor (Registry vs Publication)', 'minor', # 5
  'No Change (Registry vs Publication)', 'no_change'          # 6
)

dat_SankeyY_links <- tribble(
  
  ~source, ~target, ~value,
  ## ATTENTION! NUMBERS NOT MUTUALLY EXCLUSIVE, SO THEY ADD UP TO 311!
  
  0,  4, nrow(filter(dat, p_o_change_severe_anywithin == TRUE & p_o_change_severe_reg_pub == TRUE)),
  0,  5, nrow(filter(dat, p_o_change_severe_anywithin == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  0,  6, nrow(filter(dat, p_o_change_severe_anywithin == TRUE & no_change_reg_pub == TRUE)),
  1,  4, nrow(filter(dat, p_o_change_nonsevere_anywithin == TRUE & p_o_change_severe_reg_pub == TRUE)),
  1,  5, nrow(filter(dat, p_o_change_nonsevere_anywithin == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  1,  6, nrow(filter(dat, p_o_change_nonsevere_anywithin == TRUE & no_change_reg_pub == TRUE)),
  2,  4, nrow(filter(dat, no_change_anywithin == TRUE & p_o_change_severe_reg_pub == TRUE)),
  2,  5, nrow(filter(dat, no_change_anywithin == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  2,  6, nrow(filter(dat, no_change_anywithin == TRUE & no_change_reg_pub == TRUE)),
  3,  4, nrow(filter(dat, no_anywithin_phase == TRUE & p_o_change_severe_reg_pub == TRUE)),
  3,  5, nrow(filter(dat, no_anywithin_phase == TRUE & p_o_change_nonsevere_reg_pub == TRUE)),
  3,  6, nrow(filter(dat, no_anywithin_phase == TRUE & no_change_reg_pub == TRUE))
  
) %>%
  filter(value != 0)

Figure_Y <- sankeyNetwork(
  Links = dat_SankeyY_links,
  Nodes = dat_SankeyY_nodes,
  Source = 'source',
  Target = 'target',
  Value = 'value',
  NodeID = 'name',
  NodeGroup = 'name_type',
  fontSize = 15,
  nodeWidth = 30
  # colourScale=ColourScal, nodeWidth=40, nodePadding=20
)
Figure_Y

## Sankey - Alternative 2
test <- dat_pub %>%
  mutate(
    within_reg_change_type = case_when(
      p_o_change_severe_anywithin == TRUE ~ 'major',
      p_o_change_severe_anywithin == FALSE & p_o_change_nonsevere_anywithin == TRUE ~ 'minor',
      p_o_change_severe_anywithin == FALSE & p_o_change_nonsevere_anywithin == FALSE & no_change_anywithin == TRUE ~ 'no change',
      no_anywithin_phase == TRUE ~ 'no information'
    )
  ) %>%
  mutate(
    reg_pub_change_type = case_when(
      p_o_change_severe_reg_pub == TRUE ~ 'major',
      p_o_change_severe_reg_pub == FALSE & p_o_change_nonsevere_reg_pub == TRUE ~ 'minor',
      p_o_change_severe_reg_pub == FALSE & p_o_change_nonsevere_reg_pub == FALSE & no_change_reg_pub == TRUE ~ 'no change'
    )
  )

library(ggsankey)
test2 <- test %>%
  make_long(within_reg_change_type, reg_pub_change_type) %>%
  filter(!is.na(node)) # NAs are problem

FigureY2 <- ggplot(
  test2,
  aes(
    x = x, 
    next_x = next_x, 
    node = node, 
    next_node = next_node,
    fill = factor(node),
    label = node
  )
) +
  geom_sankey() +
  geom_sankey_label() +
  theme_sankey(base_size = 16)
FigureY2



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

# ggsave('Figure3.pdf',
#        Figure3,
#        scale = 1.25,
#        width = 7,
#        height = 5
# )

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
  xlab('Major discrepancies across multiple time points') #+
# ggtitle(label = "Major discrepancies in outcomes", subtitle = 'Sample of 300 registry entries and trial results publications')
FigureS1

# ggsave("FigureS1.pdf",
#        FigureS1,
#        scale = 1.25,
#        width = 7,
#        height = 5
# )

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
  xlab('Minor discrepancies across multiple time points') # +
# ggtitle(label = 'Minor discrepancies in primary outcomes', subtitle = 'Sample of 300 registry entries and trial results publications')
FigureS2

# ggsave("FigureS2.pdf",
#        FigureS2,
#        scale = 1.25,
#        width = 7,
#        height = 5
# )



## ---- OBJECTIVE 4: Reporting -------------------------------------------------

## Based on publications:
## We will determine the proportion of trials with transparent reporting of
## “within-registry” changes and “classical” outcome changes in the publication.
## How often both types of outcome changes are transparently reported in the
## results publications.
## (Equal to Research Questions 6 and 7 from the preregistered protocol.)


## (1) Reporting of within-registry discrepancies!

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
  # CI_reporting_severe_anywithin <-
  #   binom.bayes(n_reporting_severe_anywithin, n_severe_anywithin_sample) # multiply with 100
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
  # CI_reporting_any_anywithin <-
  #   binom.bayes(n_reporting_any_anywithin, n_any_anywithin_sample) # multiply with 100
CI_reporting_any_anywithin_freq <-
  binom.test(n_reporting_any_anywithin, n_any_anywithin_sample)$conf.int*100


## (2) Reporting of registry-publication discrepancies!

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
  # CI_reporting_severe_reg_pub <-
  #   binom.bayes(n_reporting_severe_reg_pub, n_severe_reg_pub_sample) # multiply with 100
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
  # CI_reporting_any_reg_pub  <-
  #   binom.bayes(n_reporting_any_reg_pub, n_any_reg_pub_sample) # multiply with 100
CI_reporting_any_reg_pub_freq <-
  binom.test(n_reporting_any_reg_pub, n_any_reg_pub_sample)$conf.int*100



## ---- OBJECTIVE 5: Associations, Within-Registry -----------------------------

## Based on all registry changes in all RCTs in the dataset:
## Which trial characteristics are associated with these reporting deficits? We
## will assess the association of ‘within-registry’ outcome changes with key
## candidate predictors (listed below).

## (Equal to Research Question 2 from the preregistered protocol.)

## the model will be estimated with a generalised linear model (logistic
## regression)

## bayesian model
# model_RQ2 <- brm(
#   as.numeric(p_o_change_anywithin) ~
#     phase_recoded +
#     main_sponsor +
#     publication_year +
#     registration_year +
#     medical_field_recoded +
#     # medical_field_recoded_binary +
#     registry +
#     is_multicentric +
#     enrollment +
#     intervention_type_recoded,
#   family="binomial",
#   data = dat,
#   cores = getOption('mc.cores', 2),
#   seed = 227 # I again asked Siri for a number between 1 and 999
# )
# summary(model_RQ2)

## see what priors the model used
# get_prior(
#   as.numeric(p_o_change_anywithin) ~
#     phase_recoded +
#     main_sponsor +
#     publication_year +
#     registration_year +
#     medical_field_recoded +
#     # medical_field_recoded_binary +
#     registry +
#     is_multicentric +
#     enrollment +
#     intervention_type_recoded,
#   family="binomial",
#   data = dat
# )

## frequentist model
model_RQ2_freq <- glm(
  p_o_change_anywithin ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat
)
summary(model_RQ2_freq)

## for interpretability, get the exponentiated coefficients, which transformes
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
exp(coef(model_RQ2_freq))
## retrieve the confidence intervals for the Odds Ratios - but is this necessary,
## since this is no sample?
exp(confint(model_RQ2_freq))
## the finalfit package automatically creates a table with frequencies and means
explanatory <- c(
  'phase_recoded', 'main_sponsor', 'publication_year ', 'registration_year', 'medical_field_recoded', 'registry', 'is_multicentric', 'enrollment', 'intervention_type_recoded'
)
dependent <- 'p_o_change_anywithin'
table_RQ2_freq <- finalfit(dat, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_RQ2_freq)



## ---- OBJECTIVE 5: Associations, Registry-Publication ------------------------

## Based on publications:
## Which trial characteristics are associated with these reporting deficits? We
## will assess the association of ‘registry-publication’ outcome discrepancies
## (‘classical’ outcome switching) with key candidate predictors (listed below).
## (Equal to Research Question 4 from the preregistered protocol.)

## the model will be estimated with a generalised linear model (logistic
## regression)


## bayesian model
# model_RQ4 <- brm(
#   as.numeric(p_o_change_reg_pub) ~
#     phase_recoded +
#     main_sponsor +
#     publication_year +
#     registration_year +
#     # medical_field_recoded +
#     medical_field_recoded_binary +
#     registry +
#     is_multicentric +
#     enrollment +
#     intervention_type_recoded,
#   family="binomial",
#   data = dat_pub,
#   cores = getOption('mc.cores', 2),
#   seed = 754 # I again asked Siri for a number between 1 and 999
# )
# summary(model_RQ4)

## see what priors the model used
# get_prior(
#   as.numeric(p_o_change_anywithin) ~
#     phase_recoded +
#     main_sponsor +
#     publication_year +
#     registration_year +
#     # medical_field_recoded +
#     medical_field_recoded_binary +
#     registry +
#     is_multicentric +
#     enrollment +
#     intervention_type_recoded,
#   family="binomial",
#   data = dat_pub
# )

## frequentist model
model_RQ4_freq <- glm(
  p_o_change_reg_pub ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    medical_field_recoded +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat_pub
)
summary(model_RQ4_freq)
## for interpretability, get the exponentiated coefficients, which transformes
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
exp(coef(model_RQ4_freq))
## retrieve the confidence intervals for the Odds Ratios
exp(confint(model_RQ4_freq))
## the finalfit package automatically creates a table with frequencies and means
explanatory <- c(
  'phase_recoded', 'main_sponsor', 'publication_year ', 'registration_year', 'medical_field_recoded', 'registry', 'is_multicentric', 'enrollment', 'intervention_type_recoded'
)
dependent <- 'p_o_change_reg_pub'
table_RQ4_freq <- finalfit(dat_pub, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_RQ4_freq)



## ---- OBJECTIVE 5: Associations, Cross-Over ----------------------------------

## Based on publications:
## We will assess the association between ‘within-registry’ outcome switching
## and ‘classical’ outcome switching.
## (Equal to Research Question 5 from the preregistered protocol.)

# ## bayesian model
# model_RQ5 <- brm(
#   as.numeric(p_o_change_reg_pub) ~ p_o_change_anywithin,
#   family = 'binomial',
#   data = dat_pub,
#   cores = getOption('mc.cores', 2),
#   seed = 827 # I again asked Siri for a number between 1 and 999
# )
# summary(model_RQ5)

## frequentist model
model_RQ5_freq <- glm(
  p_o_change_reg_pub ~ p_o_change_anywithin,
  family = 'binomial',
  data = dat_pub
)
summary(model_RQ5_freq)
exp(coef(model_RQ5_freq)) # options(scipen=999)
## the finalfit package automatically creates a table with frequencies and means
explanatory <- 'p_o_change_anywithin'
dependent <- 'p_o_change_reg_pub'
table_RQ5_freq <- finalfit(dat_pub, dependent, explanatory)
