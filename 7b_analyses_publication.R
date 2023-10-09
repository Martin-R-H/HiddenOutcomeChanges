library(tidyverse)
library(lubridate)
library(binom)
library(networkD3) # for Sankey plot
library(plotly) # for revised Sankey plot
library(UpSetR) # for UpSet plot
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
  relocate(enrollment, .after = phase_recoded) %>%
  relocate(is_multicentric, .after = allocation)
rm(dat_IV_add)

## add the medical fields variable
## extract journal fields based on the a copy of the Scimago Journal & Country
## Rank database (https://www.scimagojr.com/journalrank.php), which provides
## this classification

source('fun/assign_medical_fields.R') # to keep the algorithm out of this script

dat_fields <- read_delim('scimagojr 2021.csv', delim = ';') %>%
  rename(medical_field = 'Categories') %>%
  select(
    c(Title, medical_field)
  ) %>%
  mutate(Title = tolower(Title)) %>%
  rename_journals_for_matching()

## prepare matching
dat <- dat %>%
  mutate(journal_name_matching = journal_unpaywall) %>%
  mutate(
    journal_name_matching = if_else(
      is.na(journal_name_matching),
      journal_pubmed,
      journal_name_matching
    )
  ) %>%
  mutate(journal_name_matching = tolower(journal_name_matching)) %>%
  harmonise_journal_names()

## join the data
dat <- dat %>%
  left_join(dat_fields, by = c('journal_name_matching' = 'Title')) %>%
  mutate(has_medical_field = !is.na(medical_field))

## the merging creates six duplicates, which now need to be removed (it does not
## change our assignment of medical fields)
dat_duplicates <- dat[which(duplicated(dat$id)), ]
dat_duplicates_2 <- dat %>% filter(id %in% dat_duplicates$id)
rm(dat_duplicates, dat_duplicates_2)
dat <- dat %>%
  group_by(id) %>%
  slice_head() %>%
  ungroup()

## how many trials get matched with each of the databases?
sum(dat$has_medical_field)
dat_sample_checks <- dat %>%
  filter(has_medical_field == FALSE)
rm(dat_sample_checks)

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
## severe changes (additions) in any phase
n_severe_add_any <- sum(dat$p_o_change_severe_add_anywithin)
p_severe_add_any <- sum(dat$p_o_change_severe_add_anywithin)/nrow(dat)*100
## severe changes (deletions) in any phase
n_severe_rem_any <- sum(dat$p_o_change_severe_rem_anywithin)
p_severe_rem_any <- sum(dat$p_o_change_severe_rem_anywithin)/nrow(dat)*100
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
## severe changes (additions) in recruitment phase
n_severe_add_rec <- sum(dat$p_o_change_severe_add_rec)
p_severe_add_rec <- sum(dat$p_o_change_severe_add_rec)/nrow(dat)*100
## severe changes (deletions) in recruitment phase
n_severe_rem_rec <- sum(dat$p_o_change_severe_rem_rec)
p_severe_rem_rec <- sum(dat$p_o_change_severe_rem_rec)/nrow(dat)*100
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
## severe changes (additions) in post-completion phase
n_severe_add_postcomp <- sum(dat$p_o_change_severe_add_postcomp)
p_severe_add_postcomp <- sum(dat$p_o_change_severe_add_postcomp)/nrow(dat)*100
## severe changes (deletions) in post-completion phase
n_severe_rem_postcomp <- sum(dat$p_o_change_severe_rem_postcomp)
p_severe_rem_postcomp <- sum(dat$p_o_change_severe_rem_postcomp)/nrow(dat)*100
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
## severe changes (additions) in post-publication phase
n_severe_add_postpub <- sum(dat$p_o_change_severe_add_postpub)
p_severe_add_postpub <- sum(dat$p_o_change_severe_add_postpub)/nrow(dat)*100
## severe changes (deletions) in post-publication phase
n_severe_rem_postpub <- sum(dat$p_o_change_severe_rem_postpub)
p_severe_rem_postpub <- sum(dat$p_o_change_severe_rem_postpub)/nrow(dat)*100
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
CI_any_reg_pub <-
  binom.test(n_any_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## severe changes between latest registry entry and publication
n_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = T)
p_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_severe_reg_pub <-
  binom.test(n_severe_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## severe changes (additions) between latest registry entry and publication
n_severe_add_reg_pub <- sum(dat$p_o_change_severe_add_reg_pub, na.rm = T)
p_severe_add_reg_pub <- sum(dat$p_o_change_severe_add_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_severe_add_reg_pub <-
  binom.test(n_severe_add_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## severe changes (deletions) between latest registry entry and publication
n_severe_rem_reg_pub <- sum(dat$p_o_change_severe_rem_reg_pub, na.rm = T)
p_severe_rem_reg_pub <- sum(dat$p_o_change_severe_rem_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_severe_rem_reg_pub <-
  binom.test(n_severe_rem_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes between latest registry entry and publication
n_nonsevere_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = T)
p_nonsevere_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_nonsevere_reg_pub <-
  binom.test(n_nonsevere_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes (changes) between latest registry entry and publication
n_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = T)
p_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_nonsevere_c_reg_pub <-
  binom.test(n_nonsevere_c_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## non-severe changes (additions or omissions) between latest registry entry and publication
n_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = T)
p_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_nonsevere_ao_reg_pub <-
  binom.test(n_nonsevere_ao_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## no changes between latest registry entry and publication
n_no_change_reg_pub <- sum(dat$no_change_reg_pub, na.rm = T)
p_no_change_reg_pub <- sum(dat$no_change_reg_pub, na.rm = T)/sum(dat$has_publication_rating)*100
CI_no_change_reg_pub <-
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
CI_hidden_changes <-
  binom.test(n_hidden_changes, sum(dat$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_hidden_changes_severe <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_severe_anywithin == TRUE,
  na.rm = TRUE
)
p_hidden_changes_severe <- n_hidden_changes_severe / sum(dat$has_publication_rating)*100
CI_hidden_changes_severe <-
  binom.test(n_hidden_changes_severe, sum(dat$has_publication_rating))$conf.int*100
n_hidden_changes_nonsevere <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_nonsevere_anywithin == TRUE,
  na.rm = TRUE
)
p_hidden_changes_nonsevere <- n_hidden_changes_nonsevere / sum(dat$has_publication_rating)*100
CI_hidden_changes_nonsevere <-
  binom.test(n_hidden_changes_nonsevere, sum(dat$has_publication_rating))$conf.int*100

## changes ONLY between latest registry entry and publication, but NOT within
## the registry
n_only_reg_pub <- sum(
  dat$p_o_change_reg_pub == TRUE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_only_reg_pub <- n_only_reg_pub / sum(dat$has_publication_rating)*100
CI_only_reg_pub <-
  binom.test(n_only_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_only_reg_pub_severe <- sum(
  dat$p_o_change_severe_reg_pub == TRUE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_only_reg_pub_severe <- n_only_reg_pub_severe / sum(dat$has_publication_rating)*100
n_only_reg_pub_nonsevere <- sum(
  dat$p_o_change_nonsevere_reg_pub == TRUE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_only_reg_pub_nonsevere <- n_only_reg_pub_nonsevere / sum(dat$has_publication_rating)*100

## changes between latest registry entry and publication, AND additionally
## within the registry
n_within_and_reg_pub <- sum(
  dat$p_o_change_reg_pub == TRUE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_within_and_reg_pub <- n_within_and_reg_pub / sum(dat$has_publication_rating)*100
CI_within_and_reg_pub <-
  binom.test(n_within_and_reg_pub, sum(dat$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_within_and_reg_pub_severe <- sum(
  dat$p_o_change_severe_reg_pub == TRUE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_within_and_reg_pub_severe <- n_within_and_reg_pub_severe / sum(dat$has_publication_rating)*100
CI_within_and_reg_pub <-
  binom.test(n_within_and_reg_pub_severe, sum(dat$has_publication_rating))$conf.int*100
n_within_and_reg_pub_nonsevere <- sum(
  dat$p_o_change_nonsevere_reg_pub == TRUE & dat$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_within_and_reg_pub_nonsevere <- n_within_and_reg_pub_nonsevere / sum(dat$has_publication_rating)*100
CI_within_and_reg_pub <-
  binom.test(n_within_and_reg_pub_nonsevere, sum(dat$has_publication_rating))$conf.int*100

## changes neither between latest registry entry and publication, nor within the
## registry
n_neither_nor <- sum(
  dat$p_o_change_reg_pub == FALSE & dat$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_neither_nor <- n_neither_nor / sum(dat$has_publication_rating)*100

## all types of changes combined
n_hidden_and_overt <- n_hidden_changes + n_only_reg_pub + n_within_and_reg_pub
p_hidden_and_overt <- n_hidden_and_overt / sum(dat$has_publication_rating)*100
CI_hidden_and_overt <-
  binom.test(n_hidden_and_overt, sum(dat$has_publication_rating))$conf.int*100

n_hidden_and_overt_severe <- n_hidden_changes_severe + n_only_reg_pub_severe + n_within_and_reg_pub_severe
p_hidden_and_overt_severe <- n_hidden_and_overt_severe / sum(dat$has_publication_rating)*100
CI_hidden_and_overt_severe <-
  binom.test(n_hidden_and_overt_severe, sum(dat$has_publication_rating))$conf.int*100

n_hidden_and_overt_nonsevere <- n_hidden_changes_nonsevere + n_only_reg_pub_nonsevere + n_within_and_reg_pub_nonsevere
p_hidden_and_overt_nonsevere <- n_hidden_and_overt_nonsevere / sum(dat$has_publication_rating)*100
CI_hidden_and_overt_nonsevere <-
  binom.test(n_hidden_and_overt_nonsevere, sum(dat$has_publication_rating))$conf.int*100

## run a test
test_that(
  'Does every trial with a rated publication fall in one of the categories?',
  expect_equal(n_hidden_changes + n_only_reg_pub + n_within_and_reg_pub + n_neither_nor, nrow(filter(dat, has_publication_rating == TRUE)))
)



## ---- OBJECTIVE 3: Figure 3 (Sankey Plot) ------------------------------------

## What are the pathways of changes that happen within the registry across study
## phases?
## As a more comprehensive assessment of when changes are made - especially
## keeping in mind the possibility of changes at multiple time points -, we do
## a Sankey Plot for changes for the published trials.

dat_pub <- dat %>%
  filter(has_publication_rating == TRUE)
## (at this point, we work with the full dataset, but could also create tbe
## Sankey plot just for the sample of 300)

## Figure 3: Sankey plot for changes in the sample of 300
dat_Figure3_nodes <- tribble(
  ~name, ~name_type,
  'Major', 'major',      # 0
  'Minor', 'minor',      # 1
  'No Changes', 'no_change',              # 2
  'No Information', 'no_phase',                 # 3
  'Major',  'major', # 4
  'Minor', 'minor',  # 5
  'No Changes', 'no_change',          # 6
  'No Information', 'no_phase',             # 7
  'Major', 'major', # 8
  'Minor', 'minor',# 9
  'No Changes', 'no_change',         # 10
  'No Information', 'no_phase' #,            # 11
  # 'Major (Latest vs Paper)', 'major', # 12
  # 'Minor (Latest vs Paper)', 'minor', # 13
  # 'No Change (Latest vs Paper)', 'no_change'          # 14
)

dat_Figure3_links <- tribble(
  
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

Figure3 <- sankeyNetwork(
  Links = dat_Figure3_links,
  Nodes = dat_Figure3_nodes,
  Source = 'source',
  Target = 'target',
  Value = 'value',
  NodeID = 'name',
  NodeGroup = 'name_type',
  fontSize = 15,
  nodeWidth = 30
  # colourScale=ColourScal, nodeWidth=40, nodePadding=20
)
Figure3



## ---- OBJECTIVE 3: Figure 3 (Sankey Plot - Revised) --------------------------

## What are the pathways of changes that happen within the registry across study
## phases?
## As a more comprehensive assessment of when changes are made - especially
## keeping in mind the possibility of changes at multiple time points -, we do
## a Sankey Plot for changes for the published trials.

## create a dataset with unique categories
dat_F3 <- dat %>%
  mutate(
    p_o_change_nonsevere_rec_M = case_when(
      p_o_change_nonsevere_rec == TRUE & p_o_change_severe_rec == FALSE ~ TRUE,
      p_o_change_nonsevere_rec == TRUE & p_o_change_severe_rec == TRUE ~ FALSE,
      p_o_change_nonsevere_rec == FALSE ~ FALSE
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_postcomp_M = case_when(
      p_o_change_nonsevere_postcomp == TRUE & p_o_change_severe_postcomp == FALSE ~ TRUE,
      p_o_change_nonsevere_postcomp == TRUE & p_o_change_severe_postcomp == TRUE ~ FALSE,
      p_o_change_nonsevere_postcomp == FALSE ~ FALSE
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_postpub_M = case_when(
      p_o_change_nonsevere_postpub == TRUE & p_o_change_severe_postpub == FALSE ~ TRUE,
      p_o_change_nonsevere_postpub == TRUE & p_o_change_severe_postpub == TRUE ~ FALSE,
      p_o_change_nonsevere_postpub == FALSE ~ FALSE
    )
  )

Figure3_Revisions <- plot_ly(
  type = 'sankey',
  orientation = 'h',
  node = list(
    label = dat_Figure3_nodes$name,
    color = c('#FE6100', '#DC267F', '#785EF0', '#648FFF', '#FE6100', '#DC267F', '#785EF0', '#648FFF', '#FE6100', '#DC267F', '#785EF0', '#648FFF')
  ),
  valueformat = ".0f",
  link = list(
    source = c(0,0,0,0, 1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4, 5,5,5,5, 6,6,6,6, 7,7,7,7),
    target = c(4,5,6,7, 4,5,6,7, 4,5,6,7, 4,5,6,7, 8,9,10,11, 8,9,10,11, 8,9,10,11, 8,9,10,11),
    value = c(
      
      nrow(filter(dat_F3, p_o_change_severe_rec == TRUE & p_o_change_severe_postcomp == TRUE)), # Major (Start vs Completion) [0] AND Major (Completion vs Pub.) [4]
      nrow(filter(dat_F3, p_o_change_severe_rec == TRUE & p_o_change_nonsevere_postcomp_M == TRUE)), # Major (Start vs Completion) [0] AND Minor (Completion vs Pub.) [5]
      nrow(filter(dat_F3, p_o_change_severe_rec == TRUE & no_change_postcomp == TRUE)), # Major (Start vs Completion) [0] AND No Changes (Completion vs Pub.) [6]
      nrow(filter(dat_F3, p_o_change_severe_rec == TRUE & no_postcomp_phase == TRUE)), # Major (Start vs Completion) [0] AND No Information [7]
      
      nrow(filter(dat_F3, p_o_change_nonsevere_rec_M == TRUE & p_o_change_severe_postcomp == TRUE)), # Minor (Start vs Completion) [1] AND Major (Completion vs Pub.) [4]
      nrow(filter(dat_F3, p_o_change_nonsevere_rec_M == TRUE & p_o_change_nonsevere_postcomp_M == TRUE)), # Minor (Start vs Completion) [1] AND Minor (Completion vs Pub.) [5]
      nrow(filter(dat_F3, p_o_change_nonsevere_rec_M == TRUE & no_change_postcomp == TRUE)), # Minor (Start vs Completion) [1] AND No Changes (Completion vs Pub.) [6]
      nrow(filter(dat_F3, p_o_change_nonsevere_rec_M == TRUE & no_postcomp_phase == TRUE)), # Minor (Start vs Completion) [1] AND No Information [7]
      
      nrow(filter(dat_F3, no_change_rec == TRUE & p_o_change_severe_postcomp == TRUE)), # No Changes (Start vs Completion) [2] AND Major (Completion vs Pub.) [4]
      nrow(filter(dat_F3, no_change_rec == TRUE & p_o_change_nonsevere_postcomp_M == TRUE)), # No Changes (Start vs Completion) [2] AND Minor (Completion vs Pub.) [5]
      nrow(filter(dat_F3, no_change_rec == TRUE & no_change_postcomp == TRUE)), # No Changes (Start vs Completion) [2] AND No Changes (Completion vs Pub.) [6]
      nrow(filter(dat_F3, no_change_rec == TRUE & no_postcomp_phase == TRUE)), # No Changes (Start vs Completion) [2] AND No Information [7]
      
      nrow(filter(dat_F3, no_rec_phase == TRUE & p_o_change_severe_postcomp == TRUE)), # No Information [3] AND Major (Completion vs Pub.) [4]
      nrow(filter(dat_F3, no_rec_phase == TRUE & p_o_change_nonsevere_postcomp_M == TRUE)), # No Information [3] AND Minor (Completion vs Pub.) [5]
      nrow(filter(dat_F3, no_rec_phase == TRUE & no_change_postcomp == TRUE)), # No Information [3] AND No Changes (Completion vs Pub.) [6]
      nrow(filter(dat_F3, no_rec_phase == TRUE & no_postcomp_phase == TRUE)), # No Information [3] AND No Information [7]
      
      nrow(filter(dat_F3, p_o_change_severe_postcomp == TRUE & p_o_change_severe_postpub == TRUE)), # Major (Completion vs Pub.) [4] AND Major (Pub. vs Latest) [8]
      nrow(filter(dat_F3, p_o_change_severe_postcomp == TRUE & p_o_change_nonsevere_postpub_M == TRUE)), # Major (Completion vs Pub.) [4] AND Minor (Pub. vs Latest) [9]
      nrow(filter(dat_F3, p_o_change_severe_postcomp == TRUE & no_change_postpub == TRUE)), # Major (Completion vs Pub.) [4] AND No Changes (Pub. vs Latest) [10]
      nrow(filter(dat_F3, p_o_change_severe_postcomp == TRUE & no_postpub_phase == TRUE)), # Major (Completion vs Pub.) [4] AND No Information [11]
      
      nrow(filter(dat_F3, p_o_change_nonsevere_postcomp_M == TRUE & p_o_change_severe_postpub == TRUE)), # Minor (Completion vs Pub.) [5] AND Major (Pub. vs Latest) [8]
      nrow(filter(dat_F3, p_o_change_nonsevere_postcomp_M == TRUE & p_o_change_nonsevere_postpub_M == TRUE)), # Minor (Completion vs Pub.) [5] AND Minor (Pub. vs Latest) [9]
      nrow(filter(dat_F3, p_o_change_nonsevere_postcomp_M == TRUE & no_change_postpub == TRUE)), # Minor (Completion vs Pub.) [5] AND No Changes (Pub. vs Latest) [10]
      nrow(filter(dat_F3, p_o_change_nonsevere_postcomp_M == TRUE & no_postpub_phase == TRUE)), # Minor (Completion vs Pub.) [5] AND No Information [11]
      
      nrow(filter(dat_F3, no_change_postcomp == TRUE & p_o_change_severe_postpub == TRUE)), # No Changes (Completion vs Pub.) [6] AND Major (Pub. vs Latest) [8]
      nrow(filter(dat_F3, no_change_postcomp == TRUE & p_o_change_nonsevere_postpub_M == TRUE)), # No Changes (Completion vs Pub.) [6] AND Minor (Pub. vs Latest) [9]
      nrow(filter(dat_F3, no_change_postcomp == TRUE & no_change_postpub == TRUE)), # No Changes (Completion vs Pub.) [6] AND No Changes (Pub. vs Latest) [10]
      nrow(filter(dat_F3, no_change_postcomp == TRUE & no_postpub_phase == TRUE)), # No Changes (Completion vs Pub.) [6] AND No Information [11]
      
      nrow(filter(dat_F3, no_postcomp_phase == TRUE & p_o_change_severe_postpub == TRUE)), # No Information [7] AND Major (Pub. vs Latest) [8]
      nrow(filter(dat_F3, no_postcomp_phase == TRUE & p_o_change_nonsevere_postpub_M == TRUE)), # No Information [7] AND Minor (Pub. vs Latest) [9]
      nrow(filter(dat_F3, no_postcomp_phase == TRUE & no_change_postpub == TRUE)), # No Information [7] AND No Changes (Pub. vs Latest) [10]
      nrow(filter(dat_F3, no_postcomp_phase == TRUE & no_postpub_phase == TRUE)) # No Information [7] AND No Information [11]
      
    ) #,
  #   label = c(
  #     
  #     'Optional Description of the 0-4 Link',
  #     'Optional Description of the 0-5 Link',
  #     'Optional Description of the 0-6 Link',
  #     'Optional Description of the 0-7 Link',
  #     
  #     'Optional Description of the 1-4 Link',
  #     'Optional Description of the 1-5 Link',
  #     'Optional Description of the 1-6 Link',
  #     'Optional Description of the 1-7 Link',
  #     
  #     'Optional Description of the 2-4 Link',
  #     'Optional Description of the 2-5 Link',
  #     'Optional Description of the 2-6 Link',
  #     'Optional Description of the 2-7 Link',
  #     
  #     'Optional Description of the 3-4 Link',
  #     'Optional Description of the 3-5 Link',
  #     'Optional Description of the 3-6 Link',
  #     'Optional Description of the 3-7 Link',
  #     
  #     'Optional Description of the 4-8 Link',
  #     'Optional Description of the 4-9 Link',
  #     'Optional Description of the 4-10 Link',
  #     'Optional Description of the 4-11 Link',
  #     
  #     'Optional Description of the 5-8 Link',
  #     'Optional Description of the 5-9 Link',
  #     'Optional Description of the 5-10 Link',
  #     'Optional Description of the 5-11 Link',
  #     
  #     'Optional Description of the 6-8 Link',
  #     'Optional Description of the 6-9 Link',
  #     'Optional Description of the 6-10 Link',
  #     'Optional Description of the 6-11 Link',
  #     
  #     'Optional Description of the 7-8 Link',
  #     'Optional Description of the 7-9 Link',
  #     'Optional Description of the 7-10 Link',
  #     'Optional Description of the 7-11 Link',
  #     
  #   )
  )
) |>
  # layout(
  #   xaxis = list(showgrid = TRUE, zeroline = TRUE),
  #   yaxis = list(showgrid = TRUE, zeroline = TRUE)
  # ) |>
  # layout(
  #   title = list(
  #     text = 'Outcome Changes between the different timepoints',
  #     font = list(size = 13),
  #     x = 0.5,
  #     y = 1.1
  #   )
  # ) |>
  add_annotations(
    x = 0, xref = 'paper', xalign = 'left',
    y = 1.05, yref = 'paper',
    text = 'Start vs Completion',
    showarrow = FALSE
  ) |>
  add_annotations(
    x = 0.5, xref = 'paper', xalign = 'center',
    y = 1.05, yref = 'paper',
    text = 'Completion vs Publication',
    showarrow = FALSE
  ) |>
  add_annotations(
    x = 1, xref = 'paper', xalign = 'right',
    y = 1.05, yref = 'paper',
    text = 'Publication vs Latest Entry', # the 'finalised' figure still just says 'latest'!
    showarrow = FALSE
  )

Figure3_Revisions

rm(dat_F3)


## ---- OBJECTIVE 3: Figures 4, S1 & S2 (UpSet Plots) --------------------------

## What are the pathways of changes that happen within the registry across study
## phases?
## As a more comprehensive assessment of when changes are made - especially
## keeping in mind the possibility of changes at multiple time points -, we do
##Upset Plots for any changes in the sample of 300.

## reshape the dataset to create the UpSet Plots
UpSet_links <-  dat_pub %>%
  select(
    id,
    p_o_change_rec,
    p_o_change_postcomp,
    p_o_change_postpub,
    p_o_change_reg_pub,
    p_o_change_severe_rec,
    p_o_change_severe_postcomp,
    p_o_change_severe_postpub,
    p_o_change_severe_reg_pub,
    p_o_change_nonsevere_rec,
    p_o_change_nonsevere_postcomp,
    p_o_change_nonsevere_postpub,
    p_o_change_nonsevere_reg_pub
  ) %>%
  mutate(
    p_o_change_rec_alt = if_else(
      p_o_change_rec == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_postcomp_alt = if_else(
      p_o_change_postcomp == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_postpub_alt = if_else(
      p_o_change_postpub == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_reg_pub_alt = if_else(
      p_o_change_reg_pub == TRUE,
      id,
      'NA'
    )
  )%>%
  mutate(
    p_o_change_severe_rec_alt = if_else(
      p_o_change_severe_rec == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_postcomp_alt = if_else(
      p_o_change_severe_postcomp == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_postpub_alt = if_else(
      p_o_change_severe_postpub == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_reg_pub_alt = if_else(
      p_o_change_severe_reg_pub == TRUE,
      id,
      'NA'
    )
  )%>%
  mutate(
    p_o_change_nonsevere_rec_alt = if_else(
      p_o_change_nonsevere_rec == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_postcomp_alt = if_else(
      p_o_change_nonsevere_postcomp == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_postpub_alt = if_else(
      p_o_change_nonsevere_postpub == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_reg_pub_alt = if_else(
      p_o_change_nonsevere_reg_pub == TRUE,
      id,
      'NA'
    )
  ) %>%
  mutate(
    p_o_change_rec_alt = na_if(
      p_o_change_rec_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_postcomp_alt = na_if(
      p_o_change_postcomp_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_postpub_alt = na_if(
      p_o_change_postpub_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_reg_pub_alt = na_if(
      p_o_change_reg_pub_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_rec_alt = na_if(
      p_o_change_severe_rec_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_postcomp_alt = na_if(
      p_o_change_severe_postcomp_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_postpub_alt = na_if(
      p_o_change_severe_postpub_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_severe_reg_pub_alt = na_if(
      p_o_change_severe_reg_pub_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_rec_alt = na_if(
      p_o_change_nonsevere_rec_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_postcomp_alt = na_if(
      p_o_change_nonsevere_postcomp_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_postpub_alt = na_if(
      p_o_change_nonsevere_postpub_alt, 'NA'
    )
  ) %>%
  mutate(
    p_o_change_nonsevere_reg_pub_alt = na_if(
      p_o_change_nonsevere_reg_pub_alt, 'NA'
    )
  )

## Figure 4: create list items
start_completion <- na.omit(UpSet_links$p_o_change_rec_alt)
completion_publication <- na.omit(UpSet_links$p_o_change_postcomp_alt)
publication_latest_entry  <- na.omit(UpSet_links$p_o_change_postpub_alt)
latest_entry_paper <- na.omit(UpSet_links$p_o_change_reg_pub_alt)

## Figure 4: create list
list_Figure4 = list(
  'Start - Completion' = start_completion,
  'Completion - Publication' = completion_publication,
  'Publication - Latest Entry' = publication_latest_entry,
  'Latest Entry - Paper' = latest_entry_paper
)

## Figure 4: create figure
Figure4 <- upset(
  fromList(list_Figure4),
  sets = c('Start - Completion', 'Completion - Publication', 'Publication - Latest Entry', 'Latest Entry - Paper'),
  keep.order = TRUE,
  order.by = c('freq'),
  point.size = 3,
  line.size = 1.5,
  text.scale = c(1.2, 1, 0.9, 1, 1.2),
  sets.x.label = 'Comparisons between timepoints', #  (and number of trials of each)
  mainbar.y.label = 'Number of trials' #  with each combination of discrepancies
)
Figure4

## Figure S1: create list items
start_completion <- na.omit(UpSet_links$p_o_change_severe_rec_alt)
completion_publication <- na.omit(UpSet_links$p_o_change_severe_postcomp_alt)
publication_latest_entry  <- na.omit(UpSet_links$p_o_change_severe_postpub_alt)
latest_entry_paper <- na.omit(UpSet_links$p_o_change_severe_reg_pub_alt)

## Figure S1: create list
list_FigureS1 = list(
  'Start - Completion' = start_completion,
  'Completion - Publication' = completion_publication,
  'Publication - Latest Entry' = publication_latest_entry,
  'Latest Entry - Paper' = latest_entry_paper
)

## Figure S1: create figure
FigureS1 <- upset(
  fromList(list_FigureS1),
  sets = c('Start - Completion', 'Completion - Publication', 'Publication - Latest Entry', 'Latest Entry - Paper'),
  keep.order = TRUE,
  order.by = c('freq'),
  point.size = 3,
  line.size = 1.5,
  text.scale = c(1.2, 1, 0.9, 1, 1.2),
  sets.x.label = 'Comparisons between timepoints',
  mainbar.y.label = 'Number of trials with major discrepancies'
)
FigureS1

## Figure S2: create list items
start_completion <- na.omit(UpSet_links$p_o_change_nonsevere_rec_alt)
completion_publication <- na.omit(UpSet_links$p_o_change_nonsevere_postcomp_alt)
publication_latest_entry  <- na.omit(UpSet_links$p_o_change_nonsevere_postpub_alt)
latest_entry_paper <- na.omit(UpSet_links$p_o_change_nonsevere_reg_pub_alt)

## Figure S2: create list
list_FigureS2 = list(
  'Start - Completion' = start_completion,
  'Completion - Publication' = completion_publication,
  'Publication - Latest Entry' = publication_latest_entry,
  'Latest Entry - Paper' = latest_entry_paper
)

## Figure S2: create figure
FigureS2 <- upset(
  fromList(list_FigureS2),
  sets = c('Start - Completion', 'Completion - Publication', 'Publication - Latest Entry', 'Latest Entry - Paper'),
  keep.order = TRUE,
  order.by = c('freq'),
  point.size = 3,
  line.size = 1.5,
  text.scale = c(1.2, 1, 1.2, 1, 1.2),
  sets.x.label = 'Comparisons between timepoints',
  mainbar.y.label = 'Number of trials with minor discrepancies'
)
FigureS2



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
CI_reporting_severe_anywithin <-
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
CI_reporting_severe_reg_pub <-
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
CI_reporting_any_reg_pub <-
  binom.test(n_reporting_any_reg_pub, n_any_reg_pub_sample)$conf.int*100


## (3) Reporting of any discrepancies!

## How many trials with *any* severe changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_severe_any = if_else(
      (p_o_change_severe_anywithin == TRUE | p_o_change_severe_reg_pub == TRUE) & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )
n_reporting_severe_any <- sum(dat_pub$reporting_severe_any, na.rm = TRUE)
n_severe_any_sample <- nrow(filter(dat_pub, p_o_change_severe_anywithin == TRUE | p_o_change_severe_reg_pub == TRUE))
p_reporting_severe_any <- n_reporting_severe_any / n_severe_any_sample * 100
CI_reporting_severe_any <-
  binom.test(n_reporting_severe_any, n_severe_any_sample)$conf.int*100

## How many trials with *any* changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_any = if_else(
      (p_o_change_anywithin == TRUE | p_o_change_reg_pub == TRUE) & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )
n_reporting_any <- sum(dat_pub$reporting_any, na.rm = TRUE)
n_any_sample <- nrow(filter(dat_pub, p_o_change_anywithin == TRUE | p_o_change_reg_pub == TRUE))
p_reporting_any <- n_reporting_any / n_any_sample * 100
CI_reporting_any <-
  binom.test(n_reporting_any, n_any_sample)$conf.int*100



## ---- OBJECTIVE 5: Associations, Within-Registry -----------------------------

## Based on all registry changes in all RCTs in the dataset:
## Which trial characteristics are associated with these reporting deficits? We
## will assess the association of ‘within-registry’ outcome changes with key
## candidate predictors (listed below).

## (Equal to Research Question 2 from the preregistered protocol.)

## the model will be estimated with a generalised linear model (logistic
## regression)
model_RQ2 <- glm(
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
summary(model_RQ2)

## for interpretability, get the exponentiated coefficients, which transformes
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
round(exp(coef(model_RQ2)), 2)
## retrieve the confidence intervals for the Odds Ratios - but is this necessary,
## since this is no sample?
round(exp(confint(model_RQ2)), 2)
## the finalfit package automatically creates a table with frequencies and means
explanatory <- c(
  'phase_recoded', 'main_sponsor', 'publication_year ', 'registration_year', 'medical_field_recoded', 'registry', 'is_multicentric', 'enrollment', 'intervention_type_recoded'
)
dependent <- 'p_o_change_anywithin'
table_RQ2 <- finalfit(dat, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_RQ2)



## ---- OBJECTIVE 5: Associations, Registry-Publication ------------------------

## Based on publications:
## Which trial characteristics are associated with these reporting deficits? We
## will assess the association of ‘registry-publication’ outcome discrepancies
## (‘classical’ outcome switching) with key candidate predictors (listed below).
## (Equal to Research Question 4 from the preregistered protocol.)

## the model will be estimated with a generalised linear model (logistic
## regression)
model_RQ4 <- glm(
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
summary(model_RQ4)
## for interpretability, get the exponentiated coefficients, which transformes
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
round(exp(coef(model_RQ4)), 2)
## retrieve the confidence intervals for the Odds Ratios
round(exp(confint(model_RQ4)), 2)
## the finalfit package automatically creates a table with frequencies and means
explanatory <- c(
  'phase_recoded', 'main_sponsor', 'publication_year ', 'registration_year', 'medical_field_recoded', 'registry', 'is_multicentric', 'enrollment', 'intervention_type_recoded'
)
dependent <- 'p_o_change_reg_pub'
table_RQ4 <- finalfit(dat_pub, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_RQ4)



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
model_RQ5 <- glm(
  p_o_change_reg_pub ~ p_o_change_anywithin,
  family = 'binomial',
  data = dat_pub
)
summary(model_RQ5)
round(exp(coef(model_RQ5)), 2) # options(scipen=999)
round(exp(confint(model_RQ5)), 2)
## the finalfit package automatically creates a table with frequencies and means
explanatory <- 'p_o_change_anywithin'
dependent <- 'p_o_change_reg_pub'
table_RQ5 <- finalfit(dat_pub, dependent, explanatory)



## ---- SENSITIVITY ANALYSIS ---------------------------------------------------

## For a sensitivity analysis, do some of the publications-related analyses with
## just the publications in which the outcome was explicitly names as such.

## filter data
dat_pub_sensitivity <- dat %>%
  filter(
    has_publication_rating == TRUE & outcome_determined == 'explicit'
  )

## SENSITIVITY ANALYSIS - OBJECTIVE 2

## any changes between latest registry entry and publication
n_any_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_reg_pub, na.rm = T)
p_any_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_any_reg_pub_freq_SA <-
  binom.test(n_any_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## severe changes between latest registry entry and publication
n_severe_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_severe_reg_pub, na.rm = T)
p_severe_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_severe_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_severe_reg_pub_freq_SA <-
  binom.test(n_severe_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## severe changes (additions) between latest registry entry and publication
n_severe_add_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_severe_add_reg_pub, na.rm = T)
p_severe_add_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_severe_add_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_severe_add_reg_pub_freq_SA <-
  binom.test(n_severe_add_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## severe changes between latest registry entry and publication
n_severe_rem_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_severe_rem_reg_pub, na.rm = T)
p_severe_rem_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_severe_rem_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_severe_rem_reg_pub_freq_SA <-
  binom.test(n_severe_rem_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## non-severe changes between latest registry entry and publication
n_nonsevere_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_nonsevere_reg_pub, na.rm = T)
p_nonsevere_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_nonsevere_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_nonsevere_reg_pub_freq_SA <-
  binom.test(n_nonsevere_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## non-severe changes (changes) between latest registry entry and publication
n_nonsevere_c_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_nonsevere_c_reg_pub, na.rm = T)
p_nonsevere_c_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_nonsevere_c_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_nonsevere_c_reg_pub_freq_SA <-
  binom.test(n_nonsevere_c_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## non-severe changes (additions or omissions) between latest registry entry and publication
n_nonsevere_ao_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_nonsevere_ao_reg_pub, na.rm = T)
p_nonsevere_ao_reg_pub_SA <- sum(dat_pub_sensitivity$p_o_change_nonsevere_ao_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_nonsevere_ao_reg_pub_freq_SA <-
  binom.test(n_nonsevere_ao_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## no changes between latest registry entry and publication
n_no_change_reg_pub_SA <- sum(dat_pub_sensitivity$no_change_reg_pub, na.rm = T)
p_no_change_reg_pub_SA <- sum(dat_pub_sensitivity$no_change_reg_pub, na.rm = T)/sum(dat_pub_sensitivity$has_publication_rating)*100
CI_no_change_reg_pub_freq_SA <-
  binom.test(n_no_change_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100

## run some tests
test_that(
  'Between registry and publication, do trials with changes and with no changes add up?',
  expect_equal(n_any_reg_pub_SA + n_no_change_reg_pub_SA, nrow(filter(dat_pub_sensitivity, has_publication_rating == TRUE)))
)

## SENSITIVITY ANALYSIS - OBJECTIVE 3

## changes ONLY within the registry, but NOT between latest registry entry and
## publication (i.e., trials with ONLY hidden changes)
n_hidden_changes_SA <- sum(
  dat_pub_sensitivity$p_o_change_reg_pub == FALSE & dat_pub_sensitivity$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_hidden_changes_SA <- n_hidden_changes_SA / sum(dat_pub_sensitivity$has_publication_rating)*100
CI_hidden_changes_SA <-
  binom.test(n_hidden_changes_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_hidden_changes_severe_SA <- sum(
  dat_pub_sensitivity$p_o_change_reg_pub == FALSE & dat_pub_sensitivity$p_o_change_severe_anywithin == TRUE,
  na.rm = TRUE
)
n_hidden_changes_nonsevere_SA <- sum(
  dat_pub_sensitivity$p_o_change_reg_pub == FALSE & dat_pub_sensitivity$p_o_change_nonsevere_anywithin == TRUE,
  na.rm = TRUE
)

## changes ONLY between latest registry entry and publication, but NOT within
## the registry
n_only_reg_pub_SA <- sum(
  dat_pub_sensitivity$p_o_change_reg_pub == TRUE & dat_pub_sensitivity$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_only_reg_pub_SA <- n_only_reg_pub_SA / sum(dat_pub_sensitivity$has_publication_rating)*100
CI_only_reg_pub_SA <-
  binom.test(n_only_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_only_reg_pub_severe_SA <- sum(
  dat_pub_sensitivity$p_o_change_severe_reg_pub == TRUE & dat_pub_sensitivity$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
n_only_reg_pub_nonsevere_SA <- sum(
  dat_pub_sensitivity$p_o_change_nonsevere_reg_pub == TRUE & dat_pub_sensitivity$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)

## changes between latest registry entry and publication, AND additionally
## within the registry
n_within_and_reg_pub_SA <- sum(
  dat_pub_sensitivity$p_o_change_reg_pub == TRUE & dat_pub_sensitivity$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
p_within_and_reg_pub_SA <- n_within_and_reg_pub_SA / sum(dat_pub_sensitivity$has_publication_rating)*100
CI_within_and_reg_pub_SA <-
  binom.test(n_within_and_reg_pub_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100
## also calculate numbers for major and minor changes
n_within_and_reg_pub_severe_SA <- sum(
  dat_pub_sensitivity$p_o_change_severe_reg_pub == TRUE & dat_pub_sensitivity$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)
n_within_and_reg_pub_nonsevere_SA <- sum(
  dat_pub_sensitivity$p_o_change_nonsevere_reg_pub == TRUE & dat_pub_sensitivity$p_o_change_anywithin == TRUE,
  na.rm = TRUE
)

## changes neither between latest registry entry and publication, nor within the
## registry
n_neither_nor_SA <- sum(
  dat_pub_sensitivity$p_o_change_reg_pub == FALSE & dat_pub_sensitivity$p_o_change_anywithin == FALSE,
  na.rm = TRUE
)
p_neither_nor_SA <- n_neither_nor_SA / sum(dat_pub_sensitivity$has_publication_rating)*100

## all types of changes combined
n_hidden_and_overt_SA <- n_hidden_changes_SA + n_only_reg_pub_SA + n_within_and_reg_pub_SA
p_hidden_and_overt_SA <- n_hidden_and_overt_SA / sum(dat_pub_sensitivity$has_publication_rating)*100
CI_hidden_and_overt_SA <-
  binom.test(n_hidden_and_overt_SA, sum(dat_pub_sensitivity$has_publication_rating))$conf.int*100

## SENSITIVITY ANALYSIS - OBJECTIVE 5

## frequentist model
model_RQ4_SA <- glm(
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
  data = dat_pub_sensitivity
)
summary(model_RQ4_SA)
## for interpretability, get the exponentiated coefficients, which transforms
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
exp(coef(model_RQ4_SA))
## retrieve the confidence intervals for the Odds Ratios
exp(confint(model_RQ4_SA))
## the finalfit package automatically creates a table with frequencies and means
explanatory <- c(
  'phase_recoded', 'main_sponsor', 'publication_year ', 'registration_year', 'medical_field_recoded', 'registry', 'is_multicentric', 'enrollment', 'intervention_type_recoded'
)
dependent <- 'p_o_change_reg_pub'
table_RQ4_SA <- finalfit(dat_pub_sensitivity, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_RQ4_SA)



## ---- SAVE FINAL DATASET -----------------------------------------------------

dat_final <- dat %>%
  relocate(registry:is_publication_5y, .after=id) %>%
  relocate(rater_comment_add_rec:rater_comment_add_postpub, .after=rater_comment) %>%
  relocate(has_medical_field, .before=medical_field) %>%
  relocate(has_medical_field:medical_field_recoded, .after=is_publication_5y) %>%
  select(
    -referenceid,
    -change_a_i_points_to_results,
    -change_i_p_points_to_results,
    -change_p_l_points_to_results,
    -journal_name_matching
  )

dat_final %>% write_csv(
  'data/ASCERTAIN_Dataset_final.csv'
)



## ---- ADDITIONAL ANALYSIS 1 --------------------------------------------------

## analysis requested by a reviewer

dat_pub_addition_1 <- dat_pub |>
  filter(pub_sig_outcome != 'multiple_primaries') |> # drop trials with multiple primary outcomes
  mutate(
    outcome_significance = case_when(
      pub_sig_outcome == 'all_significant' | pub_sig_outcome == 'some_significant' ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  mutate(
    p_o_change_any = if_else(
      p_o_change_anywithin == TRUE | p_o_change_reg_pub == TRUE,
      TRUE,
      FALSE
    )
  )

## the model will be estimated with a generalised linear model (logistic
## regression)
model_addition_1 <- glm(
  outcome_significance ~ p_o_change_any, # or p_o_change_reg_pub?``
  family="binomial",
  data = dat_pub_addition_1
)
summary(model_addition_1)
## for interpretability, get the exponentiated coefficients, which transformes
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
round(exp(coef(model_addition_1)), 2)
## retrieve the confidence intervals for the Odds Ratios
round(exp(confint(model_addition_1)), 2)
## the finalfit package automatically creates a table with frequencies and means
explanatory <- 'p_o_change_any'
dependent <- 'outcome_significance'
table_addition_1 <- finalfit(dat_pub_addition_1, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_addition_1)


## ---- ADDITIONAL ANALYSIS 2 --------------------------------------------------

## analysis based on a question by a reviewer

## Based on publications:
## Which trial characteristics are associated with these reporting deficits? We
## will assess the association of ‘registry-publication’ outcome discrepancies
## (‘classical’ outcome switching) with key candidate predictors (listed below).
## (Equal to Research Question 4 from the preregistered protocol.)

## prepare the dataset to include a 'hidden
dat_pub_addition_2 <- dat_pub %>%
  mutate(
    has_hidden_changes = if_else(
      p_o_change_reg_pub == FALSE & p_o_change_anywithin == TRUE,
      TRUE,
      FALSE
    )
  )

## the model will be estimated with a generalised linear model (logistic
## regression)
model_addition_2 <- glm(
  has_hidden_changes ~
    phase_recoded +
    main_sponsor +
    publication_year +
    registration_year +
    # medical_field_recoded + ## too comnplex of a categorisation for 292 trials
    medical_field_recoded_binary +
    registry +
    is_multicentric +
    enrollment +
    intervention_type_recoded,
  family="binomial",
  data = dat_pub_addition_2
)
summary(model_addition_2)
## for interpretability, get the exponentiated coefficients, which transformes
## them into odds rations
## to do this, it is sometimes helpful to turn off scientific notation in R
## using options(scipen=999)
round(exp(coef(model_addition_2)), 2)
## retrieve the confidence intervals for the Odds Ratios
round(exp(confint(model_addition_2)), 2)
## the finalfit package automatically creates a table with frequencies and means
explanatory <- c(
  'phase_recoded', 'main_sponsor', 'publication_year ', 'registration_year', 'medical_field_recoded_binary', 'registry', 'is_multicentric', 'enrollment', 'intervention_type_recoded'
)
dependent <- 'has_hidden_changes'
table_addition_2 <- finalfit(dat_pub_addition_2, dependent, explanatory)
## assess model fit using the Hosmer-Lemeshow Goodness-of-Fit Test
hltest(model_addition_2)

