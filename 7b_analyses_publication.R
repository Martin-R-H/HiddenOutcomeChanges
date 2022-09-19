library(tidyverse)
library(ggupset)
library(testthat)

set.seed(638) # I asked Siri to generate a random number between 1 and 1000



## ---- NOTES ------------------------------------------------------------------

## - We can also inlude some more sanity checks there, like this one:
##   (a) if the trial has no recriutment phase, this should be reflected in the
##       Numbat ratings
##       test_that(
##        'when has_recruitment_phase = TRUE then change_a_i_no_phase = 1 or NULL and when has_recruitment_phase = FALSE then change_a_i_no_phase = 0 or NULL',
##        expect
##      )
##  (b) the same applies to all other phases,



## ---- READ IN DATA -----------------------------------------------------------

dat <- read_csv(
  'data/processed_history_data_analyses.csv',
  guess_max = 2000 # solves the parsing issue from script 7a
)



## ---- PREPARE DATA -----------------------------------------------------------

## recode the 'phase variable'
dat <- dat %>%
  mutate(
    phase_recoded = case_when(
      phase == 'Early Phase 1' | phase ==  'I' | phase == 'Phase 1' ~ 'Phase 1',
      phase == 'II' | phase ==  'IIa' | phase ==  'IIb' | phase ==  'Phase 1/Phase 2' | phase ==  'Phase 2' ~ 'Phase 2',
      phase == 'II-III' | phase ==  'III' | phase ==  'IIIb' | phase == 'Phase 2/Phase 3' | phase ==  'Phase 3' ~ 'Phase 3',
      phase == 'IV' | phase ==  'Phase 4' ~ 'Phase 3',
      TRUE ~ 'No phase'
    )
  )

## medical fields - first option according to protocol
## extract journal fields based on the a copy of the Scimago Journal & Country
## Rank database (https://www.scimagojr.com/journalrank.php), which provides
## this classification
## see also here: https://service.elsevier.com/app/answers/detail/a_id/15181/supporthub/scopus
## downloaded on 12 September 2022
dat_fields <- read_delim('scimagojr 2021.csv', delim = ';') %>%
  rename(Scimago_categories = 'Categories') %>%
  select(
    c(Title, Scimago_categories)
  ) %>%
  mutate(Title = tolower(Title))

dat <- dat %>%
  mutate(journal_name_lowercase = tolower(journal_unpaywall)) %>%
  left_join(dat_fields, by = c('journal_name_lowercase' = 'Title')) %>%
  mutate(has_medical_fields_Scimago = !is.na(Scimago_categories))

# dat_C <- dat %>%
#   filter(has_medical_fields_Scimago == TRUE) %>%
#   group_by(journal_name_lowercase) %>%
#   slice_head() %>%
#   select(
#     c(
#       journal_unpaywall,
#       journal_pubmed,
#       journal_name_lowercase,
#       Scimago_categories
#     )
#   ) %>%
#   ungroup()
# write_excel_csv(dat_C, file = 'ASCERTAIN_journal_names_categories.xls')
# rm(dat_C)


## medical fields - second option
## extract journal fields based on the Web of Science (WoS) research categories,
## which are obtained by downloading the Science Citation Index Expanded (SCIE)
## from https://mjl.clarivate.com/collection-list-downloads
## downloaded on 16 September 2022
dat_fields_alt <- read_csv('wos-core_SCIE 2022-August-18.csv') %>%
  rename(
    journal_title = 'Journal title', WoS_categories = 'Web of Science Categories'
  ) %>%
  select(
    c(journal_title, WoS_categories)
  ) %>%
  mutate(journal_title = tolower(journal_title))

dat <- dat %>%
  left_join(dat_fields_alt, by = c('journal_name_lowercase' = 'journal_title')) %>%
  mutate(has_medical_fields_WoS = !is.na(WoS_categories)) %>%
  select(!journal_name_lowercase)

## we group all journals that are in the category “Medicine, General & Internal”
## or “Multidisciplinary Sciences” as General, the rest as specialty journals
## list here: https://images.webofknowledge.com//WOKRS534DR2/help/WOS/hp_subject_category_terms_tasca.html


## CHECKS 1

## find out how many unique categories there are
length(unique(dat$Scimago_categories))
unique(dat$Scimago_categories)
# 356, with multiple categories
length(unique(dat$WoS_categories))
unique(dat$WoS_categories)
# 166, with multiple categories

dat_test <- dat %>%
  mutate(
    Scimago_categories_split = str_split(Scimago_categories, ';')
  )
dat_test <- dat_test %>%
  mutate(
    Scimago_categories_split = as.list.data.frame(Scimago_categories_split)
  )


## CHECKS 2

## how many trials get matched with each of the databases
sum(dat$has_medical_fields_Scimago)
sum(dat$has_medical_fields_WoS)

dat_sample_checks_1 <- dat %>%
  filter(has_medical_fields_Scimago == FALSE)
sample_checks_1 <- sample(dat_sample_checks_1$journal_unpaywall, 25)
rm(dat_sample_checks_1)
sample_checks_1

dat_sample_checks_2 <- dat %>%
  filter(has_medical_fields_WoS == FALSE)
sample_checks_2 <- sample(dat_sample_checks_2$journal_unpaywall, 25)
rm(dat_sample_checks_2)
sample_checks_2

## quite a few well-known journals are not matched, especially Lancet
## publications - probable because of the The
## find some of the non-matched Lancet journals
dat_fields_lancet <- dat_fields$Title[which(str_detect(dat_fields$Title, 'lancet'))]
dat_fields_alt_lancet <- dat_fields_alt$journal_title[which(str_detect(dat_fields_alt$journal_title, 'lancet'))]

rm(dat_fields, dat_fields_alt)


## ---- RESEARCH QUESTION 1 ----------------------------------------------------

## Protocol:
## "Based on all registry changes in all RCTs in the dataset, we will determine
## the proportion of ‘within-registry’ outcome changes in published clinical
## trials across key trial phases (active, inactive, published)."

## first, create variables that indicate whether trials have outcome switches in
## each of the study phases
dat <- dat %>%
  mutate(
    p_o_change_rec = ifelse(
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
        change_a_i_primary_omitted == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_postcomp= ifelse(
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
        change_i_p_primary_omitted == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_postpub= ifelse(
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
        change_p_l_primary_omitted == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_anywithin = ifelse(
      (p_o_change_rec == TRUE |
         p_o_change_postcomp == TRUE |
         p_o_change_postpub == TRUE),
      TRUE,
      FALSE
    )
  )

## second, create variables that indicate whether trials have 'severe' outcome
## changes (i.e., adding or omitting primary outcomes, moving primary outcomes
## to secondary, or the other way round) in each of the study phases, and also
## between the latest registry entry and publication
dat <- dat %>%
  mutate(
    p_o_change_severe_rec = ifelse(
      (change_a_i_new_primary == '1' |
        change_a_i_primary_omitted == '1' |
        change_a_i_primary_from_secondary == '1' |
        change_a_i_primary_to_secondary == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_severe_postcomp = ifelse(
      (change_i_p_new_primary == '1' |
        change_i_p_primary_omitted == '1' | 
        change_i_p_primary_from_secondary == '1' |
        change_i_p_primary_to_secondary == '1'),
      TRUE,
      FALSE
    )
  ) 
dat <- dat %>% 
  mutate(
    p_o_change_severe_postpub = ifelse(
      (change_p_l_new_primary == '1' |
        change_p_l_primary_omitted == '1' |
        change_p_l_primary_from_secondary == '1' |
        change_p_l_primary_to_secondary == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_severe_anywithin = ifelse(
      p_o_change_severe_rec == TRUE |
        p_o_change_severe_postcomp == TRUE |
        p_o_change_severe_postpub == TRUE,
      TRUE,
      FALSE
    )
  )

## third, create variables that indicate whether trials have 'non-severe'
## outcome changes (i.e., adding or omitting primary outcomes, moving primary
## outcomes to secondary, or the other way round) in each of the study phases,
## and also between the latest registry entry and publication

## 'non-severe' changes: changes to measurement, metric or method of
## aggregation, or timing
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_c_rec = ifelse(
      (change_a_i_change_measurement == '1' | 
        change_a_i_change_aggregation == '1' |
        change_a_i_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_postcomp = ifelse(
      (change_i_p_change_measurement == '1' |
        change_i_p_change_aggregation == '1' |
        change_i_p_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_c_postpub = ifelse(
      (change_p_l_change_measurement == '1' |
        change_p_l_change_aggregation == '1' |
        change_p_l_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_anywithin = ifelse(
      p_o_change_nonsevere_c_rec == TRUE |
        p_o_change_nonsevere_c_postcomp == TRUE |
        p_o_change_nonsevere_c_postpub == TRUE,
      TRUE,
      FALSE
    )
  )

## 'non-severe' changes: additions or omissions to measurement, metric or method
## of aggregation, or timing
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_ao_rec = ifelse(
      (change_a_i_added_measurement == '1' |
        change_a_i_added_aggregation == '1'| 
        change_a_i_added_timing == '1' |
        change_a_i_omitted_measurement == '1' |
        change_a_i_omitted_aggregation == '1' |
        change_a_i_omitted_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_ao_postcomp = ifelse(
      (change_i_p_added_measurement == '1' |
        change_i_p_added_aggregation == '1'|
        change_i_p_added_timing == '1' |
        change_i_p_omitted_measurement == '1' |
        change_i_p_omitted_aggregation == '1' |
        change_i_p_omitted_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_ao_postpub = ifelse(
      (change_p_l_added_measurement == '1' |
        change_p_l_added_aggregation == '1'| 
        change_p_l_added_timing == '1' |
        change_p_l_omitted_measurement == '1' |
        change_p_l_omitted_aggregation == '1' |
        change_p_l_omitted_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_ao_anywithin = ifelse(
      p_o_change_nonsevere_ao_rec == TRUE |
        p_o_change_nonsevere_ao_postcomp == TRUE |
        p_o_change_nonsevere_ao_postpub == TRUE,
      TRUE,
      FALSE
    )
  )

## 'non-severe' changes: any
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_rec = ifelse(
      p_o_change_nonsevere_c_rec == TRUE | p_o_change_nonsevere_ao_rec == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_postcomp = ifelse(
      p_o_change_nonsevere_c_postcomp == TRUE | p_o_change_nonsevere_ao_postcomp == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_postpub = ifelse(
      p_o_change_nonsevere_c_postpub == TRUE | p_o_change_nonsevere_ao_postpub == TRUE,
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_anywithin = ifelse(
      p_o_change_nonsevere_rec == TRUE |
        p_o_change_nonsevere_postcomp == TRUE |
        p_o_change_nonsevere_postpub == TRUE,
      TRUE,
      FALSE
    )
  )

## fourth, create a variable that indicates whether the respective study phase
## did not exist in a trial
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)
dat <- dat %>%
  mutate(
    no_phase_rec = ifelse(
      has_recruitment_phase == FALSE | change_a_i_no_phase == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_phase_postcomp = ifelse(
      has_post_completion_phase == FALSE | change_i_p_no_phase == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_phase_postpub = ifelse(
      has_post_publication_phase == FALSE | change_p_l_no_phase == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_phase_anywithin = ifelse(
      no_phase_rec == TRUE | no_phase_postcomp == TRUE | no_phase_postpub == TRUE,
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
    no_change_rec = ifelse(
      p_outcome_changed_recruitment == FALSE | change_a_i_no_change == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_postcomp = ifelse(
      p_outcome_changed_postcompletion == FALSE | change_i_p_no_change == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_postpub = ifelse(
      (has_post_publication_phase == TRUE & p_outcome_changed_postpublication == FALSE) |
        change_p_l_no_change == '1',
      # this is a bit of a different definitions - why? because by default, in
      # script 4, we defined it as "no outcome change" if the post-publication
      # phase did not exist - but then, the categories were not mutually
      # exclusive anymore, which is why we make this to make it so again
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_anywithin = ifelse(
      no_change_rec == TRUE | no_change_postcomp == TRUE | no_change_postpub == TRUE,
      TRUE,
      FALSE
    )
  )

## calculate the relevant numbers and proportions

## any changes in any phase
n_any_any <- sum(dat$p_o_change_anywithin, na.rm = TRUE)
p_any_any <- sum(dat$p_o_change_anywithin, na.rm = TRUE)/nrow(dat)*100
## severe changes in any phase
n_severe_any <- sum(dat$p_o_change_severe_anywithin, na.rm = TRUE)
p_severe_any <- sum(dat$p_o_change_severe_anywithin, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (changes) in any phase
n_nonsevere_c_any <- sum(dat$p_o_change_nonsevere_c_anywithin, na.rm = TRUE)
p_nonsevere_c_any <- sum(dat$p_o_change_nonsevere_c_anywithin, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (additions or omissions) in any phase
n_nonsevere_ao_any <- sum(dat$p_o_change_nonsevere_ao_anywithin, na.rm = TRUE)
p_nonsevere_ao_any <- sum(dat$p_o_change_nonsevere_ao_anywithin, na.rm = TRUE)/nrow(dat)*100
## phase does not exist - any phase
n_no_phase_any <- sum(dat$no_phase_anywithin, na.rm = TRUE)
p_no_phase_any <- sum(dat$no_phase_anywithin, na.rm = TRUE)/nrow(dat)*100
## no changes in any phase
n_no_change_any <- sum(dat$no_change_anywithin, na.rm = TRUE)
p_no_change_any <- sum(dat$no_change_anywithin, na.rm = TRUE)/nrow(dat)*100

## any changes in recruitment phase
n_any_rec <- sum(dat$p_o_change_rec, na.rm = TRUE)
p_any_rec <- sum(dat$p_o_change_rec, na.rm = TRUE)/nrow(dat)*100
## severe changes in recruitment phase
n_severe_rec <- sum(dat$p_o_change_severe_rec, na.rm = TRUE)
p_severe_rec <- sum(dat$p_o_change_severe_rec, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (changes) in recruitment phase
n_nonsevere_c_rec <- sum(dat$p_o_change_nonsevere_c_rec, na.rm = TRUE)
p_nonsevere_c_rec <- sum(dat$p_o_change_nonsevere_c_rec, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (additions or omissions) in recruitment phase
n_nonsevere_ao_rec <- sum(dat$p_o_change_nonsevere_ao_rec, na.rm = TRUE)
p_nonsevere_ao_rec <- sum(dat$p_o_change_nonsevere_ao_rec, na.rm = TRUE)/nrow(dat)*100
## phase does not exist - recruitment phase
n_no_phase_rec <- sum(dat$no_phase_rec, na.rm = TRUE)
p_no_phase_rec <- sum(dat$no_phase_rec, na.rm = TRUE)/nrow(dat)*100
## no changes in recruitment phase
n_no_change_rec <- sum(dat$no_change_rec, na.rm = TRUE)
p_no_change_rec <- sum(dat$no_change_rec, na.rm = TRUE)/nrow(dat)*100

## any changes in post-completion phase
n_any_postcomp <- sum(dat$p_o_change_postcomp, na.rm = TRUE)
p_any_postcomp <- sum(dat$p_o_change_postcomp, na.rm = TRUE)/nrow(dat)*100
## severe changes in post-completion phase
n_severe_postcomp <- sum(dat$p_o_change_severe_postcomp, na.rm = TRUE)
p_severe_postcomp <- sum(dat$p_o_change_severe_postcomp, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (changes) in post-completion phase
n_nonsevere_c_postcomp <- sum(dat$p_o_change_nonsevere_c_postcomp, na.rm = TRUE)
p_nonsevere_c_postcomp <- sum(dat$p_o_change_nonsevere_c_postcomp, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (additions or omissions) in post-completion phase
n_nonsevere_ao_postcomp <- sum(dat$p_o_change_nonsevere_ao_postcomp, na.rm = TRUE)
p_nonsevere_ao_postcomp <- sum(dat$p_o_change_nonsevere_ao_postcomp, na.rm = TRUE)/nrow(dat)*100
## phase does not exist - post-completion phase
n_no_phase_postcomp <- sum(dat$no_phase_postcomp, na.rm = TRUE)
p_no_phase_postcomp <- sum(dat$no_phase_postcomp, na.rm = TRUE)/nrow(dat)*100
## no changes in post-completion phase
n_no_change_postcomp <- sum(dat$no_change_postcomp, na.rm = TRUE)
p_no_change_postcomp <- sum(dat$no_change_postcomp, na.rm = TRUE)/nrow(dat)*100

## any changes in post-publication phase
n_any_postpub <- sum(dat$p_o_change_postpub, na.rm = TRUE)
p_any_postpub <- sum(dat$p_o_change_postpub, na.rm = TRUE)/nrow(dat)*100
## severe changes in post-publication phase
n_severe_postpub <- sum(dat$p_o_change_severe_postpub, na.rm = TRUE)
p_severe_postpub <- sum(dat$p_o_change_severe_postpub, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (changes) in post-publication phase
n_nonsevere_c_postpub <- sum(dat$p_o_change_nonsevere_c_postpub, na.rm = TRUE)
p_nonsevere_c_postpub <- sum(dat$p_o_change_nonsevere_c_postpub, na.rm = TRUE)/nrow(dat)*100
## non-severe changes (additions or omissions) in post-publication phase
n_nonsevere_ao_postpub <- sum(dat$p_o_change_nonsevere_ao_postpub, na.rm = TRUE)
p_nonsevere_ao_postpub <- sum(dat$p_o_change_nonsevere_ao_postpub, na.rm = TRUE)/nrow(dat)*100
## phase does not exist - post-publication phase
n_no_phase_postpub <- sum(dat$no_phase_postpub, na.rm = TRUE)
p_no_phase_postpub <- sum(dat$no_phase_postpub, na.rm = TRUE)/nrow(dat)*100
## no changes in post-publication phase
n_no_change_postpub <- sum(dat$no_change_postpub, na.rm = TRUE)
p_no_change_postpub <- sum(dat$no_change_postpub, na.rm = TRUE)/nrow(dat)*100



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
  'recruitment',      'no change',            p_no_change_rec,
  
  'post-completion',  'severe change',        p_severe_postcomp,
  'post-completion',  'non-severe change',    p_nonsevere_c_postcomp,
  'post-completion',  'non-severe add/omm',   p_nonsevere_ao_postcomp,
  'post-completion',  'phase does not exist', p_no_phase_postcomp,
  'post-completion',  'no change',            p_no_change_postcomp,
  
  'post-publication', 'severe change',        p_severe_postpub,
  'post-publication', 'non-severe change',    p_nonsevere_c_postpub,
  'post-publication', 'non-severe add/omm',   p_nonsevere_ao_postpub,
  'post-publication', 'phase does not exist', p_no_phase_postpub,
  'post-publication', 'no change',            p_no_change_postpub
  
) %>%
  mutate(phase = factor(phase)) %>%
  mutate(
    phase = fct_relevel(phase, 'recruitment', 'post-completion', 'post-publication')
  ) %>%
  mutate(
    type = factor(type)
  ) %>%
  mutate(
    type = fct_relevel(
      type,
      'severe change',
      'non-severe change',
      'non-severe add/omm',
      'no change'
    )
  )

ggplot(dat_Figure2) +
  aes(
    x = phase,
    y = percentage,
    fill = type
  ) +
  geom_bar(position = 'stack', stat = 'identity')



## ---- RESEARCH QUESTION 2 ----

## Based on all registry changes in all RCTs in the dataset:
## We will assess the association of ‘within-registry’ outcome changes with key
## candidate predictors (listed below).
## Candidate predictors to be used in the exploratory logistic regression
## analysis include study phase, industry sponsorship, publication year, medical
## specialty, registry, multicenter trial. We will carefully justify the
## selection of variables and give precise definitions before starting our
## analysis. We will use descriptive statistics where appropriate.

## WAIT FOR THE RESPECTIVE DECISIONS

# model_RQ2 <- glm(
#   p_o_change_anywithin ~
#     phase_recoded +
#     main_sponsor +
#     registration_year +
#     registry,
#   family="binomial",
#   data = dat
# )



## ---- RESEARCH QUESTION 3 ----------------------------------------------------

## Based on publications: 
## We will determine the proportion of ‘classical’ outcome switching (i.e., if
## the primary outcome reported in the publication deviates from the one
## reported in the latest version of the preregistration before publication).

## create variables that indicate whether trials have outcome switches between
## registration and publication
dat <- dat %>%
    mutate(
      p_o_change_any_reg_pub = ifelse(
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
           pub_outcome_change_primary_omitted == '1'),
        TRUE,
        FALSE
      )
    )
dat <- dat %>% 
    mutate(
      p_o_change_severe_reg_pub = ifelse(
        (pub_outcome_change_new_primary == '1' |
           pub_outcome_change_primary_omitted == '1' |
           pub_outcome_change_primary_from_secondary == '1' |
           pub_outcome_change_primary_to_secondary == '1'),
        TRUE,
        FALSE
      )
    )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_c_reg_pub = ifelse(
      (pub_outcome_change_change_measurement == '1' |
         pub_outcome_change_change_aggregation == '1' | 
         pub_outcome_change_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_ao_reg_pub = ifelse(
      (pub_outcome_change_added_measurement == '1' |
         pub_outcome_change_added_aggregation == '1'|
         pub_outcome_change_added_timing == '1' |
         pub_outcome_change_omitted_measurement == '1' |
         pub_outcome_change_omitted_aggregation == '1' |
         pub_outcome_change_omitted_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_reg_pub = ifelse(
      p_o_change_nonsevere_c_reg_pub == TRUE | p_o_change_nonsevere_ao_reg_pub == TRUE,
      TRUE,
      FALSE
    )
  )

## calculate the respective proportions

## any changes between latest registry enty and publication
n_any_reg_pub <- sum(dat$p_o_change_any_reg_pub, na.rm = TRUE)
p_any_reg_pub <- sum(dat$p_o_change_any_reg_pub, na.rm = TRUE)/sum(dat$has_publication_rating)*100
## severe changes between latest registry enty and publication
n_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = TRUE)
p_severe_reg_pub <- sum(dat$p_o_change_severe_reg_pub, na.rm = TRUE)/sum(dat$has_publication_rating)*100
## non-severe changes (changes) between latest registry enty and publication
n_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = TRUE)
p_nonsevere_c_reg_pub <- sum(dat$p_o_change_nonsevere_c_reg_pub, na.rm = TRUE)/sum(dat$has_publication_rating)*100
## non-severe changes (additions or omissions) between latest registry enty and publication
n_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = TRUE)
p_nonsevere_ao_reg_pub <- sum(dat$p_o_change_nonsevere_ao_reg_pub, na.rm = TRUE)/sum(dat$has_publication_rating)*100
## no changes between latest registry enty and publication
n_no_change_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = TRUE)
p_no_change_reg_pub <- sum(dat$p_o_change_nonsevere_reg_pub, na.rm = TRUE)/sum(dat$has_publication_rating)*100



## ---- RESEARCH QUESTION 3 (Figure 3) -----------------------------------------

## OPEN QUESTION:
## See Figure 1 from the protocol - maybe we need a more thorough assessment
## of pathways throughout a study when doing the Upset plot?

## Figure 3:
## What are the pathways of changes that happen within the registry across study
## phases?
## As a more comprehensive assessment of when changes are made - especially
## keeping in mind the possibility of changes at multiple time points -, we do
## an Upset Plot for severe changes, for the published trials.

## transform links into list column of intersection sets
dat_pub <- dat %>%
  filter(has_publication_rating == TRUE) # why 302??
links <- dat_pub %>%
  select(id,
         p_o_change_severe_rec,
         p_o_change_severe_postcomp,
         p_o_change_severe_postpub, 
         p_o_change_severe_reg_pub
  ) %>%
  rename(
    "Start - Completion" = p_o_change_severe_rec,
    "Completion - Publication" = p_o_change_severe_postcomp,
    "Publication - Last" = p_o_change_severe_postpub, 
    "Registry - Publication" = p_o_change_severe_reg_pub
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

dat_Figure3a <- bind_rows(links, no_links)
rm(links, no_links)

Figure3a <- dat_Figure3a %>%
  ggplot(aes(x=links)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_upset() + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) + 
  theme(panel.background=element_rect(fill = "white", colour = "lightgrey"), 
        panel.grid.major=element_line(colour="lightgrey", linetype = "dotted"), 
        panel.grid.minor=element_line(colour="lightgrey", linetype = "dotted")) + 
  ylab("Proportion of trials") + 
  xlab('"Severe" outcome changes across multiple time points') #+
# ggtitle(label = "'Severe' primary outcome switches", subtitle = 'Sample of 300 registry entries and trial results publications')
Figure3a

ggsave("plot-upset-sample-severe.pdf",
       upsetplotSevere,
       scale = 1.25,
       width = 7,
       height = 5
)


## for less severe changes: 
# Transform links into list column of intersection sets
links <- dat_pub %>%
  select(id,
         p_o_change_nonsevere_rec,
         p_o_change_nonsevere_postcomp, 
         p_o_change_nonsevere_postpub,
         p_o_change_nonsevere_reg_pub
  ) %>%
  rename(
    "Start - Completion" = p_o_change_nonsevere_rec,
    "Completion - Publication" = p_o_change_nonsevere_postcomp,
    "Publication - Last" = p_o_change_nonsevere_postpub, 
    "Registry - Publication" = p_o_change_nonsevere_reg_pub
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

dat_Figure3b <- bind_rows(links, no_links)
rm(links, no_links)

Figure3b <- dat_Figure2b %>%
  ggplot(aes(x=links, colour = NULL)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_upset() + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) + 
  theme(panel.background=element_rect(fill = "white", colour = "lightgrey"), 
        panel.grid.major=element_line(colour="lightgrey", linetype = "dotted"), 
        panel.grid.minor=element_line(colour="lightgrey", linetype = "dotted")) + 
  ylab("Proportion of trials") + 
  xlab('"Less severe" outcome changes across multiple time points') # + 
# ggtitle(label = '"Less severe" primary outcome changes', subtitle = 'Sample of 300 registry entries and trial results publications')
Figure3b

ggsave("plot-upset-sample-lesssevere.pdf",
       upsetplotLesssevere,
       scale = 1.25,
       width = 7,
       height = 5
)

## for any changes: 
# Transform links into list column of intersection sets
links <-  dat_pub %>%
  select(id,
         p_o_change_rec,
         p_o_change_postcomp,
         p_o_change_postpub,
         p_o_change_any_reg_pub # change variable name?
  ) %>%
  rename(
    "Start - Completion" = p_o_change_rec,
    "Completion - Publication" = p_o_change_postcomp,
    "Publication - Last" = p_o_change_postpub, 
    "Registry - Publication" = p_o_change_any_reg_pub
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

dat_Figure3c <- bind_rows(links, no_links)
rm(links, no_links)

Figure3c <- dat_Figure3c %>%
  ggplot(aes(x=links, colour = NULL)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_upset() + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.8)) + 
  theme(panel.background=element_rect(fill = "white", colour = "lightgrey"), 
        panel.grid.major=element_line(colour="lightgrey", linetype = "dotted"), 
        panel.grid.minor=element_line(colour="lightgrey", linetype = "dotted")) + 
  ylab("Proportion of trials") + 
  xlab('Any outcome changes across multiple time points') # + 
# ggtitle(label = '"Less severe" primary outcome changes', subtitle = 'Sample of 300 registry entries and trial results publications')
Figure3c

ggsave("plot-upset-sample-any-change.pdf",
       upsetplotAnyChange,
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

# wait for respective decisions
# model_RQ4 <- glm(y ~ x1, 
#              family = "binomial", 
#              data = dat_pub)
# summary(model_RQ4)



## ---- RESEARCH QUESTION 5 ----------------------------------------------------

## Based on publications: 
## We will assess the association between ‘within-registry’ outcome switching
## and ‘classical’ outcome switching.

# wait for respective decisions
# model_RQ5 <- glm(pub_outcome_switch ~ within_outcome_switch, 
#              family = "binomial", 
#              data = dat_pub)
# summary(model_RQ5)



## ---- RESEARCH QUESTION 6 ----------------------------------------------------

## Based on publications: 
## We will determine the proportion of trials with transparent reporting of
## “within-registry” changes in the publication.

## See Research Question 7.



## ---- RESEARCH QUESTION 7 ----------------------------------------------------

## Based on publications: 
## We will determine the proportion of trials with transparent reporting of
## “classical” outcome changes in the publication.

## How many trials with severe within-registry changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_severe_anywithin = if_else(
      p_o_change_severe_anywithin == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )

n_reporting_severe_anywithin <- sum(dat$reporting_severe_anywithin, na.rm = TRUE)
p_reporting_severe_anywithin <- sum(dat$reporting_severe_anywithin, na.rm = TRUE)/sum(dat$p_o_change_severe_anywithin)*100

## How many trials with *any* within-registry changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_any_anywithin = if_else(
      p_o_change_anywithin == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )

n_reporting_any_anywithin <- sum(dat$reporting_any_anywithin, na.rm = TRUE)
p_reporting_any_anywithin <- sum(dat$reporting_any_anywithin, na.rm = TRUE)/sum(dat$p_o_change_anywithin)*100

## How many trials with severe registry-publication changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_any_anywithin = if_else(
      p_o_change_severe_reg_pub == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )

n_reporting_any_anywithin <- sum(dat$reporting_any_anywithin, na.rm = TRUE)
p_reporting_any_anywithin <- sum(dat$reporting_any_anywithin, na.rm = TRUE)/sum(dat$p_o_change_severe_reg_pub)*100

## How many trials with any registry-publication changes report changes?
dat_pub <- dat_pub %>%
  mutate(
    reporting_any_anywithin = if_else(
      p_o_change_any_reg_pub == TRUE & pub_outcome_reference_binary == "1",
      TRUE,
      FALSE
    )
  )

n_reporting_any_anywithin <- sum(dat$reporting_any_anywithin, na.rm = TRUE)
p_reporting_any_anywithin <- sum(dat$reporting_any_anywithin, na.rm = TRUE)/sum(dat$p_o_change_any_reg_pub)*100
