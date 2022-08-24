library(tidyverse)
library(testthat)



## ---- NOTES ----

## - See Figure 1 from the protocol - maybe we need a more thorough assessment
##   of pathways throughout a study.
## - WE WANTED TO EXTRACT JOURNAL FIELDS! ('We will therefore use journal
##   information, using a copy of the Scimago Journal & Country Rank database
##   (https://www.scimagojr.com/journalrank.php), which provides this
##   classification.')
## - When doing this (probably in script 6), we can also delete the columns that
##   are called points_to_results, as they are pointless now. (No TRUE in the
##   dataset anymore.)
## - We can also inlude some more sanity checks there, like this one:
##   (a) if the trial has no recriutment phase, this should be reflected in the
##       Numbat ratings
##       test_that(
##        'when has_recruitment_phase = TRUE then change_a_i_no_phase = 1 or NULL and when has_recruitment_phase = FALSE then change_a_i_no_phase = 0 or NULL',
##        expect
##      )
##  (b) the same applies to all other phases,



## ---- READ IN DATA ----

dat <- read_csv(
  'data/processed_history_data_analyses.csv',
  guess_max = 2000 # solves the parsing issue from script 7a
)



## ---- RESEARCH QUESTION 1 ----

## Protocol:
## "Based on all registry changes in all RCTs in the dataset, we will determine
## the proportion of ‘within-registry’ outcome changes in published clinical
## trials across key trial phases (active, inactive, published)."

## first, create variables that indicate whether trials have outcome switches in
## each of the study phases
dat <- dat %>%
  mutate(
    p_o_change_rec = if_else(
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
    p_o_change_postcomp= if_else(
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
    p_o_change_postpub= if_else(
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
    p_o_change_anywithin = if_else(
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
    p_o_change_severe_rec = if_else(
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
    p_o_change_severe_postcomp = if_else(
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
    p_o_change_severe_postpub = if_else(
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
    p_o_change_severe_anywithin = if_else(
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
    p_o_change_nonsevere_c_rec = if_else(
      (change_a_i_change_measurement == '1' | 
        change_a_i_change_aggregation == '1' |
        change_a_i_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    p_o_change_nonsevere_c_postcomp = if_else(
      (change_i_p_change_measurement == '1' |
        change_i_p_change_aggregation == '1' |
        change_i_p_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_c_postpub = if_else(
      (change_p_l_change_measurement == '1' |
        change_p_l_change_aggregation == '1' |
        change_p_l_change_timing == '1'),
      TRUE,
      FALSE
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

## 'non-severe' changes: additions or omissions to measurement, metric or method
## of aggregation, or timing
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_ao_rec = if_else(
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
    p_o_change_nonsevere_ao_postcomp = if_else(
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
    p_o_change_nonsevere_ao_postpub = if_else(
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
    p_o_change_nonsevere_ao_anywithin = if_else(
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

## fourth, create a variable that indicates whether the respective study phase
## did not exist in a trial
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)
dat <- dat %>%
  mutate(
    no_phase_rec = if_else(
      has_recruitment_phase == FALSE | change_a_i_no_phase == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_phase_postcomp = if_else(
      has_post_completion_phase == FALSE | change_i_p_no_phase == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_phase_postpub = if_else(
      has_post_publication_phase == FALSE | change_p_l_no_phase == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_phase_anywithin = if_else(
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
    no_change_rec = if_else(
      p_outcome_changed_recruitment == FALSE | change_a_i_no_change == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_postcomp = if_else(
      p_outcome_changed_postcompletion == FALSE | change_i_p_no_change == '1',
      TRUE,
      FALSE
    )
  )
dat <- dat %>%
  mutate(
    no_change_postpub = if_else(
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
    no_change_anywithin = if_else(
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



## ---- RESEARCH QUESTION 1 (Figure 2) ----

## Figure 2: 
## What are the changes that happen within the registry across study phases?
## --> stacked bar chart for change types across time points 
## --> perhaps add the percentage values below the plot, like in ASCERTAIN JF
##     presentation 

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



## ---- RESEARCH QUESTION 3 ----

## Based on publications: 
## We will determine the proportion of ‘classical’ outcome switching (i.e., if
## the primary outcome reported in the publication deviates from the one
## reported in the latest version of the preregistration before publication).
## (We expect at least 30% of trials to exhibit this form of ‘classical’ outcome
## switching, based on previous research.)

dat <- dat %>%
    mutate(
      outcome_change_any_reg_pub = if_else(
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
      p_o_change_severe_reg_pub = if_else(
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
    p_o_change_nonsevere_c_reg_pub = if_else(
      (pub_outcome_change_change_measurement == '1' |
         pub_outcome_change_change_aggregation == '1' | 
         pub_outcome_change_change_timing == '1'),
      TRUE,
      FALSE
    )
  )
dat <- dat %>% 
  mutate(
    p_o_change_nonsevere_ao_reg_pub = if_else(
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
    p_o_change_nonsevere_reg_pub = if_else(
      p_o_change_nonsevere_c_reg_pub == TRUE | p_o_change_nonsevere_ao_reg_pub == TRUE,
      TRUE,
      FALSE
    )
  )

## ---- RESEARCH QUESTION 4 ----

## Based on publications: 
## We will assess the association of ‘classical’ outcome switching with key
## candidate predictors (listed below).
## Listed below is: Candidate predictors to be used in the exploratory logistic
## regression analysis include study phase, industry sponsorship, publication
## year, medical specialty, registry, multicenter trial. We will carefully
## justify the selection of variables and give precise definitions before
## starting our analysis. We will use descriptive statistics where appropriate.



## ---- RESEARCH QUESTION 5 ----

## Based on publications: 
## We will assess the association between ‘within-registry’ outcome switching
## and ‘classical’ outcome switching.



## ---- RESEARCH QUESTION 6 ----

## Based on publications: 
## We will determine the proportion of trials with transparent reporting of
## “within-registry” changes in the publication.



## ---- RESEARCH QUESTION 7 ----

## Based on publications: 
## We will determine the proportion of trials with transparent reporting of
## “within-registry” changes in the publication.
## (We will perform an exploratory logistic regression analysis with the
## likelihood of outcome switching in general as the outcome.)






## ---- MARTIN'S CONFERENCE ANALYSES ----

# Fig. 2: When are changes made? What are the extreme cases of switching at multiple time points? 
#   --> UPSET PLOT for combinations of “any change” over time points
library(ggupset)
## for severe changes: 
# Transform links into list column of intersection sets
links <-
  dat_pub %>%
  select(id,
         severe_a_i,
         severe_i_p,
         severe_p_l, 
         severe_l_p #,
         # lessevere_a_i, 
         # lessevere_i_p, 
         # lessevere_p_l
  ) %>%
  rename(
    "Start - Completion" = severe_a_i,
    "Completion - Publication" = severe_i_p,
    "Publication - Last" = severe_p_l, 
    "Registry - Publication" = severe_l_p # ,
    # "Change to timing, measurement, aggregation; active phase" = lessevere_a_i, 
    # "Change to timing, measurement, aggregation; after completion" = lessevere_i_p, 
    # "Change to timing, measurement, aggregation; after publication" = lessevere_p_l
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

plotdata <- bind_rows(links, no_links)

upsetplotSevere <- plotdata %>%
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
upsetplotSevere

ggsave("plot-upset-sample-severe.pdf",
       upsetplotSevere,
       scale = 1.25,
       width = 7,
       height = 5
)


## for less severe changes: 
# Transform links into list column of intersection sets
links <-
  dat_pub %>%
  select(id,
         lessevere_a_i,
         lessevere_i_p,
         lessevere_p_l, 
         lessevere_l_p  
  ) %>%
  rename(
    "Start - Completion" = lessevere_a_i,
    "Completion - Publication" = lessevere_i_p,
    "Publication - Last" = lessevere_p_l, 
    "Registry - Publication" = lessevere_l_p # ,
    # "Change to timing, measurement, aggregation; active phase" = lessevere_a_i, 
    # "Change to timing, measurement, aggregation; after completion" = lessevere_i_p, 
    # "Change to timing, measurement, aggregation; after publication" = lessevere_p_l
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

plotdata <- bind_rows(links, no_links)

upsetplotLesssevere <- plotdata %>%
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
upsetplotLesssevere

ggsave("plot-upset-sample-lesssevere.pdf",
       upsetplotLesssevere,
       scale = 1.25,
       width = 7,
       height = 5
)

## for any changes: 
# Transform links into list column of intersection sets

dat_pub <- dat_pub %>% 
  mutate(any_change_a_i = severe_a_i == 1 | lessevere_a_i == 1, 
         any_change_i_p = severe_i_p == 1 | lessevere_i_p == 1, 
         any_change_p_l = severe_p_l == 1 | lessevere_p_l == 1, 
         any_change_l_p = severe_l_p == 1 | lessevere_l_p == 1)

links <-
  dat_pub %>%
  select(id,
         any_change_a_i,
         any_change_i_p,
         any_change_p_l, 
         any_change_l_p  
  ) %>%
  rename(
    "Start - Completion" = any_change_a_i,
    "Completion - Publication" = any_change_i_p,
    "Publication - Last" = any_change_p_l, 
    "Registry - Publication" = any_change_l_p # ,
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

plotdata <- bind_rows(links, no_links)

upsetplotAnyChange <- plotdata %>%
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
upsetplotAnyChange

ggsave("plot-upset-sample-any-change.pdf",
       upsetplotAnyChange,
       scale = 1.25,
       width = 7,
       height = 5
)

# get the numbers of the outcome changes that are not captured by reg-pub-screening alone
# for any change
test <- (dat_pub$any_change_a_i | dat_pub$any_change_i_p | dat_pub$any_change_p_l) & !(dat_pub$any_change_l_p) 
sum(test, na.rm = T) / 300 * 100 # percent of sample 

# for severe change
test1 <- (dat_pub$severe_a_i | dat_pub$severe_i_p | dat_pub$severe_p_l) & !(dat_pub$severe_l_p) 
sum(test1, na.rm = T) / 300 * 100 # percent of sample 



# Fig. 3: What is the total difference in the # of trials with changes between the two approaches 
# --> STACKED BAR CHART for change types in-history and registry-publication
# --> show comparison of within-history approach to registry-publication approach

# this is also apparent from the upset plot (with some explanation necessary in the fig. caption)
# would no longer do this 

# Statistical analyses:  

# Candidate predictors to be used in the exploratory logistic regression analysis include 
# study phase, industry sponsorship, publication year, medical specialty, registry,
# multicentre trial. We will carefully justify the selection of variables and give precise
# definitions before starting our analysis.We will use descriptive statistics where appropriate.

# summarize phases before doing the analysis: 
# table(dat$phase)
dat$phase_new <- NA
dat$phase_new[dat$phase %in% c("Early Phase 1", "I", "Phase 1")] <- "Phase 1"
dat$phase_new[dat$phase %in% c("II", "IIa", "IIb", "Phase 1/Phase 2", "Phase 2")] <- "Phase 2"
dat$phase_new[dat$phase %in% c("II-III", "III", "IIIb", "Phase 2/Phase 3", "Phase 3")] <- "Phase 3"
dat$phase_new[dat$phase %in% c("IV", "Phase 4")] <- "Phase 4"
# sum(!is.na(dat$phase_new)) == sum(!is.na(dat$phase))  ## test

# make pub.year
dat$registration_year <- dat$first_reg_date %>% lubridate::year()
dat$publication_year <- dat$publication_date %>% lubridate::year()

model <- glm(within_outcome_switch~phase_new+main_sponsor+registration_year+registry, 
             family="binomial", 
             data=dat)
summary(model)


##  
# other analysis: does a within-reg change (negatively) predict a later publication change? 
model <- glm(pub_outcome_switch~within_outcome_switch, 
             family = "binomial", 
             data=dat_pub)
summary(model)

# numbers for table

dat$within_outcome_switch_rec %>% sum(na.rm = T) / nrow(dat) * 100 # num of any in active
dat$within_outcome_switch_postcomp %>% sum(na.rm = T) / nrow(dat) * 100 # num of any in inactive
dat$within_outcome_switch_postpub %>% sum(na.rm = T) / nrow(dat) * 100 # num of any in published
dat_pub$any_change_l_p %>% sum(na.rm = T) / nrow(dat_pub) * 100 # num of any in publication

dat$severe_a_i %>% sum(na.rm = T) / nrow(dat) * 100 # num of severe in active
dat$severe_i_p %>% sum(na.rm = T) / nrow(dat) * 100 # num of severe in inactive
dat$severe_p_l %>% sum(na.rm = T) / nrow(dat) * 100 # num of severe in published
dat_pub$severe_l_p %>% sum(na.rm = T) / nrow(dat_pub) * 100 # num of severe in publication

dat$changes_a_i %>% sum(na.rm = T) / nrow(dat) * 100 # num of changes to x in active
dat$changes_i_p %>% sum(na.rm = T) / nrow(dat) * 100 # num of changes to x in inactive
dat$changes_p_l %>% sum(na.rm = T) / nrow(dat) * 100 # num of changes to x in published
dat_pub$changes_l_p %>% sum(na.rm = T) / nrow(dat_pub) * 100 # num of changes to x in publication

dat$additions_a_i %>% sum(na.rm = T) / nrow(dat) * 100 # num of additions to x in active
dat$additions_i_p %>% sum(na.rm = T) / nrow(dat) * 100 # num of additions to x in inactive
dat$additions_p_l %>% sum(na.rm = T) / nrow(dat) * 100 # num of additions to x in published
dat_pub$additions_l_p %>% sum(na.rm = T) / nrow(dat_pub) * 100 # num of additions to x in publication



# reporting of changes to primary outcomes 
table(dat_pub$pub_outcome_reference_binary == "1")
4/sum(dat_pub$any_change_l_p)*100

table(dat_pub$pub_outcome_reference_binary == "1" & (dat_pub$severe_p_l == TRUE |
                                                       dat_pub$severe_a_i == TRUE | 
                                                       dat_pub$severe_i_p == TRUE | 
                                                       dat_pub$severe_l_p == TRUE))
table((dat_pub$severe_p_l == TRUE |
         dat_pub$severe_a_i == TRUE | 
         dat_pub$severe_i_p == TRUE | 
         dat_pub$severe_l_p == TRUE))





## ---- FROM 6_pilot_analyses.R script! ----


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
