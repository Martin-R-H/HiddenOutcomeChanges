

## the following functions creates a few new variables inside a dataframe 'x',
## all of which relate to outcome changes


## ANY OUTCOME CHANGES
## this function creates variables that indicate whether trials have any outcome
## changes in each of the study phases

recode_outcomes_any <- function(x) {
  mutate(
    x,
    
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
    ),
    
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
    ),
    
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
    ),
    
    p_o_change_anywithin = if_else(
      (p_o_change_rec == TRUE |
         p_o_change_postcomp == TRUE |
         p_o_change_postpub == TRUE),
      TRUE,
      FALSE
    ),
    
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
}


## MAJOR OUTCOME CHANGES
## this function creates variables that indicate whether trials have 'severe'
## outcome changes in each of the study phases

recode_outcomes_severe <- function(x) {
  mutate(
    x,
    
    p_o_change_severe_rec = case_when(
      (change_a_i_new_primary == '1' |
         change_a_i_primary_omitted == '1' |
         change_a_i_primary_from_secondary == '1' |
         change_a_i_primary_to_secondary == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_severe_postcomp = case_when(
      (change_i_p_new_primary == '1' |
         change_i_p_primary_omitted == '1' |
         change_i_p_primary_from_secondary == '1' |
         change_i_p_primary_to_secondary == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_severe_postpub = case_when(
      (change_p_l_new_primary == '1' |
         change_p_l_primary_omitted == '1' |
         change_p_l_primary_from_secondary == '1' |
         change_p_l_primary_to_secondary == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_severe_anywithin = if_else(
      p_o_change_severe_rec == TRUE |
        p_o_change_severe_postcomp == TRUE |
        p_o_change_severe_postpub == TRUE,
      TRUE,
      FALSE
    ),
    
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
}


## MINOR OUTCOME CHANGES 1
## this function creates variables that indicate whether trials have
## 'non-severe' outcome changes (changes to measurement, metric or method of
## aggregation, or timing) in each of the study phases

recode_outcomes_nonsevere_1 <- function(x) {
  
  mutate(
    
    x,
    
    p_o_change_nonsevere_c_rec = case_when(
      # p_o_change_severe_rec == FALSE &
      # this line of code would make the categories mutually exclusive
        (change_a_i_change_measurement == '1' |
           change_a_i_change_aggregation == '1' |
           change_a_i_change_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_nonsevere_c_postcomp = case_when(
      # p_o_change_severe_postcomp == FALSE &
      # this line of code would make the categories mutually exclusive
        (change_i_p_change_measurement == '1' |
           change_i_p_change_aggregation == '1' |
           change_i_p_change_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_nonsevere_c_postpub = case_when(
      # p_o_change_severe_postpub == FALSE &
      # this line of code would make the categories mutually exclusive
        (change_p_l_change_measurement == '1' |
           change_p_l_change_aggregation == '1' |
           change_p_l_change_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_nonsevere_c_anywithin = if_else(
      p_o_change_nonsevere_c_rec == TRUE |
        p_o_change_nonsevere_c_postcomp == TRUE |
        p_o_change_nonsevere_c_postpub == TRUE,
      TRUE,
      FALSE
    ),
    
    p_o_change_nonsevere_c_reg_pub = case_when(
      has_publication_rating == TRUE &
        # p_o_change_severe_reg_pub == FALSE &
        # this line of code would make the categories mutually exclusive
        (pub_outcome_change_change_measurement == '1' |
           pub_outcome_change_change_aggregation == '1' |
           pub_outcome_change_change_timing == '1') ~
        TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
    
  )
  
}


## MINOR OUTCOME CHANGES 2
## this function creates variables that indicate whether trials have
## 'non-severe' outcome changes (additions or omissions to measurement, metric
## or method of aggregation, or timing) in each of the study phases

recode_outcomes_nonsevere_2 <- function(x) {
  
  mutate(
    
    x,
    
    p_o_change_nonsevere_ao_rec = case_when(
      # p_o_change_severe_rec == FALSE & p_o_change_nonsevere_c_rec == FALSE &
      # this line of code would make the categories mutually exclusive
        (change_a_i_added_measurement == '1' |
           change_a_i_added_aggregation == '1'|
           change_a_i_added_timing == '1' |
           change_a_i_omitted_measurement == '1' |
           change_a_i_omitted_aggregation == '1' |
           change_a_i_omitted_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_nonsevere_ao_postcomp = case_when(
      # p_o_change_severe_postcomp == FALSE & p_o_change_nonsevere_c_postcomp == FALSE &
      # this line of code would make the categories mutually exclusive
        (change_i_p_added_measurement == '1' |
           change_i_p_added_aggregation == '1'|
           change_i_p_added_timing == '1' |
           change_i_p_omitted_measurement == '1' |
           change_i_p_omitted_aggregation == '1' |
           change_i_p_omitted_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_nonsevere_ao_postpub = case_when(
      # p_o_change_severe_postpub == FALSE & p_o_change_nonsevere_c_postpub == FALSE &
      # this line of code would make the categories mutually exclusive
        (change_p_l_added_measurement == '1' |
           change_p_l_added_aggregation == '1'|
           change_p_l_added_timing == '1' |
           change_p_l_omitted_measurement == '1' |
           change_p_l_omitted_aggregation == '1' |
           change_p_l_omitted_timing == '1') ~
        TRUE,
      TRUE ~ FALSE
    ),
    
    p_o_change_nonsevere_ao_anywithin = if_else(
      p_o_change_nonsevere_ao_rec == TRUE |
        p_o_change_nonsevere_ao_postcomp == TRUE |
        p_o_change_nonsevere_ao_postpub == TRUE,
      TRUE,
      FALSE
    ),
    
    p_o_change_nonsevere_ao_reg_pub = case_when(
      has_publication_rating == TRUE &
        # p_o_change_severe_reg_pub == FALSE & p_o_change_nonsevere_c_reg_pub == FALSE &
        # this line of code would make the categories mutually exclusive
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
  
}


## MINOR OUTCOME CHANGES 3
## this function creates variables that indicate whether trials have any
## 'non-severe' outcome changes in each of the study phases

recode_outcomes_nonsevere_3 <- function(x) {
  
  mutate(
    
    x,
    
    p_o_change_nonsevere_rec = if_else(
      p_o_change_nonsevere_c_rec == TRUE | p_o_change_nonsevere_ao_rec == TRUE,
      TRUE,
      FALSE
    ),
    
    p_o_change_nonsevere_postcomp = if_else(
      p_o_change_nonsevere_c_postcomp == TRUE | p_o_change_nonsevere_ao_postcomp == TRUE,
      TRUE,
      FALSE
    ),
    
    p_o_change_nonsevere_postpub = if_else(
      p_o_change_nonsevere_c_postpub == TRUE | p_o_change_nonsevere_ao_postpub == TRUE,
      TRUE,
      FALSE
    ),
    
    p_o_change_nonsevere_anywithin = if_else(
      p_o_change_nonsevere_rec == TRUE |
        p_o_change_nonsevere_postcomp == TRUE |
        p_o_change_nonsevere_postpub == TRUE,
      TRUE,
      FALSE
    ),
    
    p_o_change_nonsevere_reg_pub = case_when(
      has_publication_rating == TRUE &
        (p_o_change_nonsevere_c_reg_pub == TRUE |
           p_o_change_nonsevere_ao_reg_pub == TRUE) ~
        TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
    
  )
  
}


## PHASE DID NOT EXIST
## this function creates variables that indicate whether the respective study
## phase did not exist in a trial
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)

recode_outcomes_notexist <- function(x) {
  
  mutate(
    
    x,
    
    no_rec_phase = case_when(
      has_recruitment_phase == FALSE | change_a_i_no_phase == '1' ~ TRUE,
      TRUE ~ FALSE
    ),
    
    no_postcomp_phase = case_when(
      has_post_completion_phase == FALSE | change_i_p_no_phase == '1' ~ TRUE,
      TRUE ~ FALSE
    ),
    
    no_postpub_phase = case_when(
      has_post_publication_phase == FALSE | change_p_l_no_phase == '1' ~ TRUE,
      TRUE ~ FALSE
    ),
    
    no_anywithin_phase = if_else(
      no_rec_phase == TRUE & no_postcomp_phase == TRUE & no_postpub_phase == TRUE,
      TRUE,
      FALSE
    )
    
  )
  
}


## NO CHANGES
## this function creates variables that indicate whether there was no change in
## the respective study phase
## (here, we also count the trials that were never exported to Numbat, either
## because they did not have the phase, or no change at all)

recode_outcomes_nophase <- function(x) {
  
  mutate(
    
    x,
    
    no_change_rec = case_when(
      p_outcome_changed_recruitment == FALSE | change_a_i_no_change == '1' ~ TRUE,
      TRUE ~ FALSE
    ),
    
    no_change_postcomp = case_when(
      p_outcome_changed_postcompletion == FALSE | change_i_p_no_change == '1' ~ TRUE,
      TRUE ~ FALSE
    ),
    
    no_change_postpub = case_when(
      (has_post_publication_phase == TRUE & p_outcome_changed_postpublication == FALSE) |
        change_p_l_no_change == '1' ~ TRUE,
      # this is a bit of a different definition. why? because by default, in
      # script 4, we defined it as "no outcome change" if the post-publication
      # phase did not exist - but then, the categories were not mutually
      # exclusive anymore, which is why we make this to make it so again
      TRUE ~ FALSE
    ),
    
    no_change_anywithin = if_else(
      no_change_rec == TRUE & no_change_postcomp == TRUE & no_change_postpub == TRUE,
      TRUE,
      FALSE
    ),
    
    no_change_reg_pub = case_when(
      has_publication_rating == TRUE & pub_outcome_change_no_change == '1' ~ TRUE,
      has_publication_rating == TRUE ~ FALSE,
      TRUE ~ NA
    )
    
  )
  
}
