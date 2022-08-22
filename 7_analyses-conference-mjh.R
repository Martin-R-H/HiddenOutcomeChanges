# analyses for ascertain

library(tidyverse)

histories <- read_csv("data/processed_history_data_analyses.csv") %>% # this gives one parsing failure (row 1092)
  select(-c("pub_outcome_change_new_primary", 
            "pub_outcome_change_primary_from_secondary", 
            "pub_outcome_change_change_measurement", 
            "pub_outcome_change_change_aggregation", 
            "pub_outcome_change_change_timing", 
            "pub_outcome_change_added_measurement", 
            "pub_outcome_change_added_aggregation" , 
            "pub_outcome_change_added_timing", 
            "pub_outcome_change_omitted_measurement", 
            "pub_outcome_change_omitted_aggregation", 
            "pub_outcome_change_omitted_timing", 
            "pub_outcome_change_primary_to_secondary", 
            "pub_outcome_change_primary_omitted", 
            "pub_outcome_change_no_change", 
            "pub_outcome_reference_binary", 
            "pub_outcome_reference", 
            "pub_outcome_time", 
            "pub_sig_outcome", 
            "pub_sig_comment", 
            "pub_sig_pvalue", 
            "reviewer_comment", 
            "pub_outcome_phrasing", 
            "outcome_determined"))
str(histories)
publications275 <- read_tsv("data/2022-08-10_publication_ratings.tsv") %>% 
  rename("id"="trial_id") %>% # rename to be able to left_join with histories based on id
  select(-c("doi", 
            "pmid", 
            "url", 
            "trial_phase_final", 
            "latest_version_date", 
            "results_posted", 
            "results_posted_date", 
            "p_outcome_final",
            "s_outcome_final", 
            "referenceid", 
            "timestamp_started", 
            "timestamp_finished"))
str(publications275)
publications25 <- read_tsv("data/2022-08-12_publication-ratings-pilot.tsv") %>% 
  rename("id"="trial_id") %>% # rename to be able to left_join with histories based on id
  select(-c("doi", 
            "pmid", 
            "url", 
            "trial_phase_final", 
            "latest_version_date", 
            "results_posted", 
            "results_posted_date", 
            "p_outcome_final",
            "s_outcome_final", 
            "referenceid", 
            "timestamp_started", 
            "timestamp_finished"))
str(publications25)
publications <- rbind(publications275, 
                      publications25)

## combine the two datasets by registry ids to get to the final dataset
publications$id %in% histories$id %>% table() # test if all ids match 
dat <- histories %>% 
  left_join(publications, by = "id") # %>% 
  # rename("change_l_pub_new_primary" = "pub_outcome_change_new_primary", 
  #        "change_l_pub_primary_from_secondary" = "pub_outcome_change_primary_from_secondary", 
  #        "change_l_pub_change_measurement" = "pub_outcome_change_change_measurement", 
  #        "change_l_pub_change_aggregation" = "pub_outcome_change_change_aggregation", 
  #        "change_l_pub_change_timing" = "pub_outcome_change_change_timing", 
  #        "change_l_pub_added_measurement" = "pub_outcome_change_added_measurement", 
  #        "change_l_pub_added_aggregation" = "pub_outcome_change_added_aggregation", 
  #        "change_l_pub_added_timing" = "pub_outcome_change_added_timing", 
  #        "change_l_pub_omitted_measurement" = "pub_outcome_change_omitted_measurement", 
  #        "change_l_pub_omitted_aggregation" = "pub_outcome_change_omitted_aggregation",    
  #        "change_l_pub_omitted_timing" = "pub_outcome_change_omitted_timing", 
  #        "change_l_pub_primary_to_secondary" = "pub_outcome_change_primary_to_secondary", 
  #        "change_l_pub_primary_omitted" = "pub_outcome_change_primary_omitted", 
  #        "change_l_pub_no_change" = "pub_outcome_change_no_change",
  #        "pub_outcome_reference_binary" = "pub_outcome_reference_binary", # maybe change the last two if needed 
  #        "pub_outcome_reference" = "pub_outcome_reference") 

# remove the files that aren't needed anymore 
rm(histories, publications25, publications275)

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

dat <- dat %>%
  mutate(
    pub_outcome_switch= if_else(
      (
        pub_outcome_change_new_primary == '1' |
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
          pub_outcome_change_primary_omitted == '1'
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
# should be 0 - check: 
dat$points_to_results %>% table



## take a look at some numbers
# how many, and what proportion of, trials have any within-registry change? 
sum(dat$within_outcome_switch, na.rm = T)
sum(dat$within_outcome_switch, na.rm = T)/nrow(dat)
# 60 trials with changes, 94 trials without changes - plus the ones we never exported to numbat of course

# what proportion of trials have a change in the active phase? 
sum(dat$within_outcome_switch_rec, na.rm = T)
sum(dat$within_outcome_switch_rec, na.rm = T)/nrow(dat)
# what proportion of trials have a change in the postcompletion phase? 
sum(dat$within_outcome_switch_postcomp, na.rm = T)
sum(dat$within_outcome_switch_postcomp, na.rm = T)/nrow(dat)
# what proportion of trials have a change in the postpublication phase? 
sum(dat$within_outcome_switch_postpub, na.rm = T)
sum(dat$within_outcome_switch_postpub, na.rm = T)/nrow(dat)


## "Severe" switches across timepoints
dat <- dat %>% 
  mutate(severe_a_i = ifelse((change_a_i_new_primary == 1| dat$change_a_i_primary_omitted == 1| 
                            dat$change_a_i_primary_from_secondary == 1 | dat$change_a_i_primary_to_secondary == 1), TRUE, FALSE)) 
dat <- dat %>% 
  mutate(severe_i_p = ifelse((change_i_p_new_primary == 1| dat$change_i_p_primary_omitted == 1| 
                            dat$change_i_p_primary_from_secondary == 1 | dat$change_i_p_primary_to_secondary == 1), TRUE, FALSE)) 
dat <- dat %>% 
  mutate(severe_p_l = ifelse((change_p_l_new_primary == 1| change_p_l_primary_omitted == 1| 
                                change_p_l_primary_from_secondary == 1 | change_p_l_primary_to_secondary == 1), TRUE, FALSE))
dat <- dat %>% 
  mutate(severe_l_p = ifelse((pub_outcome_change_new_primary == 1| pub_outcome_change_primary_omitted == 1| 
                                pub_outcome_change_primary_from_secondary == 1 | pub_outcome_change_primary_to_secondary == 1), TRUE, FALSE))


## "Less severe" switches -> changes 
dat <- dat %>% 
  mutate(changes_a_i = ifelse((change_a_i_change_measurement == 1| dat$change_a_i_change_aggregation == 1| 
                                dat$change_a_i_change_timing == 1), TRUE, FALSE))
dat <- dat %>% 
  mutate(changes_i_p = ifelse((change_i_p_change_measurement == 1| dat$change_i_p_change_aggregation == 1| 
                                dat$change_i_p_change_timing == 1), TRUE, FALSE))
dat <- dat %>% 
  mutate(changes_p_l = ifelse((change_p_l_change_measurement == 1| dat$change_p_l_change_aggregation == 1| 
                                 dat$change_p_l_change_timing == 1), TRUE, FALSE))
dat <- dat %>% 
  mutate(changes_l_p = ifelse((pub_outcome_change_change_measurement == 1| pub_outcome_change_change_aggregation == 1| 
                                 pub_outcome_change_change_timing == 1), TRUE, FALSE))


## "Less severe" switches -> additions/omissions
dat <- dat %>% 
  mutate(additions_a_i = ifelse((change_a_i_added_measurement == 1 | change_a_i_added_aggregation == 1| 
                                change_a_i_added_timing == 1 | change_a_i_omitted_measurement == 1 |
                                change_a_i_omitted_aggregation == 1 | change_a_i_omitted_timing == 1), TRUE, FALSE))
dat <- dat %>% 
  mutate(additions_i_p = ifelse((change_i_p_added_measurement == 1 | change_i_p_added_aggregation == 1| 
                                change_i_p_added_timing == 1 | change_i_p_omitted_measurement == 1 |
                                change_i_p_omitted_aggregation == 1 | change_i_p_omitted_timing == 1), TRUE, FALSE)) 
dat <- dat %>% 
  mutate(additions_p_l = ifelse((change_p_l_added_measurement == 1 | change_p_l_added_aggregation == 1| 
                                   change_p_l_added_timing == 1 | change_p_l_omitted_measurement == 1 |
                                   change_p_l_omitted_aggregation == 1 | change_p_l_omitted_timing == 1), TRUE, FALSE)) 
dat <- dat %>% 
  mutate(additions_l_p = ifelse((pub_outcome_change_added_measurement == 1 | pub_outcome_change_added_aggregation == 1| 
                                   pub_outcome_change_added_timing == 1 | pub_outcome_change_omitted_measurement == 1 |
                                   pub_outcome_change_omitted_aggregation == 1 | pub_outcome_change_omitted_timing == 1), TRUE, FALSE)) 


## combined for "less severe" switches
dat <- dat %>% 
  mutate(lessevere_a_i = ifelse((changes_a_i == TRUE | additions_a_i == TRUE), TRUE, FALSE))
dat <- dat %>% 
  mutate(lessevere_i_p = ifelse((changes_i_p == TRUE | additions_i_p == TRUE), TRUE, FALSE))
dat <- dat %>% 
  mutate(lessevere_p_l = ifelse((changes_p_l == TRUE | additions_p_l == TRUE), TRUE, FALSE))
dat <- dat %>% 
  mutate(lessevere_l_p = ifelse((changes_l_p == TRUE | additions_l_p == TRUE), TRUE, FALSE))


dat_pub <- dat %>% 
  filter(id %in% publications$id)

##############################
# Fig. 1: What are the changes that happen within the registry across study phases? 
#   --> STACKED BAR CHART for change types across time points 
#   --> add the percentage values below the plot, like in ASCERTAIN JF presentation 


# can we somehow get to a data structure that allows for the stacked bar plot using gather? or any other tidyR function?

# dat %>% 
#   gather(key = registration_phase,
#          value = entry,
#          c(within_outcome_switch_rec,
#            within_outcome_switch_postcomp,
#            within_outcome_switch_postpub,
#            pub_outcome_switch), 
#          na.rm = TRUE
#   ) %>% filter(
#     entry == TRUE
#   ) %>% 
#   mutate(
#     
#   )
#   ggplot() + 
#   geom_bar(aes(registration_phase, colour = change_a_i_added_aggregation == TRUE))

## --> i did not figure this out, will have to add later  

##############################
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



##############################
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

##############################
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
