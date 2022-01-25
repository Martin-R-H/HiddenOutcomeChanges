library(tidyverse)

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
