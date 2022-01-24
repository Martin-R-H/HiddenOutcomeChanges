library(tidyverse)

## read in data
dat_history_all <- read_csv('data/combined_history_data_all.csv')
dat_IV_included <- read_csv('data/data_IntoValue_included.csv')

## assess frequency of outcome changes
dat_history_all <- dat_history_all %>%
  group_by(id) %>%
  mutate(number_outcome_changes = sum(primary_outcome_changed, na.rm = TRUE) - 1)

## assess number of primary outcomes
dat_history_all <- dat_history_all %>%
  group_by(id) %>%
  mutate(max_number_primary = max(primary_outcomes_number))

## summarise the data
dat_history_all_short <- dat_history_all %>%
  group_by(id) %>%
  slice_head()

table(dat_history_all_short$max_number_primary)
summary(dat_history_all_short$max_number_primary)
ggplot(dat_history_all_short, aes(max_number_primary)) + geom_histogram(binwidth = 1)

table(dat_history_all_short$number_outcome_changes)
summary(dat_history_all_short$number_outcome_changes)
ggplot(dat_history_all_short, aes(number_outcome_changes)) + geom_histogram(binwidth = 1)


## assess enrollment
summary(dat_IV_included$enrollment)
ggplot(dat_IV_included, aes(enrollment)) + geom_histogram(binwidth = 25) + xlim(0, 1500)

# how many trials are below 50?
dat_IV_included %>%
  nrow()
dat_IV_included %>%
  filter(enrollment < 50) %>%
  nrow()
