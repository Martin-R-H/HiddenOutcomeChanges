library(tidyverse)
library(RPostgreSQL)
library(jsonlite)


## read in the IntoValue data for our sample
dat_IV_included <- read_csv('data/data_IntoValue_included.csv')


## ---- Conditions: ClinicalTrials.gov ----

## retrieve the AACT database for ClinicalTrials.gov records
## (see https://aact.ctti-clinicaltrials.org/)

## load driver
drv <- dbDriver('PostgreSQL')

## establish connection to AACT database
## (last download: 24 January 2022)
con <- dbConnect(drv, dbname = 'aact', host = 'aact-db.ctti-clinicaltrials.org', port = 5432, user = 'martinholst', password = 'xawpuk-bufgij-qEghy9')

dat_aact <- dbGetQuery(con, 'SELECT nct_id, name FROM Conditions')
dat_aact_MeSH <- dbGetQuery(con, 'SELECT nct_id, mesh_term, mesh_type FROM Browse_Conditions')

dbDisconnect(con)

dat_aact_filtered <- dat_aact %>%
  filter(nct_id %in% dat_IV_included$id)

dat_aact_MeSH_filtered <- dat_aact_MeSH %>%
  filter(nct_id %in% dat_IV_included$id)

length(unique(dat_aact_filtered$nct_id))
# 1402
length(unique(dat_aact_MeSH_filtered$nct_id))
# 1160
# MeSH terms seem not to be available for all trials - we will go with just the conditions

## rename variables
dat_aact_filtered <- dat_aact_filtered %>%
  rename(id = nct_id, condition = name)

## create separate variables for health conditions
dat_aact_filtered <- dat_aact_filtered %>%
  group_by(id) %>%
  mutate(conditions_from_registry = paste(condition, collapse = ' ; ')) %>%
  slice_head() %>%
  select(id, conditions_from_registry)


## ---- Conditions: DRKS ----

## retrieve the DRKS data from IntoValue 2 (available on GitHub), which also
## contain the data for IntoValue1
## (last download: 24 January 2022)
dat_DRKS <- read_delim('https://raw.githubusercontent.com/quest-bih/IntoValue2/master/data/1_sample_generation/DRKS_downloaded.csv', delim = ';')
dat_DRKS <- dat_DRKS %>%
  select(c(
    drksId,
    indication.type0,
    indication.key0,
    indication.value0,
    indication.type1,
    indication.key1,
    indication.value1,
    indication.type2,
    indication.key2,
    indication.value2,
    indication.type3,
    indication.key3,
    indication.value3,
    indication.type4,
    indication.key4,
    indication.value4
  )
  )

## check for duplicates
nrow(dat_DRKS) == length(unique(dat_DRKS$drksId))

## filter for our inclusion criteria and remove duplicates
dat_DRKS_filtered <- dat_DRKS %>%
  filter(drksId %in% dat_IV_included$id) %>%
  rename(id = drksId)

## restructure the data by storing it as just one variable,
## in json format
dat_DRKS_filtered_t <- tibble(
  id = character(),
  conditions_from_registry = character()
)
for (z in 1:nrow(dat_DRKS_filtered)) {
  dat_temp <- tibble(id = as.character(dat_DRKS_filtered[z, 'id']), conditions_from_registry = as.character(toJSON(dat_DRKS_filtered[z, c(2:16)])))
  dat_DRKS_filtered_t <- bind_rows(dat_DRKS_filtered_t, dat_temp)
}


## ---- Conditions: Combine ClinicalTrials.gov and DRKS ----

dat_conditions <- bind_rows(dat_aact_filtered, dat_DRKS_filtered_t)

dat_IV_extended <- dat_IV_included %>%
  inner_join(dat_conditions, by = 'id')

## save the file
dat_IV_extended %>%
  write_csv('data/data_IntoValue_extended.csv')


## ---- Submission Dates ----

## in the past, we planned to extract submission dates for each publication, but refrained
## from doing that due to many missings in the data

## until commit #76b6580e09d1a8105e1b467c4c01b52c9feb5998 in the sampling_changes branch
## there was a file called 3_extract_publication_history.R which contained the code for
## this extraction using OVID, and also a file called api_key.txt, that was required 
