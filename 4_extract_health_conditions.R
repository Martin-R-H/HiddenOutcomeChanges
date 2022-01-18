library(tidyverse)
library(RPostgreSQL)



## ---- OLD STRATEGY: sample of 25 ----

## read in the IntoValue data for our sample
dat_IV_sample <- read_csv('data/sample_IntoValue.csv')


## ---- ClinicalTrials.gov ----

## retrieve the AACT database for ClinicalTrials.gov records
## (see https://aact.ctti-clinicaltrials.org/)

## load driver
drv <- dbDriver('PostgreSQL')

## establish connection to AACT database
con <- dbConnect(drv, dbname = 'aact', host = 'aact-db.ctti-clinicaltrials.org', port = 5432, user = 'martinholst', password = 'xawpuk-bufgij-qEghy9')

dat_aact <- dbGetQuery(con, 'SELECT nct_id, name FROM Conditions')
dat_aact2 <- dbGetQuery(con, 'SELECT nct_id, mesh_term, mesh_type FROM Browse_Conditions')

dbDisconnect(con)

dat_aact_filtered <- dat_aact %>%
  filter(nct_id %in% dat_IV_sample$id)

dat_aact2_filtered <- dat_aact2 %>%
  filter(nct_id %in% dat_IV_sample$id)

length(unique(dat_aact_filtered$nct_id))
# 20
length(unique(dat_aact2_filtered$nct_id))
# 17
# MeSH terms seem not to be available for all trials - we should go with just the conditions

## save the files
dat_aact_filtered %>%
  write_csv('data/aact_health_conditions.csv')
dat_aact2_filtered %>%
  write_csv('data/aact_health_conditions_MeSH.csv')

## ---- DRKS ----

## retrieve the DRKS data from IntoValue 1 (available on OSF)
dat_DRKS_IV1 <- read_delim('https://osf.io/smahx/download', delim = ';')
dat_DRKS_IV1 <- dat_DRKS_IV1 %>%
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

## retrieve the DRKS data from IntoValue 2 (available on GitHub)
dat_DRKS_IV2 <- read_delim('https://raw.githubusercontent.com/quest-bih/IntoValue2/master/data/1_sample_generation/DRKS_downloaded.csv', delim = ';')
dat_DRKS_IV2 <- dat_DRKS_IV2 %>%
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

## combine the DRKS datasets
dat_DRKS <- bind_rows(dat_DRKS_IV1, dat_DRKS_IV2)

## check whether the row numbers add up
nrow(dat_DRKS) == nrow(dat_DRKS_IV1) + nrow(dat_DRKS_IV2)

## save the file
dat_DRKS %>%
  write_csv('data/DRKS_health_conditions.csv')



## ---- NEW STRATEGY: run history scraper on all included trials ----

## read in the IntoValue data for our sample
dat_IV <- read_csv('data/data_IntoValue_included.csv')


## ---- ClinicalTrials.gov ----

## retrieve the AACT database for ClinicalTrials.gov records
## (see https://aact.ctti-clinicaltrials.org/)

## load driver
drv <- dbDriver('PostgreSQL')

## establish connection to AACT database
con <- dbConnect(drv, dbname = 'aact', host = 'aact-db.ctti-clinicaltrials.org', port = 5432, user = 'martinholst', password = 'xawpuk-bufgij-qEghy9')

dat_aact_all <- dbGetQuery(con, 'SELECT nct_id, name FROM Conditions')
dat_aact2_all <- dbGetQuery(con, 'SELECT nct_id, mesh_term, mesh_type FROM Browse_Conditions')

dbDisconnect(con)

dat_aact_filtered_all <- dat_aact_all %>%
  filter(nct_id %in% dat_IV$id)

dat_aact2_filtered_all <- dat_aact2_all %>%
  filter(nct_id %in% dat_IV$id)

length(unique(dat_aact_filtered_all$nct_id))
# 1402
length(unique(dat_aact2_filtered_all$nct_id))
# 1160
# MeSH terms seem not to be available for all trials - we should go with just the conditions

## save the files
dat_aact_filtered_all %>%
  write_csv('data/aact_health_conditions_all.csv')
dat_aact2_filtered_all %>%
  write_csv('data/aact_health_conditions_MeSH_all.csv')

## ---- DRKS ----

## retrieve the DRKS data from IntoValue 1 (available on OSF)
dat_DRKS_IV1 <- read_delim('https://osf.io/smahx/download', delim = ';')
dat_DRKS_IV1 <- dat_DRKS_IV1 %>%
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

## retrieve the DRKS data from IntoValue 2 (available on GitHub)
dat_DRKS_IV2 <- read_delim('https://raw.githubusercontent.com/quest-bih/IntoValue2/master/data/1_sample_generation/DRKS_downloaded.csv', delim = ';')
dat_DRKS_IV2 <- dat_DRKS_IV2 %>%
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

## combine the DRKS datasets
dat_DRKS <- bind_rows(dat_DRKS_IV1, dat_DRKS_IV2)

## check whether the row numbers add up
nrow(dat_DRKS) == nrow(dat_DRKS_IV1) + nrow(dat_DRKS_IV2)

## filter for our inclusion criteria
dat_DRKS_filtered <- dat_DRKS %>%
  filter(drksId %in% dat_IV$id)

## save the file
dat_DRKS %>%
  write_csv('data/DRKS_health_conditions_all.csv')



## restructure the data, but how? - talk to Murph and the others!
## its about how we structure the data, and which we extract
