library(tidyverse)
library(clipr)
library(readxl)

## ---- Extraction using IV data----

## select the PMIDs from the IntoValue data
dat_IV_sample <- read_csv('sample_IntoValue.csv')

## feed the extracted PMIDs to OVID
ovid_search <- as.character(dat_IV_sample$pmid) %>%
  str_replace_na() %>%
  str_replace_all('NA', ' ') %>%
  str_squish() %>%
  str_subset('.+') %>%
  str_flatten(collapse = ' or ')

ovid_search <- paste0('(', ovid_search, ').ui')
write_clip(ovid_search)

## now take the PMIDs from the clipboard, and paste them to
## the OVID search field, as described in the file named
## 'rough-instructions-for-getting-pub-hist-from-ovid.docx'

## save the resulting Excel sheet to this working directory

## read the OVID dataset in and combine with the IntoValue
## dataset

dat_ovid <- read_excel('citation.xls')
range <- paste0('A2:G', nrow(dat_ovid))
dat_ovid <- read_excel('citation.xls', range = range) %>%
  select(UI, PH) %>%
  filter(UI %in% dat_IV_sample$pmid)

dat_IV_sample <- dat_IV_sample %>%
  left_join(dat_ovid, by = c('pmid' = 'UI')) %>%
  rename(pub_history = PH)

## for later use, we should consider splitting the publication
## history into separate entries for submission, revision, and
## publication

## save the file
dat_IV_sample %>%
  write_csv('sample_IntoValue.csv')

## ---- Extraction using an automated tool (deprecated) ----
## automatically extract PMIDs for every NCT or DRKS number
## (script by Murph Carlisle)

# library(xml2)
# library(httr)
# library(jsonlite)

## Must contain a valid Pubmed API key
## Hint: Make an NCBI account, go here:
## https://www.ncbi.nlm.nih.gov/account/settings/
# apikey <- readLines("api_key.txt")
# 
# input_filename <- "nctids.csv"
# 
# output_filename <- "nctid-pmid.csv"

## This means that the script will only start a new file if the file
## doesn't exist. So, if you have a bad connexion or something and it
## stops partway through, you can just run the script again and it
## will pick up where it left off
# if (!file.exists(output_filename)) {
#   
#   tribble(~nctid, ~si, ~tiab) %>%
#     write_csv(output_filename)
#   
# }
# 
# batchsize <- 100
# 
# download_pm_results <- function (apikey, nctid, querytype, batch_size) {
#   
#   out <- tryCatch({
#     
#     search_term <- paste0(
#       nctid,
#       querytype
#     )
#     
#     pubmed_search <- list(
#       api_key = apikey,
#       term = search_term,
#       retmax = batch_size,
#       db = "pubmed"
#     )
#     
#     res <- POST(
#       "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
#       body=pubmed_search,
#       encode="form"
#     )
#     
#     result <- read_xml(res)
#     
#     closeAllConnections()
#     
#     return(result)
#     
#   },
#   error=function(cond) {
#     message(
#       paste(
#         "Error:",
#         nctid,
#         cond
#       )
#     )
#     
#     return(NA)
#   },
#   warning=function(cond) {
#     message(
#       paste(
#         "Warning:",
#         nctid,
#         cond
#       )
#     )
#     
#     return(NA)
#   },
#   finally={
#   })
#   
#   return(out)
#   
# }
# 
# ## Remove duplicate NCT's
# 
# input <- read_csv(input_filename) %>%
#   group_by(nctid) %>%
#   slice_head()
# 
# while (sum (! input$nctid %in% read_csv(output_filename, col_types=cols())$nctid) > 0) {
#   
#   input$notchecked <- ! input$nctid %in% read_csv(output_filename, col_types=cols())$nctid
#   
#   to_check <- input %>%
#     filter(notchecked)
#   
#   nctid_to_check <- to_check$nctid[1]
#   
#   si.results <- download_pm_results(apikey, nctid_to_check, "[si]", batchsize)
#   tiab.results <- download_pm_results(apikey, nctid_to_check, "[tiab]", batchsize)
#   
#   si.found_pmids <- xml_find_all(
#     si.results,
#     "/eSearchResult/IdList/Id"
#   ) %>%
#     xml_text()
#   
#   tiab.found_pmids <- xml_find_all(
#     tiab.results,
#     "/eSearchResult/IdList/Id"
#   ) %>%
#     xml_text()
#   
#   if (length(si.found_pmids) > 0) {
#     si.pmids <- toJSON(si.found_pmids)
#   } else {
#     si.pmids <- NA
#   }
#   
#   if (length(tiab.found_pmids) > 0) {
#     tiab.pmids <- toJSON(tiab.found_pmids)
#   } else {
#     tiab.pmids <- NA
#   }
#   
#   tribble(
#     ~nctid,         ~si,      ~tiab,
#     nctid_to_check, si.pmids, tiab.pmids
#   ) %>%
#     write_csv(
#       output_filename,
#       append=TRUE,
#       col_names=FALSE
#     )
#   
#   denom <- input$nctid %>%
#     unique() %>%
#     length()
#   
#   numer <- read_csv(output_filename, col_types=cols())$nctid %>%
#     unique() %>%
#     length()
#   
#   message(
#     paste0(
#       format(100*numer/denom, digits=2),
#       "% done"
#     )
#   )
#   
# }
# 
# if (sum(! input$nctid %in% read_csv(output_filename, col_types=cols())$nctid) == 0) {
#   
#   message("All done!")
#   
#   matched <- read_csv(output_filename, col_types=cols())
#   
#   matched$pmid <- ifelse(
#     !is.na(matched$si),
#     matched$si,
#     matched$tiab
#   )
#   
#   matched %>%
#     write_csv(output_filename)
#   
#   matches <- matched %>%
#     filter(!is.na(pmid)) %>%
#     nrow()
#   
#   paste0(round(100*matches/nrow(input)), "% of the NCT numbers had corresponding PMIDs") %>%
#     message()
#   
# }

## feed the extracted PMIDs to OVID

# dat_pmid <- read_csv('nctid-pmid.csv') %>%
#   select(!c(si, tiab))
# 
# ovid_search <- as.character(dat_pmid$pmid) %>%
#   str_replace_na() %>%
#   str_replace_all('NA', ' ') %>%
#   str_replace_all('\\[', ' ') %>%
#   str_replace_all('\\]', ' ') %>%
#   str_replace_all(',', ' ') %>%
#   str_trim(side = 'both') %>%
#   str_squish() %>%
#   str_subset('.+') %>%
#   str_replace_all(' ', ' or ') %>%
#   str_flatten(collapse = ' or ')
# 
# ovid_search <- paste0('(', ovid_search, ').ui')
# write_clip(ovid_search)

## now take the PMIDs from the clipboard, and feed them to
## OVID, as described in the file named
## 'rough-instructions-for-getting-pub-hist-from-ovid.docx'

## save the resulting Excel sheet to this working directory

## read the OVID dataset in and combine with the IntoValue
## dataset

# dat_IV_sample <- read_csv('sample_IntoValue.csv')
# 
# dat_ovid2 <- read_excel('citation.xls')
# range2 <- paste0('A2:G', nrow(dat_ovid2))
# dat_ovid2 <- read_excel('citation.xls', range = range2) %>%
#   select(UI, PH) %>%
#   filter(UI %in% dat_IV_sample$pmid)
# 
# dat_IV_sample <- dat_IV_sample %>%
#   left_join(dat_ovid2, by = c('pmid' = 'UI'))

## save the new file
# dat_IV_sample %>%
#   write_csv('sample_IntoValue.csv')
