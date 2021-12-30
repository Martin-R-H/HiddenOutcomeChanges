library(tidyverse)
library(xml2)
library(httr)
library(jsonlite)

## ----
## automatically extract PMIDs for every NCT or DRKS number
## (script by Murph Carlisle)

## Must contain a valid Pubmed API key
## Hint: Make an NCBI account, go here:
## https://www.ncbi.nlm.nih.gov/account/settings/
apikey <- readLines("api_key.txt")

input_filename <- "nctids.csv"

output_filename <- "nctid-pmid.csv"

## This means that the script will only start a new file if the file
## doesn't exist. So, if you have a bad connexion or something and it
## stops partway through, you can just run the script again and it
## will pick up where it left off
if (!file.exists(output_filename)) {

    tribble(~nctid, ~si, ~tiab) %>%
        write_csv(output_filename)

}

batchsize <- 100

download_pm_results <- function (apikey, nctid, querytype, batch_size) {

    out <- tryCatch({
        
        search_term <- paste0(
            nctid,
            querytype
        )

        pubmed_search <- list(
            api_key = apikey,
            term = search_term,
            retmax = batch_size,
            db = "pubmed"
        )

        res <- POST(
            "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
            body=pubmed_search,
            encode="form"
        )

        result <- read_xml(res)

        closeAllConnections()

        return(result)
        
    },
    error=function(cond) {
        message(
            paste(
                "Error:",
                nctid,
                cond
            )
        )

        return(NA)
    },
    warning=function(cond) {
        message(
            paste(
                "Warning:",
                nctid,
                cond
            )
        )

        return(NA)
    },
    finally={
    })

    return(out)

}

## Remove duplicate NCT's

input <- read_csv(input_filename) %>%
    group_by(nctid) %>%
    slice_head()

while (sum (! input$nctid %in% read_csv(output_filename, col_types=cols())$nctid) > 0) {

    input$notchecked <- ! input$nctid %in% read_csv(output_filename, col_types=cols())$nctid

    to_check <- input %>%
        filter(notchecked)

    nctid_to_check <- to_check$nctid[1]

    si.results <- download_pm_results(apikey, nctid_to_check, "[si]", batchsize)
    tiab.results <- download_pm_results(apikey, nctid_to_check, "[tiab]", batchsize)

    si.found_pmids <- xml_find_all(
        si.results,
        "/eSearchResult/IdList/Id"
    ) %>%
        xml_text()

    tiab.found_pmids <- xml_find_all(
        tiab.results,
        "/eSearchResult/IdList/Id"
    ) %>%
        xml_text()

    if (length(si.found_pmids) > 0) {
        si.pmids <- toJSON(si.found_pmids)
    } else {
        si.pmids <- NA
    }

    if (length(tiab.found_pmids) > 0) {
        tiab.pmids <- toJSON(tiab.found_pmids)
    } else {
        tiab.pmids <- NA
    }

    tribble(
        ~nctid,         ~si,      ~tiab,
        nctid_to_check, si.pmids, tiab.pmids
    ) %>%
        write_csv(
            output_filename,
            append=TRUE,
            col_names=FALSE
        )

    denom <- input$nctid %>%
        unique() %>%
        length()

    numer <- read_csv(output_filename, col_types=cols())$nctid %>%
                                                       unique() %>%
                                                       length()

    message(
        paste0(
            format(100*numer/denom, digits=2),
            "% done"
        )
    )

}

if (sum(! input$nctid %in% read_csv(output_filename, col_types=cols())$nctid) == 0) {

    message("All done!")
    
    matched <- read_csv(output_filename, col_types=cols())

    matched$pmid <- ifelse(
        !is.na(matched$si),
        matched$si,
        matched$tiab
    )

    matched %>%
        write_csv(output_filename)
    
    matches <- matched %>%
        filter(!is.na(pmid)) %>%
        nrow()
    
    paste0(round(100*matches/nrow(input)), "% of the NCT numbers had corresponding PMIDs") %>%
        message()

}

## ----
## feed the extracted PMIDs to OVID

dat_pmid <- read_csv('nctid-pmid.csv') %>%
  select(!c(si, tiab))

## now take the PMIDs from the 'pmid' column in dat_ovid,
## and feed them to OVID, as described in the file named
## 'rough-instructions-for-getting-pub-hist-from-ovid.docx'

## read the OVID dataset in and combine with the IntoValue
## dataset

dat_ovid <- ()

## TO DO TO DO TO DO
