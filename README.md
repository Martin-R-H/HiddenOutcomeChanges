# HiddenOutcomeChanges

In this project, which has been published as a [preprint on medRxiv](https://www.medrxiv.org/content/10.1101/2023.02.20.23286182v1), we aim to provide a thorough overview of changes to prespecified outcomes, from the first trial registration to the results publication.

We assess publicly available registry information (ClinicalTrials.gov, DRKS), including historical records, of a sample of clinical trials conducted in Germany between 2009 and 2017 (<https://github.com/maia-sh/intovalue-data>). Specifically, we determine the extent to which trials show discrepancies of outcomes in the registry entry (within-registration changes) and between the latest registry entry and the respective publication. We assess how changes to the outcome are reported in the respective publication.

The project has been preregistered on the [OSF](https://osf.io/t3qva). The [final dataset](https://osf.io/e2uct/) of this project, together with an [accompanying codebook](https://osf.io/werxf), have also been shared on the OSF.

The data folder contains the following datasets, which should allow you to reproduce our analyses:

| Filename                                     | Description                                                                      |
|----------------------------------------------|----------------------------------------------------------------------------------|
| data_IntoValue_included.csv                  | intermediate dataset containing IntoValue data                                   |
| data_IntoValue_extended.csv                  | intermediate dataset containing IntoValue data with some additions               |
| historical_versions_ct.csv                   | intermediate dataset containing downloads from ClinicalTrials.gov                |
| historical_versions_ct_2022-01-24.csv        | intermediate dataset containing downloads from ClinicalTrials.gov (old download) |
| historical_versions_DRKS.csv                 | intermediate dataset containing downloads from DRKS                              |
| combined_history_data.csv                    | intermediate dataset combining both downloads from ClinicalTrials.gov and DRKS   |
| processed_history_data_short.csv             | intermediate dataset with some data processing                                   |
| 2022-08-12_051025-form_3-refset_16-final.tsv | manual ratings from Numbat - part 1 (within-registry changes)                    |
| 2022-08-12_050830-form_4-refset_18-final.tsv | manual ratings from Numbat - part 2 (within-registry changes, addendum)          |
| 2022-10-11_073641-form_2-refset_9-final.tsv  | manual ratings from Numbat - part 3 (registry-publication changes, pilot)        |
| 2023-02-07_125805-form_3-refset_19-final.tsv | manual ratings from Numbat - part 4 (registry-publication changes)               |
| processed_history_data_analyses.csv          | combined dataset for analyses                                                    |
