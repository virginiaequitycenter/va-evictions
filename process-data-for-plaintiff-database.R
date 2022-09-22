# Aggregating case data up to plaintiff level for use in Shiny app
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 2022-04-04

###############################################################################
# This code takes eviction case data that has undergone plaintiff-name        #
# defuzzing (via 'defuzz-plaintiff-names.R') and aggregates it up to the      #
# plaintiff/ZIP level (plaintiffs within ZIPs within court jurisdictions).    #
# It produces a number of summary values that are displayed as part of the    #
# plaintiff-database Shiny app (e.g., number of # filings, count of cases     #
# excluding non-final serial cases, etc.)                                     #
###############################################################################

# WARNING: AS OF 2022-03-02, THIS CODE IS SET UP TO INGEST *PARTIALLY DEFUZZED,
# INCOMPLETE* CASE DATA. (SEE DATA FILE NAME BELOW.) ONCE DEFUZZING IS
# COMPLETE FOR ALL CASES/LOCALITIES, THE FILE NAME WILL NEED TO BE UPDATED.

# UPDATE: AS OF 2022-04-04 WE'VE ADDED BACK IN NON-DEFUZZED CASES FOR
# THREE MISSING LOCALITIES: RICHMOND, NORFOLK, NEWPORT NEWS.ONCE DEFUZZING IS
# COMPLETE FOR ALL CASES/LOCALITIES, THE FILE NAME WILL NEED TO BE UPDATED.

# Libraries
library(stringi)
library(tidyverse)
library(lubridate)

# Set data folder name (directory created in clean-eviction-data.R)
data_folder <- 'processed-data'
# data_file <- 'PARTIALLY-DEFUZZED-all-but-norfolk-newportnews-richmond.txt'
# data_file <- 'PARTIALLY-DEFUZZED-with-original-norfolk-newportnews-richmond.txt'
data_file <- "cases_residential_only.txt"

# Load
cases_to_aggregate <- read.csv(paste0(data_folder, '/', data_file), colClasses = 'character')

# Initial summarizing by plaintiff
plaintiff_aggregated <- cases_to_aggregate %>%
  # group_by(court_name, defuzzed_pla, pla_1_zip) %>% # skipped defuzzing step
  # group_by(court_name, pla_1, pla_1_zip) %>% # remove zip code grouping
  group_by(court_name, pla_1) %>% 
  summarize(cases_filed = n(),
            cases_filed_excluding_all_but_final_serial = sum(latest_filing_between_pla_and_def == T, na.rm = T),
            plaintiff_judgments = sum(Judgment == 'Plaintiff', na.rm = T),
            filing_years = paste0(unique(filing_year), collapse = ', '),
            def_zips = paste0(unique(def_1_zip), collapse = ', ')) %>% ungroup() %>% 
  relocate(filing_years, .after = last_col())

# Export for Shiny app
# write.csv(plaintiff_aggregated, file = paste0('plaintiff-database-Shiny/plaintiff-aggregated-data.txt'), row.names = F)
# until I get file structure aligned
write.csv(plaintiff_aggregated, file = 'plaintiff-database-Shiny/plaintiff-aggregated-data.txt', row.names = F)


# Monthly summarizing by plaintiff
plaintiff_aggregated_monthly <- cases_to_aggregate %>%
  # mutate(filing_quarter = quarter(FiledDate, type = "year.quarter")) %>% # change from quarter to month
  mutate(filing_month = paste0(year(FiledDate), "-", month(FiledDate)),
         filing_month = format(filing_month, format = "%Y-%m")) %>% 
# group_by(court_name, filing_quarter, defuzzed_pla, pla_1_zip) %>% # skipped defuzzing step
# group_by(court_name, filing_quarter, pla_1, pla_1_zip) %>% # remove zip code grouping
group_by(court_name, filing_month, pla_1) %>%
  summarize(cases_filed = n(),
            cases_filed_excluding_all_but_final_serial = sum(latest_filing_between_pla_and_def == T, na.rm = T),
            plaintiff_judgments = sum(Judgment == 'Plaintiff', na.rm = T),
            def_zips = paste0(unique(def_1_zip), collapse = ', ')) %>% ungroup() %>% 
  relocate(filing_month, .after = last_col())

# Export for Shiny app
write.csv(plaintiff_aggregated_monthly, file = 'plaintiff-database-Shiny/monthly-plaintiff-aggregated-data.txt', row.names = F)

# plaintiff_dat <- read.csv('va-evictions/plaintiff-database-Shiny/plaintiff-aggregated-data.txt', colClasses = 'character')
# quarterly_plaintiff_dat <- read.csv('va-evictions/plaintiff-database-Shiny/quarterly-plaintiff-aggregated-data.txt', colClasses = 'character')

