##############################################################
# Summarize cleaned eviction data for Shiny app              #
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn      #
# GitHub: jacob-gg, mclaibourn                               #
# Last revised: 2023-02-16                                   #
##############################################################

######################## Instructions ########################
# This code takes cleaned eviction case data (processed via
# clean.R) and aggregates it up to the plaintiff, yearly,
# and monthly levels for display in the Virginia Evictors
# Catalog Shiny app     
# Modifiable user presets
input_data_directory <- 'processed-data'
input_data_file <- "cases_residential_only.txt"
output_data_directory <- 'va-evictors-catalog'
##############################################################

# Packages
required <- c('stringi', 'tidyverse', 'lubridate')
handle_package <- function(pkg) {
  if (grepl(x = pkg, pattern = '\\/')) { devtools::install_github(pkg) }
  else if (!(pkg %in% installed.packages())) { install.packages(pkg) }
  pkg <- sub(x = pkg, pattern = '.+\\/', replacement = '')
  library(pkg, character.only = TRUE)
}
lapply(required, function(x) handle_package(x))

# Load
cases <- read.csv(paste0(input_data_directory, '/', input_data_file), colClasses = 'character')

# Summarize by plaintiff
# Note that defendant_zips drops NA, so we report it as "Known Virginia Defendant ZIP Codes"
plaintiff_aggregated <- cases %>%
  group_by(county, plaintiff_name) %>% 
  summarize(cases_filed = n(),
            plaintiff_judgments = sum(judgment == 'Plaintiff', na.rm = TRUE),
            serial_filings = sum(serial_filing == TRUE, na.rm = TRUE),
            filing_years = paste0(sort(unique(as.numeric(filed_year))), collapse = ', '),
            defendant_zips = paste0(na.omit(unique(defendant_zip)), collapse = ', ')) %>%
  ungroup() %>% 
  relocate(filing_years, .after = last_col())

# Summarize by plaintiff and year
plaintiff_aggregated_yearly <- cases %>%
  group_by(county, filed_year, plaintiff_name) %>%
  summarize(cases_filed = n(),
            plaintiff_judgments = sum(judgment == 'Plaintiff', na.rm = TRUE),
            serial_filings = sum(serial_filing == TRUE, na.rm = TRUE),
            defendant_zips = paste0(na.omit(unique(defendant_zip)), collapse = ', ')) %>%
  ungroup() %>% 
  relocate(filed_year, .after = last_col())

# Summarize by plaintiff, month, and year
plaintiff_aggregated_monthly <- cases %>%
  mutate(filing_month = paste0(year(filed_date), "-", month(filed_date)),
         filing_month = ym(filing_month), 
         filing_month = format(filing_month, format = "%Y-%m")) %>% 
  group_by(county, filing_month, plaintiff_name) %>%
  summarize(cases_filed = n(),
            plaintiff_judgments = sum(judgment == 'Plaintiff', na.rm = TRUE),
            serial_filings = sum(serial_filing == TRUE, na.rm = TRUE),
            defendant_zips = paste0(na.omit(unique(defendant_zip)), collapse = ', ')) %>%
  ungroup() %>% 
  relocate(filing_month, .after = last_col())

# Export for Shiny app
write.csv(plaintiff_aggregated, file = paste0(output_data_directory, '/data-plaintiff-aggregated.txt'), row.names = FALSE)
write.csv(plaintiff_aggregated_yearly, file = paste0(output_data_directory, '/data-yearly-plaintiff-aggregated.txt'), row.names = FALSE)
write.csv(plaintiff_aggregated_monthly, file = paste0(output_data_directory, '/data-monthly-plaintiff-aggregated.txt'), row.names = FALSE)

