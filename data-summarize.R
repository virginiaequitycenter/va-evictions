# Aggregating eviction data by ZIP code and locality
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 07-21-2021

###############################################################################
######### RUNNING ALL SCRIPTS AT ONCE WITH `RUN-ALL.R` IS RECOMMENDED #########
###############################################################################
# Ensure that `civilcases` is the working directory                           #
# This code is to be run after data-load.R                                    #
#   - data-load.R will generate cases.csv and cases_residential_only.csv      #
#   - This script takes each of those files and generates by-ZIP and by-court #
#       summaries of the # eviction filings, # plaintiff judgments, and       #
#       # default judgments                                                   #
#   - Two kinds of summaries are exported:                                    #
#       - Long data, with years stacked: by_zip.csv, by_court.csv,            #
#           by_zip_residential_only.csv, and by_court_residential_only.csv    #
#       - Wide data, with years spread: by_court_wide.csv, and                #
#           by_court_residential_only_wide.csv                                #
#             - These wide files contain additional columns indicated         #
#                 % change year-to-year in cases, plaintiff judgments,        #
#                 and default judgments                                       #
#   - Further, an HTML is exported containing interactive data tables         #
#       of the wide data in which % changes >50% are highlighted              #
###############################################################################
###############################################################################
###############################################################################

# Libraries
library(stringi)
library(tidyverse)

# Checks
if (stri_detect(getwd(), regex = '(\\/civilcases$)') == F) {
  stop('civilcases is not the working directory')
}
if (any(stri_detect(dir(), regex = 'cases.csv')) == F | any(stri_detect(dir(), regex = 'cases_residential_only.csv')) == F) {
  stop('ensure that cases.csv and cases_residential_only.csv are in the working directory; did you run data-clean.R first?')
}
if (any(stri_detect(dir(), regex = 'data-summarize-flag-cells.Rmd')) == F) {
  stop('ensure that data-summarize-flag-cells.Rmd is in the working directory')
}

# Load data
cases <- read.csv('cases.csv')
cases_residential_only <- read.csv('cases_residential_only.csv')

# Summaries for all cases
by_court <- cases %>%
  group_by(filing_year, court_name) %>%
  summarize(cases = n(),
            plaintiff_judgments = sum(Judgment == 'Plaintiff'),
            default_judgments = sum(latest_hearing_result == 'Default Judgment')) %>%
  rename(court = court_name, year = filing_year) %>%
  ungroup()
by_zip <- cases %>%
  group_by(filing_year, def_1_zip) %>%
  summarize(cases = n(),
            plaintiff_judgments = sum(Judgment == 'Plaintiff'),
            default_judgments = sum(latest_hearing_result == 'Default Judgment')) %>%
  rename(ZIP = def_1_zip, year = filing_year) %>%
  ungroup()

# Summaries for residential only cases
by_court_residential_only <- cases_residential_only %>%
  group_by(filing_year, court_name) %>%
  summarize(cases = n(),
            plaintiff_judgments = sum(Judgment == 'Plaintiff'),
            default_judgments = sum(latest_hearing_result == 'Default Judgment')) %>%
  rename(court = court_name, year = filing_year) %>%
  ungroup()
by_zip_residential_only <- cases_residential_only %>%
  group_by(filing_year, def_1_zip) %>%
  summarize(cases = n(),
            plaintiff_judgments = sum(Judgment == 'Plaintiff'),
            default_judgments = sum(latest_hearing_result == 'Default Judgment')) %>%
  rename(ZIP = def_1_zip, year = filing_year) %>%
  ungroup()

# Convert by-court data from long to wide, filling in zeros for missing courts
widen <- function(x) {
  x <- pivot_wider(x, names_from = year,
                   values_from = c(cases, plaintiff_judgments, default_judgments),
                   names_glue  = '{.value}_{year}',
                   values_fill = 0)
  x
}
by_court_wide <- widen(by_court)
by_court_residential_only_wide <- widen(by_court_residential_only)

# %-change calculator
first_year <- min(stri_extract(colnames(by_court_wide), regex = '(\\d{4})'), na.rm = T)
last_year <- max(stri_extract(colnames(by_court_wide), regex = '(\\d{4})'), na.rm = T)
perc_change <- function(x, first_yr = first_year, last_yr = last_year) {
  yr_vec <- first_yr:last_yr
  for (i in 2:length(yr_vec)) {
    x[, paste0('cases_percent_change_', yr_vec[i-1], '.', yr_vec[i])] <-
      round(((x[, paste0('cases_', yr_vec[i])] - x[, paste0('cases_', yr_vec[i-1])]) /
               x[, paste0('cases_', yr_vec[i-1])]) * 100,
            digits = 2)
  }
  for (i in 2:length(yr_vec)) {
    x[, paste0('plaintiff_judgments_percent_change_', yr_vec[i-1], '.', yr_vec[i])] <-
      round(((x[, paste0('plaintiff_judgments_', yr_vec[i])] - x[, paste0('plaintiff_judgments_', yr_vec[i-1])]) /
               x[, paste0('plaintiff_judgments_', yr_vec[i-1])]) * 100,
            digits = 2)
  }
  for (i in 2:length(yr_vec)) {
    x[, paste0('default_judgments_percent_change_', yr_vec[i-1], '.', yr_vec[i])] <-
      round(((x[, paste0('default_judgments_', yr_vec[i])] - x[, paste0('default_judgments_', yr_vec[i-1])]) /
               x[, paste0('default_judgments_', yr_vec[i-1])]) * 100,
            digits = 2)
  }
  x
}
by_court_wide <- perc_change(by_court_wide)
by_court_residential_only_wide <- perc_change(by_court_residential_only_wide)

# Export long by-court and by-ZIP files
by_export <- append(c('by_zip', 'by_court'), paste0(c('by_zip', 'by_court'), '_residential_only'))
sapply(by_export, function(x) write.csv(eval(parse(text = x)), file = paste0(x, '.csv'), row.names = F))

# Export wide by-court files with yearly % change indicated
write.csv(by_court_wide, file = 'by_court_wide.csv', row.names = F)
write.csv(by_court_residential_only_wide, file = 'by_court_residential_only_wide.csv', row.names = F)

# Call Rmd and generate formatted, interactive table highlighting cells with large % change values
rmarkdown::render(input = 'data-summarize-flag-cells.Rmd', output_file = 'by_court_interactive.html')