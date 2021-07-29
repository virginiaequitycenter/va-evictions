# Loading and cleaning Virginia eviction data from https://virginiacourtdata.org/
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 07-28-2021

###############################################################################
######### RUNNING ALL SCRIPTS AT ONCE WITH `RUN-ALL.R` IS RECOMMENDED #########
###############################################################################
# The only step in advance of running this code is ensuring that:             #
#   1. The `civilcases` directory contains (a) folders with court-data CSVs   #
#       for all of the years of interest and (b) this code file               #
#   2. The folder names for each year contain the string `DistrictCivil` and  #
#       contain the relevant year                                             #
#   3. The `data-non-residential-regex.R` file is in the `civilcases`         #
#       directory                                                             #
#   4. `civilcases` is the working directory                                  #
# With those conditions satisfied, the data cleaning process will be entirely #
#   automated, and the code will return a data frame called `cases.csv`       #
#   containing cleaned, aggregated cases for all years (with `year_filed` as  #
#   a year identifier), as well as a version of `cases.csv` excluding cases   #
#   we tag as having non-residential defendant (`cases_residential_only.csv`) #
# The code will also save a file named `cleaning_notes.txt` to the working    #
#   directory that contains information on how many true duplicates and       #
#   serial cases the code identified for each year, how many cases            #
#   were tagged as having non-residential defendants, and how many cases were #
#   flagged as not having valid VA ZIP codes listed for defendants            #
###############################################################################
###############################################################################
###############################################################################

# Libraries
library(stringi)
library(tidyverse)
library(readr)
library(rvest)
library(lubridate)

# Checks
if (stri_detect(getwd(), regex = '(\\/civilcases$)') == F) {
  stop('civilcases is not the working directory')
}
if ('data-non-residential-regex.R' %in% dir() == F) {
  stop('data-non-residential-regex.R is not in the working directory')
}

# Open file to save a few lines of relevant output (saved into `civilcases` directory)
sink(file = 'cleaning_notes.txt', type = 'output')

# Load raw district court civil case data
district_folders <- dir()[stri_detect(dir(), fixed = 'DistrictCivil')]
data_years <- stri_extract(district_folders, regex = '(\\d{4})')
for (i in 1:length(district_folders)) {
  year <- stri_extract(district_folders[i], regex = '(\\d{4})')
  assign(paste0('cases', year), read.csv(paste0(district_folders[i], '/Cases.csv')))
  assign(paste0('defendants', year), read.csv(paste0(district_folders[i], '/Defendants.csv')))
  assign(paste0('plaintiffs', year), read.csv(paste0(district_folders[i], '/Plaintiffs.csv')))
  assign(paste0('hearings', year), read.csv(paste0(district_folders[i], '/Hearings.csv')))
}

# Read in court names from Ben Schoenfeld's GitHub
district_courts <- read.csv('https://raw.githubusercontent.com/bschoenfeld/virginia-court-data-analysis/master/data/district_courts.csv')
colnames(district_courts)[which(colnames(district_courts) == 'name')] <- 'court_name'
# CANARY: Until this comment is removed, we have to manually add FIPS 710 as Norfolk General District Court
#   In 2017, Norfolk began condensing their case reporting from three separate courts (711, 712, 713) into one (710)
#   710 is how Norfolk cases are identified in our data
district_courts <- rbind(district_courts, c(710, 'Norfolk General District Court'))
district_courts$fips <- as.integer(district_courts$fips)
# Notes on courts:
#   - Unlawful detainer case in Fairfax *City* are held at the Fairfac *County* court, thus no cases linked to Fairfax City GDC appear in the unlawful detainer data, see: http://www.courts.state.va.us/courts/gd/fairfax_city/home.html
#   - "Richmond Manchester," "Mongomery/Blacksburg," and a few exclusively criminal and/or traffic courts appear in the list but not in the unlawful detainer data (the former two appear to not be real GDCs; the latter are not present for obvious reasons)

# Select only cases with `CaseType == 'Unlawful Detainer'` from cases20XX data
cases_objects <- paste0('cases', data_years)
for (i in 1:length(cases_objects)) {
  assign(cases_objects[i], filter(eval(parse(text = cases_objects[i])), CaseType == 'Unlawful Detainer'))
}

# Function to aggregate defendant, plaintiff, and hearing data by case_id
#   - First, select only those rows in the defendants/plaintiffs/hearings data sets with case_ids
#       contained in the cases20XX data frames (which now only contain unlawful detainer cases)
#   - Then, if there's >1 row for a given case_id, reflecting multiple defendants,
#       plaintiffs, or hearings, condense that data into one row
#   - In cases with >1 defendant and/or plaintiff, def_1_zip and pla_1_zip are determined by
#       extracting the first ZIP code in the set of defendant/plaintiff addresses
#   - Not all def_1_zip values are Virginia ZIP codes (e.g., an address for a defendant who moved
#       might be: "EWING, NJ 12345); later in the code, non-VA ZIPs are later converted to NA
#   - Not all defendant addresses are accompanied by ZIP codes; e.g., def_address = "HENRICO, VA"
#       - def_1_zip values for such cases are NA
#   - If there is >1 hearing associated with a given case, we keep data for the *latest* hearing
aggregator <- function(x, year, what) {
  x <- x[x$case_id %in% eval(parse(text = paste0(paste0('cases', year), '$id'))), ]
  if (what == 'defendants') {
    x <- x %>%
      group_by(case_id) %>%
      transmute(def = paste0(Name, collapse = ' | '),
                def_address = paste0(Address, collapse = ' | '),
                def_attorney = paste0(Attorney, collapse = ' | '),
                def_id = paste0(id, collapse = ' | '),
                def_count = stri_count(def, regex = '(\\|)') + 1,
                def_1 = ifelse(def_count > 1, stri_extract(def, regex = '(^.*?)(?= \\|)'), stri_extract(def, regex = '(^.*)')),
                def_1_zip = ifelse(def_count > 1, stri_extract(def_address, regex = '(\\d{5}?)(?= \\|)'), stri_extract(def_address, regex = '(\\d{5})'))) %>%
      ungroup()
  }
  if (what == 'plaintiffs') {
    x <- x %>%
      group_by(case_id) %>%
      transmute(pla = paste0(Name, collapse = ' | '),
                pla_address = paste0(Address, collapse = ' | '),
                pla_attorney = paste0(Attorney, collapse = ' | '),
                pla_id = paste0(id, collapse = ' | '),
                pla_count = stri_count(pla, regex = '(\\|)') + 1,
                pla_1 = ifelse(pla_count > 1, stri_extract(pla, regex = '(^.*?)(?= \\|)'), stri_extract(pla, regex = '(^.*)')),
                pla_1_zip = ifelse(pla_count > 1, stri_extract(pla_address, regex = '(\\d{5}?)(?= \\|)'), stri_extract(pla_address, regex = '(\\d{5})'))) %>%
      ungroup()
  }
  if (what == 'hearings') {
    x <- x %>%
      select(id, Date, Result, case_id) %>%
      group_by(case_id) %>%
      transmute(latest_hearing_id = id,
                latest_hearing_date = as.POSIXlt(Date, format = '%Y-%m-%d %H:%M:%S', tz = 'EST'),
                latest_hearing_result = Result) %>%
      filter(latest_hearing_date == max(latest_hearing_date)) %>%
      filter(latest_hearing_id == max(latest_hearing_id)) %>%
      ungroup()
  }
  distinct(x)
}

# Apply aggregator() function to defendants, plaintiffs, and hearings objects
court_dat_objects <- c(sapply(c('defendants', 'plaintiffs', 'hearings'), function(x) paste0(x, data_years)))
for (i in 1:length(court_dat_objects)) {
  assign(court_dat_objects[i], aggregator(x = eval(parse(text = court_dat_objects[i])),
                                          year = stri_extract(court_dat_objects[i], regex = '(\\d{4})'),
                                          what = stri_extract(court_dat_objects[i], regex = '(\\D+)')))
}

# Merge defendant, plaintiff, and hearing data with case data; merge cases with court names
for (i in 1:length(cases_objects)) {
  year <- stri_extract(cases_objects[i], regex = '(\\d{4})')
  assign(cases_objects[i], eval(parse(text = cases_objects[i])) %>%
           left_join(eval(parse(text = paste0('defendants', year))), by = c('id' = 'case_id')) %>%
           left_join(eval(parse(text = paste0('plaintiffs', year))), by = c('id' = 'case_id')) %>%
           left_join(eval(parse(text = paste0('hearings', year))), by = c('id' = 'case_id')) %>%
           left_join(district_courts, by = 'fips')
  )
}

# Assign quarters and years to cases
yearquarter_assigner <- function(x, yr) {
  x <- x %>%
    mutate(filing_year = yr,
           date_filed = as.Date(FiledDate, "%Y-%m-%d"),
           quarter = case_when(date_filed < as.Date(paste0(yr, '-04-01')) ~ 'Q1',
                               date_filed >= as.Date(paste0(yr, '-04-01')) & date_filed < as.Date(paste0(yr, '-07-01')) ~ 'Q2',
                               date_filed >= as.Date(paste0(yr, '-07-01')) & date_filed < as.Date(paste0(yr, '-10-01')) ~ 'Q3',
                               date_filed >= as.Date(paste0(yr, '-10-01')) ~ 'Q4'))
  x
}
for (i in 1:length(cases_objects)) {
  year <- stri_extract(cases_objects[i], regex = '(\\d{4})')
  assign(cases_objects[i], yearquarter_assigner(eval(parse(text = cases_objects[i])), year))
}

# Stack cases from all years on top of each other in one data frame
cases <- data.frame()
for (i in 1:length(cases_objects)) {
  x <- eval(parse(text = cases_objects[i]))
  cases <- rbind(cases, x)
}

# Clean ZIP codes
#   - Current approach: Don't drop any cases, as all cases have an associated VA
#       court that can be used in by-court case tabulation
#   - However: Convert def_1_zip to NA when it isn't a valid *Virginia* ZIP code
#       so as not to disrupt VA by-ZIP case tabulation
#       - For example: "Henrico, VA 00000," "Charlottesville, VA 99999," Ewing, NJ 12345"
# Scrape Virginia ZIP codes
va_zips <- read_html('https://mcdc.missouri.edu/applications/zipcodes/?state=51&place=') %>% ##### IMPROVE/CONFIRM LIST OF VALID VA ZIP CODES #####
  html_element(xpath = '/html/body/div/main/div/table') %>%
  html_table() %>%
  .$`ZIP/ZCTA`
zip_cleaner <- function(x) {
  pre_na_zips <- x %>% group_by(filing_year) %>% summarize(pre_na = sum(is.na(def_1_zip)))
  x$def_1_zip <- ifelse(x$def_1_zip %in% va_zips, x$def_1_zip, NA)
  post_na_zips <- x %>% group_by(filing_year) %>% summarize(post_na = sum(is.na(def_1_zip)))
  merge(pre_na_zips, post_na_zips, by = 'filing_year') %>%
    mutate(na_gain = post_na - pre_na) %>%
    apply(., 1, function(x) cat(paste0(x[1], ': ', x[length(x)], ' non-VA ZIPs identified and converted to NA', '\n')))
  x
}
cases <- zip_cleaner(cases)

# Identify and remove true duplicates
#   Variables used to identify true duplicates: “FiledDate”, “Judgment”, “Costs”, “AttorneyFees”, “PrincipalAmount", “OtherAmount”, "pla_1", "def_1", "def_1_zip"
deduplicater <- function(x) {
  pre_nrow <- x %>% group_by(filing_year) %>% summarize(pre_n = n())
  dupes <- x[, c('FiledDate', 'Judgment', 'Costs', 'AttorneyFees', 'PrincipalAmount', 'OtherAmount', 'pla_1', 'def_1', 'def_1_zip')]
  x <- x[!duplicated(dupes), ]
  post_nrow <- x %>% group_by(filing_year) %>% summarize(post_n = n())
  merge(pre_nrow, post_nrow, by = 'filing_year') %>%
    mutate(nrow_change = pre_n - post_n) %>%
    apply(., 1, function(x) cat(paste0(x[1], ': ', x[length(x)], ' true duplicates identified and removed', '\n')))
  x
}
cases <- deduplicater(cases)

# Identify and remove serial cases
#   Variables used to identify serial cases: "pla_1", "def_1", "def_1_zip"
# We define serial cases as: Sequential filings by a given plaintiff ("pla_1") against a given primary defendant ("def_1")
#   in a given ZIP code ("def_1_zip") within 12 months of an initial filing (note that this implies that there can be
#   multiple "groups" of serial filings for a given pla_1/def_1/def_1_zip combination)
# Within each group of serial cases, we retain the *latest one* and use that for tabulation purposes; this is because
#   we're concerned with the material effects of filings on tenants, and if there are three serial cases ending in
#   (1) dismisssal, (2) dismissal, (3) eviction, our position is that that sequence of events constitutes an eviction,
#   not a "final score" of 2-1
# deserializer_inner() is applied via a wrapper function, deserializer_outer() (see below) to a
#   tibble of cases grouped by pla_1, def_1, and def_1_zip (i.e., sets of rows where the same
#   plaintiff filed against the same defendant in the same ZIP code)
# A reasonable test of whether the deserialization is working properly is checking that the value obtained from:
#   nrow(cases %>% group_by(pla_1, def_1, def_1_zip) %>% summarize(n()))
#   is identical before and after running deserializer_inner()/deserializer_outer()
deserializer_inner <- function(z, ...) {
  # If there's only one case filed by the plaintiff against the defendant, simply keep that case
  if (nrow(z) == 1) {
    return(z)
  }
  # If there's >1 case filed by the plaintiff against the defendant...
  if (nrow(z) > 1) {
    # First: Determine whether the time span from the first case to the latest is >1 year
    cases_span_more_than_1_yr <- (interval(min(z$date_filed), max(z$date_filed)) / years(1)) > 1
    # If the interval is *not* >1 year, simply keep the latest case (because all cases selected are considered serial with the first)
    if (cases_span_more_than_1_yr == F) {
      z <- z %>%filter(date_filed == max(date_filed)) %>% filter(id == max(id))
      return(z)
    }
    # If the interval is >1 year...
    if (cases_span_more_than_1_yr == T) {
      for (i in 1:100) {
        # First determine the latest case filed that is not more than 12 months after the very first case (this is the
        #   latest in the first "group" of serial cases)
        if (i == 1) {
          latest_in_serial_group_1 <- max(z$date_filed[z$date_filed <= (min(z$date_filed) %m+% months(12))])
        }
        # Then, find the latest case in the next "group" of serial cases by determining the latest case
        #   that was filed not more than 12 months after the first case that follows the case identified
        #   as the latest in the previous serial "group"
        if (i > 1) {
          temp_min <- min(z$date_filed[z$date_filed > eval(parse(text = paste0('latest_in_serial_group_', i-1)))])
          assign(paste0('latest_in_serial_group_', i),
                 max(z$date_filed[z$date_filed >= temp_min & z$date_filed <= (temp_min %m+% months(12))]))
        }
        # Iterate this process until the case identified as the latest in a serial "group" is the most recent case
        #   filed by a given plaintiff against a given defendant in a given ZIP code
        if (eval(parse(text = paste0('latest_in_serial_group_', i))) == max(z$date_filed)) {break}
      }
      # Subset the cases previously identified, thereby selecting only the latest case in each group of serial cases
      #   for each plaintiff/defendant/ZIP combination
      dates_to_select <- sapply(paste0('latest_in_serial_group_', 1:i), function(x) eval(parse(text = x)))
      z <- z[z$date_filed %in% dates_to_select, ]
      z <- z %>% group_by(date_filed) %>% filter(id == max(id))
      z
    }
  }
}
deserializer_outer <- function(x) {
  pre_nrow <- x %>% group_by(filing_year) %>% summarize(pre_n = n())
  x <- x %>% group_by(pla_1, def_1, def_1_zip) %>% group_modify(deserializer_inner) %>% ungroup()
  post_nrow <- x %>% group_by(filing_year) %>% summarize(post_n = n())
  merge(pre_nrow, post_nrow, by = 'filing_year') %>%
    mutate(nrow_change = pre_n - post_n) %>%
    apply(., 1, function(x) cat(paste0(x[1], ': ', x[length(x)], ' serial cases identified and removed', '\n')))
  x
}
cases <- deserializer_outer(cases)

# Identify non-residential defendants
pattern <- source(file = 'data-non-residential-regex.R')$value
non_residential_flagger <- function(x) {
  if (any(is.na(x$def_1)) == T) {
    x$def_1 <- ifelse(is.na(x$def_1), '', x$def_1)
  }
  x$non_residential <- stri_detect(x$def_1, regex = pattern)
  cat('Number of cases with non-residential defendants identified and removed in cases_residential_only.csv:',
      sum(x$non_residential, na.rm = T), '\n')
  x <- x[x$non_residential == F, ]
  x
}
cases_residential_only <- non_residential_flagger(cases)

# Write cleaned and aggregated CVS containing all cases stacked
write_csv(cases, file = 'cases.csv')
write_csv(cases_residential_only, file = 'cases_residential_only.csv')

# Close file with all output, read back in, clean as desired, and overwrite
sink()
data.frame(year_rows = read_lines('cleaning_notes.txt')) %>%
  filter(stri_detect(.$year_rows,  regex = '(^\\d{4}\\:)|(^Number)')) %>%
  rbind('CODE-GENERATED TEXT FILE. DO NOT MODIFY BY HAND.',
        paste0('GENERATED ON: ', Sys.time(), '.'),
        .) %>%
  transmute(year_rows = gsub('\\s{2}', ' ', year_rows)) %>%
  apply(., 1, as.character) %>%
  writeLines(., con = 'cleaning_notes.txt')