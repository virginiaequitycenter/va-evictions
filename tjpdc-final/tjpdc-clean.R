# TJPDC eviction database:
#   Loading and cleaning eviction data for Charlottesville and Albemarle from 2018-07-01 to 2021-06-30
#   Data source: Ben Schoenfeld, https://virginiacourtdata.org/
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 09-30-2021

# Libraries
library(stringi)
library(tidyverse)
library(readr)
library(rvest)
library(lubridate)

# Open file to save relevant output (saved into working directory)
sink(file = 'tjpdc_cleaning_notes.txt', type = 'output')

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
# Note: If this link ever dies, a copy of the CSV as of 2021-09-23 is saved in the root dir as 'district_courts_list_backup.csv'
district_courts <- read.csv('https://raw.githubusercontent.com/bschoenfeld/virginia-court-data-analysis/master/data/district_courts.csv')
colnames(district_courts)[which(colnames(district_courts) == 'name')] <- 'court_name'
district_courts$fips <- as.integer(district_courts$fips)

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
#   - Not all defendant addresses are accompanied by ZIP codes; e.g., def_address = "CHARLOTTESVILLE, VA"
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
      mutate(case_hearing_count = n()) %>%
      transmute(latest_hearing_id = id,
                latest_hearing_date = as.POSIXlt(Date, format = '%Y-%m-%d %H:%M:%S', tz = 'EST'),
                latest_hearing_result = Result,
                hearing_count = case_hearing_count) %>%
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

# Select only cases with court_name == 'Charlottesville General District Court' | 'Albemarle General District Court'
#   and filing_date >= June 1, 2018 & <= June 30, 2021
case_selector <- function(x) {
  x <- x %>% filter(court_name == 'Charlottesville General District Court' | court_name == 'Albemarle General District Court')
  x <- x %>% filter(date_filed >= '2018-07-01' & date_filed <= '2021-06-30')
  x
}
for (i in 1:length(cases_objects)) {
  assign(cases_objects[i], case_selector(eval(parse(text = cases_objects[i]))))
}

# Stack cases from all years on top of each other in one data frame
cases <- data.frame()
for (i in 1:length(cases_objects)) {
  x <- eval(parse(text = cases_objects[i]))
  cases <- rbind(cases, x)
}

# Stack 2018-2020 data with 2021 data from new scraper that has been prepared and cleaned through the quarter-assignment step
cases_2021_new_scraper <- read.csv('2021-data-from-LDL-scraper/2021_data_from_LDL_scraper.csv')
cases <- rbind(cases, cases_2021_new_scraper)

##### Clean plaintiff and defendant names to improve quality of later grouping procedures
# Store unmodified versions of pla_1 and def_1 in the data frame for safekeeping
store_orig_names <- function(x) {
  x$pla_1_unmodified <- x$pla_1
  x$def_1_unmodified <- x$def_1
  x
}
cases <- store_orig_names(cases)

# General cleaning of plaintiff and defendant names
pla_and_def_cleaner <- function(x) {
  ########## Plaintiff names
  # Remove "Trading as..." and "Doing business as..." extraneous text
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = ',? T/?A\\b'), stri_extract(x$pla_1, regex = '(.*)(?=(,? T/?A\\b))'), x$pla_1)
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = ',? D/?B/?A\\b'), stri_extract(x$pla_1, regex = '(.*)(?=(,? D/?B/?A\\b))'), x$pla_1)
  # Correct instances of comma misplacements (" ," --> ", ")
  x$pla_1 <- gsub(' ,', ', ', x$pla_1)
  # Replace dashes with spaces
  x$pla_1 <- gsub('-', ' ', x$pla_1)
  # Replace slashes with spaces
  x$pla_1 <- gsub('/', ' ', x$pla_1)
  # Remove periods
  x$pla_1 <- gsub('\\.', '', x$pla_1)
  # Remove commas at ends of lines
  x$pla_1 <- stri_replace(x$pla_1, regex = '(,|,\\s{1,})$', replacement = '')
  # Remove double (or >double) spaces
  x$pla_1 <- gsub('\\s{2,}', ' ', x$pla_1)
  ########## Defendant names
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = ',? T/?A\\b'), stri_extract(x$def_1, regex = '(.*)(?=(,? T/?A\\b))'), x$def_1)
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = ',? D/?B/?A\\b'), stri_extract(x$def_1, regex = '(.*)(?=(,? D/?B/?A\\b))'), x$def_1)
  # Correct instances of comma misplacements (" ," --> ", ")
  x$def_1 <- gsub(' ,', ', ', x$def_1)
  # Replace dashes with spaces
  x$def_1 <- gsub('-', ' ', x$def_1)
  # Replace slashes with spaces
  x$def_1 <- gsub('/', ' ', x$def_1)
  # Remove periods
  x$def_1 <- gsub('\\.', '', x$def_1)
  # Remove commas at ends of lines
  x$def_1 <- stri_replace(x$def_1, regex = '(,|,\\s{1,})$', replacement = '')
  # Remove double (or >double) spaces
  x$def_1 <- gsub('\\s{2,}', ' ', x$def_1)
  ########## Return corrected names
  x
}
cases <- pla_and_def_cleaner(cases)

# Address common misspellings/alternative spellings specific to these data
bespoke_plaintiff_cleaner <- function(x) {
  # Alternative spellings and abbreviations
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bapt\\b', replacement = 'APARTMENT')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bapts\\b', replacement = 'APARTMENTS')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\b(mgt|mtg|mgmt)\\b', replacement = 'MANAGEMENT')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bco\\b', replacement = 'COMPANY')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bsq\\b', replacement = 'SQUARE')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bN\\.A\\.\\b', replacement = 'National Association')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bland & dev\\b', replacement = 'LAND AND DEVELOPMENT')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bH & H\\b', replacement = 'H&H')
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = '(?i)(pep)(.*)(uva)'), 'PEP UVA LLC', x$pla_1)
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = '(?i)(westgate?)(.*)(barclay)'), 'WESTGATE/BARCLAY PLACE LLC', x$pla_1)
  # Misspellings
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bcomany\\b', replacement = 'COMPANY')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bsquir\\b', replacement = 'SQUIRE')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bskylne\\b', replacement = 'SKYLINE')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bneighboarhood\\b', replacement = 'NEIGHBORHOOD')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bmountains\\b', replacement = 'MOUNTAIN')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\baartments\\b', replacement = 'APARTMENTS')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bdomi(on|nions)\\b', replacement = 'DOMINION')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\babbbington\\b', replacement = 'ABBINGTON')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bttust\\b', replacement = 'TRUST')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bowener\\b', replacement = 'OWNER')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bhous(in|eing)\\b', replacement = 'HOUSING')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bwoodland\\b', replacement = 'WOODLANDS')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\btre(edale|ssdale)\\b', replacement = 'TREESDALE')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\s{1,}\\(?the\\)?$', replacement = '')
  x
}
cases <- bespoke_plaintiff_cleaner(cases)

# Remove commas/semicolons in plaintiff and defendant names if they come before a business entity identifier
#   (E.g., "Official Company, LLC" --> "Official Company LLC")
#   This helps prevents differences in court data entry practices from disrupting our ability to accurately group a plaintiff's filings
#   (E.g., "Virginia Housing, LP" and "Virginia Housing LP" should be treated as the same)
selective_comma_remover <- function(x) {
  ########## Plaintiff names
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = ',|;'),
                    ifelse(stri_detect(stri_extract_last(x$pla_1, regex = '(?<=(,|;))(.*)'), regex = '\\b(?i)(l(l)?c|l(l)?p|inc)\\b'),
                           stri_replace_last(x$pla_1, regex = ',|;', replacement = ''),
                           x$pla_1),
                    x$pla_1)
  ########## Defendant names
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = ',|;'),
                    ifelse(stri_detect(stri_extract_last(x$def_1, regex = '(?<=(,|;))(.*)'), regex = '\\b(?i)(l(l)?c|l(l)?p|inc)\\b'),
                           stri_replace_last(x$def_1, regex = ',|;', replacement = ''),
                           x$def_1),
                    x$def_1)
  ########## Return corrected names
  x
}
cases <- selective_comma_remover(cases)

# Create simplified version of defendant and plaintiff names that can be optionally used for searching/grouping
name_simplifier <- function(x) {
  x <- gsub(';', ' ', x) # Replace all semicolons with single spaces
  x <- gsub('\\bJR|SR\\b', '', x) # Remove JR/SR tags
  x <- gsub('\\b(II?I?I?|IV)\\b', '', x) # Remove I, II, III, IV tags
  x <- gsub('\\b\\w{1}\\b', '', x) # Remove all single letters (middle initials)
  x <- gsub('\\s{2,}', ' ', x) # Remove instances of 2+ spaces
  x <- gsub(' , ', ', ', x) # Remove errant comma pre-spaces
  x <- gsub('(\\s{1,}|,|, | ,)$', '', x) # Remove all spaces and commas at end of names
  x
}
cases$def_1_simplified <- name_simplifier(cases$def_1)
cases$pla_1_simplified <- name_simplifier(cases$pla_1)
##### Fin plaintiff and defendant name cleaning process

# Clean ZIP codes
#   - If def_1_zip isn't a valid *Virginia* ZIP code, convert it to NA so that it doesn't
#       disrupt by-ZIP case tabulation (invalid example: "Charlottesville, VA 99999")
va_zips <- c(20100:20199, 22000:24699) # https://en.wikipedia.org/wiki/List_of_ZIP_Code_prefixes
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
#   Variables used to identify true duplicates:
#     “FiledDate”, “Judgment”, “Costs”, “AttorneyFees”, “PrincipalAmount", “OtherAmount”, "pla_1", "def_1", "def_1_zip"
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

# Identify and tag serial cases
#   Variables used to identify serial cases: "pla_1", "def_1", "def_1_zip"
# We define serial cases as: Sequential filings by a given plaintiff ("pla_1") against a given primary defendant ("def_1")
#   in a given ZIP code ("def_1_zip") within 12 months of an initial filing (note that this implies that there can be
#   multiple "groups" of serial filings for a given pla_1/def_1/def_1_zip combination)
# serial_detector_inner() is applied via a wrapper function, serial_detector_outer() (see below) to a
#   tibble of cases grouped by pla_1, def_1, and def_1_zip (i.e., sets of rows where the same
#   plaintiff filed against the same defendant in the same ZIP code)
# A reasonable test of whether the serial identifier is working properly is checking that the value obtained from:
#   nrow(cases %>% group_by(pla_1, def_1, def_1_zip) %>% summarize(n()))
#   is identical before and after running serial_detector_outer()
serial_detector_inner <- function(z, ...) {
  # If there's only one case filed by the plaintiff against the defendant, simply keep that case
  if (nrow(z) == 1) {
    z$serial_filings_by_pla_against_def <- FALSE
    return(z)
  }
  # If there's >1 case filed by the plaintiff against the defendant...
  if (nrow(z) > 1) {
    # First: Determine whether the time span from the first case to the latest is >1 year
    cases_span_more_than_1_yr <- (interval(min(z$date_filed), max(z$date_filed)) / years(1)) > 1
    # If the interval is *not* >1 year, simply mark `serial_filings_by_pla_against_def` as TRUE
    if (cases_span_more_than_1_yr == F) {
      z$serial_filings_by_pla_against_def <- TRUE
      return(z)
    }
    # If the interval is >1 year...
    if (cases_span_more_than_1_yr == T) {
      num_cases_pre <- nrow(z)
      for (i in 1:100) {
        # First determine the latest case filed that is not more than 12 months after the very first case
        #   (This is the latest in the first "group" of serial cases; it can be the sole case in the 12-mo period)
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
      # Create a temporary data frame of the cases identified by the process above
      #   I.e., only the latest case in each group of serial cases for each plaintiff/defendant/ZIP combination
      dates_to_select <- sapply(paste0('latest_in_serial_group_', 1:i), function(x) eval(parse(text = x)))
      temp_df <- z[z$date_filed %in% dates_to_select, ]
      # If multiple cases were filed on the same day, we could end up missing a TRUE serial tag, if we simply compared
      #   nrow(z) and nrow(temp_df) at this point, so we group by `date_filed` and filter by `id` in temp_df to make it
      #   possible to identify multiple same-day filings as needing a `serial_filings_by_pla_against_def` = TRUE value
      temp_df <- temp_df %>% group_by(date_filed) %>% filter(id == max(id))
      num_cases_post <- nrow(temp_df)
      z$serial_filings_by_pla_against_def <- ifelse(num_cases_post < num_cases_pre, TRUE, FALSE)
      z
    }
  }
}
serial_detector_outer <- function(x) {
  x <- x %>% group_by(pla_1, def_1, def_1_zip) %>% group_modify(serial_detector_inner) %>% ungroup()
  x
}
cases <- serial_detector_outer(cases)

# Identify if defense attorney is present
def_attorney_eval <- function(x) {
  cleaned_attorneys <- gsub('(i?)UNKNOWN|NULL|NONE', '', x$def_attorney)
  x$def_attorney_present <- stri_detect(cleaned_attorneys, regex = '\\w+')
  x
}
cases <- def_attorney_eval(cases)

# Identify whether plaintiff is likely to be a management corporation
# Read in regex for identifying management companies
source('tjpdc-mgmt-company-regex.R')
# Apply to plaintiff 1 names
cases$mgmt_company_pla <- stri_detect(cases$pla_1, regex = mgmt_comp_pattern)

# Determine if judgment has been issued by end of study period
cases$judgment_issued <- ifelse(stri_detect(cases$Judgment, regex = '\\w+'), TRUE, FALSE)

# Determine if case only has one associated hearing
cases$single_hearing <- ifelse(cases$hearing_count == 1, TRUE, FALSE)

# Determine total amount of judgments
# Convert -1 values (likely signifying "missing") to 0 in fields containing dollar amounts
cases[, c('Costs', 'AttorneyFees', 'PrincipalAmount', 'OtherAmount')] <- data.frame(
  apply(cases[, c('Costs', 'AttorneyFees', 'PrincipalAmount', 'OtherAmount')],
        2, function(x) ifelse(x < 0, 0, x))
  )
# Extract values from `OtherAwarded` and place in `OtherAwardedVal`
cases$OtherAwardedVal <- parse_number(cases$OtherAwarded)
# Remove values that are part of dates from `OtherAwardedVal` column
cases$OtherAwardedVal <- ifelse(stri_detect(cases$OtherAwarded,
                                            regex = paste0(paste0('/', cases$OtherAwardedVal), '|', paste0(cases$OtherAwardedVal), '/')),
                                NA, cases$OtherAwardedVal)
# Determine if value extracted from `OtherAwarded` column is identical to that in the `OtherAmount` column;
#   if so, convert `OtherAwardedVal` to NA to avoid double-counting costs that were entered in both columns by clerks
cases$OtherAwardedVal <- ifelse(cases$OtherAwardedVal == cases$OtherAmount, NA, cases$OtherAwardedVal)

cost_summer <- function(x) {
  x$total_judgment_amount <- rowSums(cases[, c('Costs', 'AttorneyFees', 'PrincipalAmount', 'OtherAmount', 'OtherAwardedVal')], na.rm = T)
  x
}
cases <- cost_summer(cases)

# Condense writ filing dates into one column (from WritIssuedDate and WritofEvictionIssuedDate)
cases$WIDtemp <- ifelse(stri_detect(cases$WritIssuedDate, regex = '^\\s*$'), NA, cases$WritIssuedDate)
cases$WoEIDtemp <- ifelse(stri_detect(cases$WritofEvictionIssuedDate, regex = '^\\s*$'), NA, cases$WritofEvictionIssuedDate)
cases$writ_issued_date <- ifelse(is.na(cases$WIDtemp) == F, cases$WIDtemp, cases$WoEIDtemp)

# Identify non-residential defendants
pattern <- source(file = 'tjpdc-non-residential-regex.R')$value
non_residential_flagger <- function(x, remove_cases) {
  if (any(is.na(x$def_1)) == T) {
    x$def_1 <- ifelse(is.na(x$def_1), '', x$def_1)
  }
  x$non_residential <- stri_detect(x$def_1, regex = pattern)
  if (remove_cases == T) {
    cat('Number of cases with non-residential defendants identified and removed in cases_residential_only.csv:',
        sum(x$non_residential, na.rm = T), '\n')
    x <- x[x$non_residential == F, ]
  }
  x
}
cases <- non_residential_flagger(cases, remove_cases = F)
cases_residential_only <- non_residential_flagger(cases, remove_cases = T)

# Only retain columns desired in final data set; also reorder columns to thematically group variables; also rename certain columns
col_selector <- function(x) {
  x <- x %>% select(id, CaseNumber, FiledDate, filing_year, quarter, fips, court_name, CaseType, DebtType, pla, pla_address, pla_attorney, pla_id, pla_count, pla_1, pla_1_zip, pla_1_unmodified, pla_1_simplified, mgmt_company_pla, def, def_address, def_attorney, def_id, def_count, def_1, def_1_zip, def_1_unmodified, def_1_simplified, def_attorney_present, non_residential, serial_filings_by_pla_against_def, hearing_count, latest_hearing_id, latest_hearing_date, latest_hearing_result, single_hearing, Judgment, judgment_issued, IsJudgmentSatisfied, DateSatisfactionFiled, Possession, AppealDate, AppealedBy, FurtherCaseInformation, writ_issued_date, Costs, AttorneyFees, PrincipalAmount, OtherAmount, InterestAward, OtherAwarded, OtherAwardedVal, total_judgment_amount)
  x <- x %>% rename(plaintiffs = pla,
                    plaintiff_addresses = pla_address,
                    plaintiff_attorneys = pla_attorney,
                    plaintiff_ids = pla_id,
                    plaintiff_count = pla_count,
                    plaintiff_1_standardized = pla_1,
                    plaintiff_1_zip = pla_1_zip,
                    plaintiff_1_unmodified = pla_1_unmodified,
                    plaintiff_1_simplified = pla_1_simplified,
                    mgmt_company_plaintiff = mgmt_company_pla,
                    defendants = def,
                    defendant_addresses = def_address,
                    defense_attorneys = def_attorney,
                    defendant_ids = def_id,
                    defendant_count = def_count,
                    defendant_1_standardized = def_1,
                    defendant_1_zip = def_1_zip,
                    defendant_1_unmodified = def_1_unmodified,
                    defendant_1_simplified = def_1_simplified,
                    defense_attorney_present = def_attorney_present,
                    non_residential_defendant = non_residential,
                    serial_filings_by_plaintiff_against_defendant = serial_filings_by_pla_against_def)
  x
}
cases <- col_selector(cases)
cases_residential_only <- col_selector(cases_residential_only)

# Write cleaned and aggregated CVS containing all cases stacked
write_csv(cases, file = 'cases.csv')
write_csv(cases_residential_only, file = 'cases_residential_only.csv')

# Close text file with all output, read back in, clean text as desired, and overwrite
sink()
data.frame(year_rows = read_lines('tjpdc_cleaning_notes.txt')) %>%
  filter(stri_detect(.$year_rows,  regex = '(^\\d{4}\\:)|(^Number)')) %>%
  rbind('CODE-GENERATED TEXT FILE. DO NOT MODIFY BY HAND.',
        paste0('GENERATED ON: ', Sys.time(), '.'),
        .) %>%
  transmute(year_rows = gsub('\\s{2}', ' ', year_rows)) %>%
  apply(., 1, as.character) %>%
  writeLines(., con = 'tjpdc_cleaning_notes.txt')