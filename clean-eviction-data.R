# Loading and cleaning Virginia eviction data from https://virginiacourtdata.org/
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 2022-02-02

# Possible future revisions:
#   - The name-cleaning chunk could be abbreviated in this file by moving function(s)
#       to another file and calling them here; consider for the future
#   - Currently, defendant #1 ZIPs are converted to NA if they're not valid Virginia ZIPs;
#       we could optionally (a) remove this cleaning step and clean as needed in
#       mapping scripts or (b) add a similar step to clean plaintiff ZIPs in the same way

################################################################################
# The only step in advance of running this code is ensuring that:              #
#   1. `enclosing_directory` below is set as the directory containing          #
#      (a) this code AND (b) the folders containing each year's unprocessed    #
#      court data                                                              #
#   2. `string_identifying_data_folders` below is set as a string that         #
#      identifies folders containing unprocessed court data; that is, a        #
#      string present only in the names of ALL data folders and not in the     #
#      names of ANY other files present in `enclosing_directory`               #
#   3. `target_directory` below is set to whatever folder the user wants the   #
#      cleaned data to be deposited in (directory will be created if it        #
#      doesn't already exist)                                                  #
#   4. The data-folder names for each year contain the string below AND        #
#      each contains the relevant year (YYYY) (e.g., "DistrictCivil_2020")     #
#   5. `non-residential-defendant-regex.R` is present in `enclosing_directory` #
# With those conditions satisfied, the data cleaning process will be entirely  #
#   automated, and the code will save a data frame called `cases.txt`          #
#   containing cleaned, aggregated cases for all years (with `year_filed` as   #
#   a year identifier), as well as a version of `cases.txt` excluding cases    #
#   we tag as having non-residential defendants (`cases_residential_only.txt`) #
#   to a `processed-data` directory                                            #
# The code will also save a file named `cleaning-notes.txt` to the enclosing   #
#   directory that contains information on how many true duplicates and        #
#   serial cases the code identified for each year, how many cases             #
#   were tagged as having non-residential defendants, and how many cases were  #
#   flagged as not having valid VA ZIP codes listed for defendants             #
# Warning: This code can take upward of 20 minutes to fully run                #
################################################################################

# Set up (vars, libraries) ----
# `enclosing_directory` and `string_identifying_data_folders` are case sensitive
enclosing_directory <- 'va-evictions'
string_identifying_data_folders <- 'DistrictCivil'
target_directory <- 'processed-data'

# Libraries
library(stringi)
library(tidyverse)
library(readr)
library(rvest)
library(lubridate)

# Checks ----
if (stri_detect(getwd(), regex = paste0('(\\/', enclosing_directory, '$)')) == F) {
  stop(paste0('The working directory is not the directory indicated in ', enclosing_directory))
}
if ('non-residential-defendant-regex.R' %in% dir() == F) {
  stop('non-residential-defendant-regex.R is not in the working directory')
}

# Open file to save a few lines of relevant output (saved into `enclosing_directory`)
sink(file = 'cleaning-notes.txt', type = 'output')

# Read data ----
# Load raw district court civil case data
district_folders <- list.files("civilcases", pattern = string_identifying_data_folders, full.names = TRUE)

# district_folders <- dir()[stri_detect(dir(), fixed = string_identifying_data_folders)]
if (all(dir.exists(district_folders)) == F) {stop('Ensure that `string_identifying_data_folders` is only present in *directory* names within `enclosing_directory, not *file* names')}
if (all(stri_detect(district_folders, regex = '\\d{4}')) == F) {stop('Ensure that every data folder has a year in its name')}
data_years <- stri_extract(district_folders, regex = '(\\d{4})')
for (i in 1:length(district_folders)) {
  year <- stri_extract(district_folders[i], regex = '(\\d{4})')
  assign(paste0('cases', year), read_csv(paste0(district_folders[i], '/Cases.csv')))
  assign(paste0('defendants', year), read_csv(paste0(district_folders[i], '/Defendants.csv')))
  assign(paste0('plaintiffs', year), read_csv(paste0(district_folders[i], '/Plaintiffs.csv')))
  assign(paste0('hearings', year), read_csv(paste0(district_folders[i], '/Hearings.csv')))
}

# Fix 2021 data ----
# Updated 2021 data - already filtered to unlawful detainers
#    1. slice 2021 data to keep only one distinct record per data file
#     a. cases (50,100), group_by(id) %>% slice(1) -- what is casenumber relative to id?
#     b. hearings (99,676), group_by(id) %>% slice(1)
#     c. plaintiffs (51,607), group_by(id) %>% slice(1)
#     d. defendants (68,571), group_by(id) %>% slice(1)
#    2. remove cases with filed dates prior to 2021
#    2b. if time permits, revise to find duplicates from updated 2021
#    data and retain latest case information
# Note: id in cases matches case_id in everything else
cases2021 <- cases2021 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(FiledDate >= as.Date("2021-01-01"))

hearings2021 <- hearings2021 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()

defendants2021 <- defendants2021 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()

plaintiffs2021 <- plaintiffs2021 %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()

# Data munging ----
# Read in court names from Ben Schoenfeld's GitHub
district_courts <- read.csv('https://raw.githubusercontent.com/bschoenfeld/virginia-court-data-analysis/master/data/district_courts.csv')
colnames(district_courts)[which(colnames(district_courts) == 'name')] <- 'court_name'
# Canary: Until this comment is removed, we have to manually add FIPS 710 as Norfolk General District Court
#   In 2017, Norfolk began condensing their case reporting from three separate courts (711, 712, 713) into one (710)
#   710 is how Norfolk cases are identified in our data
district_courts <- rbind(district_courts, c(710, 'Norfolk General District Court'))
district_courts$fips <- as.integer(district_courts$fips)
# Notes on courts:
#   - Unlawful detainer cases in Fairfax *City* are held at the Fairfax *County* court
#     - Thus no cases linked to Fairfax City GDC appear in the unlawful detainer data, see: http://www.courts.state.va.us/courts/gd/fairfax_city/home.html
#   - "Richmond Manchester," "Mongomery/Blacksburg," and a few exclusively criminal and/or traffic courts appear in the list but not in the unlawful
#       detainer data (the former two appear to not be real GDCs; the latter are not present for obvious reasons)

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
                def_1_zip = as.character(ifelse(def_count > 1, stri_extract(def_address, regex = '(\\d{5}?)(?= \\|)'), stri_extract(def_address, regex = '(\\d{5})')))) %>%
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
                pla_1_zip = as.character(ifelse(pla_count > 1, stri_extract(pla_address, regex = '(\\d{5}?)(?= \\|)'), stri_extract(pla_address, regex = '(\\d{5})')))) %>%
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

##### Clean plaintiff and defendant names to improve quality of later grouping procedures
# Store unmodified versions of pla_1 and def_1 in the data frame for safekeeping
store_orig_names <- function(x) {
  x$pla_1_unmodified <- x$pla_1
  x$def_1_unmodified <- x$def_1
  x
}
cases <- store_orig_names(cases)

# Remove: TA/DBA tags; dashes, slashes, and periods; commas at ends of lines; double (or more) spaces
pla_and_def_cleaner <- function(x) {
  ##### Plaintiff names
  # Remove "Trading as..." and "Doing business as..." extraneous text
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = ',? T/?A\\b'), stri_extract(x$pla_1, regex = '(.*)(?=(,? T/?A\\b))'), x$pla_1)
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = ',? D/?B/?A\\b'), stri_extract(x$pla_1, regex = '(.*)(?=(,? D/?B/?A\\b))'), x$pla_1)
  # Replace dashes with spaces
  x$pla_1 <- gsub('-', ' ', x$pla_1)
  # Replace slashes with spaces
  x$pla_1 <- gsub('/', ' ', x$pla_1)
  # Remove periods
  x$pla_1 <- gsub('\\.', '', x$pla_1)
  # Remove commas at ends of lines
  x$pla_1 <- stri_replace(x$pla_1, regex = ',\\s{1,}?$', replacement = '')
  # Remove double (or >double) spaces
  x$pla_1 <- gsub('\\s{2,}', ' ', x$pla_1)
  ##### Defendant names
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = ',? T/?A\\b'), stri_extract(x$def_1, regex = '(.*)(?=(,? T/?A\\b))'), x$def_1)
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = ',? D/?B/?A\\b'), stri_extract(x$def_1, regex = '(.*)(?=(,? D/?B/?A\\b))'), x$def_1)
  # Replace dashes with spaces
  x$def_1 <- gsub('-', ' ', x$def_1)
  # Replace slashes with spaces
  x$def_1 <- gsub('/', ' ', x$def_1)
  # Remove periods
  x$def_1 <- gsub('\\.', '', x$def_1)
  # Remove commas at ends of lines
  x$def_1 <- stri_replace(x$def_1, regex = ',\\s{1,}?$', replacement = '')
  # Remove double (or >double) spaces
  x$def_1 <- gsub('\\s{2,}', ' ', x$def_1)
  ##### Return corrected names
  x
}
cases <- pla_and_def_cleaner(cases)

# Address alternative spellings common in eviction/housing data
bespoke_plaintiff_cleaner <- function(x) {
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bapt\\b', replacement = 'APARTMENT')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bapts\\b', replacement = 'APARTMENTS')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\b(mgt|mtg|mgmt)\\b', replacement = 'MANAGEMENT')
  x$pla_1 <- stri_replace_all(x$pla_1, regex = '(?i)\\bco\\b', replacement = 'COMPANY')
  x
}
cases <- bespoke_plaintiff_cleaner(cases)

# Remove commas/semicolons in plaintiff and defendant names if they come before a business entity identifier
#   - E.g., "Official Company, LLC" --> "Official Company LLC"
#   - This helps prevents differences in court data entry practices from disrupting our ability to accurately group a plaintiff's filings
#   - E.g., "Virginia Housing, LP" and "Virginia Housing LP" should be identifiable as the same entity
selective_comma_remover <- function(x) {
  # Plaintiff names
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = ',|;'),
                    ifelse(stri_detect(stri_extract_last(x$pla_1, regex = '(?<=(,|;))(.*)'), regex = '(?i)(\\b((p)?l(lc|td|l{0,2}?p))|inc\\b)'),
                           stri_replace_last(x$pla_1, regex = ',|;', replacement = ''),
                           x$pla_1),
                    x$pla_1)
  # Defendant names
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = ',|;'),
                    ifelse(stri_detect(stri_extract_last(x$def_1, regex = '(?<=(,|;))(.*)'), regex = '(?i)(\\b((p)?l(lc|td|l{0,2}?p))|inc\\b)'),
                           stri_replace_last(x$def_1, regex = ',|;', replacement = ''),
                           x$def_1),
                    x$def_1)
  # Return corrected names
  x
}
cases <- selective_comma_remover(cases)

# Ensure that spacing around punctuation in names is correct
#   Convert instances of:
#     - "x , y" and "x ; y" --> "x, y" and "x; y"
#     - "x,y" and "x;y" --> "x, y" and "x; y"
#     - "x ,y" and "x ;y" --> "x, y" and "x; y"
punctuation_spacing_check <- function(x) {
  # Plaintiff names
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = '(,|;)(\\S)'), gsub('(,|;)(\\S)', '\\1 \\2', x$pla_1), x$pla_1)
  x$pla_1 <- ifelse(stri_detect(x$pla_1, regex = '(\\S)(\\s{1,})(,|;)'), gsub('(\\S)(\\s{1,})(,|;)', '\\1\\3', x$pla_1), x$pla_1)
  # Defendant names
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = '(,|;)(\\S)'), gsub('(,|;)(\\S)', '\\1 \\2', x$def_1), x$def_1)
  x$def_1 <- ifelse(stri_detect(x$def_1, regex = '(\\S)(\\s{1,})(,|;)'), gsub('(\\S)(\\s{1,})(,|;)', '\\1\\3', x$def_1), x$def_1)
  # Return corrected names
  x
}
cases <- punctuation_spacing_check(cases)
##### Finish plaintiff and defendant name cleaning process

# Clean defendant ZIP codes
#   - Current approach: Convert def_1_zip to NA when it isn't a valid *Virginia* ZIP code so as not to disrupt VA by-ZIP case tabulation
#   - For example: "Henrico, VA 00000," "Charlottesville, VA 99999," "Ewing, NJ 12345"
valid_va_zips <- as.character(c(20100:20199, 22000:24699)) # https://en.wikipedia.org/wiki/List_of_ZIP_Code_prefixes
def_zip_cleaner <- function(x) {
  pre_na_zips <- x %>% group_by(filing_year) %>% summarize(pre_na = sum(is.na(def_1_zip)))
  x$def_1_zip <- ifelse(x$def_1_zip %in% valid_va_zips, x$def_1_zip, NA)
  post_na_zips <- x %>% group_by(filing_year) %>% summarize(post_na = sum(is.na(def_1_zip)))
  merge(pre_na_zips, post_na_zips, by = 'filing_year') %>%
    mutate(na_gain = post_na - pre_na) %>%
    apply(., 1, function(x) cat(paste0(x[1], ': ', x[length(x)], ' non-VA defendant ZIPs identified and converted to NA', '\n')))
  x
}
cases <- def_zip_cleaner(cases)

# Identify and remove true duplicates
#   Variables used to identify true duplicates: “FiledDate”, “Judgment”, “Costs”, “AttorneyFees”, “PrincipalAmount",
#   “OtherAmount”, "pla_1_unmodified", "def_1_unmodified", "def_1_zip"
#   Note that this process currently uses pla_1, def_1, and def_1_zip, which have been cleaned
deduplicater <- function(x) {
  pre_nrow <- x %>% group_by(filing_year) %>% summarize(pre_n = n())
  dupes <- x[, c('FiledDate', 'Judgment', 'Costs', 'AttorneyFees', 'PrincipalAmount',
                 'OtherAmount', 'pla_1', 'def_1', 'def_1_zip')]
  x <- x[!duplicated(dupes), ]
  post_nrow <- x %>% group_by(filing_year) %>% summarize(post_n = n())
  merge(pre_nrow, post_nrow, by = 'filing_year') %>%
    mutate(nrow_change = pre_n - post_n) %>%
    apply(., 1, function(x) cat(paste0(x[1], ': ', x[length(x)], ' true duplicates identified and removed', '\n')))
  x
}
cases <- deduplicater(cases)

# Identify serial cases
#   Variables used to identify serial cases: "pla_1", "def_1", "def_1_zip"
# We define serial cases as: Sequential filings by a given plaintiff ("pla_1") against a given primary defendant ("def_1")
#   in a given ZIP code ("def_1_zip") filed 12 months of less after an initial filing (<=12 months)
#   (Note that this implies that there can be multiple "groups" of serial filings for a given pla_1/def_1/def_1_zip combination)
# ID_serials_inner() is applied via a wrapper function, ID_serials_outer(), to subsets of the case data grouped by
#   pla_1, def_1, and def_1_zip; the resulting data has two new columns: serial_filing (T/F) indicating whether a
#   given case is serial, and latest_filing_between_pla_and_def (T/F), indicating whether a given case is the latest
#   within a group of serial cases (defined by am inclusive 12-month window)
n_year <- length(unique(cases$filing_year))
ID_serials_inner <- function(z, max_windows = 4, ...) {
  z$serial_filing <- NA
  z$latest_filing_between_pla_and_def <- NA
  if (nrow(z) == 1) {
    z$serial_filing <- FALSE
    z$latest_filing_between_pla_and_def <- TRUE
    return(z)
  }
  if (nrow(z) > 1) {
    cases_span_more_than_1_yr <- (interval(min(z$date_filed), max(z$date_filed)) / years(1)) > 1
    if (cases_span_more_than_1_yr == F) {
      z$serial_filing <- ifelse(z$date_filed == min(z$date_filed), FALSE, TRUE)
      z$latest_filing_between_pla_and_def <- ifelse(z$date_filed == max(z$date_filed), TRUE, FALSE)
      return(z)
    }
    # If the interval is >1 year...
    if (cases_span_more_than_1_yr == T) {
      for (i in 1:100) {
        if (i == 1) {
          begin_window_1 <- min(z$date_filed)
          end_window_1 <- min(z$date_filed) %m+% months(12)
        }
        if (i > 1) {
          if (max(z$date_filed) <= eval(parse(text = paste0('end_window_', i-1)))) {break}
          assign(paste0('begin_window_', i),
                 min(z$date_filed[z$date_filed > eval(parse(text = paste0('end_window_', i-1)))]))
          assign(paste0('end_window_', i),
                 eval(parse(text = paste0('begin_window_', i))) %m+% months(12))
        }
      }
      # Identify and mark cases that are the beginnings of "serial windows"
      # (n_window_beginnings is i-1 because when the loop terminates, i is equal to n_windows+1)
      window_beginnings <- paste0('begin_window_', 1:(i-1))
      z$serial_filing <- ifelse(z$date_filed %in% sapply(window_beginnings, function(x) eval(parse(text = x))),
                                FALSE, TRUE)
      for (j in 1:length(window_beginnings)) {
        z_sub <- z[z$date_filed >= eval(parse(text = window_beginnings[j])) &
                     z$date_filed <= eval(parse(text = paste0('end_window_', j))), ]
        z$latest_filing_between_pla_and_def[z$date_filed == max(z_sub$date_filed)] <- TRUE
      }
      z$latest_filing_between_pla_and_def <- ifelse(is.na(z$latest_filing_between_pla_and_def), FALSE, z$latest_filing_between_pla_and_def)
      z
    }
  }
}

ID_serials_outer <- function(x) {
  x <- x %>% group_by(pla_1, def_1, def_1_zip) %>% group_modify(ID_serials_inner) %>% ungroup()
  x %>% group_by(filing_year) %>% summarize(serials = sum(serial_filing == T, na.rm = T)) %>%
    apply(., 1, function(w) cat(paste0(w[1], ': ', w[length(w)], ' serial cases identified', '\n')))
  x
}
cases <- ID_serials_outer(cases)

# Identify non-residential defendants
pattern <- source(file = 'non-residential-defendant-regex.R')$value
non_residential_flagger <- function(x, remove_cases) {
  if (any(is.na(x$def_1)) == T) {
    x$def_1 <- ifelse(is.na(x$def_1), '', x$def_1)
  }
  # Flag non-residential defendants
  x$non_residential_defendant <- stri_detect(x$def_1, regex = pattern)
  if (remove_cases == T) {
    cat('Number of cases with non-residential defendants identified and removed in cases_residential_only.txt:',
        sum(x$non_residential_defendant, na.rm = T), '\n')
    x <- x[x$non_residential_defendant == F, ]
  }
  x
}
cases <- non_residential_flagger(cases, remove_cases = F)
cases_residential_only <- non_residential_flagger(cases, remove_cases = T)

# Write cleaned and aggregated data containing all cases stacked
# Note: Writing to CSV results in leading zeros being dropped; as of
#       2022-01, this is not desired behavior, so we write to .txt
if (dir.exists(target_directory) == F) {dir.create(target_directory)}
write_csv(cases, file = paste0(target_directory, '/', 'cases.txt'))
write_csv(cases_residential_only, file = paste0(target_directory, '/', 'cases_residential_only.txt'))

# Close file with all output, read back in, clean as desired, and overwrite
sink()
data.frame(year_rows = read_lines('cleaning-notes.txt')) %>%
  filter(stri_detect(.$year_rows,  regex = '(^\\d{4}\\:)|(^Number)')) %>%
  rbind('CODE-GENERATED TEXT FILE. DO NOT MODIFY BY HAND.',
        paste0('GENERATED ON: ', Sys.time(), '.'),
        .) %>%
  transmute(year_rows = gsub('\\s{2}', ' ', year_rows)) %>%
  apply(., 1, as.character) %>%
  writeLines(., con = 'cleaning-notes.txt')