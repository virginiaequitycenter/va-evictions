##############################################################
# Eviction case data cleaning script                         #
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn      #
# GitHub: jacob-gg, mclaibourn                               #
# Last revised: 2024-04-19                                   #
##############################################################

######################## Instructions ########################
# 1. Check the modifiable user-preset variables below
# 2. With those set, the code should run all the way through
#    using data in the general format provided by the LSC
case_id_var <- 'c2dp_case_id'
data_directory <- 'data/original'
data_updates_directory <- 'data/updates'
output_directory <- 'processed-data'
##############################################################

# Packages
required <- c('devtools', 'dplyr', 'lubridate', 'virginiaequitycenter/ECtools')
handle_package <- function(pkg) {
  if (grepl(x = pkg, pattern = '\\/')) { devtools::install_github(pkg) }
  else if (!(pkg %in% installed.packages())) { install.packages(pkg) }
  pkg <- sub(x = pkg, pattern = '.+\\/', replacement = '')
  library(pkg, character.only = TRUE)
}
lapply(required, function(x) handle_package(x))

# Read data
keywords <- c('case', 'defendant', 'hearing', 'judgment', 'plaintiff')
files <- dir(data_directory)
dat_list <- lapply(seq_along(keywords), function(x) read.csv(paste0(data_directory, '/', files[grepl(x = files, pattern = keywords[x])])))

# Update original data with LSC periodic updates (updated dispositions, etc.)
#   This function relies on the existence of the dat_list, case_id_var, and keywords objects
updated_data_folders <- dir(data_updates_directory)
update_data <- function(folder, kywd = keywords) {
  update_files <- dir(paste0(data_updates_directory, '/', folder))
  update_dat_list <- lapply(seq_along(kywd), function(x) read.csv(paste0(data_updates_directory, '/', folder, '/', update_files[grepl(x = update_files, pattern = kywd[x])])))
  needs_updating <- update_dat_list[[which(kywd == 'case')]][[case_id_var]][update_dat_list[[which(kywd == 'case')]][[case_id_var]] %in% dat_list[[which(kywd == 'case')]][[case_id_var]]]
  dat_list <- lapply(seq_along(dat_list), function(x) eval(parse(text = paste0("dat_list[[", x, "]][(dat_list[[", x, "]][[case_id_var]] %in% needs_updating) == F, ]"))))
  dat_list <- lapply(seq_along(dat_list), function(x) rbind(dat_list[[x]], update_dat_list[[x]]))
  dat_list
}
for (i in 1:length(updated_data_folders)) { dat_list <- update_data(updated_data_folders[i]) }

# For variable names that are duplicated across data frames, prefix them with the name of their source data frame
var_names <- unlist(sapply(dat_list, function(x) colnames(x)), use.names = F)
duplicated_var_names <- unique(var_names[duplicated(var_names)]) %>% .[. != case_id_var]
dat_list <- lapply(seq_along(dat_list), function(z) {
  nms <- colnames(dat_list[[z]])
  nms[nms %in% duplicated_var_names] <- paste0(keywords[z], '_', nms[nms %in% duplicated_var_names])
  colnames(dat_list[[z]]) <- nms
  dat_list[[z]]
})

# Handle duplicated case IDs and set names of data list elements
#   These lines handle duplicated cases in a fairly unprincipled way (dropping cases with duplicated IDs)
#   This will be updated to a more principled, multi-outcome system down the line
duplicated_case_ids <- unique(dat_list[[which(keywords == 'case')]][[case_id_var]][duplicated(dat_list[[which(keywords == 'case')]][[case_id_var]])])
dat_list <- lapply(seq_along(dat_list), function(x) eval(parse(text = paste0("dat_list[[", x, "]][(dat_list[[", x, "]][[case_id_var]] %in% duplicated_case_ids) == F, ]"))))
names(dat_list) <- keywords

# Aggregate
source('functions_aggregation.R')
dat_list[['case']] <- case_aggregator(dat_list[['case']])
dat_list[['defendant']] <- defendant_aggregator(dat_list[['defendant']])
dat_list[['plaintiff']] <- plaintiff_aggregator(dat_list[['plaintiff']])
dat_list[['judgment']] <- judgment_aggregator(dat_list[['judgment']])
dat_list[['hearing']] <- hearing_aggregator(dat_list[['hearing']])

# Merge
cases <- Reduce(function(x, y) merge(x, y, by = case_id_var, all = TRUE), dat_list)

########################### Canary ###########################
# Richmond city cases vary in their court listing; some are associated with each of the
# following: Richmond City General District Court; Richmond-Civil General District Court. For now,
# we convert all to the former. Note: We currently *do not* update FIPS, which also vary (760 and 763),
# as we do not use FIPS as a grouping variable in the cleaning, summarizing, or app code.
cases[cases$county == 'Richmond-Civil General District Court', 'county'] <- 'Richmond City General District Court'
##############################################################

# Extract years of case filings
cases$filed_year <- extract_year(cases$filed_date, expect_modern = TRUE)

# Extract quarters of case filings
cases$filed_quarter <- assign_quarter(cases$filed_date, return_QX = TRUE)

# Only subset cases through certain date if a valid date is given in setup.txt
if ('setup.txt' %in% dir()) {
  setup <- readLines('setup.txt')
  use_cases_through <- setup[which(setup == 'use-data-through:') + 1]
  if (grepl(pattern = '\\d{4}-\\d{2}-\\d{2}', x = use_cases_through)) {
    cases <- cases[cases$filed_date <= use_cases_through, ]
  }
}

# Only work with cases from 2018 onward
cases$filed_year <- as.numeric(cases$filed_year)
cases <- cases[cases$filed_year >= 2018, ]

####################### Notes on names #######################
# We use LSC cleaned_party_name for plantiff names, but we still apply our standardization and cleaning
# process to defendant names (for purposes of, e.g., IDing serial cases and cases against residential defendants),
# and we convert LSC cleaned_party_name entries to uppercase
##############################################################
# Uppercase LSC clean_party_name (plaintiff name) entries and save as plaintiff_name
cases$plaintiff_name <- toupper(cases$clean_party_name)
# Correct punctuation spacing in defendant names
cases$defendant_name <- correct_punctuation_spacing(cases$defendant_name)
# Standardize defendant names
cases$defendant_name <- standardize_name(cases$defendant_name, case_out = 'upper')
# Expand common housing acronyms in defendant names
cases$defendant_name <- expand_shorthand(cases$defendant_name, type = 'housing', case_out = 'upper')

# Extract ZIP Codes
cases$defendant_zip <- extract_zip(cases$defendant_address, if_multiple = 'first', must_follow_state = TRUE)
cases$plaintiff_zip <- extract_zip(cases$plaintiff_address, if_multiple = 'first', must_follow_state = TRUE)
# Convert non-VA ZIPs to NA
va_zips <- as.character(c(20100:20199, 22000:24699)) # https://en.wikipedia.org/wiki/List_of_ZIP_Code_prefixes
cases$defendant_zip <- ifelse(cases$defendant_zip %in% va_zips, cases$defendant_zip, NA)
cases$plaintiff_zip <- ifelse(cases$plaintiff_zip %in% va_zips, cases$plaintiff_zip, NA)

# Identify and remove true duplicates (note that this process uses plaintiff_name and defendant_name, which have been cleaned)
duplicate_check_vars <- c('filed_date', 'judgment', 'costs', 'attorney_fees', 'principal_amount',
                          'other_amount', 'plaintiff_name', 'defendant_name', 'defendant_zip')
cases <- remove_duplicates_df(dat = cases, column_names = duplicate_check_vars, save_removed_rows_as = 'removed')

# Identify serial cases
source('functions_serial_cases.R')
cases <- id_serials(cases)

# Identify non-residential defendants
cases$defendant_non_residential <- identify_non_residential(cases$defendant_name)
# Un-flag cases with "OCCUPANT(S)" in the primary defendant name (likely residential, e.g., "ANY AND ALL OCCUPANTS")
cases$defendant_non_residential <- ifelse(grepl(x = cases$defendant_name, pattern = '(?i)\\boccupants?\\b'), FALSE, cases$defendant_non_residential)
# Un-flag cases with "ESTATE OF" in the defendant names (likely residential, e.g., "ESTATE OF JANE SMITH")
cases$defendant_non_residential <- ifelse(grepl(x = cases$defendant_name, pattern = '(?i)\\bestate of?\\b'), FALSE, cases$defendant_non_residential)

# Write out resulting data
if (dir.exists(output_directory) == FALSE) { dir.create(output_directory) }
write.csv(cases, file = paste0(output_directory, '/cases.txt'), row.names = FALSE)
cases_residential_only <- cases[cases$defendant_non_residential == FALSE, ]
write.csv(cases_residential_only, file = paste0(output_directory, '/cases_residential_only.txt'), row.names = FALSE)

# Log file
out <- c('run_date' = as.character(Sys.Date()),
         'time_finished' = format(Sys.time(), '%R'),
         'n_residential_cases' = nrow(cases_residential_only),
         'n_cases' = nrow(cases),
         'min_year_residential_cases' = min(cases_residential_only$filed_year, na.rm = T),
         'max_year_residential_cases' = max(cases_residential_only$filed_year, na.rm = T),
         'n_serial_residential_cases' = sum(cases_residential_only$serial_filing, na.rm = T),
         'n_true_duplicates_removed' = nrow(removed),
         'n_duplicate_case_ids_removed' = length(duplicated_case_ids),
         'duplicate_case_ids_removed' = paste0(duplicated_case_ids, collapse = ', '))
writeLines(con = paste0('log.txt'), text = paste0(names(out), ': ', out))

##############################################################
################### Miscellaneous old code ###################
# Remove commas before business identifiers in plaintiff names
# Drawn from: https://en.wikipedia.org/wiki/List_of_legal_entity_types_by_country#United_States
# LC, LLC, PLLC, LP, LLP, LLLP, CO, CO OP, COOP, CORP, CP, LTD, INC, PB, PBD, FSB, NA, L3C
# cases$plaintiff_name <- stringi::stri_replace_all(cases$plaintiff_name,
#                                                   regex = ', (?=(P?LL?C|LL?L?P|CO( ?OP|RP)?|CP|LTD|INC|PB?C|FSB|NA|L3C)$)',
#                                                   replacement = ' ')
# Rendered (at least temporarily) obsolete by the switch to using plaintiff names cleaned by LSC
