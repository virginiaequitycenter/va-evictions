# Using fuzzy-matching to "defuzz" plaintiff names
#   This script also applies bespoke plaintiff-name typo corrections specified in `corrections_file`
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Last revised: 2021-12-16

################# Customize as desired to defuzz plaintiff names for a specific locality #################
court_of_interest <- 'Albemarle General District Court'
defuzz_only_cases_with_residential_defendants <- T
cases_directory <- 'processed-data'
corrections_file <- 'defuzz-bespoke-corrections.csv' # Note: col names should be: misspelling; correction
non_residential_plaintiff_regex_file <- 'non-residential-plaintiff-regex.R'
##########################################################################################################

# Packages
library(dplyr)
library(stringi)
library(fuzzyjoin)

# Read in cleaned case data and subset cases for the court of interest
cases_file <- ifelse(defuzz_only_cases_with_residential_defendants == T,
                     paste0(cases_directory, '/cases_residential_only.csv'),
                     paste0(cases_directory, '/cases.csv'))
cases_to_defuzz <- read.csv(cases_file)
cases_to_defuzz <- cases_to_defuzz[cases_to_defuzz$court_name == court_of_interest, ]

# Apply bespoke corrections to plaintiff names, as specified in `corrections_file`
bespoke_name_corrector <- function(vec_to_correct = cases_to_defuzz$pla_1, corrections_df = corrections_file) {
  corrections <- read.csv(corrections_file, header = T)
  if (all(colnames(corrections) == c('misspelling', 'correction')) == F) {
    stop('Column names of `corrections_file` must be: misspelling, correction')}
  corrections[, 1] <- paste0('(?i)\\b', toupper(corrections[, 1]), '\\b')
  corrections[, 2] <- toupper(corrections[, 2])
  for (i in 1:nrow(corrections)) {
    vec_to_correct <- stringi::stri_replace_all(vec_to_correct, regex = corrections[i, 1], replacement = corrections[i, 2])
  }
  vec_to_correct
}
cases_to_defuzz$pla_1 <- bespoke_name_corrector(cases_to_defuzz$pla_1, corrections)

# The following function is used to extract a "defuzzed" plaintiff name for each input
# Specifically:
#     1. For a given plaintiff name, x (the input), create a one-row data frame like:
#             pla_1   match
#              John      1
#          Call this data frame "x_input"
#     2. Subset from the full data frame containing all cases/plaintiff names only rows with
#          plaintiff names that begin with the same two characters as x, *ignoring spaces* (call this data frame "sub_df")
#     3. Perform a fuzzy RIGHT join by `pla_1` between the x_input and sub_df
#     4. This results in a data frame of length nrow(sub_df) in which each row has either been matched
#          or not with the input plaintiff name, x
#     5. Subset only those plaintiff names from sub_df that have been fuzzy matched with x
#     6. If the input plaintiff name has a corresponding plaintiff ZIP (non-NA), then only keep in the subset
#          from (5) names where *either* the plaintiff ZIP matches that associated with the input name *or*
#          the plaintiff ZIP is NA
#     6. Pull the shortest of those plaintiff names and consider that the "defuzzed" name of x
# Note that the function does not produce typo-proof defuzzed name, as it selects the shortest
#     of the possible matches (this is a modifiable criterion)
fuzzy_ider <- function(name_zip_vec, right_side_data, fuzzy_method = 'lv', upper_dist_bound = 5) {
  # Create objects for plaintiff name and defendant ZIP
  x <- name_zip_vec[1]
  zip <- name_zip_vec[2]
  # Get first 2 characters of plaintiff name; use those to subset possible fuzzy matches from full case data
  #   Note: In testing for match of first 2 characters, we ignore spaces; this is to ensure that rare
  #         cases like "B & L" can be matched to "B&L"
  x_first_two_chars <- substr(gsub(x = x, pattern = '\\s{1,}', replacement = ''), 1, 2)
  right_side_data <- right_side_data[grep(pattern = paste0('(?i)^', x_first_two_chars),
                                          x = gsub(x = right_side_data$pla_1, pattern = '\\s{1,}', replacement = '')), ]
  # Apply fuzzy matching process to identify possible final defuzzed names
  temp <- stringdist_right_join(data.frame(pla_1 = x, match = 1),
                                right_side_data,
                                by = 'pla_1',
                                method = fuzzy_method,
                                max_dist = upper_dist_bound)
  temp <- temp[is.na(temp$match) == F, ]
  # If the input plaintiff name has a non-NA plaintiff ZIP associated with it, only keep in set of fuzzy-matched
  # names those names that have the same plaintiff ZIP associated with them OR that have NA as a plaintiff ZIP
  if (is.na(zip) == F) {
    temp <- temp[temp$pla_1_zip == zip | is.na(temp$pla_1_zip), ]
  }
  # Pull out vector of potential defuzzed names and only keep the unique ones
  fuzzy_matched_plaintiff_names <- unique(temp[temp$match == 1, 'pla_1.y'])
  # Pick a final defuzzed name by selecting the shortest of the unique possibilities
  defuzzed_name <- fuzzy_matched_plaintiff_names[nchar(fuzzy_matched_plaintiff_names) == min(nchar(fuzzy_matched_plaintiff_names))]
  # If there's more than one equally short defuzzed name, select the first one
  if (length(defuzzed_name) > 1) {defuzzed_name <- defuzzed_name[1]} else {defuzzed_name <- defuzzed_name}
  # Return final defuzzed name
  defuzzed_name
}

# We apply a different defuzzing procedure to *residential* and *non-residential* plaintiffs (as best as we can identify them)
# This is because of the following:
#   Applying the same "defuzzing" procedure across all plaintiff names at once using the same parameters (e.g., Levenshtein
#   distance with max edit distance = 5), results in several instances of defuzzed plaintiff names simply being *wrong*; for example:
#       - Input "JOHNSON, MARTHA K" would be fuzzy-matched per the above parameters with "JOHNSON, MARTHA K" and "JOHNSON, MARTY K"
#       - The defuzzed name selected is the *shorter* of all fuzzy-matched names
#           - (We have to pick *some* criterion selecting one out of several fuzzy-matched names)
#       - This means that the defuzzed name of "JOHNSON, MARTHA K" would be wrongly identified as "JOHNSON, MARTY K"
# From several tests (Levenshtein distance; LCS; Jaro-Winkler) using different parameters, this problem
#   appears to emerge most-often with *residential* names, where minute string edits can lead from one real name to another, e.g.:
#   "Smith, Martin" --> "Smith, Martha" --> "Smith, Marty" --> "Smith, Mark" --> etc.
# We therefore apply a more-conservative fuzzy-matching criterion to *residential* plaintiff names than *non-residential* ones

# Read in regex used for identifying residential/non-residential plaintiffs
non_res_pla_pattern <- source(non_residential_plaintiff_regex_file)$value
# Apply regex to plaintiff field
cases_to_defuzz$non_res_plaintiff <- stri_detect(cases_to_defuzz$pla_1, regex = non_res_pla_pattern)
# Break data up by residential and non-residential plaintiffs
res_pla <- cases_to_defuzz[cases_to_defuzz$non_res_plaintiff == F, ]
non_res_pla <- cases_to_defuzz[cases_to_defuzz$non_res_plaintiff == T, ]

# Fuzzy match non-residential plaintiffs (using less-conservative criterion)
# Levenshtein distance, using edit-dist of 5
non_res_pla$defuzzed_name <- apply(non_res_pla[, c('pla_1', 'pla_1_zip')], 1,
                                   function(x) fuzzy_ider(x, right_side_data = non_res_pla, fuzzy_method = 'lv', upper_dist_bound = 5))
non_res_pla <- non_res_pla %>% select(defuzzed_name, everything())

# Fuzzy match residential plaintiffs (using more-conservative criterion)
# Levenshtein distance, using edit-dist of 3
res_pla$defuzzed_name <- apply(res_pla[, c('pla_1', 'pla_1_zip')], 1,
                               function(x) fuzzy_ider(x, right_side_data = res_pla, fuzzy_method = 'lv', upper_dist_bound = 3))
res_pla <- res_pla %>% select(defuzzed_name, everything())

# Recombine into one data frame
defuzzed_cases <- rbind(non_res_pla, res_pla)

# Save defuzzed output
defuzzed_filename <- ifelse(defuzz_only_cases_with_residential_defendants == T,
                            paste0(cases_directory, '/cases_residential_only_defuzzed.csv'),
                            paste0(cases_directory, '/cases_defuzzed.csv'))
write.csv(defuzzed_cases, file = defuzzed_filename, row.names = F)