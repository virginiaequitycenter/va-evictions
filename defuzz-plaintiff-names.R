# Defuzzing plaintiff names within each locality
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Last revised: 2022-02-23

#########################
# *** Need to add process notes here ***
# Note that this code selectively defuzzes plaintiffs for which a defuzzed name is absent...
#     So selection of cases file matters...
# Applies diff defuzz process to res/non-res...
# Restricts defuzz matches to SAME ZIP + same-first-three + within MAX_DIST from name to be matched...
# Make sure you correctly pick whether you want to defuzz cases that have not previously been defuzzed, or whether you're defuzzing new cases
#########################

cases_directory <- 'processed-data'
cases_to_defuzz_file <- 'cases_residential_only.txt'
non_residential_plaintiff_regex_file <- 'non-residential-plaintiff-regex.R'

# Packages
library(dplyr)
library(stringi)
library(fuzzyjoin)

# Read in cleaned case data
cases_to_defuzz <- read.csv(paste0(cases_directory, '/', cases_to_defuzz_file), sep = ',', colClasses = 'character')

# Identify residential/non-residential plaintiffs, as a slightly different defuzzing process is applied to each group within a locality
non_residential_plaintiff_regex <- source(non_residential_plaintiff_regex_file)$value
cases_to_defuzz$non_residential_plaintiff <- stri_detect(cases_to_defuzz$pla_1, regex = non_residential_plaintiff_regex)

################################################ Defuzzing functions ################################################
# defuzz_all()          Takes full data set of cases, groups it by court and passes to defuzz_locality()            #
# defuzz_locality()     Takes one locality's (court's) data set, splits it into cases against residential and       #
#                         non-residential defendants, sequentially passes plaintiff names to defuzz_a_plaintiff()   #
#                         with the appropriate arguments, and saves the defuzzed data for that locality             #
#                         as a .txt after the process is done                                                       #
#                         (note that only names that have not previously been defuzzed are defuzzed here)           #
# defuzz_a_plaintiff()  Finds a defuzzed match for a given plaintiff name                                           #
#####################################################################################################################
defuzz_a_plaintiff <- function(name_zip_vec, right_side_data, upper_dist_bound) {
  x <- name_zip_vec[1]
  zip <- name_zip_vec[2]
  # Reduce potential matches based on plaintiff ZIP Code; vectorize input data after ZIP-based subsetting
  if (is.na(zip) == T) {
    cat('')
    right_side_data <- right_side_data$pla_1
    } else {
      cat('')
      right_side_data <- right_side_data$pla_1[right_side_data$pla_1_zip %in% c(zip, NA)]
  }
  # Reduce potential matches based on whether 1st 3 chars of potential matches match 1st 3 of name
  x_first_three_chars <- paste0(unlist(stri_extract_all(x, regex = '\\w|\\d|#'))[1:3], collapse = '')
  first_three_chars_all_plas <- sapply(right_side_data, function(p) paste0(unlist(stri_extract_all(p, regex = '\\w|\\d|#'))[1:3], collapse = ''))
  right_side_data <- right_side_data[first_three_chars_all_plas == x_first_three_chars]
  # Reduce potential matches based on whether diff(nchar(potential matches), nchar(name)) <= max edit distance for match
  right_side_data <- right_side_data[abs(nchar(right_side_data) - nchar(x)) <= upper_dist_bound]
  # Only retain *unique* potential matches
  right_side_data <- unique(right_side_data)
  cat(paste0('(', length(right_side_data), ')'))
  # Perform fuzzy match
  temp <- stringdist_right_join(data.frame(pla_1 = x),
                                data.frame(pla_1 = right_side_data), by = 'pla_1',
                                method = 'lv', max_dist = upper_dist_bound)
  temp <- temp[is.na(temp$pla_1.x) == F, ]
  if (nrow(temp) == 1) {return(x)}
  fuzzy_matched_plaintiff_names <- unique(temp$pla_1.y)
  # Final defuzzed name is shortest of matches
  defuzzed_name <- fuzzy_matched_plaintiff_names[order(nchar(fuzzy_matched_plaintiff_names), decreasing = F)][1]
}

defuzz_locality <- function(one_locality, ...) {
  cat(paste0('\n\nNumber of potential matches for each plaintiff in ', unique(one_locality$court_name),
             ' that has not been previously defuzzed: '))
  # See if `defuzzed_pla` is already present in locality data; add if not
  if ('defuzzed_pla' %in% colnames(one_locality)) {
    one_locality <- one_locality %>% select(defuzzed_pla, everything())
  } else {
    one_locality$defuzzed_pla <- NA
    one_locality <- one_locality %>% select(defuzzed_pla, everything())
  }
  # Identify whether defuzzing has already occurred for each plaintiff
  one_locality$already_defuzzed <- ifelse(is.na(one_locality$defuzzed_pla) == F, T, F)
  one_locality <- one_locality %>% select(already_defuzzed, everything())
  # Pull out yet-to-be-defuzzed rows and split those into residential/non-residential cases
  to_defuzz <- one_locality[one_locality$already_defuzzed == F, ]
  to_defuzz_res_pla <- to_defuzz[to_defuzz$non_residential_plaintiff == F, ]
  to_defuzz_non_res_pla <- to_defuzz[to_defuzz$non_residential_plaintiff == T, ]
  # Split whole locality data---which contains potential defuzzing matches---into res/non-res cases
  res_pla <- one_locality[one_locality$non_residential_plaintiff == F, ]
  non_res_pla <- one_locality[one_locality$non_residential_plaintiff == T, ]
  # Apply defuzzing process to cases in need of defuzzing, using the full res/non-res data as potential matches
  to_defuzz_res_pla$defuzzed_pla <- apply(to_defuzz_res_pla[, c('pla_1', 'pla_1_zip')], 1,
                                          function(inp) try(defuzz_a_plaintiff(as.vector(inp), res_pla, upper_dist_bound = 3)))
  to_defuzz_non_res_pla$defuzzed_pla <- apply(to_defuzz_non_res_pla[, c('pla_1', 'pla_1_zip')], 1,
                                              function(inp) try(defuzz_a_plaintiff(as.vector(inp), non_res_pla, upper_dist_bound = 5)))
  # Subset only previously defuzzed cases from one_locality
  previously_defuzzed_res_pla <- res_pla[res_pla$already_defuzzed == T, ]
  previously_defuzzed_non_res_pla <- non_res_pla[non_res_pla$already_defuzzed == T, ]
  # Bind newly defuzzed and previously defuzzed cases together
  out_dat <- rbind(to_defuzz_res_pla, to_defuzz_non_res_pla, previously_defuzzed_res_pla, previously_defuzzed_non_res_pla) %>%
    select(defuzzed_pla, everything()) %>% select(-already_defuzzed)
  # Write locality's defuzzed data to the directory
  court_path <- paste0(cases_directory, '/defuzzed-by-court/', unique(out_dat$court_name), ' defuzzed plaintiffs.txt')
  write.csv(out_dat, file = court_path, row.names = F, sep = ',')
}

defuzz_all <- function(all_localities) {
  if (dir.exists(paste0(cases_directory, '/defuzzed-by-court')) == F) {dir.create(paste0(cases_directory, '/defuzzed-by-court'))}
  court_split_data <- split.data.frame(all_localities, all_localities$court_name)
  # The lapply() call will result in K data sets for K localities being placed in the directory referenced above
  lapply(court_split_data, function(x) defuzz_locality(x))
}

bind_together <- function(folder) {
  defuzzed_indiv_localities <- dir(paste0(cases_directory, '/defuzzed-by-court'))
  defuzzed_indiv_localities <- lapply(defuzzed_indiv_localities,
                                      function(x) read.csv(paste0(cases_directory, '/defuzzed-by-court/', x),
                                                           header = T, sep = ',', colClasses = 'character'))
  defuzzed_localities <- bind_rows(defuzzed_indiv_localities)
  defuzzed_localities
}

# defuzzed_cases <- defuzz_all(cases_to_defuzz)
defuzzed_filename <- paste0(cases_directory, '/defuzzed_', cases_to_defuzz_file)
write.csv(defuzzed_cases, file = defuzzed_filename, row.names = F, sep = ',')

######################################################################################################################
# Possible edits:
# - For residential names: IF [single letter present, like SMITH, JOHN L] THEN [eliminate from set of possible matches
#                              names with single letters OTHER than the one detected]
# Test: Does the process correctly defuzz just cases with missing defuzzed plaintiff names?
# ttttttttt <- cases_to_defuzz[cases_to_defuzz$court_name %in% c('Surry General District Court', 'Amelia General District Court'), ]
# aaa <- defuzz_all(ttttttttt)
# bbb <- aaa
# bbb$defuzzed_pla[sample(1:nrow(bbb), 25)] <- NA
# bbb <- defuzz_all(bbb)
# aaa <- aaa[order(aaa$id), ]
# rownames(aaa) <- 1:nrow(aaa)
# bbb <- bbb[order(bbb$id), ]
# rownames(bbb) <- 1:nrow(bbb)
# all(aaa == bbb, na.rm = T)