# Reorganizing files with cleaned and summarized eviction data
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 07-21-2021

###############################################################################
######### RUNNING ALL SCRIPTS AT ONCE WITH `RUN-ALL.R` IS RECOMMENDED #########
###############################################################################
# Ensure that `civilcases` is the working directory                           #
# This code is to be run after data-clean.R and data-summarize.R; it simply   #
#   reorganizes files in the `civilcases` directory                           #
# All cleaned case data files will be moved to a `csvs-cases` folder,         #
#   and all cleaned summary data files will be moved to a                     #
#   `csvs-summaries` folder                                                  #
###############################################################################
###############################################################################
###############################################################################

# Libraries
library(stringi)

# Checks
if (stri_detect(getwd(), regex = '(\\/civilcases$)') == F) {
  stop('civilcases is not the working directory')
}

# Create directories
yearly_cases_dir <- 'csvs-cases'
yearly_summaries_dir <- 'csvs-summaries'
dir.create(yearly_cases_dir)
dir.create(yearly_summaries_dir)

# Identify files to be moved
cleaned_cases <- dir()[stri_detect(dir(), regex = '(cases)(\\_residential_only)?(\\.csv)')]
cleaned_summaries <- dir()[stri_detect(dir(), regex = '^by\\_')]

# Functions for moving files
cases_mover <- function(x) {
  file.copy(from = x, to = paste0(yearly_cases_dir, '/', x))
  file.remove(x)
}
summaries_mover <- function(x) {
  file.copy(from = x, to = paste0(yearly_summaries_dir, '/', x))
  file.remove(x)
}

# Move
sapply(cleaned_cases, FUN = cases_mover)
sapply(cleaned_summaries, FUN = summaries_mover)