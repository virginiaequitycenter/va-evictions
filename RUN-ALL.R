# Code for sequentially running eviction data cleaning and analysis scripts
# Authors: Jacob Goldstein-Greenwood, Michele Claibourn
# Last revised: 08-01-2021

###############################################################################
# Run all lines below to run all scripts for cleaning and summarizing         #
#   Virginia eviction data at once                                            #
# Note: This code takes ~30 minutes to run on a computer with an i5 processor #
#   and 8 GB of RAM                                                           #
# Note: The code below will install any of the following packages if they are #
#   not installed: stringi, tidyverse (incl. readr, rvest, lubridate), DT     #
###############################################################################

# Check for packages and install if necessary
reqd <- c('stringi', 'tidyverse', 'readr', 'rvest', 'lubridate', 'DT', 'tigris')
if (any(!reqd %in% installed.packages())) {
  to_install <- reqd[!reqd %in% installed.packages()]
  install.packages(to_install, character.only = T)
}
# Run scripts
library(stringi)
begin <- Sys.time()
if (stri_detect(getwd(), regex = '(\\/civilcases$)') &
    all(c('data-clean.R', 'data-non-residential-regex.R', 'data-summarize.R', 'data-summarize-flag-cells.Rmd', 'data-file-organize.R') %in% dir()) &
    any(stri_detect(dir(), regex = '(\\d{4})') & stri_detect(dir(), regex = 'DistrictCivil'))) {
  source('data-clean.R')
  source('data-summarize.R')
  source('data-file-organize.R')
} else {
  stop('Ensure that `civilcases` is the working directory\nEnsure that data-clean.R, data-summarize.R, data-summarize-flag-cells.Rmd, data-file.organize.R, and data-non-residential-regex.R are all present in `civilcases`\nEnsure that there is >= 1 folder in `civilcases` with `DistrictCivil` and a four-digit year in its name')
}
fin <- Sys.time()
cat('Running all scripts took', round(fin - begin, digits = 2), 'minutes')