##############################################################
# Run all scripts to update the VEC and deploy app           #
# Authors: Jacob Goldstein-Greenwood                         #
# GitHub: jacob-gg                                           #
# Last revised: 2023-08-21                                   #
##############################################################

######################### Instructions #######################
# When the app is ready for an update, run this from shell:  #
# Rscript --vanilla VEC.R <arg>                              #
# <arg> must be one of: UD, DA, UDaDA                        #
#   - UD = update data only                                  #
#   - DA = depoy app only                                    #
#   - UDaDA = update data and deploy app                     #
##############################################################

args <- commandArgs(trailingOnly = TRUE)
if ((length(args) == 1 && args[1] %in% c('UD', 'DA', 'UDaDA')) == FALSE) {
  stop('Input argument must be "UD" or "UDaDA"', call. = FALSE)
}

if (grepl('va\\-evictions$', getwd()) != TRUE) {
  stop('VEC.R must be run from the va-evictions directory', call. = FALSE)
}

if (args[1] == 'UD' | args[1] == 'UDaDA') {
  msg <- '# Running clean.R... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  source('clean.R')
  
  msg <- '# Running summarize.R... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  source('summarize.R')
}

if (args[1] == 'DA' | args[1] == 'UDaDA') {
  msg <- '# Running deploy.R from /va-evictors-catalog... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  setwd('va-evictors-catalog')
  source('deploy.R')
  msg <- '# Deployed successfully #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
}

# Add logging; perhaps email log file to self.
# For the later-in-fall update, consider boxR to
# check the Box for new updates, auto-download into data/updates,
# and use those data as part of the app update.