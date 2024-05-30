##############################################################
# Run all scripts to update the VEC and deploy app           #
# Authors: Jacob Goldstein-Greenwood                         #
# GitHub: jacob-gg                                           #
# Last revised: 2023-09-20                                   #
##############################################################

###################### Instructions ##########################
# When the app is ready for an update, run this from shell:  #
# Rscript --vanilla VEC.R <arg>                              #
# <arg> must be one of:                                      #
# 'GD', 'UD', 'DA', 'GDaUD', 'UDaDA', 'GDaUDaDA'             #
#   - GD = get data only                                     #
#   - UD = update data only                                  #
#   - DA = deploy app only                                   #
#   - GDaUD = get data and update data only                  #
#   - UDaDA = update data and deploy app only                #
#   - GDaUDaDA = get data and update data and deploy app     #
##############################################################

#################### Troubleshooting #########################
# Error in console:
#       Auto-refreshing stale OAuth token.
#       Error in test_request() : 
#         Unauthorized (HTTP 401). Failed to connect to box.com API.
#       Calls: source ... eval -> eval -> box_auth -> test_request -> <Anonymous>
#         In addition: Warning message:
#         Unable to refresh token: invalid_grant
#       Refresh token has expired 
#       Execution halted
# Solution: Run the following in RStudio console:
#           boxr::box_fresh_auth()
##############################################################

args <- commandArgs(trailingOnly = TRUE)
if ((length(args) == 1 && args[1] %in% c('GD', 'UD', 'DA', 'GDaUD', 'UDaDA', 'GDaUDaDA')) == FALSE) {
  stop('Input argument must be "GD", "UD", "DA", "GDaUD", "UDaDA" or "GDaUDaDA"', call. = FALSE)
}

if (grepl('va\\-evictions$', getwd()) != TRUE) {
  stop('VEC.R must be run from the va-evictions directory', call. = FALSE)
}

if (args[1] %in% c('GD', 'GDaUD', 'GDaUDaDA')) {
  msg <- '# Running get-data.R... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  source('get-data.R')
}

if (args[1] %in% c('UD', 'GDaUD', 'UDaDA', 'GDaUDaDA')) {
  msg <- '# Running clean.R... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  source('clean.R')
  
  msg <- '# Running summarize.R... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  source('summarize.R')
}

if (args[1] %in% c('DA', 'UDaDA', 'GDaUDaDA')) {
  msg <- '# Running deploy.R from /va-evictors-catalog... #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
  setwd('va-evictors-catalog')
  source('deploy.R')
  msg <- '# Deployed successfully #'
  pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
  cat('\n', pndsgns, '\n', msg, '\n', pndsgns, '\n')
}