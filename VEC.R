##############################################################
# Run all scripts to update the VEC and deploy app           #
# Authors: Jacob Goldstein-Greenwood                         #
# GitHub: jacob-gg                                           #
# Last revised: 2023-06-24                                   #
##############################################################

# When the app is ready for an update, run this from shell:
# Rscript --vanilla VEC.R

if (grepl('va\\-evictions$', getwd()) != TRUE) {
  stop('VEC.R must be run from the va-evictions directory', call. = FALSE)
}

msg <- '# Running clean.R... #'
pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
cat('\n', pndsgns, '\n', msg, '\n', pndsgns)
source('clean.R')

msg <- '# Running summarize.R... #'
pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
cat('\n', pndsgns, '\n', msg, '\n', pndsgns)
source('summarize.R')

msg <- '# Running deploy.R from /va-evictors-catalog... #'
pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
cat('\n', pndsgns, '\n', msg, '\n', pndsgns)
setwd('va-evictors-catalog')
# source('deploy.R')

msg <- '# Deployed successfully #'
pndsgns <- paste0(rep('#', nchar(msg)), collapse = '')
cat('\n', pndsgns, '\n', msg, '\n', pndsgns)

# Add logging; perhaps email log file to self.
# For the later-in-fall update, consider boxR to
# check the Box for new updates, auto-download into data/updates,
# and use those data as part of the app update.