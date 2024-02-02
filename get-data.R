##############################################################
# Retrieve unlawful detainer case data from LSC Box folder   #
# Authors: Jacob Goldstein-Greenwood                         #
# GitHub: jacob-gg                                           #
# Last revised: 2024-02-02                                   #
##############################################################

# Check packages
if ('boxr' %in% installed.packages() == FALSE) { install.packages('boxr') }
library(boxr)
readRenviron("~/.Renviron")
box_auth(client_id = Sys.getenv('BOX_CLIENT_ID'), client_secret = Sys.getenv('BOX_CLIENT_SECRET'))

# Read setup file
setup <- readLines('setup.txt')

# Check setup file structure and remove some miscellany
if (all(c('use-data-through:', 'original:', 'updates:') %in% setup) == FALSE) {
  stop('setup.txt file must contain lines reading "use-data-through:", "original:", and "updates:"', call. = FALSE)
}
if (grepl(pattern = '\\d{4}-\\d{2}-\\d{2}', x = setup[which(setup == 'use-data-through:') + 1]) == FALSE) {
  stop('line after "use-data-through:" in setup.txt must be of format YYYY-MM-DD', call. = FALSE)
}
setup <- setup[grepl('(^\\#)|(^$)', setup) == FALSE]

# Separate setup file components into list
setup <- list('use-data-through' = setup[which(grepl('use-data-through:', setup)) + 1],
              'original' = setup[which(grepl('original:', setup)) + 1],
              'updates' = setup[(which(grepl('updates:', setup)) + 1):length(setup)])
setup$original <- sapply(setup$original, \(x) ifelse(grepl('\\.zip$', x), x, paste0(x, '.zip')))
setup$updates <- sapply(setup$updates, \(x) ifelse(grepl('\\.zip$', x), x, paste0(x, '.zip')))

# Check directory structure for preexisting data/, and remove/create as needed
if (dir.exists('data')) {
  cat('\ndata/ already exists locally and will be overwritten; OK? [y/n]: ')
  ok <- readLines(con = 'stdin', n = 1)
  if (ok != 'y') {
    stop('process aborted', call. = FALSE)
  } else if (ok == 'y') {
    unlink('data', recursive = TRUE)
  }
}
dir.create('data')

# Retrieve data from Box and put it in the right places
# CD in Box folder with LSC data updates
box_setwd(lapply(box_ls(), function(x) x$id[x$id == Sys.getenv('BOX_ID_LSC_EVICTION_DATA')]) |> unlist())

# Check presence of requested folders
folder_names <- sapply(box_ls(), function(x) x$name)
if (all(c(setup$original, setup$updates) %in% folder_names) == FALSE) {
  stop('all folder names given in setup.txt must be present in the Box folder')
}

# Get original data, unzip, and remove .zip
box_dl(file_id = sapply(box_ls(), \(x) x$id[x$name == setup$original]) |> unlist(),
       local_dir = 'data/')
unzip(paste0('data/', setup$original), exdir = paste0('data/'))
file.rename(from = paste0('data/', gsub('(.+)(\\.zip$)', '\\1', setup$original)), to = 'data/original')
file.remove(paste0('data/', setup$original))

# Get update folders, unzip, and handle files accordingly
# We unfortunately can't just download and unzip each folder, as some zips unzip
# into folders (containing CSVs), and some unzip into a raw set of csvs; so, the
# function below reacts accordingly gets the files in the right place regardless
dir.create('data/updates')

dl_and_move <- function(zip_file_name) {
  box_dl(file_id = sapply(box_ls(), \(x) x$id[x$name == zip_file_name]) |> unlist(),
         local_dir = paste0('data/'))
  pre_files <- dir('data/')
  pre_n_files <- length(pre_files)
  unzip(paste0('data/', zip_file_name), exdir = 'data/')
  post_files <- dir('data/')
  post_n_files <- length(post_files)
  if (post_n_files - pre_n_files == 1) {
    file.copy(from = paste0('data/', gsub('(.+)(\\.zip$)', '\\1', zip_file_name)),
              to = 'data/updates/', recursive = TRUE)
    unlink(paste0('data/', sub('(.+)(\\.zip$)', '\\1', zip_file_name)), recursive = TRUE)
    unlink(paste0('data/', zip_file_name), recursive = TRUE)
  } else {
    dir.create(paste0('data/updates/', sub('(.+)(\\.zip$)', '\\1', zip_file_name)))
    to_move <- post_files[post_files %in% pre_files == FALSE]
    sapply(to_move, function(x) file.copy(from = paste0('data/', x),
                                          to = paste0('data/updates/', sub('(.+)(\\.zip$)', '\\1', zip_file_name), '/')))
    sapply(to_move, function(x) unlink(paste0('data/', x)))
    unlink(paste0('data/', zip_file_name), recursive = TRUE)
  }
}

sapply(setup$updates, dl_and_move)