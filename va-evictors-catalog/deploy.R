################################################################################
# Deploy the Virginia Evictors Catalog to shinyapps.io                         #
# Run from the project folder:  source("deploy.R")                             #
################################################################################

# Apps to deploy to (names as recorded in rsconnect/shinyapps.io/). Remove one
# from this list to deploy to only the other.
app_names <- c("va-evictors-catalog", "plaintiff-database-Shiny")
account   <- "virginiaequitycenter"

# Files to upload ----
# Built from the folders, so new html/, www/, or translation files are picked up
# without editing this script. Hidden files such as .DS_Store are skipped.
app_files <- c(
  "app.R",
  list.files(pattern = "^data-.*[.]txt$"),
  list.files(c("html", "translations", "www"), recursive = TRUE, full.names = TRUE)
)

# Safety check ----
# Every local file app.R names in quotes (e.g. "html/writ-note") must be in the
# upload, or the live app will crash on startup.
app_code   <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
referenced <- unique(regmatches(app_code, gregexpr('"[^"[:space:]]+"', app_code))[[1]])
referenced <- gsub('"', "", referenced)
referenced <- referenced[file.exists(referenced) & !dir.exists(referenced)]
missing    <- setdiff(referenced, app_files)
if (length(missing) > 0) {
  stop("app.R uses these files, but they are not in the upload:\n  ",
       paste(missing, collapse = "\n  "))
}

message("Uploading ", length(app_files), " files to: ", paste(app_names, collapse = ", "))

# Deploy ----
for (app_name in app_names) {
  rsconnect::deployApp(appName  = app_name,
                       appFiles = app_files,
                       account  = account)
}
