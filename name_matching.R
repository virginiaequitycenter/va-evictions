# think through name matching via list
library(tidyverse)

# processed data
data_folder <- 'processed-data'
data_file <- "cases_residential_only.txt"
cases_to_aggregate <- read.csv(paste0(data_folder, '/', data_file), colClasses = 'character')


# Experiment with list method of correcting names
#   use manually curated list of same names to match and replace
# pull in charlottesville and albemarle plaintiff_names sheets
library(googlesheets4)
sheet_url <- "https://docs.google.com/spreadsheets/d/1WhWmluKv4hA721jUT_y3u9ESB2WiPJ3MF05ji8LgPmE/edit?usp=sharing"
cvlnames <- read_sheet(sheet_url, sheet = "Charlottesville") %>% as.list()
albnames <- read_sheet(sheet_url, sheet = "Albemarle")

sum(cases_to_aggregate$pla_1 %in% cvlnames$AlternativeNames)
sum(cases_to_aggregate$pla_1 %in% albnames$AlternativeNames)

test_nomatch <- cases_to_aggregate %>% 
  filter(fips == "540")

# this creates a vector of row names
test_vector <- test_nomatch[which(test_nomatch$pla_1 %in% cvlnames$AlternativeNames), ] %>% row.names() 

# next step: figure out how to apply a function across vector
# of row numbers that
# replaces pla_1 with correctly indexed value of primary name