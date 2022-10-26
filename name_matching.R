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

######################################### jgg name-match testing ######################################### 
# Name-replacement function (arguments: a plaintiff name and a table of corrections like cvlnames or albnames)
replace_pla1_name <- function(pla_1, locality_names) {
  ifelse(tolower(pla_1) %in% tolower(locality_names$AlternativeNames), locality_names$PrimaryName[tolower(locality_names$AlternativeNames) == tolower(pla_1)], pla_1)
}
test_nomatch$pla_1_defuzzed <- sapply(test_nomatch$pla_1, function(x) replace_pla1_name(x, cvlnames))

# Checks:
#   1. Number of unique names is reduced, as expected
length(unique(test_nomatch$pla_1))
length(unique(test_nomatch$pla_1_defuzzed))
#   2. We want the following corrections
#         GARDENWOOD AARTMENTS LLC --> GARDENWOOD APARTMENTS LLC
#         GARDENWOOD APARTMENTS --> GARDENWOOD APARTMENTS LLC
#         And leave GARDENWOOD APARTMENTS LLC as GARDENWOOD APARTMENTS LLC
View(test_nomatch[grepl(x = test_nomatch$pla_1, pattern = 'GARDENWO'), c('pla_1', 'pla_1_defuzzed')])

# This process can be generalized to defuzz to >1 locality at a time, or a similar line
# can just be written twice if the manual defuzzing is only to be applied to Cville + Alb.
######################################################################################################