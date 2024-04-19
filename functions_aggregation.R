if ('dplyr' %in% devtools::loaded_packages()$package == F) { library(dplyr) }

case_aggregator <- function(dat) {
  case_filed_date_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'date')]
  dat[[case_filed_date_var]] <- as.Date(dat[[case_filed_date_var]], format = '%Y-%m-%d')
  dat
}

defendant_aggregator <- function(dat) {
  case_id_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'id')]
  name_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'name')]
  address_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'address')]
  order_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'order')]
  
  dat <- dat %>% group_by(eval(as.symbol(case_id_var))) %>% 
    arrange(eval(as.symbol(order_var)), .by_group = TRUE) %>% 
    mutate(defendant_count = n(),
           all_defendant_names = paste0(eval(as.symbol(name_var)), collapse = ' | '),
           all_defendant_addresses = paste0(eval(as.symbol(address_var)), collapse = ' | '),
           defendant_order_validation = paste0(eval(as.symbol(order_var)), collapse = ' | ')) %>% 
    filter(eval(as.symbol(order_var)) == 1) %>% ungroup()
  
  dat <- dat %>% select(-contains(c('judgment', 'eval', eval(order_var))))
  dat
} 

plaintiff_aggregator <- function(dat) {
  case_id_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'id')]
  name_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'plaintiff_name')] # `plaintiff_name` instead of `name` to distinguish from `clean_party_name`
  address_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'address')]
  order_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'order')]
  
  dat <- dat %>% group_by(eval(as.symbol(case_id_var))) %>% 
    arrange(eval(as.symbol(order_var)), .by_group = TRUE) %>% 
    mutate(plaintiff_count = n(),
           all_plaintiff_names = paste0(eval(as.symbol(name_var)), collapse = ' | '),
           all_plaintiff_addresses = paste0(eval(as.symbol(address_var)), collapse = ' | '),
           plaintiff_order_validation = paste0(eval(as.symbol(order_var)), collapse = ' | ')) %>% 
    filter(eval(as.symbol(order_var)) == 1) %>% ungroup()
  
  dat <- dat %>% select(-contains(c('judgment', 'eval', eval(order_var))))
  dat
} 

hearing_aggregator <- function(dat, date_format = '%m/%d/%Y', time_format = '%R %p') {
  case_id_var <- colnames(dat)[grepl(x = colnames(dat), pattern = 'id')]
  
  dat <- dat %>% group_by(eval(as.symbol(case_id_var))) %>%
    mutate(date = as.Date(date, format = '%m/%d/%Y')) %>% 
    arrange(desc(date)) %>% 
    summarize(hearing_count = n(),
              disposition_date = date[result %in% c("Judgment", "Default Judgment", "Other")][1],
              disposition = result[result %in% c("Judgment", "Default Judgment", "Other")][1]) %>% 
    ungroup()
  
  dat <- rename_with(dat, function(x) sub('^.+$', {{case_id_var}}, x), contains('eval'))
  dat
}

judgment_aggregator <- function(dat) {
  dat <- dat %>% select(everything(), judgment = starts_with('judgment'))
  dat
}