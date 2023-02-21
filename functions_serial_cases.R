id_serials_inner <- function(z, format = '%Y-%m-%d') {
  z$serial_filing <- NA
  z$latest_filing_in_serial_window <- NA
  z$filed_date <- as.Date(z$filed_date, format = format)

  # If one case --> no serial filings
  if (nrow(z) == 1) {
    z$serial_filing <- FALSE
    z$latest_filing_in_serial_window <- TRUE
    return(z)
  }
  
  if (nrow(z) > 1) {
    cases_span_more_than_1_yr <- (lubridate::interval(min(z$filed_date), max(z$filed_date)) / years(1)) > 1
    
    # If >1 case but all occur <= 1 year, all serial but first case
    if (cases_span_more_than_1_yr == FALSE) {
      z$serial_filing <- ifelse(z$filed_date == min(z$filed_date), FALSE, TRUE)
      z$latest_filing_in_serial_window <- ifelse(z$filed_date == max(z$filed_date), TRUE, FALSE)
      return(z)
    }
    
    # If >1 case and they span >1 year, use iterative process to determine serial "windows"
    if (cases_span_more_than_1_yr == T) {
      max_windows <- ceiling(interval(min(z$filed_date), max(z$filed_date)) / years(1))
      broke_early <- FALSE
      
      for (i in 1:max_windows) {
        if (i == 1) {
          begin_window_1 <- min(z$filed_date)
          end_window_1 <- min(z$filed_date) %m+% months(12)
        }
        if (i > 1) {
          if (max(z$filed_date) <= eval(as.symbol(paste0('end_window_', i-1)))) {broke_early <- TRUE; break}
          assign(paste0('begin_window_', i), min(z$filed_date[z$filed_date > eval(as.symbol(paste0('end_window_', i-1)))]))
          assign(paste0('end_window_', i), eval(as.symbol(paste0('begin_window_', i))) %m+% months(12))
        }
      }
      
      # Identify cases that are at the start of serial windows
      window_beginnings <- paste0('begin_window_', 1:(ifelse(broke_early == TRUE, i-1, i)))
      z$serial_filing <- ifelse(z$filed_date %in% sapply(window_beginnings, function(x) eval(as.symbol(x))), FALSE, TRUE)
      
      # Identify cases that are at the close of serial windows
      window_endings <- paste0('end_window_', 1:(ifelse(broke_early == TRUE, i-1, i)))
      for (j in 1:length(window_endings)) {
        z_sub <- z$filed_date[z$filed_date >= eval(as.symbol(window_beginnings[j])) & z$filed_date <= eval(as.symbol(window_endings[j]))]
        z$latest_filing_in_serial_window[z$filed_date == max(z_sub)] <- TRUE
      }
      z$latest_filing_in_serial_window <- ifelse(is.na(z$latest_filing_in_serial_window), FALSE, z$latest_filing_in_serial_window)
    }
  }
  z
}

id_serials <- function(x) {
  x %>% group_by(plaintiff_name, defendant_name, defendant_zip) %>% group_modify(id_serials_inner) %>% ungroup()
}

# Test data
# z <- cases %>% filter(defendant_name == 'WILLIS, RAPHAEL' & plaintiff_name == 'ENGLAND RUN TOWNHOMES, LP' & defendant_zip == '22406')