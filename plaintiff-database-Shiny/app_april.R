# Shiny app: Plaintiff database
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Last revised: 2022-03-02

################################################# CANARY #################################################
canary_message <- HTML(paste0('<br><font color="red">NOTE: the database contains data that has been ',
                              ' processed to deduplicate plaintiff names through fuzzy matching.',
                              ' This process has not yet been completed for the cities of Norfolk, ',
                              ' Newport News, or Richmond.</font><br><br>'))
##########################################################################################################

# Packages
library(shiny)
library(DT)
library(dplyr)
library(shinyalert)

# Set required password
password <- 'tenant'

# Load user notes
user_notes <- HTML(readLines('app-user-notes'))

# Preprocess ----
# Note: consider reading in defuzzed data and aggregating in app...

plaintiff_dat <- read.csv('plaintiff-aggregated-data.txt', colClasses = 'character')
# Make certain variables numeric so that sorting (e.g, cases hi-->lo) works appropriately
plaintiff_dat <- plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                          cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                          plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  arrange(desc(cases_filed))
# plaintiff_dat <- plaintiff_dat %>% select(-filing_years)
plaintiff_dat$pla_1_zip <- ifelse(is.na(plaintiff_dat$pla_1_zip), 'NA', plaintiff_dat$pla_1_zip)

# ADD SELECTION OF DATA BY QUARTER
quarterly_plaintiff_dat <- read.csv('quarterly-plaintiff-aggregated-data.txt', colClasses = 'character')
quarterly_plaintiff_dat <- quarterly_plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                          cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                          plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  arrange(desc(cases_filed))
quarterly_plaintiff_dat$pla_1_zip <- ifelse(is.na(quarterly_plaintiff_dat$pla_1_zip), 'NA', quarterly_plaintiff_dat$pla_1_zip)


# User interface ----
ui <- fluidPage(
    titlePanel('Unlawful Detainer Plaintiff Data, Virginia, 2018 - 2021 Q1'),
    mainPanel(canary_message),
    selectInput('court', 'Choose court', choices = c('All', unique(plaintiff_dat$court_name))),
    radioButtons("time", 'All Data/Quarterly Data',
                 choices = list("All Data" = "All", "Quarterly Data" = "Quarter"), 
                 selected = "All"), 
    
    tabsetPanel(type = 'tabs',
                tabPanel('Data', DTOutput('plaintiff_table')),
                tabPanel('User Notes', uiOutput('notes'))),
    hr()
)

# Server ----
server <- function(input, output) {

  shinyalert(html = TRUE, text = tagList(
    textInput("pass", "Password: ", ),
  ))

  get_locality_dat <- reactive({

    req(input$pass == password)

    if (input$time == 'All' & input$court == 'All') {
      locality_dat <- plaintiff_dat
    } else if (input$time == 'All' & input$court != 'All') { 
      locality_dat <- plaintiff_dat[plaintiff_dat$court_name == input$court, ] %>% dplyr::arrange(desc(cases_filed))
    } else if (input$time == 'Quarter' & input$court == 'All') {
      locality_dat <- quarterly_plaintiff_dat
    } else {
      locality_dat <- quarterly_plaintiff_dat[quarterly_plaintiff_dat$court_name == input$court, ] %>% dplyr::arrange(desc(cases_filed))
    }
  
    cases_filed_col_no <- which(colnames(locality_dat) == 'cases_filed')
    dt <- datatable(locality_dat, rownames = F,
                    caption = 'Sources: Ben Schoenfeld (virginiacourtdata.org)',
                    class = 'display nowrap',
                    filter = 'top',
                    options = list(searchHighlight = T,
                                   scrollX = T,
                                   pageLength = 20,
                                   order = list(cases_filed_col_no, 'desc')),
                    colnames = c('Court Jurisdiction', 'Plaintiff Name', 'Plaintiff ZIP', '# Filings', 'Serial-adjusted # Filings',
                                 '# Eviction Judgments', 'ZIPs of Defendants',
                                 'Time Frame of Cases'))
      # %>% formatStyle(columns = 'court_name', background = 'lightblue') <-- if column colors are desired
    dt
  })
  output$plaintiff_table <- DT::renderDT({get_locality_dat()})
  output$notes <- renderUI(user_notes)
}

# Run app
shinyApp(ui = ui, server = server)