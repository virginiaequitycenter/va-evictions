# Shiny app: Plaintiff database
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Last revised: 2022-04-11

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
library(bslib)

# Set required password
password <- 'tenant'

# Load user notes
user_notes <- HTML(readLines('app-user-notes'))
viz_notes <- HTML(readLines('app-viz-notes'))

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

# Title code
title2 <- tags$div(style = "display: inline; position: relative",
                   img(src = "eclogo.png", height='60' 
                       #style="width:100%; max-width:100%; position: absolute; z-index:-1;"
                   ),
                   HTML("&nbsp;"),
                   h1('Housing Justice Atlas: Eviction Filers', style="display: inline;"),
                   HTML("&nbsp;"),
                   img(src="rvalogo.jpg", height='50'
                   )
)

# User interface ----
ui <- fluidPage(theme = bs_theme(version = 5),
                tags$head(
                  tags$style(
                    ".title {margin: auto; width: 1000px;}"
                  )
                ),
                tags$div(class="title", title2),
                
                tags$br(),
                tags$hr(),
                
                
                tags$div(tags$p('This site contains data about plaintiffs filing unlawful detainers (evictions) with the Virginia State District Courts from January 2018 through March of 2021. Each row in the table below represents a plaintiff with a given ZIP code.'),
                         tags$ul(
                           tags$li('Filter the table to a specific court or locality by selecting the court name under Select Court Jurisdiction or by entering a court name into the field above the Court Jurisdiction column in the table.'),
                           tags$li('Display the total filings or the filings for each quarter during this time period by selecting either Totals by Full Period or Totals by Quarter. If selecting by quarter, the table can be further filtered to a specified quarter by typing the quarter into the field above the Time Frame of Cases column in the table.'),
                           tags$li('For any selection of data, the table can be sorted by the number of filings, filings removing serial eviction filings, or filings resulting in eviction judgments (click the up/down arrows next to the column to sort by).'),
                           tags$li('Filter by plaintiff by typing the plaintiff name into the field above Plaintff Name in the table. This can also be used to find individual plaintiffs with variations on the same name.')
                         )),
                tags$hr(),
                
                #mainPanel(canary_message),
                
                fluidRow(#theme = bs_theme(bg = "#B8BCC2", fg = "#202123"),
                  column(1),
                  column(5,
                         wellPanel(
                           selectInput('court', 'Select Court Jurisdiction', 
                                       choices = c('All', unique(plaintiff_dat$court_name)),
                                       multiple = FALSE)
                         )),
                  
                  column(5,
                         wellPanel(
                           radioButtons("time", 'Totals by Full Period/by Quarter',
                                        choices = list("Totals for Period" = "All", "Totals by Quarter" = "Quarter"), 
                                        selected = "All")
                         )),
                ),
                
                tags$hr(),
                
                tabsetPanel(type = 'tabs',
                            tabPanel('Data Table', DTOutput('plaintiff_table')),
                            tabPanel('Visuals', uiOutput('viz')),
                            tabPanel('Data Notes', uiOutput('notes'))),
                tags$hr()
)

# Server ----
server <- function(input, output) {
  
}

# Run app
shinyApp(ui = ui, server = server)