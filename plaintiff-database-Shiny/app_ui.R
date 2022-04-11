# Shiny app: Plaintiff database
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Last revised: 2022-04-11

# ################################################# CANARY #################################################
# canary_message <- HTML(paste0('<br><font color="red">NOTE: the database contains data that has been ',
#                               ' processed to deduplicate plaintiff names through fuzzy matching.',
#                               ' This process has not yet been completed for the cities of Norfolk, ',
#                               ' Newport News, or Richmond.</font><br><br>'))
# ##########################################################################################################

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
# The app needs to be able to read in some version of the underlying
# data to generate the jurisdiction list. Because we don't want to
# upload the data to a public repo, for the app_ui.R version I'm 
# creating a toy data set to read in that will contain the court
# names. The final app reads in the full plaintiff data.

plaintiff_dat <- read.csv('plaintiff_courtnamesonly.csv')


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