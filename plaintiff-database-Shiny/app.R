# Shiny app: Plaintiff database
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Author: Michele Claibourn / mclaibourn@virginia.edu / GitHub: mclaibourn
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
library(tidyverse)
library(shinyalert)
library(bslib)
library(plotly)

# Set required password
password <- 'tenant'

# Load user notes
user_notes <- HTML(readLines('app-user-notes'))
orient_notes <- HTML(readLines('app-orient-notes'))

# Preprocess ----
# Note: consider reading in defuzzed data and aggregating in app...

plaintiff_dat <- read.csv('plaintiff-aggregated-data.txt', colClasses = 'character')
# Make certain variables numeric so that sorting (e.g, cases hi-->lo) works appropriately
plaintiff_dat <- plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                          cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                          plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_years, .before = def_zips)
plaintiff_dat$pla_1_zip <- ifelse(is.na(plaintiff_dat$pla_1_zip), 'NA', plaintiff_dat$pla_1_zip)

# Add attributes
attributes(plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
attributes(plaintiff_dat$cases_filed_excluding_all_but_final_serial)  <- list(labels = "Number of Non-Serial Cases Filed")
attributes(plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")

# ADD SELECTION OF DATA BY QUARTER
quarterly_plaintiff_dat <- read.csv('quarterly-plaintiff-aggregated-data.txt', colClasses = 'character')
quarterly_plaintiff_dat <- quarterly_plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                                              cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                                              plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_quarter, .before = def_zips)
quarterly_plaintiff_dat$pla_1_zip <- ifelse(is.na(quarterly_plaintiff_dat$pla_1_zip), 'NA', quarterly_plaintiff_dat$pla_1_zip)

# Add attributes
attributes(quarterly_plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
attributes(quarterly_plaintiff_dat$cases_filed_excluding_all_but_final_serial)  <- list(labels = "Number of Non-Serial Cases Filed")
attributes(quarterly_plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")


# Title code
title2 <- tags$div(style = "display: inline; position: relative;",
                   (tags$a(img(src = "eclogo.png", height='60'),href="https://virginiaequitycenter.org/", target="_blank")),
                   HTML("&nbsp;"),
                   h1('Housing Justice Atlas: Eviction Filers', style="display: inline;"),
                   HTML("&nbsp;"),
                   (tags$a(img(src="rvalogo.jpg", height='50'),href="https://rampages.us/rvaevictionlab/", target="_blank")
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
                #mainPanel(canary_message),
                
                fluidRow(
                  column(12,
                  htmlOutput('orient')
                )),
                
                tags$hr(),
                
                fluidRow(#theme = bs_theme(bg = "#B8BCC2", fg = "#202123"),
                  column(5,
                         wellPanel(
                           selectInput('court', 'Select Court Jurisdiction',
                                       multiple = TRUE,
                                       choices = unique(plaintiff_dat$court_name),
                                       selected = unique(plaintiff_dat$court_name),
                                       size = 5,
                                       selectize = FALSE)
                         )),
                  
                  column(3,
                         wellPanel(
                           radioButtons("time", 'Totals to Display',
                                        choices = list("Totals for Full Period" = "All", "Totals by Quarter" = "Quarter"), 
                                        selected = "All")
                         )),
                  
                  column(3,
                         wellPanel(
                           radioButtons("var", 'Outcome to Visualize',
                                        choices = list("Number of Cases Filed" = "cases_filed",
                                                       "Number of Non-Serial Cases Filed" = "cases_filed_excluding_all_but_final_serial",
                                                       "Number of Eviction Judgments" = "plaintiff_judgments")
                           ))
                  ),
                  
                  tags$hr(),
                  
                  tabsetPanel(type = 'tabs',
                              tabPanel('Table', DTOutput('plaintiff_table')),
                              
                              tabPanel('Visuals', 
                                       textOutput("viztitle"),
                                       plotlyOutput('viz', width = '100%', height = '700')),
                              
                              tabPanel('Notes', uiOutput('notes'))),
                  tags$hr()
                ))

# Server ----
server <- function(input, output) {
  
  shinyalert(html = TRUE, text = tagList(
    textInput("pass", "Password: ", ),
  ))
  
  # create data for data table
  df <- reactive({
    
    req(input$pass == password)
    
    d <- switch(input$time,
                "All" = plaintiff_dat,
                "Quarter" = quarterly_plaintiff_dat)
    
    d <- d %>% filter(court_name %in% input$court) %>% 
      mutate(outcome_cases = !!sym(input$var))
  })
  
  
  # output datatable
  output$plaintiff_table <- DT::renderDT(
    
    # prep table
    #cases_filed_col_no <- which(colnames(df()) == 'cases_filed') -1
    
    datatable(select(df(), -outcome_cases), rownames = F,
              # caption = 'Sources: Ben Schoenfeld (virginiacourtdata.org)',
              class = 'display nowrap',
              filter = 'top',
              options = list(searchHighlight = T,
                             scrollX = T,
                             pageLength = 20,
                             order = list(3, 'desc')),
              colnames = c('Court Jurisdiction', 'Plaintiff Name', 'Plaintiff ZIP', 
                           '# Filings', 'Serial-adjusted # Filings', '# Eviction Judgments', 
                           'Time Frame of Cases', 'ZIPs of Defendants'))
    # %>% formatStyle(columns = 'court_name', background = 'lightblue') <-- if column colors are desired
  )
  
  # output viz title
  output$viztitle <- renderText({
    paste(attr(df()[,input$var], "labels"), "within Selected Localities (January 2018-March 2021)")
  })

  # output visuals
  output$viz <- renderPlotly({
    
    if (input$time == 'All') {
      
      p <- df() %>%
        mutate(court_name = str_remove(court_name, "General District Court")) %>%
        group_by(court_name) %>%
        summarize(total = sum(outcome_cases)) %>%
        ggplot(aes(y = fct_reorder(court_name, total),
                   x = total,
                   court = court_name)) +
        geom_segment(aes(x = 0, xend = total,
                         y = fct_reorder(court_name, total), yend = fct_reorder(court_name, total)),
                     color = "gray") +
        geom_point(size = 2, color = "tan4") +
        labs(x = "Selected Outcome", y = "") +
        theme(axis.text.y = element_text(size = 2)) +
        theme_classic()
      
      ggplotly(p, tooltip = c("x", "court"))
      
    } else {
      
      p <- df() %>%
        group_by(filing_quarter) %>%
        summarize(total = sum(outcome_cases)) %>%
        ggplot(aes(x = filing_quarter, y = total, group = 1)) +
        geom_point(color = "tan4") +
        geom_line(color = "tan4") +
        labs(x = "Year.Quarter", y = "") +
        theme(axis.text.x = element_text(angle = 45))
      
      ggplotly(p)
      
    }
  })
  
  # output orienting instructions
  output$orient <- renderUI(orient_notes)
  
  # output notes
  output$notes <- renderUI(user_notes)
}

# Run app
shinyApp(ui = ui, server = server)