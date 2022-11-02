# Shiny app: Plaintiff database
# Author: Jacob Goldstein-Greenwood / jacobgg@virginia.edu / GitHub: jacob-gg
# Author: Michele Claibourn / mclaibourn@virginia.edu / GitHub: mclaibourn
# Author: Elizabeth Mitchell / beth@virginia.edu
# Last revised: 2022-11-1

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
library(bsplus)

# Load user notes
data_notes <- HTML(readLines('app-data-notes'))
orient_notes <- HTML(readLines('app-orient-notes'))
about_notes <- HTML(readLines('app-project-notes'))

# Preprocess ----
# Note: consider reading in defuzzed data and aggregating in app...

plaintiff_dat <- read.csv('plaintiff-aggregated-data.txt', colClasses = 'character')
# Make certain variables numeric so that sorting (e.g, cases hi-->lo) works appropriately
plaintiff_dat <- plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                          cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                          plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_years, .before = def_zips) %>% 
  select(-cases_filed_excluding_all_but_final_serial)
# plaintiff_dat$pla_1_zip <- ifelse(is.na(plaintiff_dat$pla_1_zip), 'NA', plaintiff_dat$pla_1_zip)

# Add attributes
attributes(plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
#attributes(plaintiff_dat$cases_filed_excluding_all_but_final_serial)  <- list(labels = "Number of Non-Serial Cases Filed")
attributes(plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")

# ADD SELECTION OF DATA BY YEAR
yearly_plaintiff_dat <- read.csv('yearly-plaintiff-aggregated-data.txt', colClasses = 'character')
yearly_plaintiff_dat <- yearly_plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                                          cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                                          plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_year, .before = def_zips) %>% 
  select(-cases_filed_excluding_all_but_final_serial)

# Add attributes
attributes(yearly_plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
#attributes(yearly_plaintiff_dat$cases_filed_excluding_all_but_final_serial)  <- list(labels = "Number of Non-Serial Cases Filed")
attributes(yearly_plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")

# ADD SELECTION OF DATA BY MONTH
monthly_plaintiff_dat <- read.csv('monthly-plaintiff-aggregated-data.txt', colClasses = 'character')
monthly_plaintiff_dat <- monthly_plaintiff_dat %>% mutate(cases_filed = as.numeric(cases_filed),
                                                              cases_filed_excluding_all_but_final_serial = as.numeric(cases_filed_excluding_all_but_final_serial),
                                                              plaintiff_judgments = as.numeric(plaintiff_judgments)) %>% 
  relocate(filing_month, .before = def_zips) %>% 
  select(-cases_filed_excluding_all_but_final_serial)
# monthly_plaintiff_dat$pla_1_zip <- ifelse(is.na(monthly_plaintiff_dat$pla_1_zip), 'NA', monthly_plaintiff_dat$pla_1_zip)

# Add attributes
attributes(monthly_plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
#attributes(monthly_plaintiff_dat$cases_filed_excluding_all_but_final_serial)  <- list(labels = "Number of Non-Serial Cases Filed")
attributes(monthly_plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")

# palette
pal_lake_superior <- c("#c87d4b", "#324b64")


# User interface ----
ui <- htmlTemplate(filename = "app-template.html", main = 
    navbarPage(
      theme = bs_theme(version = 4), 
      collapsible = TRUE,
      fluid = TRUE,
      id = "home",
      title = actionLink("title","Virginia Evictors Catalog"),
      tabPanel("VA Evictors Catalog", 
        fluidPage(
          fluidRow(
            column(12,
              tags$h1(class="page-title", "Who is Filing Evictions in Virginia?"),
              tags$p(class = "page-description", "The Virginia Evictors Catalog provides data about plaintiffs filing unlawful detainers (evictions) with the Virginia State District Courts from January 2018 through September 2022. Each row in the table below represents a plaintiff filing in a specific court jurisdiction."),
              
              bs_button("How to search the catalog", button_type = "info", class = "collapsible") %>%
                bs_attach_collapse("yeah"),
              bs_collapse(
                id = "yeah", 
                content = tags$div(class = "well", 
                                   tags$div(
                                     htmlOutput('orient'), class = "orient"
                                   ))
              )
            )
              
          ),
          
          fluidRow(
            column(6,
              wellPanel(
                  selectInput('court', 'Select Court Jurisdictions to Include',
                                      multiple = TRUE,
                                      choices = unique(plaintiff_dat$court_name),
                                      selected = unique(plaintiff_dat$court_name),
                                      size = 5,
                                      selectize = FALSE
                  ),
                  helpText("Note: Select one or more court jurisdictions to show in the table and visuals. Select multiple jurisdictions by clicking on court names while holding down the control (Windows) or command key (Mac).")
              )
            ),
            column(6,
              wellPanel(
                  radioButtons("time", 'Select a Time Period to Display',
                              choices = list("Totals across All Years" = "All", 
                                            "Totals by Year" = "Year",
                                            "Totals by Month" = "Month"), 
                              selected = "All"
                  ),
                  # helpText("Note: For the sum of all filings for a plaintiff in the catalog, select \"Totals across All Years\"; for the sum within each year or month, select \"Totals by Year\" or \"Totals by Month\"."),
                  helpText("Note: Select a time period for the aggregated eviction filings in the table and visuals. When selecting \"Totals by Month\", the table can be further filtered by typing the year-month into the search field below the \"Time Frame\" column in the table (for example, \"2020-1\" will filter the table to cases filed during January, 2020).")
              )
            )),
          fluidRow(
            column(12,
            tabsetPanel(type = 'pills',
              tabPanel('Table', icon = icon('table'), downloadButton("downloadBtn", "Download"), DTOutput('plaintiff_table')),
              tabPanel('Visuals', icon = icon('chart-bar'),
                      textOutput("viztitle"),
                      plotlyOutput('viz', width = '100%', height = '700')
              ),
            ),
            )
          )
        )
      ),
      tabPanel("Data Notes", 
        fluidPage(
            uiOutput('notes')
        )
      ),
      tabPanel("About the Project", 
         fluidPage(
             uiOutput('about')
         )
      )
      # ,
      # tabPanel("Contact Us",
      #          fluidPage(
      #            tags$h1("Contact Us"),
      #            tags$p("Please fill out the form below with your comments and/or questions. The form can also be found"),
      #            tags$a(href= "", "here."),
      #            htmlOutput("frame")
      #          ))
  )
)

# Server ----
server <- function(input, output, session) {
  
  # make navbarPage title element link to catalog tab
  observeEvent(input$title, {
    updateNavbarPage(session, "home", "VA Evictors Catalog")
  })
  
  #add google form to iframe
  # output$frame <- renderUI({
  #   my_test <- tags$iframe(src="", height=600, width=535)
  #   print(my_test)
  #   my_test
  # })
  
  # create data for data table
  df <- reactive({
    
    d <- switch(input$time,
                "All" = plaintiff_dat,
                "Year" = yearly_plaintiff_dat,
                "Month" = monthly_plaintiff_dat)
    
    d <- d %>% filter(court_name %in% input$court) #%>% 
      #mutate(outcome_cases = !!sym(input$var))
  })
  
  # function for download button
  output$downloadBtn <- downloadHandler(
    filename = "va-evictors-catalog.csv",
    content = function(file) {
      write.csv(df(), file)
    }
  )
  
  # output datatable
  output$plaintiff_table <- DT::renderDT(
    
    # prep table
    #cases_filed_col_no <- which(colnames(df()) == 'cases_filed') -1
    
    datatable(df(), 
              rownames = F,
              # caption = 'Sources: Ben Schoenfeld (virginiacourtdata.org)',
              class = 'display nowrap',
              filter = 'top',
              options = list(searchHighlight = T,
                             scrollX = T,
                             pageLength = 20,
                             order = list(2, 'desc'), # for order, column indexing starts at 0 (3rd column visually is indexed as 2)
                             dom = 'lfrtip'), 
              colnames = c('Court Jurisdiction', 'Plaintiff Name', 
                           'Filings', 'Evictions', #'Serial-adjusted # Filings', 
                           'Time Frame', 'Defendant Zip Codes')
              )
    # %>% formatStyle(columns = 'court_name', background = 'lightblue') <-- if column colors are desired
  )

  # output visuals
  output$viz <- renderPlotly({
    
    if (input$time == 'All') {
      # output viz title
      output$viztitle <- renderText({
        "Cases and Evictions within Selected Court Jurisdictions (Totals by Selected Jurisdictions, All Years)"
      })
      
      p <- df() %>%
        
        mutate(court_name = str_remove(court_name, "General District Court"),
               court_name = str_trim(court_name)) %>% 
        group_by(court_name) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -court_name, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases", "Evictions"),
               Outcome = factor(Outcome, levels = c("Evictions", "Cases"))) %>% 
        
        ggplot(aes(y = fct_reorder(court_name, Number), 
                   x = Number,
                   color = Outcome,
                   label = Outcome,
                   court = court_name)) +
        geom_segment(aes(x = 0, xend = Number, 
                         y = fct_reorder(court_name, Number), yend = fct_reorder(court_name, Number)),
                     color = "gray") +
        geom_point(size = 2) +
        scale_color_manual(values = pal_lake_superior,
                           labels = c("Evictions", "Cases"),
                           name = "") +
        scale_x_continuous(expand = expansion(mult = c(0,.05))) +
        labs(x = "", y = "") +
        theme(axis.text.y = element_text(size = 2),
              legend.position = "top") +
        theme_classic()
      
      ggplotly(p, tooltip = c("x", "label", "color", "court")) %>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>% 
        config(displayModeBar = TRUE)
      
    } else if (input$time == 'Year')  {
      output$viztitle <- renderText({
        "Cases and Evictions within Selected Court Jurisdictions (Totals by Year, of Selected Jurisdictions)"
      })
      
      p <- df() %>% 
        group_by(filing_year) %>% 
        summarize(Cases = sum(cases_filed),
                  Evictions = sum(plaintiff_judgments)) %>%
        mutate(cases = "Cases", evictions = "Evictions") %>% 
        
        ggplot() +
        geom_col(aes(x = filing_year, y = Cases, fill = cases)) +
        geom_col(aes(x = filing_year, y = Evictions, fill = evictions),
                 width = 0.70) +
        scale_fill_manual(values = pal_lake_superior[c(2,1)],
                          labels = c("Cases", "Evictions"),
                          name = "") +
        labs(x = "Year", y = "") +
        theme(legend.position = "bottom") 

      ggplotly(p, tooltip = c("x", "y"))%>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>% 
        config(displayModeBar = TRUE)
      
    } else {
      # output viz title
      output$viztitle <- renderText({
        "Cases and Evictions within Selected Court Jurisdictions (Totals by Month, of Selected Jurisdictions)"
      })
      
      p <- df() %>% 
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases", "Evictions"),
               Outcome = factor(Outcome, levels = c("Evictions", "Cases"))) %>%
        
        ggplot(aes(x = filing_month, y = Number, 
                   group = Outcome, color = Outcome)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = pal_lake_superior,
                           labels = c("Evictions", "Cases"),
                           name = "") +
        labs(x = "Year-Month", y = "") +
        theme(axis.text.x = element_text(angle = 45),
              legend.position = "bottom") 
     
      ggplotly(p, tooltip = c("x", "y", "group"))%>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>% 
        config(displayModeBar = TRUE)
      
      # p <- df() %>%
      #   group_by(filing_month) %>%
      #   summarize(total = sum(outcome_cases)) %>%
      #   ggplot(aes(x = filing_month, y = total, group = 1)) +
      #   geom_point(color = "tan4") +
      #   geom_line(color = "tan4") +
      #   labs(x = "Year-Month", y = "") +
      #   theme(axis.text.x = element_text(angle = 45))
      # 
      # ggplotly(p, tooltip = c("x", "color", "court")) %>% 
      #   layout(legend = list(orientation = "h"))
      
      
    }
  })
  
  # output orienting instructions
  output$orient <- renderUI(orient_notes)
  
  # output data notes
  output$notes <- renderUI(data_notes)
  
  # output about project
  output$about <- renderUI(about_notes)
}

# Run app
shinyApp(ui = ui, server = server)