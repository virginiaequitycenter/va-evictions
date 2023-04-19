################################################################################
# Virginia Evictors Catalog                                                    #
# Author: Jacob Goldstein-Greenwood | jacobgg@virginia.edu | GitHub: jacob-gg  #
# Author: Michele Claibourn | mclaibourn@virginia.edu | GitHub: mclaibourn     #
# Author: Elizabeth Mitchell | beth@virginia.edu | GitHub: eam5                #
# Last revised: 2023-02-22                                                     #
################################################################################

# Packages ----
library(shiny)
library(DT)
library(tidyverse)
library(shinyalert)
library(bslib)
library(plotly)
library(bsplus)

# User notes ----
data_notes <- HTML(readLines('app-data-notes'))
orient_notes <- HTML(readLines('app-orient-notes'))
about_notes <- HTML(readLines('app-project-notes'))

# User notes - Spanish----
data_notes_sp <- HTML(readLines('app-data-notes-sp'))
orient_notes_sp <- HTML(readLines('app-orient-notes-sp'))
about_notes_sp <- HTML(readLines('app-project-notes-sp'))

# Preprocess ----
plaintiff_dat <- read.csv('data-plaintiff-aggregated.txt', colClasses = 'character')
yearly_plaintiff_dat <- read.csv('data-yearly-plaintiff-aggregated.txt', colClasses = 'character')
monthly_plaintiff_dat <- read.csv('data-monthly-plaintiff-aggregated.txt', colClasses = 'character')
# Ensure some variables are numeric to guarantee correct behavior in ordering functions
# In later app editions: Check behavior without this chunk; it may not be an issue without zero-pad-loss to protect against
numerify <- function(dat) {
  dat %>% dplyr::mutate(cases_filed = as.numeric(cases_filed),
                        serial_filings = as.numeric(serial_filings),
                        plaintiff_judgments = as.numeric(plaintiff_judgments))
}
plaintiff_dat <- numerify(plaintiff_dat) %>% relocate(filing_years, .before = defendant_zips)
yearly_plaintiff_dat <- numerify(yearly_plaintiff_dat) %>% relocate(filed_year, .before = defendant_zips)
monthly_plaintiff_dat <- numerify(monthly_plaintiff_dat) %>%  relocate(filing_month, .before = defendant_zips)
# Add attributes
attributes(plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
attributes(plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")
attributes(yearly_plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
attributes(yearly_plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")
attributes(monthly_plaintiff_dat$cases_filed)  <- list(labels = "Number of Cases Filed")
attributes(monthly_plaintiff_dat$plaintiff_judgments)  <- list(labels = "Number of Evictions")

# Palette ----
pal_lake_superior <- c("#c87d4b", "#324b64")

# Identify time span of current data
id_time_span <- function(dat) {
  min_month <- min(ym(dat$filing_month)) %>% format('%m') %>% as.numeric()
  min_year <- min(ym(dat$filing_month)) %>% format('%Y')
  max_month <- max(ym(dat$filing_month)) %>% format('%m') %>% as.numeric()
  max_year <- max(ym(dat$filing_month)) %>% format('%Y')
  list(paste0(month.name[min_month], ' ', min_year), paste0(month.name[max_month], ' ', max_year))
}
time_span <- id_time_span(monthly_plaintiff_dat)

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "form-inline", inputs)
  navbar[[4]][[1]][[1]]$children[[1]]$children[[1]] <-  htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[1]], form)
  navbar
}

# User interface ----
ui <- htmlTemplate(filename = "app-template.html", main = 
                     navbarPageWithInputs(
                       theme = bs_theme(version = 4), 
                       collapsible = TRUE,
                       fluid = TRUE,
                       id = "home",
                       title = actionLink("title","Virginia Evictors Catalog"),
                       tabPanel("VA Evictors Catalog", 
                                fluidPage(
                                  # fluidRow(
                                  #   column(12,
                                  #   selectInput('language', 'Language', choices = c("English", "Español"), selected = "English")
                                  #   )
                                  # ),
                                  fluidRow(
                                    column(12,
                                           tags$h1(class = "page-title", "Who is Filing Evictions in Virginia?"),
                                           tags$p(class = "page-description", paste0("The Virginia Evictors Catalog provides data about plaintiffs filing unlawful detainers (evictions) in Virginia's General District Courts from ",
                                                                                     time_span[[1]], " through ", time_span[[2]], ". Each row in the table below represents a plaintiff filing in a specific court jurisdiction.")),
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
                                                         choices = unique(plaintiff_dat$county),
                                                         selected = unique(plaintiff_dat$county),
                                                         size = 5,
                                                         selectize = FALSE
                                             ),
                                             helpText("Note: Select one or more court jurisdictions to show in the table and visualizations. Select multiple jurisdictions by clicking on court names while holding down the control or command key.")
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
                                             helpText("Note: Select a time period to see the aggregated eviction filings in the table and visualization. The visualization will update based on the time period selected. When selecting \"Totals by Month\", the table can be further filtered by typing the year-month into the search field below the \"Time Frame\" column in the table (for example, \"2020-01\" will filter the table to cases filed during January, 2020).")
                                           )
                                    )),
                                  fluidRow(
                                    column(12,
                                           tabsetPanel(type = 'pills',
                                                       tabPanel('Table', icon = icon('table'), downloadButton("downloadBtn", "Download"), DTOutput('plaintiff_table')),
                                                       tabPanel('Visualize', icon = icon('chart-bar'), textOutput("viztitle"), plotlyOutput('viz', width = '100%', height = '700')),
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
                       ),
                       inputs = selectInput('language', NULL, c("English", "Español"), selected = "English")
                       
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
  
  # Make navbarPage title element link to catalog tab
  observeEvent(input$title, {
    updateNavbarPage(session, "home", "VA Evictors Catalog")
  })
  
  # Pick and subset data for datatable
  df <- reactive({
    d <- switch(input$time,
                "All" = plaintiff_dat,
                "Year" = yearly_plaintiff_dat,
                "Month" = monthly_plaintiff_dat)
    d <- d %>% filter(county %in% input$court)
  })
  
  # Function for download button
  output$downloadBtn <- downloadHandler(
    filename = "va-evictors-catalog.csv",
    content = function(file) {
      write.csv(df(), file)
    }
  )
  
  # Render datatable
  output$plaintiff_table <- DT::renderDT(
    datatable(df(), 
              rownames = F,
              caption = 'Sources: Legal Services Corporation (lsc.gov)',
              class = 'display nowrap',
              filter = 'top',
              options = list(searchHighlight = T,
                             scrollX = T,
                             pageLength = 20,
                             order = list(2, 'desc'), # column indexing starts at 0 (3rd column visually is indexed as 2)
                             dom = 'lfrtip'), 
              colnames = c('Court Jurisdiction', 'Plaintiff Name', 
                           'Cases Filed', 'Eviction Judgments', 'Serial Filings',
                           'Time Frame', 'Known Virginia Defendant ZIP Codes')
    )
  )
  
  # Output visuals
  output$viz <- renderPlotly({
    
    if (input$time == 'All') {
      output$viztitle <- renderText({
        "Cases Filed and Eviction Judgments within Selected Court Jurisdictions (Totals by Selected Jurisdictions, All Years)"
      })
      
      p <- df() %>%
        mutate(county = str_remove(county, "General District Court"),
               county = str_trim(county)) %>% 
        group_by(county) %>% 
        summarize(cases_filed = sum(cases_filed, na.rm = T),
                  cases_eviction = sum(plaintiff_judgments, na.rm = T)) %>% 
        pivot_longer(cols = -county, names_to = "Outcome", values_to = "Number", names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed"))) %>% 
        ggplot(aes(y = fct_reorder(county, Number), 
                   x = Number,
                   color = Outcome,
                   label = Outcome,
                   text = paste0('County: ', county))) +
        geom_segment(aes(x = 0, xend = Number, y = fct_reorder(county, Number), yend = fct_reorder(county, Number)),
                     color = "gray") +
        geom_point(size = 2) +
        scale_color_manual(values = pal_lake_superior,
                           labels = c("Eviction Judgments", "Cases Filed"),
                           name = "") +
        scale_x_continuous(expand = expansion(mult = c(0,.05))) +
        labs(x = "", y = "") +
        theme(axis.text.y = element_text(size = 1), legend.position = "top") +
        theme_classic()
      
      ggplotly(p, tooltip = c("x", "label", 'text')) %>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>% 
        config(displayModeBar = TRUE)
      
    } else if (input$time == 'Year')  {
      output$viztitle <- renderText({
        "Cases Filed and Eviction Judgments within Selected Court Jurisdictions (Totals by Year, of Selected Jurisdictions)"
      })
      
      p <- df() %>% 
        group_by(filed_year) %>% 
        summarize(`Cases Filed` = sum(cases_filed),
                  `Eviction Judgments` = sum(plaintiff_judgments)) %>%
        mutate(cases = "Cases Filed", evictions = "Eviction Judgments") %>% 
        rename(Year = filed_year) %>% 
        ggplot() +
        geom_col(aes(x = Year, y = `Cases Filed`, fill = cases)) +
        geom_col(aes(x = Year, y = `Eviction Judgments`, fill = evictions), width = 0.70) +
        scale_fill_manual(values = pal_lake_superior[c(2,1)], labels = c("Cases Filed", "Eviction Judgments"), name = "") +
        labs(x = "Year", y = "") +
        theme(legend.position = "bottom") 
      
      ggplotly(p, tooltip = c("x", "y")) %>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>% 
        config(displayModeBar = TRUE)
      
    } else {
      output$viztitle <- renderText({
        "Cases Filed and Eviction Judgments within Selected Court Jurisdictions (Totals by Month, of Selected Jurisdictions)"
      })
      
      p <- df() %>% 
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed"))) %>%
        rename(Month = filing_month) %>% 
        ggplot(aes(x = Month, y = Number, group = Outcome, color = Outcome)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = pal_lake_superior, labels = c("Eviction Judgments", "Cases Filed"), name = "") +
        labs(x = "Year-Month", y = "") +
        theme(axis.text.x = element_text(angle = 45), legend.position = "bottom") 
      
      ggplotly(p, tooltip = c("x", "y", "group"))%>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>% 
        config(displayModeBar = TRUE)
      
    }
  })
  
  # Output orienting instructions
  output$orient <- renderUI(orient_notes)
  # Output data notes
  output$notes <- renderUI(data_notes)
  # Output about project
  output$about <- renderUI(about_notes)
}

# Run app
shinyApp(ui = ui, server = server)

