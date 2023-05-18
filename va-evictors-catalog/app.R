################################################################################
# Virginia Evictors Catalog                                                    #
# Author: Jacob Goldstein-Greenwood | jacobgg@virginia.edu | GitHub: jacob-gg  #
# Author: Michele Claibourn | mclaibourn@virginia.edu | GitHub: mclaibourn     #
# Author: Elizabeth Mitchell | beth@virginia.edu | GitHub: eam5                #
# Last revised: 2023-05-16                                                     #
# Last deployed: -                                                     #
################################################################################

# Packages ----
library(shiny)
library(DT)
library(tidyverse)
library(bslib)
library(plotly)
library(bsplus)
library(scales)
library(ggpattern)

# Read in HTML ----
data_notes <- HTML(readLines("html/app-data-notes"))
orient_notes <- HTML(readLines("html/app-orient-notes"))
about_notes <- HTML(readLines("html/app-project-notes"))
news_page <- HTML(readLines("html/app-news"))
header <- HTML(readLines("html/header"))
footer <- HTML(readLines("html/footer"))

# Preprocess ----
plaintiff_dat <- read.csv("data-plaintiff-aggregated.txt", colClasses = "character")
yearly_plaintiff_dat <- read.csv("data-yearly-plaintiff-aggregated.txt", colClasses = "character")
monthly_plaintiff_dat <- read.csv("data-monthly-plaintiff-aggregated.txt", colClasses = "character")
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

# User interface ----
ui <- bootstrapPage(
  lang = "en",
  tags$head(includeHTML(("html/head.html"))),
  includeCSS("www/styles.css"),
  ##-- Header ----
  uiOutput("header"),
  ##-- navbarPage ----
    navbarPage(
      theme = bs_theme(version = 4),
      collapsible = TRUE,
      fluid = TRUE,
      id = "main-page",
      title = actionLink("title", "Virginia Evictors Catalog"),
      tabPanel("Home",
              fluidPage(
                fluidRow(
                  column(12,
                    tags$h1(class = "page-title", "Who is Filing Evictions in Virginia?"),
                    tags$p(class = "page-description", paste0("The Virginia Evictors Catalog provides data about plaintiffs filing unlawful detainers (evictions) in Virginia's General District Courts from ",
                                                              time_span[[1]], " through ", time_span[[2]], ". Each row in the table below represents a plaintiff filing in a specific court jurisdiction.")),
                    bs_button("How to search the catalog", button_type = "info", class = "collapsible") %>%
                      bs_attach_collapse("orient-collapse"),
                    bs_collapse(
                      id = "orient-collapse",
                      content = tags$div(
                        class = "well",
                        tags$div(
                          htmlOutput("orient"), class = "orient")))
                )), # end fluidRow
                fluidRow(
                  column(6,
                    wellPanel(
                      selectInput("court", "Select Court Jurisdictions",
                                  multiple = TRUE,
                                  choices = unique(plaintiff_dat$county),
                                  selected = unique(plaintiff_dat$county),
                                  size = 5,
                                  selectize = FALSE),
                      helpText("Note: Select one or more court jurisdictions to show in the table and visualizations. Select multiple jurisdictions by clicking on court names while holding down the control or command key.")
                    )
                  ),
                  column(6,
                    wellPanel(
                      radioButtons("time", "Select a Time Period to Display",
                                  choices = list("Totals across All Years" = "All",
                                                  "Totals by Year" = "Year",
                                                  "Totals by Month" = "Month"),
                                  selected = "All"),
                      helpText("Note: Select a time period to see the aggregated eviction filings in the table and visualization. The visualization will update based on the time period selected. When selecting \"Totals by Month\", the table can be further filtered by typing the year-month into the search field below the \"Time Frame\" column in the table (for example, \"2020-01\" will filter the table to cases filed during January, 2020).")
                    )
                )), # end fluidRow
                fluidRow(
                  column(12,
                    tabsetPanel(type = "pills",
                                tabPanel("Table", icon = icon("table"), downloadButton("downloadBtn", "Download"), DTOutput("plaintiff_table")),
                                tabPanel("Visualize", icon = icon("chart-bar"), textOutput("viztitle"), plotlyOutput("viz", width = "100%", height = "700")),
                    ),
                )) # end fluidRow
              ) # end fluidPage
      ), # end tabPanel
      tabPanel("Data Notes",
        fluidPage(uiOutput("notes"))
      ),
      tabPanel("About the Project",
        fluidPage(uiOutput("about"))
      ),
      tabPanel("In the News",
        fluidPage(uiOutput("news"))
      )
    ), # end navbarPage
  ##-- Footer ----
  uiOutput("footer")
) # end bootstrapPage

# Server ----
server <- function(input, output, session) {

  # Render HTML content
  output$header <- renderUI(header)
  output$footer <- renderUI(footer)
  output$orient <- renderUI(orient_notes)
  output$notes <- renderUI(data_notes)
  output$about <- renderUI(about_notes)
  output$news <- renderUI(news_page)

  # Make navbarPage title element link to catalog tab
  observeEvent(input$title, {
    updateNavbarPage(session, "main-page", "Home")
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
              rownames = FALSE,
              caption = 'Sources: Legal Services Corporation (lsc.gov)',
              class = 'display nowrap',
              filter = 'top',
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
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
        summarize(cases_filed = sum(cases_filed, na.rm = TRUE),
                  cases_eviction = sum(plaintiff_judgments, na.rm = TRUE)) %>%
        pivot_longer(cols = -county, names_to = "Outcome", values_to = "Number", names_prefix = "cases_") %>%
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed"))) %>%
        ggplot(aes(y = fct_reorder(county, Number),
                   x = Number,
                   color = Outcome,
                   label = Outcome,
                   text = paste0("County: ", county))) +
        geom_segment(aes(x = 0, xend = Number, y = fct_reorder(county, Number), yend = fct_reorder(county, Number)),
                     color = "gray") +
        geom_point(size = 2) +
        scale_color_manual(values = pal_lake_superior,
                           labels = c("Eviction Judgments", "Cases Filed"),
                           name = "") +
        scale_x_continuous(expand = expansion(mult = c(0, .05))) +
        labs(x = "", y = "") +
        theme(axis.text.y = element_text(size = 1), legend.position = "top") +
        theme_classic()

      ggplotly(p, tooltip = c("x", "label", "text")) %>%
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>%
        config(displayModeBar = TRUE)

    } else if (input$time == "Year")  {
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
      
      # Need separate df for moratorium label, to keep geom_text from plotting overlapping text
      label_df <- df() %>%
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(ymax = max(Number),
               label = paste(
                 "<span style='font-weight:700;'>State-Wide Eviction Moratorium</span>",
                 "Mar 16, 2020 - Jun 28, 2020",
                 "Aug 10, 2020 - Sep 7, 2020",
                 "Jan 1, 2021 - Jun 30, 2021",
                 "Aug 10, 2021 - Jun 30, 2022", "",
                 "<span style='font-weight:700;color:#004E5E;'>Federal Eviction Moratorium (CDC Order)</span>",
                 "<span style='color:#004E5E;'>Sep 4, 2020 - Aug 26, 2021</span>",
                 sep = "\n")) %>%
        head(1)
      
      test_df <- monthly_plaintiff_dat %>%
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed")),
               Month = as.Date(paste(filing_month, "-01", sep="")))

      p <- df() %>%
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed")),
               Month = as.Date(paste(filing_month, "-01", sep=""))) %>%
        # rename(Month = filing_month) %>%
        ggplot() +
        # Federal Eviction Moratorium (CDC Order)
        geom_rect(aes(xmin = as.Date("2020-09-04", "%Y-%m-%d"), xmax = as.Date("2021-08-26", "%Y-%m-%d"),
                      ymin = 0, ymax = (max(Number)+(.05*(max(Number))))), alpha = .2, fill = "#004E5E") +
        # VA Eviction Moratorium 03-16-2020 to 06-31-2022 (https://evictionlab.org/eviction-tracking/virginia/)
        geom_rect(aes(xmin = as.Date("2020-03-16", "%Y-%m-%d"), xmax = as.Date("2020-06-28", "%Y-%m-%d"),
                      ymin = 0, ymax = (max(Number)+(.05*(max(Number))))), alpha = .25, fill = "#A9A9A9") +
        geom_rect(aes(xmin = as.Date("2020-08-10", "%Y-%m-%d"), xmax = as.Date("2020-09-07", "%Y-%m-%d"),
                      ymin = 0, ymax = (max(Number)+(.05*(max(Number))))), alpha = .25, fill = "#A9A9A9") +
        geom_rect(aes(xmin = as.Date("2021-01-01", "%Y-%m-%d"), xmax = as.Date("2021-06-30", "%Y-%m-%d"),
                      ymin = 0, ymax = (max(Number)+(.05*(max(Number))))), alpha = .25, fill = "#A9A9A9") +
        geom_rect(aes(xmin = as.Date("2021-08-10", "%Y-%m-%d"), xmax = as.Date("2022-06-30", "%Y-%m-%d"),
                      ymin = 0, ymax = (max(Number)+(.05*(max(Number))))), alpha = .25, fill = "#A9A9A9") +
        geom_text(data = label_df, aes(x = as.Date("2021-05-01", "%Y-%m-%d"), y = (ymax-(.075*(ymax))), label = label),
                  size = 3.3, check_overlap = TRUE) +
        geom_point(aes(x = Month, y = Number, group = Outcome, color = Outcome,
                       text = paste('Month:', format(Month, "%b %Y"),
                                    '<br>Number: ', Number,
                                    '<br>Outcome: ', Outcome))) +
        geom_line(aes(x = Month, y = Number, group = Outcome, color = Outcome)) +
        scale_x_date(date_breaks = "months", date_labels =  "%b %Y", expand = c(0.01,0.01)) +
        scale_y_continuous(
          # limits = c(0,10),
                           # breaks = c(0,2,4,6,8,10),
                           # name = "Rate per 1000 People"
                           expand = c(0.01,0.01)
                           ) +
        scale_color_manual(values = pal_lake_superior, labels = c("Eviction Judgments", "Cases Filed"), name = "") +
        labs(x = "Year-Month", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45), legend.position = "bottom")

      gg <- ggplotly(p, tooltip = c("text")) %>%
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>%
        config(displayModeBar = TRUE)

      # Removes tooltip from annotation (https://stackoverflow.com/questions/71213317/can-you-get-rid-of-the-trace-labels-in-the-annotations-when-using-ggplotly-ggpl)
      gg$x$data[[2]]$hoverinfo <- "none"
      gg

    }
  })

}

# Run app
shinyApp(ui = ui, server = server)
