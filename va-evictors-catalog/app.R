################################################################################
# Virginia Evictors Catalog                                                    #
# Author: Jacob Goldstein-Greenwood | jacobgg@virginia.edu | GitHub: jacob-gg  #
# Author: Michele Claibourn | mclaibourn@virginia.edu | GitHub: mclaibourn     #
# Author: Elizabeth Mitchell | beth@virginia.edu | GitHub: eam5                #
# Last revised: 2023-04-19                                                     #
################################################################################

# Packages ----
library(shiny)
library(DT)
library(tidyverse)
library(shinyalert)
library(bslib)
library(plotly)
library(bsplus)
library(shiny.i18n)

# Set language translations
i18n <- Translator$new(translation_csvs_path = "translations/")
i18n$set_translation_language("English") # here you select the default translation to display

# User notes ----
data_notes <- HTML(readLines("html/app-data-notes"))
orient_notes <- HTML(readLines("html/app-orient-notes"))
about_notes <- HTML(readLines("html/app-project-notes"))
news_page <- HTML(readLines("html/news"))
header <- HTML(readLines("html/header"))
footer <- HTML(readLines("html/footer"))

# User notes  Spanish Language ----
data_notes_sp <- HTML(readLines("html/app-data-notes-sp"))
orient_notes_sp <- HTML(readLines("html/app-orient-notes-sp"))
about_notes_sp <- HTML(readLines("html/app-project-notes-sp"))
news_page_sp <- HTML(readLines("html/news-sp"))
footer_sp <- HTML(readLines("html/footer-sp"))

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

# Main page panel. Language picker is stored here due to limitations of the navbarPage
main_page_panel <-  fluidPage(
    shiny.i18n::usei18n(i18n),
    fluidRow(
      column(12,
        div(class = "lang_select",
          selectInput("selected_language",
                      i18n$t("Select language"),
                      choices = i18n$get_languages(),
                      selected = i18n$get_key_translation()))
    )),
    fluidRow(
      column(12,
        tags$h1(class = "page-title", i18n$t("Who is Filing Evictions in Virginia?")),
        tags$p(class = "page-description", paste0("The Virginia Evictors Catalog provides data about plaintiffs filing unlawful detainers (evictions) in Virginia's General District Courts from ",
                                                  time_span[[1]], " through ", time_span[[2]], ". Each row in the table below represents a plaintiff filing in a specific court jurisdiction.")),
        bs_button(i18n$t("How to search the catalog"), button_type = "info", class = "collapsible") %>%
          bs_attach_collapse("orient-collapse"),
        bs_collapse(
          id = "orient-collapse",
          content = tags$div(class = "well",
                            tags$div(
                              htmlOutput("orient"), class = "orient"
                            ))
        )
    )),
    fluidRow(
      column(6,
        wellPanel(
          selectInput("court", "Select Court Jurisdictions to Include",
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
          radioButtons("time", "Select a Time Period to Display",
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
        tabsetPanel(type = "pills",
          tabPanel(
            i18n$t("Table"),
            icon = icon("table"),
            downloadButton("downloadBtn",
            i18n$t("Download")),
            DTOutput("plaintiff_table")),
          tabPanel(i18n$t("Visualize"), icon = icon("chart-bar"), textOutput("viztitle"), plotlyOutput("viz", width = "100%", height = "700")),
        ),
      )
    )
  )

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
      id = "my-page",
      title = i18n$t("Virginia Evictors Catalog"),
    #  windowTitle = "Virginia Evictors Catalog",
    #  title = actionLink("title",i18n$t("Virginia Evictors Catalog")),
      tabPanel(i18n$t("Home"),
        main_page_panel
      ),
      tabPanel(i18n$t("Data Notes"),
        fluidPage(uiOutput("notes"))
      ),
      tabPanel(i18n$t("About the Project"),
        fluidPage(uiOutput("about"))
      ),
      tabPanel(i18n$t("In the News"),
        fluidPage(uiOutput("news"))
      )
    ),
    ##-- Footer ----
    uiOutput("footer")
)

# Server ----
server <- function(input, output, session) {

  output$header <- renderUI(header)

  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(input$selected_language)

    if (input$selected_language == "Español"){
      output$orient <- renderUI(orient_notes_sp)
      output$notes <- renderUI(data_notes_sp)
      output$about <- renderUI(about_notes_sp)
      output$news <- renderUI(news_page_sp)
      output$footer <- renderUI(footer_sp)
    } else {
      output$orient <- renderUI(orient_notes)
      output$notes <- renderUI(data_notes)
      output$about <- renderUI(about_notes)
      output$news <- renderUI(news_page)
      output$footer <- renderUI(footer)
    }
  })

  # NOT WORKING WITH SHINY I18N
  # Make navbarPage title element link to catalog tab
  # observeEvent(input$title, {
  #   updateNavbarPage(session, "my-page", i18n$t("Home"))
  # })

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
  output$plaintiff_table <- DT::renderDT({
    datatable(df(),
              rownames = F,
              caption = i18n$t("Sources: Legal Services Corporation (lsc.gov)"),
              class = "display nowrap",
              filter = "top",
              options = list(searchHighlight = T,
                             scrollX = T,
                             pageLength = 20,
                             order = list(2, "desc"), # column indexing starts at 0 (3rd column visually is indexed as 2)
                             dom = "lfrtip",
                             language = if (input$selected_language == "Español"){
                              # Data table translation options: https://datatables.net/plug-ins/i18n/#Translations
                              list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/es-MX.json")
                             } else NULL
                            ),
              colnames = c(i18n$t("Court Jurisdiction"), i18n$t("Plaintiff Name"), 
                           i18n$t("Cases Filed"), i18n$t("Eviction Judgments"), i18n$t("Serial Filings"),
                           i18n$t("Time Frame"), i18n$t("Known Virginia Defendant ZIP Codes")),
            #NEED TO FIX BELOW TO CALL TO A UNIQUE TABLE ID/DATA TABLE ID CHANGES WITH EACH REACTIVE INPUT
              callback = JS("
                var tips = ['The general district court where the case was filed. Court jurisdictions are tied to localities (counties or cities) in Virginia.',
                 'The entity filing an eviction case against a tenant with the court. In Virginia, eviction cases can be filed by \"the landlord, [their] agent, attorney, or other person.\"', 
                 'The total number of eviction cases filed by the plaintiff in the selected time period and jurisdiction.',
                'The total number of cases filed by the plaintiff that ended in an eviction judgment (a judgment for the plaintiff).', 
                  'We consider serial cases to be repeated cases filed by a given plaintiff against a given defendant in a given ZIP code within a 12-month period.',
                  'Time period(s) of total filings and evictions. Shown as all years, by year, or by month based on selection above.',
                  'The ZIP codes provided for the defendants (tenants) against whom the unlawful detainer/eviction is filed.'
                  ],

                  header = table.columns().header();
                for (var i = 0; i < tips.length; i++) {
                  $(header[i]).append('<svg version=\"1.1\" id=\"tooltip-icon\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" x=\"0px\" y=\"0px\" viewBox=\"0 0 16 16\" width=\"16px\" height=\"16px\" style=\"enable-background:new 0 0 16 16;margin-left:10px;margin-bottom:3px;\" xml:space=\"preserve\"><path d=\"M14.5,8c0-3.59-2.91-6.5-6.5-6.5S1.5,4.41,1.5,8s2.91,6.5,6.5,6.5S14.5,11.59,14.5,8z M0,8c0-4.42,3.58-8,8-8 s8,3.58,8,8s-3.58,8-8,8S0,12.42,0,8z M5.31,5.17C5.55,4.47,6.22,4,6.96,4h1.82c1.09,0,1.97,0.88,1.97,1.97 c0,0.71-0.38,1.36-0.99,1.71L8.75,8.26C8.74,8.67,8.41,9,8,9C7.58,9,7.25,8.67,7.25,8.25V7.83c0-0.27,0.14-0.52,0.38-0.65l1.38-0.79 C9.16,6.3,9.25,6.14,9.25,5.97c0-0.26-0.21-0.47-0.47-0.47H6.96c-0.11,0-0.2,0.07-0.23,0.17L6.71,5.71C6.57,6.1,6.14,6.3,5.75,6.16 S5.16,5.59,5.3,5.21L5.31,5.17L5.31,5.17z M7,11c0-0.55,0.45-1,1-1s1,0.45,1,1s-0.45,1-1,1S7,11.55,7,11z\"/></svg>');
                  $(header[i]).find('svg').attr('data-toggle', 'tooltip');
                  $(header[i]).find('svg').attr('data-placement', 'top');
                  $(header[i]).find('svg').attr('title', tips[i]);
                }
                ")
    )
}, server = TRUE)


  # Output visuals
  output$viz <- renderPlotly({

    if (input$time == "All") {
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
                   text = paste0("County: ", county))) +
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

      m <- monthly_plaintiff_dat %>% 
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed"))) %>%
        rename(Month = filing_month)
      
      p <- df() %>% 
        group_by(filing_month) %>% 
        summarize(cases_filed = sum(cases_filed),
                  cases_eviction = sum(plaintiff_judgments)) %>% 
        pivot_longer(cols = -filing_month, names_to = "Outcome", values_to = "Number",
                     names_prefix = "cases_") %>% 
        mutate(Outcome = ifelse(Outcome == "filed", "Cases Filed", "Eviction Judgments"),
               Outcome = factor(Outcome, levels = c("Eviction Judgments", "Cases Filed"))) %>%
        rename(Month = filing_month) %>% 
        ggplot() +
        # VA Eviction Moratorium 03-16-2020 to 06-31-2022
        geom_rect(aes(xmin = "2020-03", xmax = "2022-07"),
                  ymin = 0, ymax = 17000, alpha = .15, fill = "#467BB0") +
        # annotate("rect", xmin = "2020-03", xmax = "2022-07", ymin = 0, ymax = 17000,
        #          alpha = .15,fill = "#467BB0") +
        annotate("text", x = "2021-05", y = 16000, label = "Virginia Eviction Moratorium") +
        annotate("text", x = "2021-05", y = 15500, label = "March 2020 - June 2022") +
        geom_point(aes(x = Month, y = Number, group = Outcome, color = Outcome)) +
        geom_line(aes(x = Month, y = Number, group = Outcome, color = Outcome)) +
        scale_color_manual(values = pal_lake_superior, labels = c("Eviction Judgments", "Cases Filed"), name = "") +
        labs(x = "Year-Month", y = "") +
        theme(axis.text.x = element_text(angle = 45), legend.position = "bottom") 

      ggplotly(p, tooltip = c("x", "y", "group"))%>% 
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>%
        config(displayModeBar = TRUE)
     

    }
  })

}

# Run app
shinyApp(ui = ui, server = server)

