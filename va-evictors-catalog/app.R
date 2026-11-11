################################################################################
# Virginia Evictors Catalog                                                    #
# Author: Jacob Goldstein-Greenwood | jacobgg@virginia.edu | GitHub: jacob-gg  #
# Author: Michele Claibourn | mclaibourn@virginia.edu | GitHub: mclaibourn     #
# Author: Elizabeth Mitchell | beth@virginia.edu | GitHub: eam5                #
# Last revised: 2026-11-11                                                     #
# Last deployed: 2026-11-11                                                    #
#   - Deployed to both /va-evictors-catalog and /plaintiff-database-shiny      # 
################################################################################

# Packages ----
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(bslib)
library(plotly)
library(bsplus)
library(scales)
library(shiny.i18n)
library(shinyWidgets)

# Set language translations
i18n <- Translator$new(translation_csvs_path = "translations/") # set path to translation csv
i18n$set_translation_language("English") # select the default translation to display

# Read in HTML ----
read_html <- function(file) HTML(readLines(file, warn = FALSE))

# Page text in each language. Each name matches an output in the UI
# (e.g. pages$Español$writ fills htmlOutput("writ")). Names of the languages
# match the column headers in translations/translation_sp.csv.
pages <- list(
  English = list(
    writ                = read_html("html/writ-note"),
    orient              = read_html("html/app-orient-notes"),
    selection_note_time = read_html("html/selection-note-time"),
    notes               = read_html("html/app-data-notes"),
    about               = read_html("html/app-project-notes"),
    news                = read_html("html/app-news"),
    footer              = read_html("html/footer")
  ),
  Español = list(
    writ                = read_html("html/writ-note-sp"),
    orient              = read_html("html/app-orient-notes-sp"),
    selection_note_time = read_html("html/selection-note-time-sp"),
    notes               = read_html("html/app-data-notes-sp"),
    about               = read_html("html/app-project-notes-sp"),
    news                = read_html("html/news-sp"),
    footer              = read_html("html/footer-sp")
  )
)
# The header is the same in both languages
header <- read_html("html/header")

# Read data ----
# Counts are read as numbers so the table sorts them correctly. Everything else
# stays text, so ZIP codes, years and months are never turned into numbers.
# The time column is moved before the ZIP codes to match the table's headers.
read_plaintiff_data <- function(file, time_col) {
  read.csv(file,
           colClasses = c(cases_filed = "numeric",
                          plaintiff_judgments = "numeric",
                          serial_filings = "numeric",
                          county = "character",
                          plaintiff_name = "character",
                          defendant_zips = "character",
                          setNames("character", time_col))) %>%
    relocate(all_of(time_col), .before = defendant_zips)
}
plaintiff_dat         <- read_plaintiff_data("data-plaintiff-aggregated.txt", "filing_years")
yearly_plaintiff_dat  <- read_plaintiff_data("data-yearly-plaintiff-aggregated.txt", "filed_year")
monthly_plaintiff_dat <- read_plaintiff_data("data-monthly-plaintiff-aggregated.txt", "filing_month")

# Palette ----
pal_lake_superior <- c("#c87d4b", "#324b64")

# Eviction moratoriums (shaded on the monthly chart) ----
# Sources: https://evictionlab.org/eviction-tracking/virginia/ (state); CDC order (federal)
moratoriums <- tibble::tribble(
  ~start,       ~end,         ~type,
  "2020-03-16", "2020-06-28", "state",
  "2020-08-10", "2020-09-07", "state",
  "2021-01-01", "2021-06-30", "state",
  "2021-08-10", "2022-06-30", "state",
  "2020-09-04", "2021-08-26", "federal"
) %>%
  mutate(across(c(start, end), as.Date))

# "Mar 16, 2020 - Jun 28, 2020"
format_span <- function(start, end) {
  fmt <- function(d) paste0(format(d, "%b "), as.integer(format(d, "%d")), format(d, ", %Y"))
  paste(fmt(start), "-", fmt(end))
}

# Totals for the charts: cases filed and eviction judgments for each value of
# `by` (a court, year or month), in long form with columns
# <by>, Outcome ("filed" or "eviction") and Number.
summarise_outcomes <- function(d, by) {
  d %>%
    group_by({{ by }}) %>%
    summarize(cases_filed    = sum(cases_filed, na.rm = TRUE),
              cases_eviction = sum(plaintiff_judgments, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols = c(cases_filed, cases_eviction),
                 names_to = "Outcome", values_to = "Number", names_prefix = "cases_")
}

# Identify time span of current data
id_time_span <- function(dat) {
  min_month <- min(ym(dat$filing_month)) %>% format('%m') %>% as.numeric()
  min_year <- min(ym(dat$filing_month)) %>% format('%Y')
  max_month <- max(ym(dat$filing_month)) %>% format('%m') %>% as.numeric()
  max_year <- max(ym(dat$filing_month)) %>% format('%Y')
  list(paste0(month.name[min_month], ' ', min_year), paste0(month.name[max_month], ' ', max_year))
}
time_span <- id_time_span(monthly_plaintiff_dat)

# Court picker ----
court_choices <- sort(unique(plaintiff_dat$county))
# Button and status text for the court picker, in the current language.
# `tr` is a translate function: identity at startup, i18n$t in the server.
court_picker_options <- function(tr = identity) {
  pickerOptions(
    actionsBox = TRUE,                # "Select all" / "Deselect all" buttons
    liveSearch = TRUE,                # type to filter the list
    liveSearchNormalize = TRUE,       # match without accents
    selectedTextFormat = "count > 3", # show "125 of 125 ..." instead of a long list
    size = 10,
    selectAllText = tr("Select all"),
    deselectAllText = tr("Deselect all"),
    noneSelectedText = tr("No court jurisdictions selected"),
    noneResultsText = tr("No courts match {0}"),
    liveSearchPlaceholder = tr("Search courts"),
    countSelectedText = tr("{0} of {1} court jurisdictions selected")
  )
}

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
        tags$p(class = "page-description", 
          tags$span(i18n$t("The Virginia Evictors Catalog provides data about plaintiffs filing unlawful detainers (evictions) in Virginia's General District Courts from ")),
          tags$span(time_span[[1]]),
          tags$span(i18n$t(" through ")),
          tags$span(paste0(time_span[[2]],". ")),
          tags$span(i18n$t("Each row in the table below represents a plaintiff filing in a specific court jurisdiction."))
        ),
        tags$div(htmlOutput("writ")),
        bs_button(i18n$t("How to search the catalog"), button_type = "info", class = "collapsible") %>%
          bs_attach_collapse("orient-collapse"),
        bs_collapse(
          id = "orient-collapse",
          content = tags$div(class = "well",
                            tags$div(
                              htmlOutput("orient"), class = "orient")))
    )),
    fluidRow(
      column(6,
        wellPanel(
          pickerInput("court", i18n$t("Select Court Jurisdictions:"),
                      choices = court_choices,
                      selected = court_choices,
                      multiple = TRUE,
                      width = "100%",
                      options = court_picker_options()),
          helpText(i18n$t("Note: Select one or more court jurisdictions to show in the table and visualizations. Open the list to search for a court, click courts to add or remove them, or use Select all and Deselect all."))
        )
      ),
      column(6,
        wellPanel(
          radioButtons("time", i18n$t("Select Time Period:"),
                      choices = c("Totals across All Years" = "All",
                                      "Totals by Year" = "Year",
                                      "Totals by Month" = "Month"),
                      selected = "All"
          ),
          helpText(htmlOutput("selection_note_time"))
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
            helpText(i18n$t("Downloads the rows shown in the table, including any search or column filters.")),
            DTOutput("plaintiff_table")),
          tabPanel(i18n$t("Visualize"), 
            icon = icon("chart-bar"), 
            textOutput("viztitle"), 
            plotlyOutput("viz", width = "100%", height = "700"))
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
      id = "main-page",
      title = i18n$t("Virginia Evictors Catalog"),
    #  windowTitle = "Virginia Evictors Catalog",
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
    ), # end navbarPage
  ##-- Footer ----
  uiOutput("footer")
) # end bootstrapPage

# Server ----
server <- function(input, output, session) {

  # Language ----
  # Switch the session's language, then update the inputs whose labels are set
  # on the server (radio buttons and the court picker).
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
    updateRadioButtons(session, "time",
                      label = i18n$t("Select Time Period:"),
                      choices = setNames(c("All", "Year", "Month"),
                                         i18n$t(c("Totals across All Years", "Totals by Year", "Totals by Month"))),
                      selected = input$time)
    updatePickerInput(session, "court",
                      selected = input$court,
                      options = court_picker_options(i18n$t))
  })

  # Page text ----
  # Each output shows its page in the selected language (see `pages` above)
  output$header <- renderUI(header)
  for (page_name in names(pages$English)) {
    local({
      name <- page_name
      output[[name]] <- renderUI(pages[[req(input$selected_language)]][[name]])
    })
  }

  # Chart title, one per time period
  output$viztitle <- renderText({
    switch(input$time,
      All   = i18n$t("Cases Filed and Eviction Judgments within Selected Court Jurisdictions (Totals by Selected Jurisdictions, All Years)"),
      Year  = i18n$t("Cases Filed and Eviction Judgments within Selected Court Jurisdictions (Totals by Year, of Selected Jurisdictions)"),
      Month = i18n$t("Cases Filed and Eviction Judgments within Selected Court Jurisdictions (Totals by Month, of Selected Jurisdictions)"))
  })

  # Pick and subset data for datatable
  df <- reactive({
    # Show a message instead of broken charts when no court is selected
    validate(need(length(input$court) > 0,
                  i18n$t("Select at least one court jurisdiction.")))
    d <- switch(input$time,
                "All" = plaintiff_dat,
                "Year" = yearly_plaintiff_dat,
                "Month" = monthly_plaintiff_dat)
    d <- d %>% filter(county %in% input$court)
  })

  # Function for download button
  # Download exactly the rows the table shows: the court and time period picked
  # above, plus any search box or column filters typed into the table itself.
  output$downloadBtn <- downloadHandler(
    filename = function() {
      period <- c(All = "all-years", Year = "by-year", Month = "by-month")[[input$time]]
      paste0("va-evictors-catalog-", period, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- df()
      # Row numbers (in the table's current sort order) that survive the table's
      # filters. NULL until the table has drawn, so fall back to every row.
      rows <- input$plaintiff_table_rows_all
      if (!is.null(rows)) {
        rows <- rows[rows <= nrow(d)]
        d <- d[rows, , drop = FALSE]
      }
      write.csv(d, file, row.names = FALSE)
    }
  )

  # Render datatable ----
  output$plaintiff_table <- DT::renderDT({
    datatable(df(),
              rownames = FALSE,
              caption = i18n$t("Sources: Legal Services Corporation (lsc.gov)"),
              class = 'display nowrap',
              filter = 'top',
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
                             pageLength = 20,
                             order = list(2, 'desc'), # column indexing starts at 0 (3rd column visually is indexed as 2)
                             dom = 'lfrtip',
              language = if (input$selected_language == "Español") {
                  # Data table translation options: https://datatables.net/plug-ins/i18n/#Translations
                  list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/es-MX.json")
                } else {
                  NULL
                }),
              colnames = c(i18n$t("Court Jurisdiction"), i18n$t("Plaintiff Name"), 
                           i18n$t("Cases Filed"), i18n$t("Eviction Judgments"), i18n$t("Serial Filings"),
                           i18n$t("Time Frame"), i18n$t("Known Virginia Defendant ZIP Codes")),
              # the following JS inserts an aria-label for the column search fields to address the missing form control label for accessibility
              callback = JS("
                formControl = $('#plaintiff_table tr td div.form-group input.form-control');
                $(formControl).attr('aria-label', 'Search Column');
              ")
              # Draft column-header tooltips are kept in notes/column-tooltips-draft.js
    )
  }, server = TRUE)

  # Output visuals ----
  output$viz <- renderPlotly({

    # Translated labels (i18n$t follows the language picked in this session)
    lbl_filed   <- i18n$t("Cases Filed")
    lbl_evict   <- i18n$t("Eviction Judgments")
    outcome_pal <- setNames(pal_lake_superior, c(lbl_evict, lbl_filed))
    label_outcome <- function(x) {
      factor(ifelse(x == "filed", lbl_filed, lbl_evict), levels = c(lbl_evict, lbl_filed))
    }

    if (input$time == 'All') {
      p <- df() %>%
        mutate(county = str_remove(county, "General District Court"),
               county = str_trim(county)) %>%
        summarise_outcomes(county) %>%
        mutate(Outcome = label_outcome(Outcome),
               tip = paste0(i18n$t("Court Jurisdiction"), ": ", county,
                            "<br>", Outcome, ": ", comma(Number))) %>%
        ggplot(aes(y = fct_reorder(county, Number),
                   x = Number,
                   color = Outcome,
                   text = tip)) +
        geom_segment(aes(x = 0, xend = Number, y = fct_reorder(county, Number), yend = fct_reorder(county, Number)),
                     color = "gray") +
        geom_point(size = 2) +
        scale_color_manual(values = outcome_pal, name = "") +
        scale_x_continuous(expand = expansion(mult = c(0, .05))) +
        labs(x = "", y = "") +
        # Complete theme first, then tweaks (a complete theme resets anything set before it)
        theme_classic() +
        theme(legend.position = "top")

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>%
        config(displayModeBar = TRUE)

    } else if (input$time == "Year")  {
      yearly <- df() %>%
        summarise_outcomes(filed_year) %>%
        mutate(Outcome = label_outcome(Outcome),
               tip = paste0(i18n$t("Year"), ": ", filed_year,
                            "<br>", Outcome, ": ", comma(Number)))

      # Eviction bars are drawn narrower, on top of the cases-filed bars
      p <- ggplot(yearly, aes(x = filed_year, y = Number, fill = Outcome, text = tip)) +
        geom_col(data = filter(yearly, Outcome == lbl_filed)) +
        geom_col(data = filter(yearly, Outcome == lbl_evict), width = 0.70) +
        scale_fill_manual(values = outcome_pal, breaks = c(lbl_filed, lbl_evict), name = "") +
        labs(x = i18n$t("Year"), y = "") +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>%
        config(displayModeBar = TRUE)

    } else {
      monthly <- df() %>%
        summarise_outcomes(filing_month) %>%
        mutate(Outcome = label_outcome(Outcome),
               Month = as.Date(paste(filing_month, "-01", sep="")),
               tip = paste0(i18n$t("Month"), ": ", format(Month, "%b %Y"),
                            "<br>", i18n$t("Number"), ": ", comma(Number),
                            "<br>", i18n$t("Outcome"), ": ", Outcome))

      ymax <- max(monthly$Number)
      state <- filter(moratoriums, type == "state")
      federal <- filter(moratoriums, type == "federal")

      moratorium_label <- paste(
        c(paste0("<span style='font-weight:700;'>", i18n$t("State-Wide Eviction Moratoriums"), "</span>"),
          format_span(state$start, state$end),
          "",
          paste0("<span style='font-weight:700;color:#004E5E;'>", i18n$t("Federal Eviction Moratorium (CDC Order)"), "</span>"),
          paste0("<span style='color:#004E5E;'>", format_span(federal$start, federal$end), "</span>")),
        collapse = "\n")

      p <- ggplot(monthly) +
        # Moratorium bands: one rectangle per moratorium (inherit.aes = FALSE keeps
        # ggplot from repeating each band once per row of the monthly data)
        geom_rect(data = federal, aes(xmin = start, xmax = end),
                  ymin = 0, ymax = ymax * 1.05, inherit.aes = FALSE,
                  alpha = .1, fill = "#004E5E") +
        geom_rect(data = state, aes(xmin = start, xmax = end),
                  ymin = 0, ymax = ymax * 1.05, inherit.aes = FALSE,
                  alpha = .15, fill = "#A9A9A9") +
        annotate("text", x = as.Date("2021-05-01"), y = ymax * 0.925,
                 label = moratorium_label, size = 3.3) +
        suppressWarnings(geom_point(aes(x = Month, y = Number, group = Outcome, color = Outcome, text = tip))) +
        geom_line(aes(x = Month, y = Number, group = Outcome, color = Outcome)) +
        scale_x_date(date_breaks = "3 months", date_labels =  "%b %Y", expand = c(0.01,0.01)) +
        scale_y_continuous(expand = c(0.01,0.01)) +
        scale_color_manual(values = outcome_pal, name = "") +
        labs(x = i18n$t("Year-Month"), y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45), legend.position = "bottom")

      gg <- ggplotly(p, tooltip = c("text")) %>%
        layout(legend = list(orientation = "h", x = 0, y = 10)) %>%
        config(displayModeBar = TRUE)

      # Turn off hover on the moratorium note. Find its trace by type rather than by
      # position, so adding or reordering layers doesn't break this.
      text_traces <- which(vapply(gg$x$data, function(tr) identical(tr$mode, "text"), logical(1)))
      for (i in text_traces) gg$x$data[[i]]$hoverinfo <- "none"
      gg

    }
  })

}

# Run app
shinyApp(ui = ui, server = server)
