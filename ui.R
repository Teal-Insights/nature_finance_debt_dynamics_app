# starts: -----------------------------------------------------------------
# loading necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(DT)
  library(highcharter)
})

# Rscripts: ---------------------------------------------------------------
source(file = "R/imf_countries.R")
source(file = "R/imf_indicators.R")
source(file = "R/imf_series.R")
source(file = "R/imf_format_years.R")
source(file = "R/ui_create_shock_table.R")

# data: -------------------------------------------------------------------
df_countries <- imfweo::weo_list_countries() %>% 
  rename(iso3c = 'country_code', label = "country_name")

# pickers: ----------------------------------------------------------------
select_country <- df_countries %>% 
  filter(!is.na(label)) %>% 
  pull(label) %>% 
  unique() %>% 
  sort()

# ui ----------------------------------------------------------------------
ui <- bslib::page_navbar(
  # Initialize shinyjs
  shinyjs::useShinyjs(),  
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "logo.png", 
      height = "40px", 
      style = "margin-right: 10px; border-radius: 50%;"
    ),
    span(
      "Public Debt Policy shock analysis",
      style = "font-weight: bold;"
    )
  ),
  header = tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "logo.png"),
    tags$style(HTML("
      .bslib-page-fill {
        min-height: 100vh !important;
        height: auto !important;
      } 
      .table-container {
        background-color: #ffffff;
        border-radius: 4px;
      }
      .row-bordered {
        border-bottom: 1px solid #dee2e6;
        padding: 8px 0;
      }
      .header-row {
        border-bottom: 2px solid #b0b2b4;
        border-top: 2px solid #b0b2b4;
        background-color: #f8f9fa;
        font-weight: bold;
        padding: 8px 0;
        margin-bottom: 8px;
      }
      .card {
        margin-bottom: 1rem;
      }
      .form-control {
        font-size: 0.9rem;
      }
    "))
  ),
  bslib::nav_spacer(),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty"
  ) %>%
    bslib::bs_add_rules("
      hr {
        margin-top: 5px !important;
        margin-bottom: 5px !important;
      }
    "),
  
  # Input Panel
  bslib::nav_panel(
    title = "Input",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # Select Country
          hr(),
          shinyWidgets::pickerInput(
            inputId = "id_country",
            label = "Select: Country",
            choices = select_country,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 3",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Search country..."
            ),
            multiple = FALSE
          ),
          hr()
        ),
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header("Shocking key indicators", class = "bg-primary text-white"),
            bslib::layout_column_wrap(
              width = NULL,
              heights_equal = "row",
              layout_column_wrap(
                width = 1/3,
                heights_equal = "row",
                # Primary Balance Card
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "bg-primary text-white",
                    "Primary Balance Shock"
                  ),
                  ui_create_shock_table("pb")
                ),
                # Real Interest Rate Card
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "bg-primary text-white",
                    "Real Interest Rate Shock"
                  ),
                  ui_create_shock_table("ir")
                ),
                
                # GDP Growth Card
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "bg-primary text-white",
                    "GDP Growth Shock"
                  ),
                  ui_create_shock_table("gdp")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Graph Panel
  bslib::nav_panel(
    title = "Graph",
    div(
      id = "graph-page",
      bslib::card(
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            # Shock selection
            hr(),
            shinyWidgets::pickerInput(
              inputId = "id_shock",
              label = "Select shock",
              choices = c("All","GDP growth", "Primary balance", "Real effective interest rate"),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3"
              ),
              multiple = FALSE
            ),
            hr(),
            selectInput("file_type_projection", "Select: File Type", choices = c("CSV", "Excel")),
            downloadButton("download_projection", "Download Data"),
            hr()
          ),
          bslib::layout_column_wrap(
            width = NULL,
            heights_equal = "row",
            bslib::layout_column_wrap(
              width = 1/2,
              bslib::card(
                full_screen = TRUE,
                bslib::card_header("Historical Debt trends", class = "bg-primary text-white"),
                highcharter::highchartOutput(outputId = "plot_full")
              ),
              bslib::card(
                full_screen = TRUE,
                bslib::card_header("Projected Debt trends", class = "bg-primary text-white"),
                highcharter::highchartOutput(outputId = "plot_projection")
              )
            ),
            bslib::card(
              width = 1,
              full_screen = TRUE,
              bslib::card_header("Shock analysis result table", class = "bg-primary text-white"),
              DT::DTOutput(outputId = "data_projection")
            )
          )
        )
      )
    )
  ),
  
  # Data Panel
  bslib::nav_panel(
    title = "Data",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          hr(),
          selectInput("data_format", "Select: Data format", choices = c("Wide", "Long")),
          hr(),
          selectInput("file_type", "Select: File Type", choices = c("CSV", "Excel")),
          hr(),
          downloadButton("downloadData", "Download Data"),
          hr()
        ),
        bslib::layout_column_wrap(
          bslib::card(
            DT::DTOutput(outputId = "full_data")
          )
        )
      )
    )
  ),
  
  # Documentation Panel
  bslib::nav_panel(
    title = "Documentation"
  ),
  
  # Footer
  footer = tags$footer(
    style = "
      text-align: center; 
      padding: 10px; 
      background-color: #f8f9fa; 
      font-size: 14px; 
      font-style: bold;
    ",
    tags$p(
      tags$a(
        href = "https://www.naturefinance.net/",
        target = "_blank",
        "Nature Finance"
      ),
      " | "
      ,
      tags$a(
        href = "#",
        "Teal Insights"
      ),
      paste0(
        " | © ", lubridate::year(Sys.Date())
      )
    )
  )
)