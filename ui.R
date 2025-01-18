# starts: -----------------------------------------------------------------
# loading necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(DT)
})
# Rscripts: ---------------------------------------------------------------
source(file = "R/imf_countries.R")
source(file = "R/imf_indicators.R")
source(file = "R/imf_series.R")
source(file = "R/imf_format_years.R")
source(file = "R/ui_create_shock_table.R")

# data: -------------------------------------------------------------------
df_countries <- imf_countries()

# pickers: ----------------------------------------------------------------
select_country <- df_countries %>% 
  filter(!is.na(label)) %>% 
  pull(label) %>% 
  unique() %>% 
  sort()

# ui ----------------------------------------------------------------------
ui <- bslib::page_navbar(
  shinyjs::useShinyjs(),  # Initialize shinyjs,
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
  # Add favicon to the head section
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "logo.png"),
    tags$style(HTML("
      .table-container {
        background-color: #ffffff;
        border-radius: 4px;
      }
      .row-bordered {
        border-bottom: 1px solid #dee2e6;
        padding: 8px 0;
      }
      .header-row {
        border-bottom: 2px solid #0d6efd;
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
  tags$div(
    style = "text-align: center; font-weight: bold; font-size: 20px; color: red; margin-bottom: 0px;",
    "This app is under development"
  )
  ,
  bslib::nav_spacer(),
  theme = bslib::bs_theme(
    version = 5, # Use Bootstrap 5 for bslib
    bootswatch = "minty"
  ) %>%
    bslib::bs_add_rules("
      hr {
        margin-top: 5px !important;
        margin-bottom: 5px !important;
      }
    "),
  bslib::nav_panel(
    title = "Input",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # Select Country
          shinyWidgets::pickerInput(
            inputId = "id_country", 
            label = "Select: Country", 
            choices = select_country, 
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = FALSE
          ),
          hr()
        ),
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header("Shocking key indicators"),
            bslib::layout_column_wrap(
              width = NULL, # Allow nested layouts with manual control
              heights_equal = "row", # Ensure equal heights within rows
              # First row with two cards
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
  bslib::nav_panel(
    title = "Graph",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # Shock selection
          shinyWidgets::pickerInput(
            inputId = "id_shock", 
            label = "Select shock", 
            choices = c("Primary balance", "GDP growth", "Real effective interest rate"), 
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE, 
              size = 10,
              selectedTextFormat = "count > 3"
            ), 
            multiple = FALSE
          ),
          hr(),
          selectInput("file_type_projection", "Select: File Type", choices = c("CSV", "Excel")),
          downloadButton("download_projection", "Download Data")
          ),
        bslib::layout_column_wrap(
          width = NULL, # Allow nested layouts with manual control
          heights_equal = "row", # Ensure equal heights within rows
          # First row with two cards
          bslib::layout_column_wrap(
            width = 1 / 2, # Each card takes half the row
            bslib::card(
              bslib::card_header("Plot: Baseline vs Projection"),
              shiny::plotOutput(outputId = "plot_full")
            ),
            bslib::card(
              bslib::card_header("Plot: Projection"),
              shiny::plotOutput(outputId = "plot_projection")
            )
          ),
          # Second row with a full-width card
          bslib::card(
            width = 1, # Full-width for this card
            bslib::card_header("Data"),
            DT::DTOutput(outputId = "data_projection")
          )
        )
        )
      )
    ),
  bslib::nav_panel(
    title = "Data",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # open = TRUE,
          # Placeholder for additional inputs
          selectInput("data_format", "Select: Data format", choices = c("Wide", "Long")),
          selectInput("file_type", "Select: File Type", choices = c("CSV", "Excel")),
          downloadButton("downloadData", "Download Data")
        ),
        bslib::layout_column_wrap(
          bslib::card(
            DT::DTOutput(outputId = "full_data")
          )
        )
      )
    )
                   ),
  bslib::nav_panel(title = "Documentation")
  ,
  # Footer -----------------------------------------------------------------
  tags$footer(
    style = "
      text-align: center; 
      padding: 10px; 
      background-color: #f8f9fa; 
      font-size: 14px; 
      font-style: bold;
    ",
    tags$p(
      paste0(
        "Nature Finance | © ", lubridate::year(Sys.Date()), " | All Rights Reserved."
      )
    )
  )
)