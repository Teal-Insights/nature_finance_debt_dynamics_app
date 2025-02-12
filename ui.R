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
source(file = "R/imf_key_data.R")
source(file = "R/imf_countries.R")
source(file = "R/imf_indicators.R")
source(file = "R/imf_format_years.R")
source(file = "R/ui_create_shock_table.R")

# components
source(file = "components/ui_footer_component.R")
source(file = "components/ui_documentation_component.R")
# data: -------------------------------------------------------------------
df_countries <- imfweo::weo_list_countries() %>% 
  rename(iso3c = 'country_code', label = "country_name")

# pickers: ----------------------------------------------------------------
select_country <- df_countries %>% 
  filter(!is.na(label)) %>% 
  pull(label) %>% 
  unique() %>% 
  gsub(pattern = "The ", x = ., replacement = "") %>% 
  sort() 

# projection year
current_year <- lubridate::year(Sys.Date())
projection_years <- seq(from = current_year, by = 1, length.out = 4)
# ui ----------------------------------------------------------------------
ui <- bslib::page_navbar(
  # Initialize shinyjs
  shinyjs::useShinyjs(), 
  # nav section background colour
  bg = "#2c3e50",
  title = div(
    class = "header-container",
    tags$img(
      src = "logo.svg", 
      class = "header-logo"
    ),
    span(
      class = "header-title",
      "Public Debt Policy Shock Analysis"
    )
  ),
  header = div(  # This will stay at the top
    # div(
    #   style = "text-align: center; font-weight: bold; font-size: 20px; color: red; margin-bottom: 0px;",
    #   "This app is under development"
    # )
  ),
  tags$head(
    # icon part
    tags$link(rel = "icon", type = "image/x-icon", href = "icon.png"),
    # navbar css file
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/navbar.css"),
    # Integrated styles
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/integrated.css"),
    # footer styles
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/footer.css"),
    # documentation styles
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/documentation.css")
  ),
  bslib::nav_spacer(),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty"
  ) %>%
    bslib::bs_add_rules("
      hr {
        margin-top: 5px;
        margin-bottom: 5px;
      }
    "),
  
  # Home Panel
  bslib::nav_panel(
    title = "Home",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # sidebar section background colour
          bg = "#2c3e50",
          # Select Country
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
          hr(),
          # projection year picker
          shinyWidgets::pickerInput(
            inputId = "projection_year",
            label = "Projections start in",
            choices = projection_years,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 3",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Search year..."
            ),
            multiple = FALSE
          ),
          hr(),
          # Guidance text with custom styling
          tags$div(
            class = "guide-box p-3 mb-3 border rounded",
            tags$p(
              h5(icon("info-circle"), 
                 "Overview:"),
              class = "fw-bold"
            ),
            tags$ul(
              tags$li(
                "Key indicators in debt projections are ",tags$b("Primary balance")," , ",
                tags$b("Real effective interest rate"), " and ", tags$b("GDP growth. "),
                "All these indicators are in ",tags$b("%.")
              ),
              tags$li(
                "Navigate to the policy shock column under each indicator",
                tags$br(),
                tags$span(
                  "Look for ", 
                  tags$b("input boxes"),
                  " to modify shock values."
                )
              ),
              tags$li(
                "Final shock is the sum of ",tags$b("baseline")," values plus ",tags$b("policy")," shock values"
              )
            )
          ),
          hr()
        ),
        bslib::layout_column_wrap(
          width = NULL,
          heights_equal = "row",
          layout_column_wrap(
            width = 1/3,
            heights_equal = "row",
            # GDP Growth Card
            card(
              full_screen = TRUE,
              card_header(
                class = "bg-primary text-white",
                tags$span(textOutput(outputId = "selected_country_header_gdp", inline = TRUE),": GDP Growth Shock")
              ),
              ui_create_shock_table("gdp")
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "bg-primary text-white",
                tags$span(textOutput(outputId = "selected_country_header_pb", inline = TRUE),": Primary Balance Shock")
              ),
              ui_create_shock_table("pb")
            ),
            # Real Interest Rate Card
            card(
              full_screen = TRUE,
              card_header(
                class = "bg-primary text-white",
                tags$span(textOutput(outputId = "selected_country_header_ir", inline = TRUE),": Real Interest Rate Shock")
              ),
              ui_create_shock_table("ir")
            )
          )
        ),
        bslib::layout_column_wrap(
          width = NULL,
          heights_equal = "row",
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                tags$span(textOutput(outputId = "selected_country_header_ih", inline = TRUE),": Historical Debt trends"), 
                class = "bg-primary text-white"),
              echarts4r::echarts4rOutput(outputId = "plot_full_input")
            ),
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                tags$span(textOutput(outputId = "selected_country_header_ip", inline = TRUE),": Projected Debt trends"), 
                class = "bg-primary text-white"),
              echarts4r::echarts4rOutput(outputId = "plot_projection_input")
            )
          )
        )
      )
    )
  ),
  
  # Graph panel: ------------------------------------------------------------
  # Graph Panel
  bslib::nav_panel(
    title = "Graph",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # sidebar background colour
          bg = "#2c3e50",
          # Shock selection
          shinyWidgets::pickerInput(
            inputId = "id_shock",
            label = "Select shock",
            choices = c("Policy shock","GDP growth", "Primary balance", "Real effective interest rate"),
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 3"
            )
          ),
          hr(),
          selectInput("file_type_projection", "Select: File Type", choices = c("CSV", "Excel")),
          downloadButton("download_projection", "Download Data"),
          hr(),
          h6("Excel Template"),
          downloadButton("download_template", "Download Template"),
          hr()
        ),
        bslib::layout_column_wrap(
          width = NULL,
          heights_equal = "row",
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              full_screen = TRUE,
              bslib::card_header(
                tags$span(textOutput(outputId = "selected_country_header_gh", inline = TRUE),": Historical Debt trends"), 
                class = "bg-primary text-white"),
              echarts4r::echarts4rOutput(outputId = "plot_full")
            ),
            bslib::card(
              width = 1,
              full_screen = TRUE,
              bslib::card_header(
                tags$span(textOutput(outputId = "selected_country_header_gp", inline = TRUE),": Projected Debt trends"), 
                class = "bg-primary text-white"),
              echarts4r::echarts4rOutput(outputId = "plot_projection")
            )
          ),
          card(
            width = 1,
            full_screen = TRUE,
            bslib::card_header(
              tags$span(textOutput(outputId = "selected_country_header_gt", inline = TRUE),": Shock analysis result table"), 
              class = "bg-primary text-white"),
            card_body(
              div(
                class = "table-container",
                div(
                  class = "table-responsive",
                  DT::DTOutput(outputId = "data_projection")
                )
              )
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
          # sidebar background colour
          bg = "#2c3e50",
          selectInput("data_format", "Display data", choices = c("Wide", "Long")),
          hr(),
          selectInput("file_type", "Select: File Type", choices = c("CSV", "Excel")),
          hr(),
          downloadButton("downloadData", "Download Data"),
          hr()
        ),
        bslib::layout_column_wrap(
          card(
            card_body(
              div(
                class = "table-container",
                div(
                  class = "table-responsive",
                  DTOutput("full_data")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Documentation Panel
  bslib::nav_panel(
    title = "Documentation",
    # overview header card
    ui_documentation_component()
  ),
  
  # Footer
  ui_footer_component()
)