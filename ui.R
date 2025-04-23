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
source(file = "components/ui_contact_component.R")
# data: -------------------------------------------------------------------
df_countries <- imf_countries()
# pickers: ----------------------------------------------------------------
select_country <- df_countries %>%
  filter(!is.na(label)) %>%
  pull(label) %>%
  unique() %>%
  gsub(pattern = "The ", x = ., replacement = "") %>%
  sort()

# projection year
current_year <- lubridate::year(Sys.Date())
projection_years <- seq(from = current_year, by = 1, length.out = 4)[1]
# ui ----------------------------------------------------------------------
ui <- bslib::page_navbar(
  # Initialize shinyjs
  shinyjs::useShinyjs(),
  id = "main_navbar",
  # nav section background colour
  bg = "#2c3e50",
  title = div(
    class = "header-container",
    tags$img(
      src = "ssdh_logo.svg",
      class = "header-logo"
    ),
    div(
      class = "header-title-container",
      style = "display: flex; flex-direction: column; justify-content: center;",
      span(
        class = "header-title",
        style = "font-size: 1.5rem; font-weight: 600; color: white; line-height: 1.2;",
        "Debt Path Explorer"
      )
    )
  ),
  # CSS imports
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/x-icon", href = "ssdh_icon.png"),
    
    # tabs
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_home.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_analysis.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_data.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/documentation.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_contact.css"),
    
    # navbar and footer
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/navbar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/footer.css"),
    
    # integrated
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/integrated.css"),
    
    # bootstrap
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
    )
  ),
  
  # theme of the app
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  
  # adding space between title and other tabs
  bslib::nav_spacer(),
  
  # -------------------------------------------------------------------------
  # Home Panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Home",
    bslib::layout_column_wrap(
      width = 1 / 2,
      heights_equal = "row",
      # About the FIMA Explorer App
      bslib::card(
        full_screen = TRUE,
        height = 400,
        class = "border-0 shadow-none",
        bslib::card_header(h5("About the Debt Path Explorer App")),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE,
          # About the app with title on border
          navset_tab(
            nav_panel(
              title = "About the app", 
              tags$div(
                class = "home-box-container", 
                style = "position: relative; margin-top: 10px; padding-top: 1px;",
                
                # Title that will appear on the border
                tags$div(class = "home-box-title", "About the app"),
                
                # Content box
                tags$div(
                  class = "home-box p-2 mb-0 border rounded steps",
                  tags$h6("Welcome to the Debt Path Explorer (PROTOTYPE)", class = "top-header"),
                  p(
                    "
                      This web application demonstrates how policy changes could 
                      affect sovereign debt trajectories. It uses IMF data and 
                      standard debt dynamics equations to project how different 
                      macroeconomic factors influence debt sustainability. All 
                      projections should be viewed as illustrative scenarios, 
                      not forecasts.
                    "
                  ),
                  
                  # -------------------------------
                  # Purpose of This Tool
                  # -------------------------------
                  
                  tags$h6("Purpose of This Tool"),
                  tags$ul(
                    class = "green-bullets",
                    tags$li(
                      span("Explore how policy changes affect sovereign debt sustainability through interactive scenarios")
                    ),
                    tags$li(
                      span("Visualize debt trajectories based on adjustments to key macroeconomic factors")
                    ),
                    tags$li(
                      span("Understand debt dynamics using standard equations and IMF baseline data")
                    ),
                    tags$li(
                      span("Create illustrative scenarios to support policy discussions and analysis")
                    ),
                    tags$li(
                      span("Compare historical debt trends with projected outcomes based on different policy shocks")
                    ),
                    tags$li(
                      span("Provide a transparent, accessible tool for evaluating debt sustainability across countries")
                    )
                  ),
                  p(
                    "
                      This tool provides order-of-magnitude approximations as a 
                      first step before detailed economic analysis on a 
                      country-specific basis.
                    " 
                  ),
                  p(
                    "
                      This is an early prototype - we welcome your feedback on 
                      functionality, usability, and potential applications to 
                      help guide future development.
                    "
                  )
                )
              )
            ),
            nav_panel(
              title = "Guide on using the app", 
              # home tab with title on border
              tags$div(
                class = "home-box-container", 
                style = "position: relative; margin-top: 10px; padding-top: 1px;",
                # Title that will appear on the border
                tags$div(class = "home-box-title", "Home tab"),
                
                # Content box
                tags$div(
                  class = "home-box steps",
                  # step 1
                  h6("Step 1 : Select Country", class = "top-header"),
                  p(
                    "
                      Under select country dropdown, select country of your choice.
                    "
                  )
                )
              ),
              # Analysis tab box with title on border
              tags$div(
                class = "home-box-container", 
                style = "position: relative; margin-top: 10px; padding-top: 1px;",
                
                # Title that will appear on the border
                tags$div(class = "home-box-title", "Analysis tab"),
                
                # Content box
                tags$div(
                  class = "home-box steps",
                  # step 2
                  h6("Step 2 : Shock input", class = "top-header"),
                  tags$ul(
                    class = "green-bullets",
                    tags$li(
                      span(
                        "Key indicators in debt projections are ",
                        tags$b("Primary balance"), " , ",
                        tags$b("Real effective interest rate"), " and ",
                        tags$b("GDP growth. "),
                        "All these indicators are in ", tags$b("%.")
                      )
                    ),
                    tags$li(
                      span(
                        "
                          Navigate to the Policy Shock column under each 
                          indicator Look for ",tags$b("input boxes"),
                          " to modify shock values.
                        "
                      )
                    ),
                    tags$li(
                      span(
                        "The ", tags$b("Policy-Adjusted Forecast"),
                        " is the sum of the", tags$b("IMF WEO Baseline"),
                        " values plus the user-provided ", tags$b("Policy Shock"),
                        " values.", 
                      )
                    ),
                    tags$li(
                      span(
                        " The ", tags$b("Policy-Adjusted Forecast"),
                        "values are plugged into the debt dynamics equation to
                         calculate the new debt path."
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # -------------------------------
      # Assessment
      # -------------------------------
      
      bslib::card(
        full_screen = TRUE,
        height = 400,
        class = "border-0 shadow-none",
        bslib::card_header(h5("Assessment")),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE,
          #---------------
          # select country
          #---------------
          tags$div(
            class = "home-box-container", 
            style = "position: relative; margin-top: 10px; padding-top: 1px;",
            # Title that will appear on the border
            tags$div(class = "home-box-title", "Country"),
            # Content box
            tags$div(
              class = "home-box steps",
              style = "padding-top: 10px; padding-bottom: 0px;", 
              div(
                style = "margin-top: 5px; margin-bottom: 5px;", 
                shinyWidgets::pickerInput(
                  inputId = "id_country",
                  label = NULL,
                  choices = select_country,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    size = 10,
                    selectedTextFormat = "count > 3",
                    liveSearch = TRUE,
                    liveSearchStyle = "contains",
                    liveSearchPlaceholder = "Select country...",
                    title = "Select country..."
                  ),
                  multiple = FALSE,
                  selected = NULL,
                  width = "300px"
                )
              )
            )
          ),
          # projections start in
          conditionalPanel(
            condition = "input.id_country !== null && input.id_country !== ''",
            tags$div(
              class = "home-box-container", 
              style = "position: relative; margin-top: 10px; padding-top: 1px;",
              
              # Title that will appear on the border
              tags$div(class = "home-box-title", "Projection start"),
              
              # Content box
              tags$div(
                class = "home-box steps",
                style = "padding-top: 10px; padding-bottom: 0px;", 
                div(
                  style = "margin-top: 5px; margin-bottom: 5px;", 
                  shinyWidgets::pickerInput(
                    inputId = "projection_year",
                    label = NULL,
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
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  # -------------------------------------------------------------------------
  # Analysis
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Analysis",
    value = "analysis",
    # main content area
    bslib::layout_column_wrap(
      width = NULL,
      heights_equal = "row",
      layout_column_wrap(
        width = 1 / 3,
        heights_equal = "row",
        # GDP Growth Card
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            tags$span("GDP Growth Shock")
          ),
          ui_create_shock_table("gdp")
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            tags$span("Primary Balance Shock")
          ),
          ui_create_shock_table("pb")
        ),
        # Real Interest Rate Card
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            tags$span("Real Interest Rate Shock"
            )
          ),
          ui_create_shock_table("ir")
        )
      )
    ),
    bslib::layout_column_wrap(
      width = NULL,
      heights_equal = "row",
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            tags$span("Historical Debt trends"),
            class = "bg-primary text-white"
          ),
          echarts4r::echarts4rOutput(outputId = "plot_full_input")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            tags$span("Projected Debt trends"),
            class = "bg-primary text-white"
          ),
          echarts4r::echarts4rOutput(outputId = "plot_projection_input")
        )
      )
    )
  ),
  
  # -------------------------------------------------------------------------
  # Graph panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Graph",
    value = "graph",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        # sidebar background colour
        bg = "#2c3e50",
        # Shock selection
        shinyWidgets::pickerInput(
          inputId = "id_shock",
          label = "Select shock",
          choices = c(
            "Policy shock", "GDP growth", "Primary balance",
            "Real effective interest rate"
          ),
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            size = 10,
            selectedTextFormat = "count > 3"
          )
        ),
        hr(),
        selectInput("file_type_projection", "Select: File Type",
                    choices = c("CSV", "Excel")
        ),
        downloadButton("download_projection", "Download Data"),
        hr(),
        h6("Excel Template"),
        downloadButton("download_template", "Download Template"),
        hr()
      ),
      card(
          width = 1,
          full_screen = TRUE,
          bslib::card_header(
            tags$span("Shock analysis result table"
            ),
            class = "bg-primary text-white"
          ),
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
  ),
  
  # -------------------------------------------------------------------------
  # Data Panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Data",
    value = "data",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          # sidebar background colour
          bg = "#2c3e50",
          # select input for data display type
          selectInput("data_format", "Display data",
            choices = c("Wide", "Long")
          ),
          hr(),
          # select input for file type
          selectInput("file_type", "Select: File Type",
            choices = c("CSV", "Excel")
          ),
          hr(),
          # download button
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
  # -------------------------------------------------------------------------
  # Documentation panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Documentation",
    value = "docs",
    # Documentation component
    ui_documentation_component()
  ),
  # -------------------------------------------------------------------------
  # Contact panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Contact",
    value = "contact",
    # contact component
    ui_contact_component()
  ),
  # -------------------------------------------------------------------------
  # general footer
  # -------------------------------------------------------------------------
  footer = ui_footer_component()
)

# end: --------------------------------------------------------------------
