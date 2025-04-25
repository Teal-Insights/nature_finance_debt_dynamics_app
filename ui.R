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
#  main data
df_main_weo <- readr::read_rds(file = "data/IMFweo.rds")
# incomplete countries
incomplete <- df_main_weo %>% 
  select(country_name, weo_subject_code) %>% 
  distinct() %>% 
  summarise(.by = country_name, count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count < 3) %>% 
  pull(country_name)

# pickers: ----------------------------------------------------------------
select_country <- imf_countries() %>% 
  filter(!(label %in% incomplete)) %>% pull(label)

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
        full_screen = FALSE,
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
                        " values."
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
        full_screen = FALSE,
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
                  selected = select_country[1],
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
  # Data Panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Data",
    value = "data",
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      style = "grid-template-rows: auto; align-items: start;", 
      
      div(
        id = "weo_data_card",
        class = "data-card",
        style = "height: auto; display: flex; flex-direction: column; margin-bottom: 1rem; border: 1px solid rgba(0,0,0,0.125); border-radius: 0.25rem; overflow: hidden;",
        
        # Header (replacing card_header)
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 0.75rem 1.25rem; background-color: #2c3e50; border-bottom: 1px solid rgba(0,0,0,0.125); color: white;",
          span("Weo Data"),
          div(
            style = "position: relative;",
            tags$div(
              class = "dropdown",
              tags$button(
                id = "weo_download_btn",
                class = "btn btn-light btn-sm dropdown-toggle",
                type = "button", 
                `data-bs-toggle` = "dropdown",
                `aria-expanded` = "false",
                tags$i(class = "bi bi-list", style = "color: white; font-size: 18px;"),
                style = "border: none; background: transparent; padding: 2px 5px;"
              ),
              tags$ul(
                class = "dropdown-menu",
                tags$li(downloadLink("download_weo_csv", "CSV", class = "dropdown-item")),
                tags$li(downloadLink("download_weo_excel", "Excel", class = "dropdown-item"))
              )
            )
          )
        ),
        
        # Body (replacing card_body)
        div(
          style = "padding: 0; flex: 1; display: flex; flex-direction: column; min-height: 0;", 
          div(
            style = "flex: 1; display: flex; flex-direction: column; min-height: 0;",
            reactable::reactableOutput("full_data")
          )
        ),
        
        # Add expand/fullscreen button functionality
        tags$button(
          id = "weo_fullscreen_btn",
          class = "btn btn-sm btn-light fullscreen-button",
          style = "position: absolute; top: 5px; right: 40px; border: none; background: transparent; padding: 2px 5px;",
          tags$i(class = "bi bi-arrows-fullscreen")
        ),
        tags$script(HTML("
        $(document).ready(function() {
          $('#weo_fullscreen_btn').click(function() {
            $('#weo_data_card').toggleClass('fullscreen');
          });
        });
      "))
      ),
      
      # Shock analysis result table with dynamic height
      div(
        id = "shock_analysis_card",
        class = "data-card",
        style = "height: auto; display: flex; flex-direction: column; margin-bottom: 1rem; border: 1px solid rgba(0,0,0,0.125); border-radius: 0.25rem; overflow: hidden;",
        
        # Header (replacing card_header)
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 0.75rem 1.25rem; background-color: #2c3e50; border-bottom: 1px solid rgba(0,0,0,0.125); color: white;",
          span("Shock Analysis"),
          div(
            style = "position: relative;",
            tags$div(
              class = "dropdown",
              tags$button(
                id = "shock_download_btn",
                class = "btn btn-light btn-sm dropdown-toggle",
                type = "button", 
                `data-bs-toggle` = "dropdown",
                `aria-expanded` = "false",
                tags$i(class = "bi bi-list", style = "color: white; font-size: 18px;"),
                style = "border: none; background: transparent; padding: 2px 5px;"
              ),
              tags$ul(
                class = "dropdown-menu",
                tags$li(downloadLink("download_shock_csv", "CSV", class = "dropdown-item")),
                tags$li(downloadLink("download_shock_excel", "Excel", class = "dropdown-item")),
                tags$li(downloadLink("download_shock_template", "Template", class = "dropdown-item"))
              )
            )
          )
        ),
        
        # Body (replacing card_body)
        div(
          style = "padding: 0; flex: 1; display: flex; flex-direction: column; min-height: 0;",
          div(
            style = "flex: 1; display: flex; flex-direction: column; min-height: 0;",
            reactable::reactableOutput("data_projection")
          )
        ),
        
        # Add expand/fullscreen button functionality
        tags$button(
          id = "shock_fullscreen_btn",
          class = "btn btn-sm btn-light fullscreen-button",
          style = "position: absolute; top: 5px; right: 40px; border: none; background: transparent; padding: 2px 5px;",
          tags$i(class = "bi bi-arrows-fullscreen")
        ),
        tags$script(HTML("
        $(document).ready(function() {
          $('#shock_fullscreen_btn').click(function() {
            $('#shock_analysis_card').toggleClass('fullscreen');
          });
        });
      "))
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
