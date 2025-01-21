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
  header = div(  # This will stay at the top
    div(
      style = "text-align: center; font-weight: bold; font-size: 20px; color: red; margin-bottom: 0px;",
      "This app is under development"
    )
  ),
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "logo.png"),
    # Integrated styles
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
    .math-section {
      background-color: #f8f9fa;
      padding: 20px;
      border-radius: 5px;
      margin: 10px 0;
    }
    .math-section h5 {
      color: #2c3e50;
      margin-top: 20px;
      margin-bottom: 10px;
    }
    .math-section ul {
      margin-left: 20px;
    }
    .card {
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      margin-bottom: 20px;
    }
    .card-header {
      border-bottom: 1px solid rgba(0,0,0,.125);
    }
    .bg-primary {
      background-color: #2c3e50 !important;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
      padding: 20px;
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
    title = "Documentation",
    
    # Main container
    card(
      height = "100%",
      full_screen = TRUE,
      card_header(
        class = "bg-primary text-white",
        h2("Debt Dynamics Under Uncertainty")
      ),
      
      card_body(
        div(
          class = "container",
          
          # Overview section
          card(
            card_header(
              class = "bg-light",
              h4("Overview")
            ),
            card_body(
              div(
                class = "row-bordered",
                tags$div(
                  h4("Debt Dynamics Under Uncertainty", style = "font-weight: bold;"),
                  p("Debt dynamics under uncertainty examines how sovereign debt evolves in the face of unpredictable economic, financial, and geopolitical factors. This framework considers the complex interplay between macroeconomic variables such as real GDP growth, interest rates, exchange rates, fiscal balances, and the stochastic shocks that can disrupt their trajectories."),
                  p("Uncertainty in debt dynamics arises from external and domestic sources. External factors include volatile global financial markets, fluctuating commodity prices, and changes in international monetary policies. Domestic challenges might involve political instability, natural disasters, or unexpected revenue shortfalls. These uncertainties complicate forecasting and policy formulation, making debt sustainability assessments a probabilistic exercise rather than a deterministic one."),
                  p("Key analytical tools in this field include stochastic simulations, scenario analysis, and Value-at-Risk (VaR) models, which estimate the probability of adverse debt outcomes under different scenarios. These methods help policymakers assess the likelihood of crossing critical thresholds, such as unsustainable debt-to-GDP ratios, and identify vulnerabilities in public debt portfolios."),
                  p("Effective debt management under uncertainty relies on adopting strategies that enhance resilience. This includes maintaining fiscal buffers, diversifying the debt portfolio, extending maturities, and linking debt to real economic performance through innovative instruments like GDP-linked bonds."),
                  p("Understanding debt dynamics under uncertainty is crucial for developing robust debt policies that minimize default risks and maintain access to capital markets. It also informs international stakeholders, such as the IMF and World Bank, in designing tailored support programs for vulnerable economies."),
                  p("By integrating uncertainty into debt analysis, governments can prepare for adverse scenarios, optimize policy responses, and align debt strategies with long-term development objectives, promoting macroeconomic stability and fiscal sustainability.")
                )
              )
            )
          ),
          
          # Formula section
          card(
            class = "mt-4",
            card_header(
              class = "bg-light",
              h4("Key Equations")
            ),
            card_body(
              withMathJax(
                HTML("
                <div class='math-section'>
                  <h5>Primary Debt Dynamics Equation:</h5>
                  \\[ \\Delta d_t = \\frac{r_t - g_t}{1 + g_t}d_{t-1} - pb_t + dda_t + \\epsilon_t \\]
                  
                  <h5>Where:</h5>
                  <ul>
                    <li>\\( d_t \\) = debt-to-GDP ratio at time t</li>
                    <li>\\( r_t \\) = real interest rate</li>
                    <li>\\( g_t \\) = real GDP growth rate</li>
                    <li>\\( pb_t \\) = primary balance ratio to GDP</li>
                    <li>\\( dda_t \\) = deficit-debt adjustments</li>
                    <li>\\( \\epsilon_t \\) = stochastic shock term</li>
                  </ul>
                  
                  <h5>Uncertainty Components:</h5>
                  \\[ \\epsilon_t \\sim N(0, \\sigma^2) \\]
                  \\[ \\sigma^2 = \\sum_{i=1}^{n} w_i \\sigma_i^2 \\]
                </div>
              ")
              )
            )
          ),
          
          # Methodology section
          card(
            class = "mt-4",
            card_header(
              class = "bg-light",
              h4("Methodology")
            ),
            card_body(
              tags$div(
                h5("Stochastic Simulation Approach"),
                p("The analysis employs Monte Carlo simulations to generate multiple debt trajectories:"),
                tags$ol(
                  tags$li("Generate random shocks for key variables (growth, interest rates, primary balance)"),
                  tags$li("Simulate debt paths using the primary equation"),
                  tags$li("Calculate confidence intervals and fan charts"),
                  tags$li("Assess probability of debt exceeding specific thresholds")
                ),
                
                h5("Key Assumptions", class = "mt-4"),
                p("The model assumes:"),
                tags$ul(
                  tags$li("Normally distributed shocks"),
                  tags$li("Constant variance-covariance structure"),
                  tags$li("No structural breaks in relationships")
                )
              )
            )
          )
        )
      )
    )
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