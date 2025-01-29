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
    tags$link(rel = "icon", type = "image/x-icon", href = "logo.png"),
    # navbar css file
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/navbar.css"),
    # Integrated styles
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/integrated.css")
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
    card_body(
      div(
        class = "container",
        # Overview section
        card(
          class = "mt-4",
          card_header(class = "bg-light", h4("Overview")),
          card_body(
            withMathJax(
              HTML("
                  <div class='math-section'>
                     <p>The projection of debt-to-GDP ratio using the debt dynamics equation provides a systematic framework for analyzing how public debt evolves over time under different macroeconomic conditions and policy scenarios. This approach begins with baseline values derived from historical data and IMF World Economic Outlook projections, which include the real effective interest rate, GDP growth rate, and primary balance. These baseline values represent the starting point for understanding the expected trajectory of public debt under current conditions and policies.</p>
                  
                     <p>To assess the impact of various policy interventions or economic shocks, the framework incorporates policy shock values that can be applied to each of the key variables. These shock values represent deviations from the baseline scenario and can be positive or negative, allowing for the analysis of both favorable and adverse scenarios. For instance, policymakers might want to examine how an increase in interest rates, a slowdown in economic growth, or a change in fiscal policy affecting the primary balance would impact debt sustainability.</p>
                  
                     <p>The final shock values are then calculated by combining the baseline values with their respective policy shock values. These final values capture the total effect of both the existing economic conditions and the proposed policy changes or external shocks. By inputting these final shocked values into the debt dynamics equation, analysts can generate projections of the debt-to-GDP ratio that reflect the combined impact of baseline conditions and policy interventions. This comprehensive approach enables policymakers to evaluate different policy options and their implications for debt sustainability, making it an invaluable tool for fiscal policy planning and debt management strategies. The resulting projections help identify potential risks to debt sustainability and inform decisions about necessary policy adjustments to maintain debt at sustainable levels.</p>
                   </div>
               ")
            )
          )
        ),
        # Key equation header card
        card(
          class = "mt-4",
          card_header(class = "bg-light", h4("Key Equation")),
          card_body(
            withMathJax(
              HTML("
                  <div class='math-section'>
                    <h5>Debt Dynamics Equation:</h5>
                    \\[ d_t = \\frac{(1 + r_t)}{(1 + g_t)}d_{t-1} - pb_t \\]
                    
                    <h5>Where:</h5>
                    <ul>
                      <li>\\( d_t \\) = public-debt-to-GDP ratio at time t</li>
                      <li>\\( r_t \\) = real effective interest rate</li>
                      <li>\\( g_t \\) = real GDP growth rate</li>
                      <li>\\( pb_t \\) = primary-balance-to-GDP ratio</li>
                      <li>\\( t \\) = time period</li>
                    </ul>
                  </div>
                  ")
            )
          )
        ),
        # Methodology header card
        card(
          class = "mt-4",
          card_header(class = "bg-light", h4("Methodology")),
          card_body(
            withMathJax(HTML("
               <div class='math-section'>
                 <p>The computation process for debt dynamics analysis is fundamentally grounded in three essential variables obtained from the IMF World Economic Outlook database: Gross domestic product at current prices, General government primary net lending/borrowing (primary balance), and General government gross debt. This comprehensive framework enables researchers and policymakers to analyze the evolution of public debt over time, taking into account the complex interplay between economic growth, interest rates, and fiscal policy decisions. The methodology's strength lies in its ability to decompose debt dynamics into its constituent components, allowing for a detailed understanding of how different macroeconomic factors contribute to changes in the debt-to-GDP ratio.</p>
                 <h5>An imfweo R package</h5>
                 <p>The <code>imfweo</code> R package, developed by <a href='https://github.com/Teal-Insights/imfweo' target = '_blank'>Teal-Insights</a>, represents a significant advancement in accessing and analyzing IMF World Economic Outlook (WEO) data. This package streamlines the process of retrieving and working with WEO data directly within the R programming environment, making it particularly valuable for economists, researchers, and policy analysts. The package offers functionality to download data from various WEO releases, handle multiple variables across different countries and time periods, and process the data into formats suitable for analysis. Despite being a minimum viable product under active development, it already provides essential features such as automatic data updating, efficient data transformation, and compatibility with common R data manipulation packages. The package's architecture is designed to accommodate future enhancements while maintaining a user-friendly interface that simplifies the often complex task of working with international economic data. For researchers conducting debt dynamics analysis or broader macroeconomic studies, <code>imfweo</code> serves as a valuable tool that reduces the technical barriers to accessing and utilizing IMF WEO data.</p>
                 <h5>Key Equations:</h5>
                 
                 <h6>1. Main Debt Dynamics Equation:</h6>
                 \\[ \\tag{1} d_t = \\frac{1 + r_t}{1 + g_t}d_{t-1} - pb_t \\]
                 
                 <h6>2. GDP Growth Rate Calculation:</h6>
                 \\[ \\tag{2} g_t = \\frac{GDP_t - GDP_{t-1}}{GDP_{t-1}} \\times 100 \\]
                 
                 <h6>3. Real Effective Interest Rate Derivation:</h6>
                 \\[ \\tag{3} r_t = \\frac{(d_t + pb_t)(1 + g_t)}{d_{t-1}} - 1 \\]
                 
                 <h6>4. Final Shock Calculations:</h6>
                 \\[ \\tag{4a} r_t^{final} = r_t^{baseline} + r_t^{shock} \\]
                 \\[ \\tag{4b} g_t^{final} = g_t^{baseline} + g_t^{shock} \\]
                 \\[ \\tag{4c} pb_t^{final} = pb_t^{baseline} + pb_t^{shock} \\]
                 
                 <h5>Where:</h5>
                 <ul>
                   <li>\\( d_t \\) = public-debt-to-GDP ratio at time t</li>
                   <li>\\( r_t \\) = real effective interest rate</li>
                   <li>\\( g_t \\) = real GDP growth rate</li>
                   <li>\\( pb_t \\) = primary-balance-to-GDP ratio</li>
                   <li>\\( GDP_t \\) = Gross Domestic Product at time t</li>
                   <li>\\( r_t^{final}, g_t^{final}, pb_t^{final} \\) = final shocked values</li>
                   <li>\\( r_t^{baseline}, g_t^{baseline}, pb_t^{baseline} \\) = baseline values</li>
                   <li>\\( r_t^{shock}, g_t^{shock}, pb_t^{shock} \\) = policy shock values</li>
                 </ul>
                 
                 <h5>Computation of GDP Growth Rate:</h5>
                 <p>The analytical process begins with calculating the GDP growth rate using current price GDP data through equation (2). This calculation is crucial as it captures the economy's expansion or contraction over time, which directly impacts debt sustainability. The growth rate calculation uses GDP values from consecutive periods to measure the percentage change in economic output. This rate serves as a fundamental indicator of the economy's capacity to generate resources and manage its debt burden, making it a critical component in assessing long-term debt sustainability and fiscal policy effectiveness.</p>
                 
                 <h5>Computation of Real Effective Interest Rate:</h5>
                 <p>The real effective interest rate (\\(r_t\\)) computation, represented in equation (3), employs a backward calculation approach utilizing the debt dynamics equation. This derivation is made possible by having access to actual debt levels, calculated GDP growth rates, and primary balances from historical data. The methodology involves rearranging the original debt dynamics equation to isolate the interest rate term, thereby revealing the implicit rate that would have generated the observed changes in debt ratios. This approach captures the effective cost of borrowing faced by the government across its entire debt portfolio, incorporating various maturities, currencies, and interest rate structures.</p>
                 
                 <h5>Debt Projection:</h5>
                 <p>The final projection methodology incorporates policy shocks through equations (4a), (4b), and (4c), where baseline values for real effective interest rates, GDP growth, and primary balance are adjusted by user-specified shock values. These shock calculations are performed in percentage terms, with the final shocked values representing the sum of baseline and policy shock values for each respective variable. The resulting final values (\\(r_t^{final}, g_t^{final}, pb_t^{final}\\)) are then input into the main debt dynamics equation (1) to generate the debt projection under the specified shock scenario. This approach allows for a comprehensive analysis of how policy changes or external shocks might affect debt sustainability, providing policymakers with valuable insights for debt management and fiscal policy decisions.</p>
               </div>
             "))
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
      background-color: #2c3e50; 
      font-size: 14px; 
      font-style: bold;
    ",
    tags$p(
      tags$a(
        href = "https://www.naturefinance.net/",
        target = "_blank",
        "Nature Finance"
      ),
      span(
        style = "color: #FFFF;",
        " | "
      )
      ,
      tags$a(
        href = "#",
        "Teal Insights"
      ),
        span(
          style = "color: #FFFF;",
          " | © ",
          lubridate::year(Sys.Date())
          )
    )
  )
)