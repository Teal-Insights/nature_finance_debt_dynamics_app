# # starts: -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(reactable)
})
# Rscripts: ---------------------------------------------------------------
source(file = "R/imf_key_data.R")
source(file = "R/imf_countries.R")
source(file = "R/imf_indicators.R")
source(file = "R/imf_format_years.R")
source(file = "R/echarts_main.R")

# loading necessary components
source(file = "components/server_prepare_shock_data.R")
source(file = "components/server_excel_template.R")
source(file = "components/server_shock_outputs.R")
source(file = "components/server_input_reactive_data_functions.R")
source(file = "components/server_shock_analysis.R")
source(file = "components/server_debt_projections.R")
source(file = "components/server_api_call.R")

# Rscripts: ---------------------------------------------------------------
server <- function(input, output, session) {
  # -------------------------------------------------------------------------
  # Hide tabs initially
  # -------------------------------------------------------------------------
  shiny::hideTab(inputId = "main_navbar", target = "analysis")
  shiny::hideTab(inputId = "main_navbar", target = "data")
  shiny::hideTab(inputId = "main_navbar", target = "docs")
  
  observeEvent(input$id_country, {
    if (!is.null(input$id_country) && input$id_country != "") {
      shiny::showTab(inputId = "main_navbar", target = "analysis")
      shiny::showTab(inputId = "main_navbar", target = "data")
      shiny::showTab(inputId = "main_navbar", target = "docs")
      # updateNavbarPage(session, "main_navbar", selected = "analysis")
    }
  })
  # -------------------------------------------------------------------------
  # inputs preparation
  # -------------------------------------------------------------------------
  # selected country
  server_input_country <- reactive({input$id_country})
  
  # selected projection year
  server_input_projection_start <- reactive({
      as.numeric(input$projection_year)
    })
  # -------------------------------------------------------------------------
  # data preparation
  # -------------------------------------------------------------------------
  # imf countries
  df_countries <- imf_countries()
  
  # Get main data
  df_main <- shiny::reactive({
    shiny::req(server_input_country())
    process_main_data(
      df_countries = df_countries,
      input_country = server_input_country(),
      imf_data = read_imf_weo()
    )
  })
  
  # Get specific data
  df_specific <- shiny::reactive({
    process_specific_data(
      main_data = df_main(),
      projection_year = server_input_projection_start()
    )
  })
  # -------------------------------------------------------------------------
  # analysis panel
  # -------------------------------------------------------------------------
  # server visualization data
  server_data_viz <- reactive({
    req(df_policy())
    
    df_policy() %>%
      select(year, debt_policy_shock,Baseline) %>% 
      tidyr::gather(key = "indicators", value = "outcome", -c("year")) %>%
      dplyr::mutate(outcome = round(x = outcome, digits = 2)) %>%
      dplyr::mutate(
        indicators = dplyr::case_when(
          indicators == "debt_policy_shock" ~ "Debt Projection : Policy Shock",
          indicators == "Baseline" ~ "Baseline Debt",
          .default =  indicators
        )
      )
  })
  
  # visualize historical data
  output$plot_full_input <- echarts4r::renderEcharts4r({
    echarts_main(
      data = server_data_viz(),
      x_col = "year",
      y_col = "outcome",
      group_col = "indicators"
    )
  })
  
  # visualize projection data
  output$plot_projection_input <- echarts4r::renderEcharts4r({
    echarts_main(
      data = server_data_viz() %>% 
        filter(year >= (server_input_projection_start() - 1)),
      x_col = "year",
      y_col = "outcome",
      group_col = "indicators"
    )
  })
  # -------------------------------------------------------------------------
  # data panel
  # -------------------------------------------------------------------------
  # ----------------------------------
  # weo data
  # ----------------------------------
  weo_data <- reactive({
    req(df_specific())
    
    df_specific() %>%
      dplyr::mutate(across(
        dplyr::where(is.numeric) & !all_of(c("year", "weo_country_code")),
        ~ round(., digits = 2)
      )) %>%
      tidyr::spread(key = year, value = outcome) %>%
      dplyr::arrange(units) %>%
      dplyr::select(-estimates_start_after) %>% 
      select(
        -c(weo_country_code,country_name, iso3c, weo_subject_code, units,	scale)
      ) %>% 
      rename(
        indicators = "subject_descriptor"
      )
  })
  # ----------------------------------
  # weo table
  # ----------------------------------
  output$full_data <- reactable::renderReactable({
    data <- weo_data()
    
    # Define the required columns
    required_columns <- c("indicators")
    
    # validation of data
    validate(
      need(
        all(required_columns %in% colnames(data)),
        "No data available to display."
      )
    )
    
    reactable::reactable(
      data,
      height = "auto",
      defaultPageSize = nrow(data),
      pagination = FALSE,
      showPagination = FALSE,
      showPageInfo = FALSE,
      filterable = FALSE,
      searchable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      columns = list(
        indicators = colDef(minWidth = 200,width = 400,sticky = "left")
      )
    )
  })
  # ----------------------------------
  # shock analysis data
  # ----------------------------------
  shock_analysis <- reactive({
    start_year <- server_input_projection_start() - 13
    
    df_policy() %>%
      tidyr::gather(key = indicators, value = outcome, -c("year")) %>%
      dplyr::filter(year >= start_year) %>%
      dplyr::mutate(outcome = round(x = outcome, digits = 2)) %>%
      tidyr::spread(key = year, value = outcome) %>%
      dplyr::mutate(
        indicators = dplyr::case_when(
          indicators == "debt_GDP_shock" ~
            "Debt Projection : GDP Shock",
          indicators == "debt_PB_shock" ~
            "Debt Projection : Primary balance Shock",
          indicators == "debt_Interest_shock" ~
            "Debt Projection : Real effective interest rate Shock",
          indicators == "debt_policy_shock" ~
            "Debt Projection : Policy Shock",
          indicators == "Baseline" ~ "Baseline Debt",
          TRUE ~ indicators
        )
      )
  })
  
  # ----------------------------------
  # shock analysis table
  # ----------------------------------
  output$data_projection <- reactable::renderReactable({
    data <- shock_analysis()
    
    # Return reactable with options
    reactable::reactable(
      data,
      height = "auto",
      defaultPageSize = nrow(data),
      pagination = FALSE,
      showPagination = FALSE,
      showPageInfo = FALSE,
      filterable = FALSE,
      searchable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      columns = list(
        indicators = colDef(minWidth = 200,width = 280,sticky = "left")
      )
    )
  })
  
  # ----------------------------------
  # weo data download
  # ----------------------------------
  
  output$download_weo_csv <- downloadHandler(
    filename = function() {
      paste0(input$id_country, "-weo-data-",".csv")
    },
    content = function(file) {
      write.csv(weo_data(), file, row.names = FALSE)
    }
  )
  
  output$download_weo_excel <- downloadHandler(
    filename = function() {
      paste0(input$id_country, "-weo-data-",".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(weo_data(), file)
    }
  )
  
  # ----------------------------------
  # shock analysis data download
  # ----------------------------------
  
  output$download_shock_csv <- downloadHandler(
    filename = function() {
      paste0(input$id_country, "-shock-analysis-",".csv")
    },
    content = function(file) {
      write.csv(shock_analysis(), file, row.names = FALSE)
    }
  )
  
  output$download_shock_excel <- downloadHandler(
    filename = function() {
      paste0(input$id_country, "-shock-analysis-",".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(shock_analysis(), file)
    }
  )
  
  # ----------------------------------
  # template data download
  # ----------------------------------
  output$download_shock_template <- downloadHandler(
    filename = function() {
      paste0(input$id_country, "-Policy shock-template", ".xlsx")
    },
    content = function(file) {
      server_excel_template(file, df_main)
    }
  )
  # -------------------------------------------------------------------------
  # shocks analysis ---------------------------------------------------------
  # Create baseline data reactive expression
  df_baseline <- reactive({
    create_baseline_data(
      df_main = df_main(),
      df_specific = df_specific(),
      year_when_estimations_start = server_input_projection_start()
    )
  })

  # Create policy analysis reactive expression
  df_policy <- reactive({
    # analysing policy shock
    analyze_policy_shock(
      df_baseline = df_baseline(),
      shock_values = reactive_shock_values(),
      year_when_estimations_start = server_input_projection_start()
    )
  })
  # -------------------------------------------------------------------------
  # input: ------------------------------------------------------------------
  # Create reactive expressions using the imported functions
  available_years <- get_available_years(
    df_baseline,
    server_input_projection_start
  )

  pb_data <- get_pb_data(df_baseline, server_input_projection_start)
  ir_data <- get_ir_data(df_baseline, server_input_projection_start)
  gdp_data <- get_gdp_data(df_baseline, server_input_projection_start)

  # Create coefficients using the reactive data
  coefficients <- get_coefficients(pb_data, ir_data, gdp_data)

  # Create reactive values to store final values
  shock_values <- reactiveValues(pb = NULL, ir = NULL, gdp = NULL)

  # Create outputs for all three shocks
  create_shock_outputs(
    "pb", output, input, shock_values,
    available_years, coefficients
  )
  create_shock_outputs(
    "ir", output, input, shock_values,
    available_years, coefficients
  )
  create_shock_outputs(
    "gdp", output, input, shock_values,
    available_years, coefficients
  )

  # Create reactive shock values dataframe
  reactive_shock_values <- get_reactive_shock_values(
    available_years,
    shock_values
  )

}

# ends: -------------------------------------------------------------------
