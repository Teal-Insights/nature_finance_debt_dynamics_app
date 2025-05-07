# # starts: -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(reactable)
})
# Rscripts: ---------------------------------------------------------------
# source(file = "R/imf_key_data.R")
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
  server_data_countries <- imf_countries()
  
  # main weo data
  server_weo_data <- reactive({
    # reading data
    data_main_weo <- readr::read_rds(file = "data/IMFweo.rds") %>% 
      select(
        -c(subject_notes,country_series_specific_notes,estimates_start_after)
      )
    
    data_var <- data_main_weo %>% 
      select(weo_subject_code,subject_descriptor, units, scale) %>% 
      distinct()
    
    data_weo_clean <- data_main_weo %>% 
      select(
        weo_country_code,iso3c, country_name, weo_subject_code, year, outcome
      ) %>% 
      # mutate(outcome = case_when(is.na(outcome) ~ 0, TRUE ~ outcome)) %>% 
      filter(!is.na(year)) %>% 
      pivot_wider(names_from = weo_subject_code, values_from = outcome) %>% 
      pivot_longer(
        cols = -c(weo_country_code,iso3c, country_name, year),
        names_to = "weo_subject_code",
        values_to = "outcome"
      ) %>% 
      full_join(
        x = data_var,
        y = .,
        by = c("weo_subject_code")
      )
    
    # returning dataframe
    return(data_weo_clean)
  })
  
  # Get main data
  server_data_main <- shiny::reactive({
    
    country_iso3c <- server_data_countries %>%
      dplyr::filter(label == server_input_country()) %>%
      dplyr::pull(iso3c)
    
    # get data
    data <- server_weo_data() %>% dplyr::filter(iso3c == country_iso3c)
    # return dataframe
    return(data)
  })

  # Get specific data
  server_data_specific <- shiny::reactive({
    process_specific_data(
      main_data = server_data_main(),
      projection_year = server_input_projection_start()
    )
  })

  # Create baseline data
  server_data_baseline <- reactive({
    create_baseline_data(
      data_specific = server_data_specific(),
      year = server_input_projection_start()
    )
  })

  # ----------------------------------
  # weo data
  # ----------------------------------
  weo_data <- reactive({
    req(server_data_specific())

    server_data_specific() %>%
      dplyr::mutate(across(
        dplyr::where(is.numeric) & !all_of(c("year", "weo_country_code")),
        ~ round(., digits = 2)
      )) %>%
      tidyr::spread(key = year, value = outcome) %>%
      dplyr::arrange(units) %>%
      select(
        -c(weo_country_code,country_name, iso3c, weo_subject_code, units,	scale)
      ) %>%
      rename(
        indicators = "subject_descriptor"
      )
  })

  # ----------------------------------
  # shock analysis data
  # ----------------------------------
  shock_analysis <- reactive({
    req(df_policy())

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

  # get available years
  available_years <- get_available_years(
    server_data_baseline,
    server_input_projection_start
  )

  # storing final values
  shock_values <- reactiveValues(pb = NULL, ir = NULL, gdp = NULL)

  # shock values dataframe
  reactive_shock_values <- get_reactive_shock_values(
    available_years,
    shock_values
  )

  # ----------------------------------
  # Create policy analysis reactive expression
  # ----------------------------------

  df_policy <- reactive({
    # analysing policy shock
    analyze_policy_shock(
      data_baseline = server_data_baseline(),
      shock_values = reactive_shock_values(),
      year_when_estimations_start = server_input_projection_start()
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
      server_excel_template(file, server_data_main)
    }
  )

  # -------------------------------------------------------------------------
  # input for indicators
  # -------------------------------------------------------------------------

  pb_data <- get_pb_data(server_data_baseline, server_input_projection_start)
  ir_data <- get_ir_data(server_data_baseline, server_input_projection_start)
  gdp_data <- get_gdp_data(server_data_baseline, server_input_projection_start)

  # Create coefficients using the reactive data
  coefficients <- get_coefficients(pb_data, ir_data, gdp_data)

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
  
  

}

# ends: -------------------------------------------------------------------
