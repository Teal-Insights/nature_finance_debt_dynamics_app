# # starts: -----------------------------------------------------------------

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
  shiny::hideTab(inputId = "main_navbar", target = "graph")
  shiny::hideTab(inputId = "main_navbar", target = "data")
  shiny::hideTab(inputId = "main_navbar", target = "docs")
  
  observeEvent(input$id_country, {
    if (!is.null(input$id_country) && input$id_country != "") {
      shiny::showTab(inputId = "main_navbar", target = "analysis")
      shiny::showTab(inputId = "main_navbar", target = "graph")
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

  # -------------------------------------------------------------------------
  # Graphs: -----------------------------------------------------------------
  observe({
    # Ensure id_shock has a value
    req(input$id_shock)

    # Create reactive for processed data - shared across all conditions
    projection_processed_data <- reactive({
      projections_start_in <- server_input_projection_start()
      start_year <- projections_start_in - 9

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

    # Common data table rendering for all cases
    output$data_projection <- renderDT({
      req(projection_processed_data())
      # return datatable
      DT::datatable(
        projection_processed_data()
      )
    })

    # Common download handler for all cases
    output$download_projection <- downloadHandler(
      filename = function() {
        file_ext <- ifelse(input$file_type_projection == "CSV", ".csv",
          ".xlsx"
        )
        paste(input$id_country, "-", "Policy shock analysis", "-", Sys.Date(),
          file_ext,
          sep = ""
        )
      },
      content = function(file) {
        if (input$file_type_projection == "CSV") {
          write.csv(projection_processed_data(), file, row.names = FALSE)
        } else if (input$file_type_projection == "Excel") {
          openxlsx::write.xlsx(projection_processed_data(), file,
            overwrite = TRUE
          )
        }
      }
    )
  })
  # download handler for excel template
  output$download_template <- downloadHandler(
    filename = function() {
      paste0(
        input$id_country, "-Policy shock", "-template", "-", Sys.Date(),
        ".xlsx"
      )
    },
    content = function(file) {
      server_excel_template(file, df_main)
    }
  )

  
  # -------------------------------------------------------------------------
  # Data: -------------------------------------------------------------------
  # Reactive data preparation
  processed_data <- reactive({
    req(df_specific())

    # return data
    if (input$data_format == "Long") {
      df_specific() %>%
        dplyr::mutate(across(
          dplyr::where(is.numeric) & !all_of(c("year", "weo_country_code")),
          ~ round(., digits = 2)
        )) %>%
        dplyr::select(-estimates_start_after)
    } else {
      df_specific() %>%
        dplyr::mutate(across(
          dplyr::where(is.numeric) & !all_of(c("year", "weo_country_code")),
          ~ round(., digits = 2)
        )) %>%
        tidyr::spread(key = year, value = outcome) %>%
        dplyr::arrange(units) %>%
        dplyr::select(-estimates_start_after)
    }
  })

  # Render the data table
  output$full_data <- DT::renderDT({
    # Define the required columns
    required_columns <- c("iso3c")
    # validation of data
    validate(
      need(
        all(required_columns %in% colnames(processed_data())),
        "No data available to display."
      )
    )
    # return datatable
    DT::datatable(processed_data())
  })
  # Reactive to check if the required columns exist
  observe({
    required_columns <- c("iso3c")
    if (all(required_columns %in% colnames(processed_data()))) {
      # Show the download button if data is available
      shinyjs::show("downloadData")
      # Show the file type if data is missing
      shinyjs::show("file_type")
    } else {
      # Hide the download button if data is missing
      shinyjs::hide("downloadData")
      # Hide the file type if data is missing
      shinyjs::hide("file_type")
    }
  })

  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      # Define file extension based on user input
      file_ext <- ifelse(input$file_type == "CSV", ".csv", ".xlsx")
      paste(input$id_country, "-", Sys.Date(), file_ext, sep = "")
    },
    content = function(file) {
      # Define the required columns
      required_columns <- c("iso3c")

      # Validate if the required columns exist in the data
      validate(
        need(
          all(required_columns %in% colnames(processed_data())),
          "No data available to display."
        )
      )

      # If columns are valid, proceed to download
      if (input$file_type == "CSV") {
        # Write to CSV
        write.csv(processed_data(), file, row.names = FALSE)
      } else if (input$file_type == "Excel") {
        # Write to Excel
        openxlsx::write.xlsx(processed_data(), file, overwrite = TRUE)
      }
    }
  )

}

# ends: -------------------------------------------------------------------
