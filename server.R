
# starts: -----------------------------------------------------------------
# loading necessary components
source(file = "components/server_create_debt_plot.R")
source(file = "components/server_prepare_shock_data.R")
source(file = "components/server_excel_template.R")
source(file = "components/server_create_country_headers.R")
source(file = "components/server_shock_outputs.R")
source(file = "components/server_input_reactive_data_functions.R")
source(file = "components/server_shock_analysis.R")
source(file = "components/server_api_call.R")

# Rscripts: ---------------------------------------------------------------
server <- function(input, output, session){
  df_countries <- imfweo::weo_list_countries() %>% 
    rename(iso3c = 'country_code',label = "country_name")
  
  # Call the header component
  server_create_country_headers(id = "id", output = output, input = input)
  
  reactive_data <- setup_reactive_data(input, df_countries, imf_key_data)
  
  # get main data
  df_main <- reactive_data$df_main
  
  # get country specific data
  df_specific <- reactive_data$df_specific
  
  # projections starts after - reactive
  year_when_estimations_start <- reactive_data$year_when_estimations_start
  
  # -------------------------------------------------------------------------
  # shocks analysis ---------------------------------------------------------
  # Create baseline data reactive expression
  df_baseline <- reactive({
    req(df_main(), df_specific(), year_when_estimations_start())
    
    create_baseline_data(
      df_main = df_main(),
      df_specific = df_specific(),
      year_when_estimations_start = year_when_estimations_start()
    )
  })
  
  # Create policy analysis reactive expression
  df_policy <- reactive({
    req(df_baseline(), reactive_shock_values(), year_when_estimations_start())
    
    analyze_policy_shock(
      df_baseline = df_baseline(),
      shock_values = reactive_shock_values(),
      year_when_estimations_start = year_when_estimations_start()
    )
  })
  # -------------------------------------------------------------------------
  # input: ------------------------------------------------------------------
  # Create reactive expressions using the imported functions
  available_years <- get_available_years(df_baseline, year_when_estimations_start)
  
  pb_data <- get_pb_data(df_baseline, year_when_estimations_start)
  ir_data <- get_ir_data(df_baseline, year_when_estimations_start)
  gdp_data <- get_gdp_data(df_baseline, year_when_estimations_start)
  
  # Create coefficients using the reactive data
  coefficients <- get_coefficients(pb_data, ir_data, gdp_data)
  
  # Create reactive values to store final values
  shock_values <- reactiveValues(pb = NULL, ir = NULL, gdp = NULL)
  
  # Create outputs for all three shocks
  create_shock_outputs("pb",  output, input, shock_values, available_years, coefficients)
  create_shock_outputs("ir",  output, input, shock_values, available_years, coefficients)
  create_shock_outputs("gdp", output, input, shock_values, available_years, coefficients)
  
  # Create reactive shock values dataframe
  reactive_shock_values <- get_reactive_shock_values(available_years, shock_values)
  
  # -------------------------------------------------------------------------
  # Graphs: -----------------------------------------------------------------
  observe({
    # Ensure id_shock has a value
    req(input$id_shock)
    
    # Create reactive for processed data - shared across all conditions
    projection_processed_data <- reactive({
      req(df_policy(), year_when_estimations_start())
      projections_start_in <- year_when_estimations_start()
      start_year <- projections_start_in - 9
      
      df_policy() %>% 
        gather(key = indicators, value = outcome, -c("year")) %>% 
        filter(year >= start_year) %>% 
        mutate(outcome = round(x = outcome, digits = 2)) %>%
        spread(key = year, value = outcome) %>% 
        mutate(
          indicators = case_when(
            indicators == "debt_GDP_shock" ~ "Debt Projection : GDP Shock",
            indicators == "debt_PB_shock" ~ "Debt Projection : Primary balance Shock",
            indicators == "debt_Interest_shock" ~ "Debt Projection : Real effective interest rate Shock",
            indicators == "debt_policy_shock" ~ "Debt Projection : Policy Shock",
            indicators == "Baseline" ~ "Baseline Debt",
            TRUE ~ indicators
          )
        )
    })
    
    # Common data table rendering for all cases
    output$data_projection <- renderDT({
      req(projection_processed_data())
      DT::datatable(
        projection_processed_data()
      )
    })
    
    # Common download handler for all cases
    output$download_projection <- downloadHandler(
      filename = function() {
        file_ext <- ifelse(input$file_type_projection == "CSV", ".csv", ".xlsx")
        paste(input$id_country, "-","Policy shock analysis","-", Sys.Date(), file_ext, sep = "")
      },
      content = function(file) {
        if (input$file_type_projection == "CSV") {
          write.csv(projection_processed_data(), file, row.names = FALSE)
        } else if (input$file_type_projection == "Excel") {
          openxlsx::write.xlsx(projection_processed_data(), file, overwrite = TRUE)
        }
      }
    )
  })
  # download handler for excel template
  output$download_template <- downloadHandler(
    filename = function() {
      paste0(input$id_country, "-Policy shock","-template","-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      server_excel_template(file, df_main)
    }
  )
  
  # Main observer code
  observe({
    # Ensure required reactive values are available
    req(df_policy(), year_when_estimations_start())
    
    # Calculate years once
    projections_start_in <- year_when_estimations_start()
    start_year <- projections_start_in - 9
    
    # Helper function to create plots
    create_plots <- function(output_full_id, output_projection_id) {
      # Full plot
      output[[output_full_id]] <- echarts4r::renderEcharts4r({
        df_long <- server_prepare_shock_data(df_policy(), input$id_shock, start_year)
        server_create_debt_plot(df_long)
      })
      
      # Projection plot
      output[[output_projection_id]] <- echarts4r::renderEcharts4r({
        df_long <- df_policy() %>%
          filter(year >= projections_start_in) %>%
          server_prepare_shock_data(input$id_shock)
        server_create_debt_plot(df_long)
      })
    }
    
    # Create main tab plots
    create_plots("plot_full", "plot_projection")
    
    # Create home tab plots
    create_plots("plot_full_input", "plot_projection_input")
  })
  # -------------------------------------------------------------------------
  # Data: -------------------------------------------------------------------
  # Reactive data preparation
  processed_data <- reactive({
    req(df_specific())
    
    # return data
    if (input$data_format == "Long") {
      df_specific() %>% 
        select(-estimates_start_after)
    }else {
      df_specific() %>% 
        spread(key = year, value = outcome) %>% 
        arrange(units) %>% 
        select(-estimates_start_after)
    }
  })
  
  # Render the data table
  output$full_data <- DT::renderDT({
    # Define the required columns
    required_columns <- c("iso3c")
    # validation of data
    validate(
      need(all(required_columns %in% colnames(processed_data())), "No data available to display.")
    )
    
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
        need(all(required_columns %in% colnames(processed_data())), "No data available to display.")
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
  
  # -------------------------------------------------------------------------
  # end: --------------------------------------------------------------------
}

# ends: -------------------------------------------------------------------