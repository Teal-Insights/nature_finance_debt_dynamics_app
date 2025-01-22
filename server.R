
# starts: -----------------------------------------------------------------
# loading necessary components
source(file = "components/server_create_debt_plot.R")
source(file = "components/server_prepare_shock_data.R")

# Rscripts: ---------------------------------------------------------------
server <- function(input, output, session){
  df_countries <- imfweo::weo_list_countries() %>% 
    rename(iso3c = 'country_code',label = "country_name")
  
  # get main data
  df_main <- reactive({
    country_iso3c <- df_countries %>% 
      filter(label %in% c(input$id_country)) %>% 
      pull(iso3c)
    
    imf_key_data() %>% 
      filter(iso3c == country_iso3c)
  })
  
  # get country specific data
  df_specific <- reactive({
    req(df_main())
    df_start <- df_main() %>% 
      select(c(weo_country_code, iso3c, country_name, weo_subject_code, 
               subject_descriptor,units, scale, estimates_start_after, year, outcome)) %>% 
      filter(year >= df_main() %>% 
               pull(estimates_start_after) %>% 
               unique() %>% 
               max() - 13)
    
    df_compute <- df_start %>% 
      select(weo_subject_code, year, outcome) %>% 
      spread(key = weo_subject_code, value = outcome) %>% 
      mutate(
        gdp_growth = (NGDP - lag(NGDP)) / lag(NGDP) * 100
      ) %>% 
      mutate(
        real_effective_rate = ((1 + gdp_growth/100)*((GGXWDG_NGDP + GGXONLB_NGDP)/(lag(GGXWDG_NGDP))) - 1)*100
      ) %>% 
      gather(key = weo_subject_code, value = outcome, -c("year"))
    
    full_join(
      x = df_start,
      y = df_compute,
      by = c("weo_subject_code","year","outcome")
    ) %>% 
      mutate(
        units = case_when(
          weo_subject_code == "gdp_growth" ~ "Percent change",
          weo_subject_code == "real_effective_rate" ~ "Percent",
          .default = units
        ),
        scale = case_when(
          weo_subject_code == "gdp_growth" ~ "Units",
          weo_subject_code == "real_effective_rate" ~ "Units",
          .default = scale
        ),
        subject_descriptor = case_when(
          weo_subject_code == "gdp_growth" ~ "GDP growth",
          weo_subject_code == "real_effective_rate" ~ "Real effective interest rate",
          .default = subject_descriptor
        )
      ) %>% 
      fill(weo_country_code, .direction = "down") %>% 
      fill(iso3c, .direction = "down") %>% 
      fill(country_name, .direction = "down") %>% 
      mutate(outcome = round(x = outcome, digits = 2))
  })
  
  # projections starts after - reactive
  year_when_estimations_start_after <- reactive({
    req(df_main())
    
    # option: weo
    start_value <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE) 
    start_value + 0
    
    # option: current year
    # lubridate::year(Sys.Date()) - 1
  })
  # -------------------------------------------------------------------------
  # shocks analysis ---------------------------------------------------------
  
  df_baseline <- reactive({
    req(df_main(), df_specific(),year_when_estimations_start_after())
    projections_start_after <- year_when_estimations_start_after()
    
    df_specific() %>% 
      select(weo_subject_code, year, outcome) %>% 
      filter(weo_subject_code %in% c("GGXWDG_NGDP","gdp_growth", "GGXONLB_NGDP", "real_effective_rate")) %>% 
      spread(key = weo_subject_code, value = outcome) %>% 
      mutate(
        gdp_growth = case_when(
          year <= projections_start_after ~ NA, .default = gdp_growth
        ),
        GGXONLB_NGDP = case_when(
          year <= projections_start_after ~ NA, .default = GGXONLB_NGDP
        ),
        real_effective_rate = case_when(
          year <= projections_start_after ~ NA, .default = real_effective_rate
        )
      )
  })
  
  df_policy <- reactive({
    req(df_baseline(), reactive_shock_values(), 
        year_when_estimations_start_after(),available_years())
    
    # Extract `projections_start_after`
    projections_start_after <- year_when_estimations_start_after()
    
    # Determine `value_after`
    value_after <- ifelse(
      test = (projections_start_after == 2024),
      yes = projections_start_after,
      no = projections_start_after + 1
    )
    
    # Create `df_new_shock`
    new_pb_shock = reactive_shock_values()$pb
    new_ir_shock = reactive_shock_values()$ir
    new_gdp_shock = reactive_shock_values()$gdp
    new_year = seq(from = value_after, by = 1, length.out = length(available_years()))
    
    df_new_shock <- data.frame(
      year = new_year,
      pb_shock = new_pb_shock,
      ir_shock = new_ir_shock,
      gdp_shock = new_gdp_shock
    ) 
    
    # Join and mutate
    full_join(
      x = df_baseline(),
      y = df_new_shock,
      by = "year"
    ) %>%
      mutate(
        debt_PB_shock = (((1 + real_effective_rate / 100) / (1 + gdp_growth / 100)) * lag(GGXWDG_NGDP) - pb_shock),
        debt_Interest_shock = (((1 + ir_shock / 100) / (1 + gdp_growth / 100)) * lag(GGXWDG_NGDP) - GGXONLB_NGDP),
        debt_GDP_shock = (((1 + real_effective_rate / 100) / (1 + gdp_shock / 100)) * lag(GGXWDG_NGDP) - GGXONLB_NGDP),
        debt_policy_shock = (((1 + ir_shock / 100) / (1 + gdp_shock / 100)) * lag(GGXWDG_NGDP) - pb_shock),
      ) %>% 
      mutate(
        debt_PB_shock = case_when(
          year == projections_start_after ~ GGXWDG_NGDP, .default =  debt_PB_shock
        ),
        debt_Interest_shock = case_when(
          year == projections_start_after ~ GGXWDG_NGDP, .default =  debt_Interest_shock
        ),
        debt_GDP_shock = case_when(
          year == projections_start_after ~ GGXWDG_NGDP, .default =  debt_GDP_shock
        )
      ) %>% 
      mutate(across(where(is.numeric) & !all_of("year"), ~ round(., digits = 2))) %>% 
      rename(
        Baseline = "GGXWDG_NGDP",
        "GDP growth" = "gdp_growth",
        "GDP growth shock" = "gdp_shock",
        "Primary balance (% of GDP)" = "GGXONLB_NGDP",
        "Primary balance (% of GDP) shock" = "pb_shock",
        "Real effective interest rate" = "real_effective_rate",
        "Real effective interest rate shock" = "ir_shock"
      )
  })
  
  # -------------------------------------------------------------------------
  # input: ------------------------------------------------------------------
  # Get available years from the baseline data
  available_years <- reactive({
    req(df_baseline(), year_when_estimations_start_after())
    projections_start_after <- year_when_estimations_start_after()
    
    df_baseline() %>%
      filter(year > projections_start_after) %>%
      pull(year) %>%
      unique() %>% 
      sort()
  })
  
  # primary balance baseline data
  pb_data <- reactive({
    req(df_baseline(), year_when_estimations_start_after()) 
    projections_start_after <- year_when_estimations_start_after()
    
    df_baseline() %>%
      select(year, value = GGXONLB_NGDP) %>%
      filter(year > projections_start_after)
  })
  # Real interest rate baseline data
  ir_data <- reactive({
    req(df_baseline(), year_when_estimations_start_after()) 
    projections_start_after <- year_when_estimations_start_after()
    
    df_baseline() %>%
      select(year, value = real_effective_rate) %>%
      filter(year > projections_start_after)
  })
  # GDP baseline data
  gdp_data <- reactive({
    req(df_baseline(), year_when_estimations_start_after()) 
    projections_start_after <- year_when_estimations_start_after()
    
    df_baseline() %>%
      select(year, value = gdp_growth) %>%
      filter(year > projections_start_after)
  })
  
  # Helper function to convert dataframe to list
  df_to_list <- function(df, shock_name) {
    tryCatch({
      if (nrow(df) == 0) {
        return(setNames(numeric(0), character(0)))
      }
      values <- setNames(df$value, paste0("y", df$year))
      return(as.list(values))
    }, error = function(e) {
      stop(sprintf("Error converting %s data to list: %s", shock_name, e$message))
    })
  }
  
  # Make coefficients reactive
  coefficients <- reactive({
    list(
      pb = df_to_list(pb_data(), "Primary Balance"),
      ir = df_to_list(ir_data(), "Interest Rate"),
      gdp = df_to_list(gdp_data(), "GDP Growth")
    )
  })
  
  # Create reactive values to store final values
  shock_values <- reactiveValues(
    pb = NULL,
    ir = NULL,
    gdp = NULL
  )
  
  # Function to create outputs for each shock
  create_shock_outputs <- function(shock_id) {
    # Create dynamic rows for each shock
    output[[sprintf("%s_rows", shock_id)]] <- renderUI({
      years <- available_years()
      req(years)
      
      # Initialize shock values vector with the correct length
      shock_values[[shock_id]] <- numeric(length(years))
      
      # Create a row for each year
      lapply(seq_along(years), function(i) {
        year <- years[i]
        div(
          class = "row-bordered",
          fluidRow(
            column(2, year),
            column(3, div(style = "padding-top: 7px;", 
                          textOutput(sprintf("%s_%d_coef", shock_id, year)))),
            column(4, numericInput(sprintf("%s_%d_avg", shock_id, year), 
                                   NULL, value = 0.1, step = 0.05)),
            column(3, textOutput(sprintf("%s_%d_score", shock_id, year)))
          )
        )
      })
    })
    
    # Create reactive outputs for coefficients and scores
    observe({
      years <- available_years()
      req(years)
      
      for (i in seq_along(years)) {
        local({
          year <- years[i]
          year_index <- i
          
          coef_id <- sprintf("%s_%d_coef", shock_id, year)
          avg_id <- sprintf("%s_%d_avg", shock_id, year)
          score_id <- sprintf("%s_%d_score", shock_id, year)
          
          # Render coefficient
          output[[coef_id]] <- renderText({
            req(coefficients()[[shock_id]][[sprintf("y%d", year)]])
            coefficients()[[shock_id]][[sprintf("y%d", year)]]
          })
          
          # Render score and update shock values
          output[[score_id]] <- renderText({
            req(input[[avg_id]], 
                coefficients()[[shock_id]][[sprintf("y%d", year)]])
            
            final_value <- round(
              coefficients()[[shock_id]][[sprintf("y%d", year)]] + 
                input[[avg_id]], 2)
            shock_values[[shock_id]][year_index] <- final_value
            final_value
          })
        })
      }
    })
  }
  
  # Create outputs for all three shocks
  create_shock_outputs("pb")
  create_shock_outputs("ir")
  create_shock_outputs("gdp")
  
  # Make shock values accessible for external use
  reactive_shock_values <- reactive({
    years <- available_years()
    req(years)
    
    # Create a named list for each shock type
    list(
      pb = setNames(shock_values$pb, years),
      ir = setNames(shock_values$ir, years),
      gdp = setNames(shock_values$gdp, years)
    )
  })
  
  # -------------------------------------------------------------------------
  # Graphs: -----------------------------------------------------------------
  observe({
    # Ensure id_shock has a value
    req(input$id_shock)
    
    # Create reactive for processed data - shared across all conditions
    projection_processed_data <- reactive({
      req(df_policy(), year_when_estimations_start_after())
      projections_start_after <- year_when_estimations_start_after()
      start_year <- projections_start_after - 9
      
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
            indicators == "Baseline" ~ "Baseline Debt",
            TRUE ~ indicators
          )
        )
    })
    
    # Common data table rendering for all cases
    output$data_projection <- renderDT({
      req(projection_processed_data())
      DT::datatable(projection_processed_data())
    })
    
    # Common download handler for all cases
    output$download_projection <- downloadHandler(
      filename = function() {
        file_ext <- ifelse(input$file_type_projection == "CSV", ".csv", ".xlsx")
        paste(input$id_country, "-", input$id_shock, "-", "shock", "-", 
              Sys.Date(), file_ext, sep = "")
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
  
  # Main observer code
  observe({
    req(df_policy(), year_when_estimations_start_after())
    projections_start_after <- year_when_estimations_start_after()
    start_year <- projections_start_after - 9
    
    # Full plot
    output$plot_full <- echarts4r::renderEcharts4r({
      df_long <- server_prepare_shock_data(df_policy(), input$id_shock, start_year)
      server_create_debt_plot(df_long)
    })
    
    # Projection plot
    output$plot_projection <- echarts4r::renderEcharts4r({
      df_long <- df_policy() %>%
        filter(year > projections_start_after) %>%
        server_prepare_shock_data(input$id_shock)
      server_create_debt_plot(df_long)
    })
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
    
    datatable(processed_data())
    
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