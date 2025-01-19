
# starts: -----------------------------------------------------------------


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

# -------------------------------------------------------------------------
# shocks analysis ---------------------------------------------------------

  df_baseline <- reactive({
    req(df_main(), df_specific())
    projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
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
  
# Primary balance ---------------------------------------------------------
  # shock primary balance
  df_shock_pb <- reactive({
    req(df_baseline())
    df_baseline() %>% 
      mutate(
        shock_pb_by_200_bp = (GGXONLB_NGDP + (200/100)),
        shock_pb_by_300_bp = (GGXONLB_NGDP + (300/100)),
        shock_pb_by_400_bp = (GGXONLB_NGDP + (400/100))
      ) %>% 
      # modelling debt using debt dynamic equation and new shocks
      mutate(
        debt_shock_pb_by_200_bp = (((1+real_effective_rate/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - shock_pb_by_200_bp),
        debt_shock_pb_by_300_bp = (((1+real_effective_rate/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - shock_pb_by_300_bp),
        debt_shock_pb_by_400_bp = (((1+real_effective_rate/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - shock_pb_by_400_bp)
      )
  })
  # debt projection for primary balance
  df_debt_projection_pb <- reactive({
    req(df_shock_pb())
    df_shock_pb() %>% 
      select(year, baseline = GGXWDG_NGDP, starts_with("debt"))
  })

# interest rate -----------------------------------------------------------
  # shock real effective rate
  df_shock_ir <- reactive({
    req(df_baseline())
    
    df_baseline() %>% 
      mutate(
        shock_ir_by_10_bp = (real_effective_rate - (10/100)),
        shock_ir_by_20_bp = (real_effective_rate - (20/100)),
        shock_ir_by_30_bp = (real_effective_rate - (30/100))
      ) %>% 
      # modelling debt using debt dynamic equation and new shocks
      mutate(
        debt_shock_ir_by_10_bp = (((1+shock_ir_by_10_bp/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - GGXONLB_NGDP),
        debt_shock_ir_by_20_bp = (((1+shock_ir_by_20_bp/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - GGXONLB_NGDP),
        debt_shock_ir_by_30_bp = (((1+shock_ir_by_30_bp/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - GGXONLB_NGDP)
      )
  })
  # debt projection for real effective rate
  df_debt_projection_ir <- reactive({
    req(df_shock_ir())
    
    df_shock_ir() %>% 
      select(year, baseline = GGXWDG_NGDP, starts_with("debt"))
  })

# GDP ---------------------------------------------------------------------
  # shock GDP
  df_shock_gdp <- reactive({
    req(df_baseline())
    df_baseline() %>% 
      mutate(
        shock_gdp_by_10_bp = (gdp_growth + (10/100)),
        shock_gdp_by_20_bp = (gdp_growth + (20/100)),
        shock_gdp_by_30_bp = (gdp_growth + (30/100))
      ) %>% 
      # modelling debt using debt dynamic equation and new shocks
      mutate(
        debt_shock_gdp_by_10_bp = (((1+real_effective_rate/100)/(1+shock_gdp_by_10_bp/100))*lag(GGXWDG_NGDP) - GGXONLB_NGDP),
        debt_shock_gdp_by_20_bp = (((1+real_effective_rate/100)/(1+shock_gdp_by_20_bp/100))*lag(GGXWDG_NGDP) - GGXONLB_NGDP),
        debt_shock_gdp_by_30_bp = (((1+real_effective_rate/100)/(1+shock_gdp_by_30_bp/100))*lag(GGXWDG_NGDP) - GGXONLB_NGDP)
      )
  })
  # debt projection for GDP
  df_debt_projection_gdp <- reactive({
    req(df_shock_gdp())
    
    df_shock_gdp() %>% 
      select(year, baseline = GGXWDG_NGDP, starts_with("debt"))
  })
  
# -------------------------------------------------------------------------
# input: ------------------------------------------------------------------
  # primary balance baseline data
  pb_data <- reactive({
    req(df_baseline(), df_main()) 
    # projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
    
    df_baseline() %>%
      select(year, value = GGXONLB_NGDP) %>%
      filter(year > 2023)
  })
  # Real interest rate baseline data
  ir_data <- reactive({
    req(df_baseline(), df_main()) 
    # projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
    
    df_baseline() %>%
      select(year, value = real_effective_rate) %>%
      filter(year > 2023)
  })
  # GDP baseline data
  gdp_data <- reactive({
    req(df_baseline(), df_main()) 
    # projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
    
    df_baseline() %>%
      select(year, value = gdp_growth) %>%
      filter(year > 2023)
  })
  
  # Function to convert dataframe to named list for easier access
  df_to_list <- function(df) {
    values <- setNames(df$value, paste0("y", df$year))
    return(as.list(values))
  }
  
  # Make coefficients reactive
  coefficients <- reactive({
    list(
      pb = df_to_list(pb_data()),
      ir = df_to_list(ir_data()),
      gdp = df_to_list(gdp_data())
    )
  })
  
  # Create reactive values to store final values
  shock_values <- reactiveValues(
    pb = numeric(6),
    ir = numeric(6),
    gdp = numeric(6)
  )
  
  # Function to create outputs for each shock
  create_shock_outputs <- function(shock_id) {
    lapply(2024:2029, function(year) {
      coef_id <- sprintf("%s_%d_coef", shock_id, year)
      avg_id <- sprintf("%s_%d_avg", shock_id, year)
      score_id <- sprintf("%s_%d_score", shock_id, year)
      year_index <- year - 2023
      
      # Render coefficient using shock-specific values
      output[[coef_id]] <- renderText({
        coefficients()[[shock_id]][[sprintf("y%d", year)]]
      })
      
      # Render score and update shock values
      output[[score_id]] <- renderText({
        req(input[[avg_id]], coefficients())
        final_value <- round(coefficients()[[shock_id]][[sprintf("y%d", year)]] * input[[avg_id]], 2)
        shock_values[[shock_id]][year_index] <- final_value
        final_value
      })
    })
  }
  
  # Create outputs for all three shocks
  create_shock_outputs("pb")
  create_shock_outputs("ir")
  create_shock_outputs("gdp")
  
  # Make shock values accessible for external use
  reactive_shock_values <- reactive({
    list(
      pb = shock_values$pb,
      ir = shock_values$ir,
      gdp = shock_values$gdp
    )
  })

# -------------------------------------------------------------------------
# Graphs: -----------------------------------------------------------------
  observe({
    # Ensure id_shock has a value
    req(input$id_shock) 
    
    if (input$id_shock == "Primary balance") {
      # Primary balance full plot
      output$plot_full <- renderPlot({
        req(df_debt_projection_pb())
        df_debt_projection_pb() %>% 
          gather(key = indicator, value = outcome, -c("year")) %>% 
          ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
          theme_classic() +
          geom_line() +
          scale_x_continuous(expand = c(0, 0)) +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          )
      })
      
      # Primary balance projection plot
      output$plot_projection <- renderPlot({
        req(df_debt_projection_pb(), df_main())
        projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
        plot_start <- (projections_start_after + 1)
        
        df_debt_projection_pb() %>% 
          filter(year > projections_start_after) %>% 
          gather(key = indicator, value = outcome, -c("year")) %>% 
          ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
          theme_classic() +
          geom_line() +
          scale_x_continuous(expand = c(0, 0)) +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          )
      })
      
      # Primary balance data table
      projection_processed_data <- reactive({
        req(df_debt_projection_pb(), df_main())
        projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
        start_year <- projections_start_after - 9
        
        # process data
        df_debt_projection_pb() %>% 
          gather(key = indicators, value = outcome,-c("year")) %>% 
          filter(year >= start_year) %>% 
          mutate(outcome = round(x = outcome, digits = 2)) %>% 
          spread(key = year, value = outcome)
      })
      
      output$data_projection <- renderDT({
        req(projection_processed_data())
        # display data in datatable format
        DT::datatable(projection_processed_data())
      })
      
      # Download handler
      output$download_projection <- downloadHandler(
        filename = function() {
          # Define file extension based on user input
          file_ext <- ifelse(input$file_type_projection == "CSV", ".csv", ".xlsx")
          paste(input$id_country,"-",input$id_shock, "-","shock","-", Sys.Date(), file_ext, sep = "")
        },
        
        content = function(file) {
          # If columns are valid, proceed to download
          if (input$file_type_projection == "CSV") {
            # Write to CSV
            write.csv(projection_processed_data(), file, row.names = FALSE)
          } else if (input$file_type_projection == "Excel") {
            # Write to Excel
            openxlsx::write.xlsx(projection_processed_data(), file, overwrite = TRUE)
          }
        }
      )
      
    } else if (input$id_shock == "GDP growth") {
      # GDP growth full plot
      output$plot_full <- renderPlot({
        req(df_debt_projection_gdp())
        df_debt_projection_gdp() %>% 
          gather(key = indicator, value = outcome, -c("year")) %>% 
          ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
          theme_classic() +
          geom_line() +
          scale_x_continuous(expand = c(0, 0)) +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          )
      })
      
      # GDP growth projection plot
      output$plot_projection <- renderPlot({
        req(df_debt_projection_gdp(), df_main())
        projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
        
        df_debt_projection_gdp() %>% 
          filter(year > projections_start_after) %>% 
          gather(key = indicator, value = outcome, -c("year")) %>% 
          ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
          theme_classic() +
          geom_line() +
          scale_x_continuous(expand = c(0, 0)) +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          )
      })
      
      # GDP growth data table
      projection_processed_data <- reactive({
        req(df_debt_projection_gdp(), df_main())
        projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
        start_year <- projections_start_after - 9
        
        df_debt_projection_gdp() %>% 
          gather(key = indicators, value = outcome,-c("year")) %>% 
          filter(year >= start_year) %>% 
          mutate(outcome = round(x = outcome, digits = 2)) %>%
          spread(key = year, value = outcome)
      })
      
      output$data_projection <- renderDT({
        req(projection_processed_data())
        # display data in datatable format
        DT::datatable(projection_processed_data())
      })
      
      # Download handler
      output$download_projection <- downloadHandler(
        filename = function() {
          # Define file extension based on user input
          file_ext <- ifelse(input$file_type_projection == "CSV", ".csv", ".xlsx")
          paste(input$id_country,"-",input$id_shock, "-","shock","-", Sys.Date(), file_ext, sep = "")
        },
        
        content = function(file) {
          # If columns are valid, proceed to download
          if (input$file_type_projection == "CSV") {
            # Write to CSV
            write.csv(projection_processed_data(), file, row.names = FALSE)
          } else if (input$file_type_projection == "Excel") {
            # Write to Excel
            openxlsx::write.xlsx(projection_processed_data(), file, overwrite = TRUE)
          }
        }
      )
      
    } else if (input$id_shock == "Real effective interest rate") {
      # Real effective interest rate full plot
      output$plot_full <- renderPlot({
        req(df_debt_projection_ir())
        df_debt_projection_ir() %>% 
          gather(key = indicator, value = outcome, -c("year")) %>% 
          ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
          theme_classic() +
          geom_line() +
          scale_x_continuous(expand = c(0, 0)) +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          )
      })
      
      # Real effective interest rate projection plot
      output$plot_projection <- renderPlot({
        req(df_debt_projection_ir(), df_main())
        projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
        
        df_debt_projection_ir() %>% 
          filter(year > projections_start_after) %>% 
          gather(key = indicator, value = outcome, -c("year")) %>% 
          ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
          theme_classic() +
          geom_line() +
          scale_x_continuous(expand = c(0, 0)) +
          theme(
            legend.position = "top",
            legend.title = element_blank()
          )
      })
      
      # Real effective interest rate data table
      projection_processed_data <- reactive({
        req(df_debt_projection_ir(),df_main())
        projections_start_after <- df_main() %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
        start_year <- projections_start_after - 9
        
        df_debt_projection_ir() %>% 
          gather(key = indicators, value = outcome,-c("year")) %>% 
          filter(year >= start_year) %>% 
          mutate(outcome = round(x = outcome, digits = 2)) %>%
          spread(key = year, value = outcome)
      })
      
      output$data_projection <- renderDT({
        req(projection_processed_data())
        # display data in datatable format
        DT::datatable(projection_processed_data())
      })
      
      # Download handler
      output$download_projection <- downloadHandler(
        filename = function() {
          # Define file extension based on user input
          file_ext <- ifelse(input$file_type_projection == "CSV", ".csv", ".xlsx")
          paste(input$id_country,"-",input$id_shock, "-","shock","-", Sys.Date(), file_ext, sep = "")
        },
        
        content = function(file) {
          # If columns are valid, proceed to download
          if (input$file_type_projection == "CSV") {
            # Write to CSV
            write.csv(projection_processed_data(), file, row.names = FALSE)
          } else if (input$file_type_projection == "Excel") {
            # Write to Excel
            openxlsx::write.xlsx(projection_processed_data(), file, overwrite = TRUE)
          }
        }
      )
      
    }
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


