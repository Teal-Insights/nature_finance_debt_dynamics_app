
# starts: -----------------------------------------------------------------
read_imf_weo <- function(){
  readr::read_rds(file = "data/IMFweo.rds")
}
# Helper functions for data processing
process_main_data <- function(df_countries, input_country, imf_data) {
  country_iso3c <- df_countries %>% 
    mutate(label = gsub(pattern = "The ", x = label, replacement = "")) %>% 
    filter(label %in% c(input_country)) %>% 
    pull(iso3c)
  
  imf_data %>% 
    filter(iso3c == country_iso3c)
}

process_specific_data <- function(main_data, projection_year) {
  projections_start_in <- projection_year
  
  # Initial data selection and filtering
  df_start <- main_data %>% 
    select(c(weo_country_code, iso3c, country_name, weo_subject_code, 
             subject_descriptor, units, scale, estimates_start_after, year, outcome)) %>% 
    filter(year >= projections_start_in - 13)
  
  # Compute additional metrics
  df_compute <- df_start %>% 
    select(weo_subject_code, year, outcome) %>% 
    spread(key = weo_subject_code, value = outcome) %>% 
    mutate(
      real_effective_rate = ((1 + replace_na(NGDP_RPCH, 0)/100) * 
                               ((replace_na(GGXWDG_NGDP, 0) + replace_na(GGXONLB_NGDP, 0))/(lag(replace_na(GGXWDG_NGDP, 0)))) - 1) * 100
    ) %>% 
    gather(key = weo_subject_code, value = outcome, -c("year"))
  
  # Join and format final dataset
  full_join(
    x = df_start,
    y = df_compute,
    by = c("weo_subject_code", "year", "outcome")
  ) %>% 
    mutate(
      units = case_when(
        weo_subject_code == "real_effective_rate" ~ "Percent",
        .default = units
      ),
      scale = case_when(
        weo_subject_code == "real_effective_rate" ~ "Units",
        .default = scale
      ),
      subject_descriptor = case_when(
        weo_subject_code == "real_effective_rate" ~ "Real effective interest rate",
        .default = subject_descriptor
      )
    ) %>% 
    fill(weo_country_code, .direction = "down") %>% 
    fill(iso3c, .direction = "down") %>% 
    fill(country_name, .direction = "down") 
  # %>% 
  #   mutate(outcome = round(x = outcome, digits = 2))
}

# Function to setup reactive expressions
setup_reactive_data <- function(input, df_countries, read_imf_weo) {
  # Get main data
  df_main <- reactive({
    req(input$id_country)
    process_main_data(
      df_countries = df_countries,
      input_country = input$id_country,
      imf_data = read_imf_weo()
    )
  })
  
  # Get projection year
  year_when_estimations_start <- reactive({
    req(df_main(), input$projection_year)
    as.numeric(input$projection_year)
  })
  
  # Get specific data
  df_specific <- reactive({
    req(df_main(), year_when_estimations_start())
    process_specific_data(
      main_data = df_main(),
      projection_year = year_when_estimations_start()
    )
  })
  
  # Return all reactive expressions
  list(
    df_main = df_main,
    df_specific = df_specific,
    year_when_estimations_start = year_when_estimations_start
  )
}

# ends: -------------------------------------------------------------------
