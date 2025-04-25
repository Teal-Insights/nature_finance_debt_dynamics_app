# starts: -----------------------------------------------------------------

process_specific_data <- function(main_data, projection_year) {
  projections_start_in <- projection_year

  # Initial data selection and filtering
  df_start <- main_data %>%
    dplyr::select(
      c(
        weo_country_code, iso3c, country_name, weo_subject_code,
        subject_descriptor, units, scale, year,
        outcome
      )
    ) %>%
    dplyr::filter(year >= projections_start_in - 13)

  # Compute additional metrics
  df_compute <- df_start %>%
    dplyr::select(weo_subject_code, year, outcome) %>%
    tidyr::spread(key = weo_subject_code, value = outcome) %>%
    dplyr::mutate(
      real_effective_rate =
        ((1 + tidyr::replace_na(NGDP_RPCH, 0) / 100) *
         ((tidyr::replace_na(GGXWDG_NGDP, 0) +
           tidyr::replace_na(GGXONLB_NGDP, 0)) /
          (lag(tidyr::replace_na(GGXWDG_NGDP, 0)))) - 1) * 100
    ) %>%
    tidyr::gather(key = weo_subject_code, value = outcome, -c("year"))

  # Join and format final dataset
  dplyr::full_join(
    x = df_start,
    y = df_compute,
    by = c("weo_subject_code", "year", "outcome")
  ) %>%
    dplyr::mutate(
      units = dplyr::case_when(
        weo_subject_code == "real_effective_rate" ~ "Percent",
        .default = units
      ),
      scale = dplyr::case_when(
        weo_subject_code == "real_effective_rate" ~ "Units",
        .default = scale
      ),
      subject_descriptor = dplyr::case_when(
        weo_subject_code == "real_effective_rate" ~
          "Real effective interest rate",
        .default = subject_descriptor
      )
    ) %>%
    tidyr::fill(weo_country_code, .direction = "down") %>%
    tidyr::fill(iso3c, .direction = "down") %>%
    tidyr::fill(country_name, .direction = "down")
}

# Function to setup reactive expressions
setup_reactive_data <- function(input, df_countries, read_imf_weo) {
  # Get main data
  df_main <- shiny::reactive({
    shiny::req(input$id_country)
    process_main_data(
      df_countries = df_countries,
      input_country = input$id_country,
      imf_data = read_imf_weo()
    )
  })

  # Get projection year
  year_when_estimations_start <- shiny::reactive({
    shiny::req(df_main(), input$projection_year)
    as.numeric(input$projection_year)
  })

  # Get specific data
  df_specific <- shiny::reactive({
    shiny::req(df_main(), year_when_estimations_start())
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
