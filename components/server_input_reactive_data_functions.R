# starts: -----------------------------------------------------------------
#' Create a reactive expression for available years
#' @param df_baseline Reactive expression containing baseline data
#' @param year_when_estimations_start Reactive expression containing start year
#' @return Reactive expression returning sorted unique years
get_available_years <- function(df_baseline, year_when_estimations_start) {
  shiny::reactive({
    shiny::req(df_baseline(), year_when_estimations_start())
    projections_start_in <- year_when_estimations_start()

    df_baseline() %>%
      dplyr::filter(year >= projections_start_in) %>%
      dplyr::pull(year) %>%
      base::unique() %>%
      base::sort()
  })
}

#' Create a reactive expression for shock values
#' @param available_years Reactive expression returning available years
#' @param shock_values ReactiveValues object containing pb, ir, and gdp shocks
#' @return Reactive expression returning dataframe of shock values
get_reactive_shock_values <- function(available_years, shock_values) {
  shiny::reactive({
    years <- available_years()
    shiny::req(years)

    data.frame(
      year      = years,
      pb_shock  = shock_values$pb,
      ir_shock  = shock_values$ir,
      gdp_shock = shock_values$gdp
    )
  })
}

#' Create a reactive expression for primary balance data
#' @param df_baseline Reactive expression containing baseline data
#' @param year_when_estimations_start Reactive expression containing start year
#' @return Reactive expression returning filtered primary balance data
get_pb_data <- function(df_baseline, year_when_estimations_start) {
  shiny::reactive({
    shiny::req(df_baseline(), year_when_estimations_start())
    projections_start_in <- year_when_estimations_start()

    df_baseline() %>%
      dplyr::select(year, value = GGXONLB_NGDP) %>%
      dplyr::filter(year >= projections_start_in)
  })
}

#' Create a reactive expression for interest rate data
#' @param df_baseline Reactive expression containing baseline data
#' @param year_when_estimations_start Reactive expression containing start year
#' @return Reactive expression returning filtered interest rate data
get_ir_data <- function(df_baseline, year_when_estimations_start) {
  shiny::reactive({
    shiny::req(df_baseline(), year_when_estimations_start())
    projections_start_in <- year_when_estimations_start()

    df_baseline() %>%
      dplyr::select(year, value = real_effective_rate) %>%
      dplyr::filter(year >= projections_start_in)
  })
}

#' Create a reactive expression for GDP data
#' @param df_baseline Reactive expression containing baseline data
#' @param year_when_estimations_start Reactive expression containing start year
#' @return Reactive expression returning filtered GDP data
get_gdp_data <- function(df_baseline, year_when_estimations_start) {
  shiny::reactive({
    shiny::req(df_baseline(), year_when_estimations_start())
    projections_start_in <- year_when_estimations_start()

    df_baseline() %>%
      dplyr::select(year, value = NGDP_RPCH) %>%
      dplyr::filter(year >= projections_start_in)
  })
}

#' Convert dataframe to named list
#' @param df Dataframe containing year and value columns
#' @param shock_name Name of the shock for error messages
#' @return Named list of values
df_to_list <- function(df, shock_name) {
  tryCatch(
    {
      if (nrow(df) == 0) {
        return(setNames(numeric(0), character(0)))
      }
      values <- setNames(df$value, paste0("y", df$year))
      return(as.list(values))
    },
    error = function(e) {
      stop(sprintf(
        "Error converting %s data to list: %s",
        shock_name, e$message
      ))
    }
  )
}

#' Create a reactive expression for coefficients
#' @param pb_data Reactive expression containing primary balance data
#' @param ir_data Reactive expression containing interest rate data
#' @param gdp_data Reactive expression containing GDP data
#' @return Reactive expression returning list of coefficients
get_coefficients <- function(pb_data, ir_data, gdp_data) {
  shiny::reactive({
    list(
      pb = df_to_list(pb_data(), "Primary Balance"),
      ir = df_to_list(ir_data(), "Interest Rate"),
      gdp = df_to_list(gdp_data(), "GDP Growth")
    )
  })
}

# ends: -------------------------------------------------------------------
