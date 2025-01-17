
# starts: -----------------------------------------------------------------
imf_series <- function(series = c("NGDP_RPCH", "pb", "d"), by_iso3c = NULL, retries = 3) {
  # Helper function to fetch data for a single series
  fetch_series_data <- function(series, retries) {
    url_series <- glue::glue("https://www.imf.org/external/datamapper/api/v1/{series}")
    
    # Ensure HTTP/1.1 for compatibility
    httr::set_config(httr::config(http_version = 1.1))
    
    attempt <- 1
    success <- FALSE
    response <- NULL
    
    while (attempt <= retries && !success) {
      tryCatch({
        # Fetch response
        response <- httr::GET(url_series)
        
        # Check if the status is OK (200)
        if (httr::status_code(response) == 200) {
          success <- TRUE
        } else {
          warning("Failed to fetch data, status: ", httr::status_code(response))
        }
      }, error = function(e) {
        warning("Error fetching series data: ", e$message)
      })
      
      if (!success) {
        Sys.sleep(3)  # Wait before retrying
        attempt <- attempt + 1
      }
    }
    
    if (!success) {
      warning("Failed to fetch data after ", retries, " attempts.")
      return(NULL)
    }
    
    # Parse the response
    response_series <- tryCatch(
      jsonlite::fromJSON(httr::content(response, "text")),
      error = function(e) {
        warning(glue::glue("Error parsing JSON for series '{series}': {e$message}"))
        return(NULL)
      }
    )
    
    if (is.null(response_series) || !("values" %in% names(response_series))) {
      warning("No data found for series: ", series)
      return(NULL)
    }
    
    list_values <- response_series$values[[1]]
    if (is.null(list_values) || length(list_values) == 0) {
      warning(glue::glue("No data available for series '{series}'"))
      return(NULL)
    }
    
    # Convert to tibble
    purrr::map_df(names(list_values), ~ {
      data <- list_values[[.x]]
      if (!is.null(data)) {
        dplyr::tibble(iso3c = .x, !!!data)
      }
    }) %>%
      mutate(indicator = series) %>%
      relocate(indicator, .before = iso3c)
  }
  
  # Fetch data for all requested series
  combined_data <- purrr::map_dfr(series, fetch_series_data, retries = retries)
  
  # If no data was fetched, return an empty tibble
  if (nrow(combined_data) == 0) {
    warning("No data was fetched for the specified series.")
    return(dplyr::tibble())
  }
  
  # Filter by specified ISO3C codes if provided
  if (!is.null(by_iso3c)) {
    combined_data <- combined_data %>% filter(iso3c %in% by_iso3c)
  }
  
  # Rename indicators for better readability
  combined_data <- combined_data %>%
    mutate(indicator = case_when(
      indicator == "NGDP_RPCH" ~ "real_gdp_growth",
      indicator == "pb" ~ "primary_balance",
      indicator == "d" ~ "public_debt_pct_gdp",
      TRUE ~ indicator
    ))
  
  # Reshape data: Wide to Long and back to Wide
  final_dataframe <- combined_data %>%
    tidyr::gather(key = year, value = value, -c(iso3c, indicator)) %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= 1980) %>%
    tidyr::spread(key = indicator, value = value)
  
  # Return final tibble
  return(final_dataframe)
}


# ends: -------------------------------------------------------------------


