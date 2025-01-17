
# starts: -----------------------------------------------------------------
imf_countries <- function() {
  # API URL
  url_countries <- "https://www.imf.org/external/datamapper/api/v1/countries"
  
  # Ensure HTTP/1.1 for compatibility
  httr::set_config(httr::config(http_version = 1.1))
  
  # API request
  response <- httr::GET(url_countries)
  
  # Check for successful response
  if (httr::status_code(response) != 200) {
    stop(
      "Failed to fetch data. HTTP status code: ", httr::status_code(response),
      ". Reason: ", httr::http_status(response)$message
    )
  }
  
  # Parse JSON content
  response_countries <- tryCatch(
    jsonlite::fromJSON(httr::content(response, "text")),
    error = function(e) {
      stop("Error parsing JSON response: ", e$message)
    }
  )
  
  # Extract countries
  list_countries <- response_countries$countries
  if (is.null(list_countries) || length(list_countries) == 0) {
    stop("No countries found in the response.")
  }
  
  # Convert the list of countries to a tibble
  merged_countries <- purrr::map_df(
    names(list_countries),
    ~ {
      data <- list_countries[[.x]]
      if (!is.null(data)) {
        dplyr::tibble(iso3c = .x, !!!data)
      }
    }
  )
  
  # Return the resulting tibble
  return(merged_countries)
}

# ends: -------------------------------------------------------------------


