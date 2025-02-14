# starts: -----------------------------------------------------------------
imf_indicators <- function() {
  # API URL
  url_indicators <- "https://www.imf.org/external/datamapper/api/v1/indicators"

  # Ensure HTTP/1.1 for compatibility
  httr::set_config(httr::config(http_version = 1.1))

  # API request
  response <- httr::GET(url_indicators)

  # Check for successful response
  if (httr::status_code(response) != 200) {
    stop(
      "Failed to fetch data. HTTP status code: ", httr::status_code(response),
      ". Reason: ", httr::http_status(response)$message
    )
  }

  # Parse JSON content
  response_indicators <- tryCatch(
    jsonlite::fromJSON(httr::content(response, "text")),
    error = function(e) {
      stop("Error parsing JSON response: ", e$message)
    }
  )

  # Extract indicators
  list_indicators <- response_indicators$indicators
  if (is.null(list_indicators) || length(list_indicators) == 0) {
    stop("No indicators found in the response.")
  }
  # Convert the list of indicators to a tibble
  merged_indicators <- purrr::map_df(
    names(list_indicators),
    ~ {
      data <- list_indicators[[.x]]
      if (!is.null(data)) {
        dplyr::tibble(indicator = .x, !!!data)
      }
    }
  )
  # Return the resulting tibble
  return(merged_indicators)
}

# ends: -------------------------------------------------------------------
