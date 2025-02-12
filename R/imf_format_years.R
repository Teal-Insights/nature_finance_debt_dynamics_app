# starts: -----------------------------------------------------------------
imf_format_years <- function(years) {
  sapply(years, function(y) {
    if (y == min(years)) {
      as.character(y) # Full year for the first value
    } else {
      paste0("", substr(y, 3, 4)) # Abbreviate to 'YY
    }
  })
}

# ends: -------------------------------------------------------------------
