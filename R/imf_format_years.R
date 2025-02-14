# starts: -----------------------------------------------------------------
imf_format_years <- function(years) {
  sapply(years, function(y) {
    if (y == min(years)) {
      # Full year for the first value
      as.character(y)
    } else {
      # Abbreviate to 'YY
      paste0("", substr(y, 3, 4))
    }
  })
}

# ends: -------------------------------------------------------------------
