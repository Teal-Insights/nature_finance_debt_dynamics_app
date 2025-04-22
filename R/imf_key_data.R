# starts: -----------------------------------------------------------------
imf_key_data <- function() {
  current_month <- lubridate::month(x = Sys.Date(), label = TRUE, abbr = FALSE)
  releases_months <- c("April", "October")
  releases_status <- (current_month %in% releases_months)
  
  # Add error handling and default return value
  tryCatch(
    {
      # get data when release month is either April or October
      if (releases_status) {
        # link
        countries <- imfweo::weo_list_releases() %>%
          dplyr::mutate(date = lubridate::ym(paste0(year, "-", month))) %>%
          dplyr::arrange(desc(date)) %>%
          dplyr::mutate(month_code = stringr::str_sub(
            string = month, start = 1,
            end = 3
          )) %>%
          dplyr::mutate(
            month = tolower(month),
            full_link = glue::glue(
              "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/{year}/{month}/WEO{month_code}{year}all.ashx"
            )
          ) %>%
          dplyr::slice(1) %>%
          dplyr::pull(full_link)
        
        # URL check using
        url_check <- tryCatch({
          if (!requireNamespace("httr", quietly = TRUE)) {
            install.packages("httr")
            library(httr)
          } else {
            library(httr)
          }
          
          response <- httr::HEAD(
            countries,
            httr::user_agent("R/4.2.0 (https://www.r-project.org/)"),
            httr::timeout(10)
          )
          
          status <- httr::status_code(response)
          status >= 200 && status < 400
        }, error = function(e) {
          # If HEAD request fails, try GET as a fallback
          tryCatch({
            test_get <- httr::GET(
              countries,
              httr::user_agent("R/4.2.0 (https://www.r-project.org/)"),
              httr::timeout(10)
            )
            status <- httr::status_code(test_get)
            status >= 200 && status < 400
          }, error = function(e2) {
            FALSE
          })
        })
        
        # If URL check fails, try a direct download attempt as last resort
        if (!url_check) {
          message("URL check failed, attempting direct download...")
        }
        
        # Try to read data even if URL check failed
        df_raw_countries <- tryCatch({
          read.delim(file = countries, skipNul = TRUE)
        }, error = function(e) {
          warning("Failed to download data: ", e$message)
          return(NULL)
        })
        
        if (is.null(df_raw_countries) || nrow(df_raw_countries) == 0) {
          warning("No data retrieved")
          return(NULL)
        }
        
        base_columns <- c(
          names(df_raw_countries)[!startsWith(names(df_raw_countries), "X")]
        )
        
        # clean data
        df_clean_countries <- df_raw_countries %>%
          tidyr::pivot_longer(
            names_to = "year",
            values_to = "outcome",
            cols = -all_of(base_columns)
          ) %>%
          dplyr::mutate(
            year = as.integer(gsub(x = year, pattern = "X", replacement = "")),
            outcome = dplyr::case_when(
              outcome %in% c("n/a", "--") ~ NA_character_,
              .default = outcome
            ),
            outcome = as.numeric(
              gsub(x = outcome, pattern = ",", replacement = "")
            )
          ) %>%
          janitor::clean_names() %>%
          dplyr::filter(!is.na(weo_subject_code)) %>%
          dplyr::rename("iso3c" = "iso") %>%
          dplyr::mutate(
            country_name = countrycode::countrycode(
              iso3c, "iso3c", "country.name"
            ),
            country_name = dplyr::case_when(
              is.na(country_name) ~ country,
              .default = country_name
            ),
            country_name = dplyr::case_when(
              iso3c == "TUR" ~ country,
              .default = country_name
            )
          ) %>%
          dplyr::relocate(
            c(iso3c, country_name),
            .after = weo_country_code
          ) %>%
          dplyr::select(-country) %>%
          dplyr::filter(
            weo_subject_code %in% c(
              "NGDP_RPCH", "GGXONLB_NGDP", "GGXWDG_NGDP"
            )
          )
        
        # export data
        tryCatch({
          readr::write_rds(
            x = df_clean_countries,
            file = "data/IMFweo.rds", compress = "xz"
          )
          message("Data successfully saved to data/IMFweo.rds")
        }, error = function(e) {
          warning("Failed to save data: ", e$message)
        })
        
        # Always return the data frame
        return(df_clean_countries)
      } else {
        message("Not a release month. Current month: ", current_month)
        return(NULL)
      }
    },
    error = function(e) {
      warning("Error in imf_key_data: ", e$message)
      return(NULL)
    }
  )
}

# ends --------------------------------------------------------------------
