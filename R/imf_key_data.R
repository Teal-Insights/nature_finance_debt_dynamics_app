
# starts: -----------------------------------------------------------------

imf_key_data <- function(){
  # link
  countries = imfweo::weo_list_releases() %>% 
    dplyr::mutate(date = lubridate::ym(paste0(year,"-",month))) %>% 
    dplyr::arrange(desc(date)) %>% 
    dplyr::mutate(month_code = str_sub(string = month,start = 1,end = 3)) %>% 
    dplyr::mutate(
      full_link = paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/",
                         year,"/",month,"/WEO",month_code,year,"all.ashx")
    ) %>% 
    dplyr::slice(1) %>% 
    dplyr::pull(full_link)
  # raw data
  df_raw_countries <- read.delim(file = countries, skipNul = TRUE)
  base_columns <- c(names(df_raw_countries)[-tidyr::starts_with(match = "X", ignore.case = TRUE, vars = names(df_raw_countries))])
  
  # clean data
  df_clean_countries <- df_raw_countries %>%
    tidyr::pivot_longer(names_to = "year", values_to = "outcome", cols = -all_of(base_columns)) %>%
    dplyr::mutate(
      year = as.integer(gsub(x = year, pattern = "X", replacement = "")),
      outcome = dplyr::case_when(outcome %in% c("n/a","--") ~ NA, .default = outcome),
      outcome = as.numeric(gsub(x = outcome, pattern = ",", replacement = ""))
    ) %>%
    janitor::clean_names() %>%
    dplyr::filter(weo_subject_code != "") %>%
    rename("iso3c" = "iso") %>% 
    dplyr::mutate(country_name = countrycode::countrycode(iso3c,  "iso3c", "country.name"),
                  country_name = case_when(is.na(country_name) ~ country, .default = country_name)) %>%
    dplyr::mutate(country_name = case_when(iso3c == "TUR" ~ country, .default = country_name)) %>% 
    relocate(c(iso3c, country_name), .after = weo_country_code) %>% 
    dplyr::select(-country) %>% 
    filter(
      weo_subject_code %in% c(
        "NGDP","GGXONLB_NGDP","GGXWDG_NGDP"
      )
    )
  
  # return data
  return(df_clean_countries)
}


# ends: -------------------------------------------------------------------

