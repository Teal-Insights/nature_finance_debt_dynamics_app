# starts: -----------------------------------------------------------------
imf_countries <- function() {
  # read data
  imf_data <- readr::read_rds(file = "data/IMFweo.rds") %>% 
    select(iso3c, label = country_name) %>% 
    distinct()
  
  # return dataframe
  return(imf_data)
}

# ends: -------------------------------------------------------------------
