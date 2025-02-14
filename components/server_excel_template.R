# starts: -----------------------------------------------------------------
server_excel_template <- function(file, df_main) {
  sheet_name <- "WEO_Data"
  excel_path <- "data/Policy_shock.xlsx"
  projections_start_after <- as.numeric(lubridate::year(Sys.Date()))

  # new data
  new_data <- df_main() %>%
    dplyr::select(c(
      iso3c, country_name, weo_subject_code,
      subject_descriptor, units, scale, estimates_start_after, year,
      outcome
    )) %>%
    dplyr::filter(year > (projections_start_after - 11)) %>%
    tidyr::spread(key = year, value = outcome) %>%
    dplyr::arrange(units) %>%
    dplyr::select(-estimates_start_after)

  # Load the workbook
  wb <- openxlsx::loadWorkbook(excel_path)

  # Remove WEO_Data sheet if it exists
  if (sheet_name %in% names(wb)) {
    openxlsx::removeWorksheet(wb = wb, sheet = sheet_name)
  }

  # Add WEO_Data as the first sheet
  openxlsx::addWorksheet(wb = wb, sheet = sheet_name, gridLines = TRUE)
  openxlsx::worksheetOrder(wb) <- c(
    length(names(wb)),
    1:(length(names(wb)) - 1)
  )

  # Write the new data
  openxlsx::writeData(wb = wb, sheet = sheet_name, new_data)

  # Save the workbook
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# ends: -------------------------------------------------------------------
