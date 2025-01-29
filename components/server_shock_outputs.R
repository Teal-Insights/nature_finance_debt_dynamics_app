
# starts: -----------------------------------------------------------------
create_shock_outputs <- function(shock_id, output, input, shock_values, 
                                 available_years, coefficients) {
  # Create dynamic rows for each shock
  output[[sprintf("%s_rows", shock_id)]] <- renderUI({
    years <- available_years()
    req(years)
    
    # Initialize shock values vector with the correct length
    shock_values[[shock_id]] <- numeric(length(years))
    
    # Create a row for each year
    lapply(seq_along(years), function(i) {
      year <- years[i]
      div(
        class = "row-bordered",
        fluidRow(
          column(2, year),
          column(3, div(style = "padding-top: 7px;", 
                        textOutput(sprintf("%s_%d_coef", shock_id, year)))),
          column(4, numericInput(sprintf("%s_%d_avg", shock_id, year), 
                                 NULL, value = 0, step = 0.05)),
          column(3, textOutput(sprintf("%s_%d_score", shock_id, year)))
        )
      )
    })
  })
  
  # Create reactive outputs for coefficients and scores
  observe({
    years <- available_years()
    req(years)
    
    for (i in seq_along(years)) {
      local({
        year <- years[i]
        year_index <- i
        
        coef_id <- sprintf("%s_%d_coef", shock_id, year)
        avg_id <- sprintf("%s_%d_avg", shock_id, year)
        score_id <- sprintf("%s_%d_score", shock_id, year)
        
        # Render coefficient
        output[[coef_id]] <- renderText({
          req(coefficients()[[shock_id]][[sprintf("y%d", year)]])
          coefficients()[[shock_id]][[sprintf("y%d", year)]]
        })
        
        # Render score and update shock values
        output[[score_id]] <- renderText({
          req(input[[avg_id]], 
              coefficients()[[shock_id]][[sprintf("y%d", year)]])
          
          final_value <- round(
            coefficients()[[shock_id]][[sprintf("y%d", year)]] + 
              input[[avg_id]], 2)
          shock_values[[shock_id]][year_index] <- final_value
          final_value
        })
      })
    }
  })
}

# ends: -------------------------------------------------------------------