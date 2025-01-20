
# starts: -----------------------------------------------------------------
# Function to create shock table UI
ui_create_shock_table <- function(id) {
  div(
    class = "table-container p-2",
    style = "font-size: 0.9rem;",
    
    # Header row
    div(
      class = "header-row",
      fluidRow(
        column(2, "Year"),
        column(3, "Baseline (%)"),
        column(4, "Policy shock (%)"),
        column(3, "Final shock (%)")
      )
    ),
    
    # Year rows
    lapply(2024:2029, function(year) {
      div(
        class = "row-bordered",
        fluidRow(
          column(2, year),
          column(3, div(style = "padding-top: 7px;", 
                        textOutput(sprintf("%s_%d_coef", id, year)))),
          column(4, numericInput(sprintf("%s_%d_avg", id, year), 
                                 NULL, value = 0.1, step = 0.05)),
          column(3, textOutput(sprintf("%s_%d_score", id, year)))
        )
      )
    })
  )
}

# ends: -------------------------------------------------------------------


