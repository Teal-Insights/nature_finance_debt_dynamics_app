
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
        column(3, "Year"),
        column(3, "Baseline"),
        column(3, "Policy shock"),
        column(3, "Final")
      )
    ),
    
    # Year rows
    lapply(2024:2029, function(year) {
      div(
        class = "row-bordered",
        fluidRow(
          column(3, year),
          column(3, div(style = "padding-top: 7px;", 
                        textOutput(sprintf("%s_%d_coef", id, year)))),
          column(3, numericInput(sprintf("%s_%d_avg", id, year), 
                                 NULL, value = 3.525, step = 0.001)),
          column(3, textOutput(sprintf("%s_%d_score", id, year)))
        )
      )
    })
  )
}

# ends: -------------------------------------------------------------------


