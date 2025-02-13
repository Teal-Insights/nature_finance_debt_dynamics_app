# starts: -----------------------------------------------------------------
# Function to create shock table UI
ui_create_shock_table <- function(id) {
  div(
    class = "table-container p-2",
    style = "font-size: 0.9rem;",

    # Error/warning display
    uiOutput(sprintf("%s_error", id)),

    # Header row
    div(
      class = "header-row",
      fluidRow(
        column(2, "Year"),
        column(3, "IMF WEO Baseline (%)"),
        column(4, "Policy shock (%)"),
        column(3, "Policy-Adjusted Forecast (%)")
      )
    ),

    # Dynamic year rows
    uiOutput(sprintf("%s_rows", id))
  )
}

# ends: -------------------------------------------------------------------
