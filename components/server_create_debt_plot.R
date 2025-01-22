
# starts ------------------------------------------------------------------
server_create_debt_plot <- function(df_long) {
  # Get min value for y-axis (rounded down to nearest whole number)
  y_min <- floor(min(df_long$outcome))
  
  df_long %>%
    group_by(indicator) %>%
    e_charts(year) %>%
    e_line(outcome) %>%
    e_x_axis(
      name = "",
      type = "category"
    ) %>%
    e_y_axis(
      name = "Debt (% of GDP)",
      scale = TRUE,          # This ensures axis starts from min value
      min = y_min           # Set minimum value
    ) %>%
    e_legend(
      top = "0",
      orient = "horizontal",
      x = "center"
    ) %>%
    e_tooltip(
      trigger = "axis",
      axisPointer = list(
        type = "cross"
      )
    ) %>%
    e_grid(
      containLabel = TRUE,
      top = "15%"
    )
}

# ends --------------------------------------------------------------------


