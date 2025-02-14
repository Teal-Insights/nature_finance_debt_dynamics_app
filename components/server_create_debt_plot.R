# starts ------------------------------------------------------------------
server_create_debt_plot <- function(df_long) {
  # Get min value for y-axis (rounded down to nearest whole number)
  y_min <- base::floor(base::min(df_long$outcome))

  df_long %>%
    dplyr::group_by(indicator) %>%
    echarts4r::e_charts(year) %>%
    echarts4r::e_line(outcome) %>%
    echarts4r::e_x_axis(
      name = "",
      type = "category"
    ) %>%
    echarts4r::e_y_axis(
      name = "Debt (% of GDP)",
      scale = TRUE,
      min = y_min
    ) %>%
    echarts4r::e_legend(
      top = "0",
      orient = "horizontal",
      x = "center"
    ) %>%
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS("
        function(params) {
          var year = params[0].axisValue;
          var result = year;
          params.forEach(function(param) {
            var value = Number(param.value[1]).toFixed(3);
            result += '<br/>' + param.marker + param.seriesName + ': ' + value;
          });
          return result;
        }
      "),
      axisPointer = list(
        type = "cross"
      )
    ) %>%
    echarts4r::e_grid(
      containLabel = TRUE,
      top = "15%"
    )
}

# ends --------------------------------------------------------------------
