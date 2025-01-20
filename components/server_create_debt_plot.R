
# starts ------------------------------------------------------------------
server_create_debt_plot <- function(df_long) {
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_xAxis(
      categories = unique(df_long$year)
    ) %>%
    hc_yAxis(
      title = list(text = "Debt (% of GDP)")
    ) %>%
    hc_add_series_list(
      lapply(unique(df_long$indicator), function(ind) {
        data <- df_long %>% 
          filter(indicator == ind) %>% 
          select(outcome) %>% 
          pull()
        
        list(
          name = ind,
          data = data
        )
      })
    ) %>%
    hc_legend(
      align = "center",
      verticalAlign = "top",
      layout = "horizontal"
    ) %>%
    hc_title(text = "") %>%
    hc_tooltip(
      crosshairs = TRUE,
      shared = TRUE
    )
}

# ends --------------------------------------------------------------------


