
# starts: -----------------------------------------------------------------
# loading necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(imfweo)
  library(echarts4r)
  library(plotly)
  library(highcharter)
})
# loading necessary Rscripts
source(file = "R/imf_countries.R")
source(file = "R/imf_key_data.R")
source(file = "R/imf_format_years.R")

# api call: ---------------------------------------------------------------
df_main <- imf_key_data() %>% 
  filter(iso3c == "KEN")


# full data: --------------------------------------------------------------
# projections_start_after <- df_main %>% pull(estimates_start_after) %>% max(na.rm = TRUE) + 2
projections_start_after <- 2025

df_template <- df_main %>% 
  select(c(iso3c, country_name, weo_subject_code, 
           subject_descriptor,units, scale, estimates_start_after, year, 
           outcome)) %>% 
  filter(year > (projections_start_after - 11)) %>% 
  spread(key = year, value = outcome) %>% 
  arrange(units) %>% 
  select(-estimates_start_after)
  
df_specific <- df_main %>% 
  select(c(weo_country_code, iso3c, country_name, weo_subject_code, 
           subject_descriptor,units, scale, estimates_start_after, year, 
           outcome)) %>% 
  filter(year >= (projections_start_after - 13))

df_long <- df_specific %>% 
  spread(key = year, value = outcome) %>% 
  arrange(units) %>% 
  select(-estimates_start_after)

df_compute <- df_specific %>% 
  select(weo_subject_code, year, outcome) %>% 
  spread(key = weo_subject_code, value = outcome) %>% 
  mutate(
    gdp_growth = (NGDP - lag(NGDP)) / lag(NGDP) * 100
  ) %>% 
  mutate(
    real_effective_rate = ((1 + gdp_growth/100)*((GGXWDG_NGDP + GGXONLB_NGDP)/(lag(GGXWDG_NGDP))) - 1)*100
  ) %>% 
  gather(key = weo_subject_code, value = outcome, -c("year"))

df_final <- full_join(
  x = df_specific,
  y = df_compute,
  by = c("weo_subject_code","year","outcome")
) %>% 
  mutate(
    units = case_when(
      weo_subject_code == "gdp_growth" ~ "Percent change",
      weo_subject_code == "real_effective_rate" ~ "Percent",
      .default = units
      ),
    scale = case_when(
      weo_subject_code == "gdp_growth" ~ "Units",
      weo_subject_code == "real_effective_rate" ~ "Units",
      .default = scale
    ),
    subject_descriptor = case_when(
      weo_subject_code == "gdp_growth" ~ "GDP growth",
      weo_subject_code == "real_effective_rate" ~ "Real effective interest rate",
      .default = subject_descriptor
    )
  ) %>% 
  fill(weo_country_code, .direction = "down") %>% 
  fill(iso3c, .direction = "down") %>% 
  fill(country_name, .direction = "down") %>% 
  mutate(outcome = round(x = outcome, digits = 2))

# shock analysis ----------------------------------------------------------
df_baseline <- df_final %>% 
  select(weo_subject_code, year, outcome) %>% 
  filter(weo_subject_code %in% c("GGXWDG_NGDP","gdp_growth", "GGXONLB_NGDP", "real_effective_rate")) %>% 
  spread(key = weo_subject_code, value = outcome) %>% 
  mutate(
    gdp_growth = case_when(
      year <= projections_start_after ~ NA, .default = gdp_growth
      ),
    GGXONLB_NGDP = case_when(
      year <= projections_start_after ~ NA, .default = GGXONLB_NGDP
    ),
    real_effective_rate = case_when(
      year <= projections_start_after ~ NA, .default = real_effective_rate
    )
  )

# shocking Primary balance ------------------------------------------------
# shock primary balance
df_shock_pb <- df_baseline %>% 
  mutate(
    shock_pb_by_200_bp = (GGXONLB_NGDP + (200/100)),
    shock_pb_by_300_bp = (GGXONLB_NGDP + (300/100)),
    shock_pb_by_400_bp = (GGXONLB_NGDP + (400/100))
  ) %>% 
  # modelling debt using debt dynamic equation and new shocks
  mutate(
    debt_shock_pb_by_200_bp = (((1+real_effective_rate/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - shock_pb_by_200_bp),
    debt_shock_pb_by_300_bp = (((1+real_effective_rate/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - shock_pb_by_200_bp),
    debt_shock_pb_by_400_bp = (((1+real_effective_rate/100)/(1+gdp_growth/100))*lag(GGXWDG_NGDP) - shock_pb_by_200_bp)
  ) %>% 
  mutate(
    debt_shock_pb_by_200_bp = case_when(
      year == projections_start_after ~ GGXWDG_NGDP, .default =  debt_shock_pb_by_200_bp
    ),
    debt_shock_pb_by_300_bp = case_when(
      year == projections_start_after ~ GGXWDG_NGDP, .default =  debt_shock_pb_by_300_bp
    ),
    debt_shock_pb_by_400_bp = case_when(
      year == projections_start_after ~ GGXWDG_NGDP, .default =  debt_shock_pb_by_400_bp
    )
  ) %>% 
  mutate(across(where(is.numeric) & !all_of("year"), ~ round(., digits = 2)))

# debt projection for primary balance
df_debt_projection_pb <- df_shock_pb %>% 
  select(year, baseline = GGXWDG_NGDP, starts_with("debt"))

# main chart to show the impact: Primary balance shock
df_long <- df_debt_projection_pb %>% 
  gather(key = indicator, value = outcome, -c("year"))

highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(
    categories = unique(df_long$year),
    title = list(text = "Year")
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
  hc_title(text = "") %>%  # Empty title to match original
  hc_tooltip(
    crosshairs = TRUE,
    shared = TRUE
  )

#  plotly 
plot_ly(data = df_long, 
        x = ~year, 
        y = ~outcome, 
        color = ~indicator, 
        type = 'scatter', 
        mode = 'lines+markers',  # This adds both lines and points
        marker = list(size = 8),  # Customize point size
        line = list(width = 1)) %>%  # Customize line width
  plotly::layout(
    xaxis = list(
      title = "Year",
      categoryorder = "array",
      categoryarray = unique(df_long$year)
    ),
    yaxis = list(
      title = "Debt (% of GDP)"
    ),
    legend = list(
      orientation = "h",
      y = 1.1,
      x = 0.5,
      xanchor = "center"
    ),
    hovermode = "x unified"
  )

# First reshape the data (if not already done)
df_long <- df_debt_projection_pb %>% 
  filter(year >= 2023) %>% 
  gather(key = indicator, value = outcome, -c("year"))

# Create the highchart
highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(
    categories = unique(df_long$year),
    title = list(text = "Year")
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
  hc_title(text = "") %>%  # Empty title to match original
  hc_tooltip(
    crosshairs = TRUE,
    shared = TRUE
  )
# shock interest rate -----------------------------------------------------



# ends: -------------------------------------------------------------------


