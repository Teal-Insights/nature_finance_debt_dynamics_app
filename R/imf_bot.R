
# starts: -----------------------------------------------------------------
# loading necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(imfweo)
})
# loading necessary Rscripts
source(file = "R/imf_countries.R")
source(file = "R/imf_key_data.R")
source(file = "R/imf_format_years.R")

# api call: ---------------------------------------------------------------
df_main <- imf_key_data() %>% 
  filter(iso3c == "THA")


# full data: --------------------------------------------------------------
df_specific <- df_main %>% 
  select(c(weo_country_code, iso3c, country_name, weo_subject_code, 
           subject_descriptor,units, scale, estimates_start_after, year, 
           outcome)) %>% 
  filter(year >= df_main %>% 
           pull(estimates_start_after) %>% 
           unique() %>% 
           max() - 13
  )

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
projections_start_after <- df_main %>% pull(estimates_start_after) %>% max(na.rm = TRUE)
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
  )
# debt projection for primary balance
df_debt_projection_pb <- df_shock_pb %>% 
  select(year, baseline = GGXWDG_NGDP, starts_with("debt"))

# main chart to show the impact: Primary balance shock
df_debt_projection_pb %>% 
  gather(key = indicator, value = outcome, -c("year")) %>% 
  ggplot(aes(x = year, y = outcome, group = indicator, color = indicator)) +
  # Use a clean and polished theme
  theme_minimal(base_size = 14) +
  # Add lines with better visibility
  geom_line(linewidth = 1.2) +
  # Optionally add points for clarity
  geom_point(size = 2) +
  # Adjust x-axis scaling
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(min(df_debt_projection_pb$year), 
                                  max(df_debt_projection_pb$year), 
                                  by = 1)) +
  # Adjust y-axis scaling
  scale_y_continuous(labels = scales::comma) +
  # Customize colors for better distinction
  scale_color_brewer(palette = "Set2") +
  # Add titles and labels
  labs(
    title = "Debt Projection as a Percentage of GDP",
    subtitle = "Projections over the years",
    x = "Year",
    y = "Outcome (%)",
    caption = "Source: Debt Projection Data"
  ) +
  # Enhance legend position and style
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# projection chart to show the impact: Primary balance shock
df_debt_projection_pb %>% 
  filter(year >= 2023) %>% 
  gather(key = indicator, value = outcome, -c("year")) %>% 
  ggplot(aes(x = year, y = outcome, group = indicator, color = indicator))+
  theme_classic()+
  geom_line()+
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

# shock interest rate -----------------------------------------------------



# ends: -------------------------------------------------------------------


