
# starts ------------------------------------------------------------------
# Function to create baseline data
create_baseline_data <- function(df_main, df_specific, year_when_estimations_start) {
  projections_start_in <- year_when_estimations_start
  
  df_specific %>% 
    select(weo_subject_code, year, outcome) %>% 
    filter(weo_subject_code %in% c("GGXWDG_NGDP", "gdp_growth", "GGXONLB_NGDP", "real_effective_rate")) %>% 
    spread(key = weo_subject_code, value = outcome) %>% 
    mutate(
      gdp_growth = case_when(
        year < projections_start_in ~ NA_real_, 
        .default = gdp_growth
      ),
      GGXONLB_NGDP = case_when(
        year < projections_start_in ~ NA_real_, 
        .default = GGXONLB_NGDP
      ),
      real_effective_rate = case_when(
        year < projections_start_in ~ NA_real_, 
        .default = real_effective_rate
      )
    )
}

# Function to perform policy shock analysis
analyze_policy_shock <- function(df_baseline, shock_values, year_when_estimations_start) {
  # Extract projections_start_in
  projections_start_in <- year_when_estimations_start - 1
  
  # Join and mutate
  full_join(
    x = df_baseline %>% mutate(year = as.integer(year)),
    y = shock_values %>% mutate(year = as.integer(year)),
    by = "year"
  ) %>%
    mutate(
      debt_PB_shock = (((1 + real_effective_rate / 100) / (1 + gdp_growth / 100)) * lag(GGXWDG_NGDP) - pb_shock),
      debt_Interest_shock = (((1 + ir_shock / 100) / (1 + gdp_growth / 100)) * lag(GGXWDG_NGDP) - GGXONLB_NGDP),
      debt_GDP_shock = (((1 + real_effective_rate / 100) / (1 + gdp_shock / 100)) * lag(GGXWDG_NGDP) - GGXONLB_NGDP),
      debt_policy_shock = (((1 + ir_shock / 100) / (1 + gdp_shock / 100)) * lag(GGXWDG_NGDP) - pb_shock)
    ) %>% 
    mutate(
      debt_PB_shock = case_when(
        year == projections_start_in ~ GGXWDG_NGDP, 
        .default = debt_PB_shock
      ),
      debt_Interest_shock = case_when(
        year == projections_start_in ~ GGXWDG_NGDP, 
        .default = debt_Interest_shock
      ),
      debt_GDP_shock = case_when(
        year == projections_start_in ~ GGXWDG_NGDP, 
        .default = debt_GDP_shock
      ),
      debt_policy_shock = case_when(
        year == projections_start_in ~ GGXWDG_NGDP, 
        .default = debt_policy_shock
      )
    ) %>% 
    mutate(across(where(is.numeric) & !all_of("year"), ~ round(., digits = 2))) %>% 
    rename(
      Baseline = "GGXWDG_NGDP",
      "GDP growth" = "gdp_growth",
      "GDP growth shock" = "gdp_shock",
      "Primary balance (% of GDP)" = "GGXONLB_NGDP",
      "Primary balance (% of GDP) shock" = "pb_shock",
      "Real effective interest rate" = "real_effective_rate",
      "Real effective interest rate shock" = "ir_shock"
    )
}

# ends --------------------------------------------------------------------


