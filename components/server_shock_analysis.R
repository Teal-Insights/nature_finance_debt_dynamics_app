
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
  df_full_join <- full_join(
    x = df_baseline %>% mutate(year = as.integer(year)),
    y = shock_values %>% mutate(year = as.integer(year)),
    by = "year"
  ) 
  
  # initial debt before projection starts
  initial_debt <- df_full_join %>% 
    filter(year == projections_start_in) %>% 
    pull(GGXWDG_NGDP)
  
  # Get projections
  df_dp <- df_full_join %>% filter(year >= year_when_estimations_start)
  
  # projections
  debt_policy_shock = server_project_debt(
    initial_debt = initial_debt,
    growth_rates = df_dp$gdp_shock,
    interest_rates = df_dp$ir_shock,
    primary_balances = df_dp$pb_shock
  )
  
  debt_PB_shock = server_project_debt(
    initial_debt = initial_debt,
    growth_rates = df_dp$gdp_growth,
    interest_rates = df_dp$real_effective_rate,
    primary_balances = df_dp$pb_shock
  )
  
  debt_Interest_shock = server_project_debt(
    initial_debt = initial_debt,
    growth_rates = df_dp$gdp_growth,
    interest_rates = df_dp$ir_shock,
    primary_balances = df_dp$GGXONLB_NGDP
  )
  
  debt_GDP_shock = server_project_debt(
    initial_debt = initial_debt,
    growth_rates = df_dp$gdp_shock,
    interest_rates = df_dp$real_effective_rate,
    primary_balances = df_dp$GGXONLB_NGDP
  )
  # result
  result <- data.frame(
    year = shock_values$year,
    debt_PB_shock = debt_PB_shock,
    debt_Interest_shock = debt_Interest_shock,
    debt_GDP_shock = debt_GDP_shock,
    debt_policy_shock = debt_policy_shock
  )
  
  df_full_join %>%
    left_join(y = result,by = "year") %>% 
    mutate(
      across(
        .cols = starts_with("debt_") & ends_with("_shock"),
        .fns = ~case_when(
          year == projections_start_in ~ GGXWDG_NGDP,
          .default = .x
        )
      )
    ) %>% 
    # mutate(across(where(is.numeric) & !all_of("year"), ~ round(., digits = 2))) %>%
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


