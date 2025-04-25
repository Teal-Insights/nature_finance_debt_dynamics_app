# starts ------------------------------------------------------------------
# Function to create baseline data
create_baseline_data <- function(data_specific, year) {
  projections_start_in <- year

  data_specific %>%
    dplyr::select(weo_subject_code, year, outcome) %>%
    dplyr::filter(weo_subject_code %in% c(
      "GGXWDG_NGDP", "NGDP_RPCH", "GGXONLB_NGDP",
      "real_effective_rate"
    )) %>%
    tidyr::spread(key = weo_subject_code, value = outcome) %>%
    dplyr::mutate(
      NGDP_RPCH = dplyr::case_when(
        year < projections_start_in ~ NA_real_,
        .default = NGDP_RPCH
      ),
      GGXONLB_NGDP = dplyr::case_when(
        year < projections_start_in ~ NA_real_,
        .default = GGXONLB_NGDP
      ),
      real_effective_rate = dplyr::case_when(
        year < projections_start_in ~ NA_real_,
        .default = real_effective_rate
      )
    )
}

# Function to perform policy shock analysis
analyze_policy_shock <- function(data_baseline, shock_values,
                                 year_when_estimations_start) {
  # Extract projections_start_in
  projections_start_in <- year_when_estimations_start - 1
  
  # Join and mutate
  df_full_join <- dplyr::full_join(
    x = data_baseline %>% dplyr::mutate(year = as.integer(year)),
    y = shock_values %>% dplyr::mutate(year = as.integer(year)),
    by = "year"
  )
  
  # initial debt before projection starts
  initial_debt <- df_full_join %>%
    dplyr::filter(year == projections_start_in) %>%
    dplyr::pull(GGXWDG_NGDP)
  
  # Get projections
  df_dp <- df_full_join %>% dplyr::filter(year >= year_when_estimations_start)
  
  # projections
  debt_policy_shock <- server_project_debt(
    initial_debt = initial_debt,
    growth_rates = df_dp$gdp_shock,
    interest_rates = df_dp$ir_shock,
    primary_balances = df_dp$pb_shock
  )
  
  # debt_pb_shock <- server_project_debt(
  #   initial_debt = initial_debt,
  #   growth_rates = df_dp$NGDP_RPCH,
  #   interest_rates = df_dp$real_effective_rate,
  #   primary_balances = df_dp$pb_shock
  # )
  # 
  # debt_interest_shock <- server_project_debt(
  #   initial_debt = initial_debt,
  #   growth_rates = df_dp$NGDP_RPCH,
  #   interest_rates = df_dp$ir_shock,
  #   primary_balances = df_dp$GGXONLB_NGDP
  # )
  # 
  # debt_gdp_shock <- server_project_debt(
  #   initial_debt = initial_debt,
  #   growth_rates = df_dp$gdp_shock,
  #   interest_rates = df_dp$real_effective_rate,
  #   primary_balances = df_dp$GGXONLB_NGDP
  # )
  # result
  result <- data.frame(
    year = shock_values$year,
    # debt_pb_shock = debt_pb_shock,
    # debt_interest_shock = debt_interest_shock,
    # debt_gdp_shock = debt_gdp_shock,
    debt_policy_shock = debt_policy_shock
  )
  
  df_full_join %>%
    dplyr::left_join(y = result, by = "year") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("debt_") & ends_with("_shock"),
        .fns = ~ dplyr::case_when(
          year == projections_start_in ~ GGXWDG_NGDP,
          .default = .x
        )
      )
    ) %>%
    dplyr::rename(
      Baseline = "GGXWDG_NGDP",
      "GDP growth" = "NGDP_RPCH",
      "GDP growth shock" = "gdp_shock",
      "Primary balance (% of GDP)" = "GGXONLB_NGDP",
      "Primary balance (% of GDP) shock" = "pb_shock",
      "Real effective interest rate" = "real_effective_rate",
      "Real effective interest rate shock" = "ir_shock"
    )
}

# ends --------------------------------------------------------------------
