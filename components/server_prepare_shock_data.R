# starts: -----------------------------------------------------------------
server_prepare_shock_data <- function(
    df_policy,
    shock_type,
    start_year = NULL) {
  # a list of shock items
  shock_columns <- list(
    "Policy shock" = c(
      "Baseline",
      "Debt Projection : Policy Shock" = "debt_policy_shock"
    ),
    "Primary balance" = c(
      "Baseline",
      "Debt Projection : Primary Balance Shock" = "debt_PB_shock"
    ),
    "GDP growth" = c(
      "Baseline",
      "Debt Projection : GDP Shock" = "debt_GDP_shock"
    ),
    "Real effective interest rate" = c(
      "Baseline",
      "Debt Projection : Real effective interest rate Shock" =
        "debt_Interest_shock"
    )
  )
  # select key variables
  df <- df_policy %>%
    dplyr::select(year, !!!shock_columns[[shock_type]])
  # filter key observations
  if (!is.null(start_year)) {
    df <- df %>% dplyr::filter(year >= start_year)
  }
  # gather data into long format
  df %>%
    tidyr::gather(key = indicator, value = outcome, -c("year"))
}

# ends: -------------------------------------------------------------------
