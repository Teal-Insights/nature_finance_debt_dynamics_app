# starts: -----------------------------------------------------------------
server_prepare_shock_data <- function(
    df_policy,
    shock_type,
    start_year = NULL) {
  shock_columns <- list(
    "Policy shock" = c(
      "Baseline",
      "Debt Projection : Policy Shock" = "debt_policy_shock"
    ),
    "Primary balance" = c(
      "Baseline",
      "Debt Projection : Primary balance Shock" = "debt_PB_shock"
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

  df <- df_policy %>%
    select(year, !!!shock_columns[[shock_type]])

  if (!is.null(start_year)) {
    df <- df %>% filter(year >= start_year)
  }

  df %>%
    gather(key = indicator, value = outcome, -c("year"))
}

# ends: -------------------------------------------------------------------
