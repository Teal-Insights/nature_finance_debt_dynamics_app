# starts: -----------------------------------------------------------------
# Function to calculate first year debt
calculate_first_year_debt <- function(initial_debt, r, g, pb) {
  ((1 + r / 100) / (1 + g / 100)) * initial_debt - pb
}

# Function to calculate subsequent years debt
calculate_next_year_debt <- function(previous_debt, r, g, pb) {
  ((1 + r / 100) / (1 + g / 100)) * previous_debt - pb
}

# Main function to project debt for all years using individual vectors
server_project_debt <- function(initial_debt, growth_rates, interest_rates,
                                primary_balances) {
  # Get length from any of the input vectors
  n <- length(growth_rates)

  # Check if all inputs have the same length
  if (!all(c(length(interest_rates), length(primary_balances)) == n)) {
    stop("All input vectors must have the same length")
  }

  # Initialize results vector
  projected_debt <- numeric(n)

  # Calculate first year
  projected_debt[1] <- calculate_first_year_debt(
    initial_debt = initial_debt,
    r = interest_rates[1],
    g = growth_rates[1],
    pb = primary_balances[1]
  )

  # Calculate remaining years
  for (i in 2:n) {
    projected_debt[i] <- calculate_next_year_debt(
      previous_debt = projected_debt[i - 1],
      r = interest_rates[i],
      g = growth_rates[i],
      pb = primary_balances[i]
    )
  }

  return(projected_debt)
}
# ends: -------------------------------------------------------------------
