library(tidyverse)

# Define the coefficients 'a' for each scenario
a_values <- c(EY_Predator = 32.43120750628193,
              MLW_Predator = 32.62049470962599,
              EY_Fungus = 15.83702934645285,
              MLW_Fungus = 16.089412284244926,
              EY_Reveg = 9.653647370546956,
              MLW_Reveg = 9.906030308339034)

# Initial investments for each scenario
initial_investments <- data.frame(
  Scenario = c("EY_Predator", "MLW_Predator", "EY_Fungus", "MLW_Fungus", "EY_Reveg", "MLW_Reveg"),
  Investment = c(569079, 1429750, 0, 0, 294944, 84668)
)

# Define the range of investments
investment_values <- seq(0, 5e6, by = 1e5)  # From $0 to $5 million in $100,000 steps

# Define the range of 'z' values
z_values <- seq(0.1, 0.3, by = 0.05)

# Create a dataframe for plotting
plot_data <- expand.grid(Investment = investment_values, 
                         Scenario = names(a_values), 
                         z = z_values) %>%
  mutate(S = a_values[Scenario] * Investment ^ z)

# Plotting
ggplot(plot_data, aes(x = Investment, y = S, color = Scenario, group = interaction(Scenario, z))) +
  geom_line() +
  geom_vline(data = initial_investments, aes(xintercept = Investment), linetype = "dashed") +
  facet_wrap(~ z, scales = "free_y") +
  labs(title = "Species-Investment Curves with Initial Investments",
       x = "Investment ($)",
       y = "Number of Assets Protected (S)",
       color = "Intervention and Ecoregion") +
  theme_minimal()
