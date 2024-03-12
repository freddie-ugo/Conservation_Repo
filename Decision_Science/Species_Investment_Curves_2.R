library(tidyverse)

# New coefficients 'a' for each scenario
a_values <- c(EY_Predator = 28.54242304525335,
              MLW_Predator = 33.63630423682513,
              EY_Fungus = 10.86482002769821,
              MLW_Fungus = 13.79866858264879,
              EY_Reveg = 9.142835376180896,
              MLW_Reveg = 13.124199312570493)

# Investment required for the area needing action
investment_required <- c(EY_Predator = 1893919,
                         MLW_Predator = 857850,
                         EY_Fungus = 6580455,
                         MLW_Fungus = 2155336,
                         EY_Reveg = 1312359,
                         MLW_Reveg = 244982)

# Maximum values of S for each scenario
max_S_values <- c(EY_Predator = 514,
                  MLW_Predator = 517,
                  EY_Fungus = 251,
                  MLW_Fungus = 255,
                  EY_Reveg = 153,
                  MLW_Reveg = 157)

max_investment = 5e6
# Define the range of investments
investment_values <- seq(0, max(max_investment), by = 1e4)  # From $0 to max investment

# z value set to 0.2
z_fixed <- 0.2

# Create a dataframe for plotting
plot_data <- expand.grid(Investment = investment_values, 
                         Scenario = names(a_values), 
                         z = z_fixed) %>%
  mutate(S = pmin(a_values[Scenario] * Investment ^ z, max_S_values[Scenario]))

# Plotting
ggplot(plot_data, aes(x = Investment, y = S, color = Scenario)) +
  geom_line() +
  geom_vline(xintercept = investment_required, linetype = "dashed", color = "black") +
  labs(title = "Species-Investment Curves with Revised a Values for Required Action (z = 0.2)",
       x = "Investment ($)",
       y = "Number of Assets Protected (S)",
       color = "Intervention and Ecoregion") +
  theme_minimal()
