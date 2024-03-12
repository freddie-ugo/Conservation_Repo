library(tidyverse)

# Updated coefficients 'a' for each scenario with total investment
a_values <- c(EY_Predator = 25.956915089091986,
              MLW_Predator = 28.43370714094852,
              EY_Fungus = 10.86482002769821,
              MLW_Fungus = 13.79866858264879,
              EY_Reveg = 7.374006403670363,
              MLW_Reveg = 11.225688499589518)

# Total investment for each scenario (existing + new)
total_investment <- c(EY_Predator = 3044740,
                      MLW_Predator = 1987400,
                      EY_Fungus = 6580455,
                      MLW_Fungus = 2155336,
                      EY_Reveg = 3845383,
                      MLW_Reveg = 535098)

# Initial investment for each scenario (already existing)
initial_investment <- c(EY_Predator = 1150821,
                        MLW_Predator = 1129550,
                        EY_Fungus = 0,
                        MLW_Fungus = 0,
                        EY_Reveg = 2533024,
                        MLW_Reveg = 290116)

# Maximum values of S for each scenario
max_S_values <- c(EY_Predator = 514,
                  MLW_Predator = 517,
                  EY_Fungus = 251,
                  MLW_Fungus = 255,
                  EY_Reveg = 153,
                  MLW_Reveg = 157)

# Define the range of investments
investment_values <- seq(0, max(total_investment), by = 1e4)  # From $0 to max total investment

# z value set to 0.2
z_fixed <- 0.2

# Create a dataframe for plotting
plot_data <- expand.grid(Investment = investment_values, 
                         Scenario = names(a_values), 
                         z = z_fixed) %>%
  mutate(S = pmin(a_values[Scenario] * Investment ^ z, max_S_values[Scenario]))

# Create a dataframe for the initial investments points
initial_investment_points <- data.frame(
  Scenario = names(initial_investment),
  Investment = initial_investment,
  S = sapply(names(initial_investment), function(name) a_values[name] * initial_investment[name] ^ z_fixed)
)

# Plotting
ggplot(plot_data, aes(x = Investment, y = S, color = Scenario)) +
  geom_line() +
  geom_point(data = initial_investment_points, aes(x = Investment, y = S), 
             shape = 4,  # Shape 4 is a cross
             size = 5,  # Adjust the size of the cross here
             stroke = 3) +
  labs(title = "Species-Investment Curves with Total Investment (z = 0.2)",
       x = "Investment ($)",
       y = "Number of Assets Protected (S)",
       color = "Intervention and Ecoregion") +
  theme_minimal()
