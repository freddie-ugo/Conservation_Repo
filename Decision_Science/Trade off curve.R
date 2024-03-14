library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Define the function
f <- function(x) 10.8648 * x^0.2

# Create a tibble of x and y values
data <- tibble(x = seq(0, 5000000, length.out = 400)) %>%
  mutate(y = f(x))

# Endpoints
x_start <- 0
x_end <- 5000000
y_start <- f(x_start)
y_end <- f(x_end)

# Function to calculate distance from a point to a line
distance_to_line <- function(x, y, x1, y1, x2, y2) {
  # Line equation coefficients (Ax + By + C = 0)
  A <- y2 - y1
  B <- x1 - x2
  C <- x2 * y1 - x1 * y2
  
  # Distance formula
  abs(A * x + B * y + C) / sqrt(A^2 + B^2)
}

# Calculate distances
data <- data %>%
  mutate(distance = distance_to_line(x, y, x_start, y_start, x_end, y_end))

# Find the point of maximum distance
elbow_point <- data %>%
  arrange(desc(distance)) %>%
  slice(1)

# Plot
ggplot(data, aes(x, y)) + 
  geom_line(color = "blue") +
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end), linetype = "dashed", color = "red") +
  geom_point(data = elbow_point, aes(x = x, y = y), color = "green", size = 4) +
  annotate("text", x = elbow_point$x, y = elbow_point$y, label = paste("Optimal ($", signif(elbow_point$x/1e6, 2), "M, ", round(elbow_point$y, 0), ")", sep=""), vjust = -1) +
  scale_x_continuous(breaks = seq(0, 5000000, by = 1000000), labels = label_number(scale = 1e-6, suffix = "M")) +
  theme_minimal() +
  labs(title = "Investment Curve for Cinnamon Fungus in Eyre and York mallee", x = "Investment (Millions of Dollars)", y = "Species")

