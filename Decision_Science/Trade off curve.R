library(ggplot2)
library(tidyr)
library(dplyr)

# Define the original function
f <- function(x) 10.8648 * x^0.2

# Endpoints
x_start <- 0
x_end <- 5000000
y_start <- f(x_start)
y_end <- f(x_end)

# Function to calculate distance from a point to a line
distance_to_line <- function(x, x1, y1, x2, y2) {
  # Line equation coefficients (Ax + By + C = 0)
  A <- y2 - y1
  B <- x1 - x2
  C <- x2*y1 - x1*y2
  
  # Distance formula
  abs(A * x + B * f(x) + C) / sqrt(A^2 + B^2)
}

# Find the point of maximum distance
optimize(function(x) distance_to_line(x, x_start, y_start, x_end, y_end), 
         interval = c(0, 5000000), 
         maximum = TRUE)
