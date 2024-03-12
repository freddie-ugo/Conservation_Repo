library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(purrr)

data_3 <- read_excel("Data_3.xlsx", sheet = "Data", skip = 3)

#data_3 <- setNames(data_3, c("common_name","scientific_name",
                             #"benefit", "phy_dis", "cost",
                             #"p_suc", "threat_status", "gap",
                             #"cost_efficiency", "n_cost_efficiency"))

str(data_3)


# Fix phylogenetic distinctiveneeses --------------------------------------

data_3 <- data_3 %>%
  mutate(phy_dis = as.numeric(str_trim(phy_dis)))

data_3 %>% pull(n_cost_efficiency) %>% sort()


# Plot cost efficiency against cost ---------------------------------------

data_3 %>%
  ggplot(aes(x = n_cost_efficiency, y = cost)) +
  geom_point() +
  geom_text(aes(label = common_name), vjust = -0.5, hjust = 1)


# Plot phylogenetic distinctiveness weighted cost efficiency again --------

data_3 %>%
  mutate(phy_dis = as.numeric(str_trim(phy_dis))) %>%
  mutate(dis_weighted = phy_dis*cost_efficiency) %>%
  mutate(n_dis_weighted = dis_weighted/max(dis_weighted)) %>%
  ggplot(aes(x = n_dis_weighted, y = cost)) +
  geom_point()
  #geom_text(aes(label = common_name), vjust = -0.5, hjust = 1)


# Line of best fit --------------------------------------------------------

lm_model <- lm(phy_dis ~ n_cost_efficiency, data = data_3)

data_3 %>%
  ggplot(aes(x = n_cost_efficiency, y = phy_dis)) +
  geom_point() +
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2]) +
  geom_text(aes(label = common_name), vjust = -0.5, hjust =1)

data_3 %>%
  ggplot(aes(x = n_ce_pd, y = cost)) +
  geom_point()

chosen_data <- data_3 %>%
  arrange(desc(n_ce_pd)) %>%
  mutate(cumulative_sum = cumsum(cost)) %>%
  mutate(chosen = ifelse(row_number() <= 8, common_name, NA))

chosen_data %>%
  ggplot(aes(x = n_ce_pd, y = cost)) +
  geom_point() +
  geom_text(aes(label = chosen), vjust = -0.5, hjust =1)


# Choose based on phy_dis weighted eff, skipping values that would exceed 20m --------

critical_number <- 20000000

# Sort the data frame by cost in descending order
data_3_ranked <- data_3 %>% arrange(desc(n_ce_pd))

# Initialize variables
cumulative_sum <- 0
chosen_names <- character(0)

# Iterate through rows
for (i in 1:nrow(data_3_ranked)) {
  if (cumulative_sum + data_3_ranked$cost[i] <= critical_number) {
    cumulative_sum <- cumulative_sum + data_3_ranked$cost[i]
    chosen_names <- c(chosen_names, data_3_ranked$common_name[i])
  } else {
    next
  }
}

# Create a new column in the original data frame
data_3_ranked$chosen_column <- ifelse(data_3_ranked$common_name %in% chosen_names, data_3_ranked$common_name, NA_character_)


# Just cost_effectiveness ranked ------------------------------------------

data_3_ce_ranked <- data_3 %>% arrange(desc(cost_efficiency))

cumulative_sum_2 <- 0
chosen_names_2 <- character(0)

for (i in 1:nrow(data_3_ce_ranked)) {
  if (cumulative_sum_2 + data_3_ce_ranked$cost[i] <= critical_number) {
    cumulative_sum_2 <- cumulative_sum_2 + data_3_ce_ranked$cost[i]
    chosen_names_2 <- c(chosen_names_2, data_3_ce_ranked$common_name[i])
  } else {
    next
  }
}

data_3_ce_ranked$chosen_column <- ifelse(data_3_ce_ranked$common_name %in% chosen_names_2, data_3_ce_ranked$common_name, NA_character_)

