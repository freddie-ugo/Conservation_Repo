# Load Libraries ----------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)


# Wood Extraction ---------------------------------------------------------

woodex <- read_excel("S5_Prac/S5_data.xlsx", sheet = "woodextraction")

# Check the structure
str(woodex)

# Are there any NAs?
woodex %>%
  pull(wood) %>% is.na() %>% any()

# Find mean
m_woodex <- mean(woodex$wood)

# Plot a historgram
woodex %>%
  ggplot(aes(x = wood)) +
  geom_histogram(binwidth = 30, col = "black") +
  geom_vline(aes(xintercept= m_woodex, colour = m_woodex))

# QQ plot
woodex %>%
  ggplot(aes(sample = wood)) +
  geom_qq() +
  geom_qq_line()

# Kolmogorov-Smirnov test
ks.test(x = woodex %>% pull(wood), 
        y = "pnorm", #pnorm = normal distribution
        alternative = "two.sided")

# Do a t-test

t.test(x = woodex %>% pull(wood), 
       alternative = "greater", 
       mu = 300) 

# Get the two-way confidence interval

t.test(x = woodex %>% pull(wood))$conf.int
t.test(x = woodex %>% pull(wood), conf.level = 0.99)$conf.int


# Recycling ---------------------------------------------------------------

rec <- read_excel("S5_Prac/S5_data.xlsx", sheet = "recycling")

str(rec)

# Check for NAs
rec %>%
  mutate(across(everything(), is.na)) %>%
  any()

# Histograms

# 2 Panels (best as we only care if each sample is normal in and of itself)
rec %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
  ggplot(aes(x = value, fill = treatment)) +
  geom_histogram(bins = 10) +
  facet_wrap(~treatment)

# Next to each other (weird for histogram as two bins should be overlapping)
rec %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
  ggplot(aes(x = value, fill = treatment)) +
  geom_histogram(bins = 10, position = position_dodge())

# Position identity shows overlapping
rec %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
  ggplot(aes(x = value, fill = treatment)) +
  geom_histogram(bins = 10, position = position_identity())


# Make transparent to avoid overlapping
rec %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
  ggplot(aes(x = value, fill = treatment)) +
  geom_histogram(bins = 10, position = position_identity(), alpha = 0.3)

# QQ Plot

rec %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
  ggplot(aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~treatment)

# Do a ks.test if you like!

# Check variances are equal?

rec %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
  group_by(treatment) %>%
  summarise(sd = sd(value))

# T test

t.test(x = rec %>% pull(after), # X must be greater than Y therefore after has to be first
       y = rec %>% pull(before),
       alternative = "greater",
       paired = TRUE,
       conf.level = 0.95) 


# Pollinators -------------------------------------------------------------

biomass <- read_excel("S5_Prac/S5_data.xlsx", sheet = "biomass")

str(biomass)

biomass %>%
  pivot_longer(cols = c("control", "treatment"), names_to = "set") %>%
  ggplot(aes(x = value, fill = set)) +
  geom_histogram(bins = 15) +
  facet_wrap(~set)

biomass %>%
  pivot_longer(cols = c("control", "treatment"), names_to = "set") %>%
  ggplot(aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~set)

#ks test
ks.test(x = biomass %>% pull(control), 
        y = "pnorm", #pnorm = normal distribution
        alternative = "two.sided")

ks.test(x = biomass %>% pull(treatment), 
        y = "pnorm", #pnorm = normal distribution
        alternative = "two.sided")

# Same variance?

biomass %>%
  pivot_longer(cols = c("control", "treatment"), names_to = "set") %>%
  group_by(set) %>%
  summarise(sd = sd(value))

#T test
#Two sided
t.test(x = biomass %>% pull(treatment),
       y = biomass %>% pull(control),
       alternative = "two.sided",
       paired = FALSE,
       var.equal = TRUE,
       conf.level = 0.95) 

#One Sided
t.test(x = biomass %>% pull(treatment),
       y = biomass %>% pull(control),
       alternative = "greater",
       var.equal = TRUE,
       paired = FALSE,
       conf.level = 0.95)


# Binomial Test -----------------------------------------------------------

binom.test(x = 16, n = 40, p = 0.2,
           alternative = "greater")


