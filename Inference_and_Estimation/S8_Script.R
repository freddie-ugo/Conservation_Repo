library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(dgof)

suit <- read.csv("S7_Prac/suitability.csv")

# Check they are Normal
suit %>%
  ggplot(aes(sample = temperature)) +
  geom_qq() +
  geom_qq_line()

suit %>%
  ggplot(aes(sample = humidity)) +
  geom_qq() +
  geom_qq_line()

# Test Correlation

suit %>%
  select(temperature, humidity) %>%
  cor(method = "pearson")

cor.test(suit$temperature, suit$humidity, method = "pearson")

# Run some GLMs

glm.temphum <- glm(yield ~ temperature + humidity, data = suit)

anova(glm.temphum, test = "F")

glm.humtemp <- glm(yield ~ humidity + temperature, data = suit)

anova(glm.humtemp, test = "F")

summary(glm.temphum)
summary(glm.humtemp)

# Plots
# Two individual linear models. Give wrong intercept and slope for a glm.
suit %>%
  ggplot(aes(x = temperature, y = yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

suit %>%
  ggplot(aes(x = humidity, y = yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

autoplot(glm.temphum) # Test validity of model

# But, GLM model fits an overall pattern of yield given humidity and temperature together:



# Hypothesis: more orchid species in primary forest -------------------------------------------------------------------------
# foresttype:categorical
# richness: continuous

suit %>%
  ggplot(aes(x = richness)) +
  geom_histogram(bins = 10)


suit %>%
  ggplot(aes(sample = richness)) +
  geom_qq() +
  geom_qq_line()

# Count data not normally distributed. # Do transformations to approximate normal. Then back-transform.

# Forest type:
glm.richfor <- glm(richness ~ foresttype, data = suit)

autoplot(glm.richfor)
#STOP: this is not normally distributed.
#Let's try transforming manually.

glm.richfor <- glm(log(richness) ~ foresttype, data = suit)
autoplot(glm.richfor)
summary(glm.richfor)
suit %>%
  ggplot(aes(x= foresttype, y = richness)) +
  geom_violin()

# Let's let GLM take care of it for us
glm.richfor <- glm(richness ~ foresttype, data = suit, family = "poisson")
autoplot(glm.richfor)
summary(glm.richfor)


# Hypothesis: Orchid presence is affected by herbivore species and soil organic matter
# Response: orchidpa
# Explanatory 1: species
# Explanatory 2: soilom

# 0th: check data is in format we want, including structure.
# First: Check response is normal, check correlation of explanatory variables
# Second: Set threshold p value for deciding significance. Plan order of terms in model. Re-run with multiple orders?
# Third: Run our test
# Fourth: Check model criticism (autoplot)
# Fifth: Check the anova table
# Sixth: Plot, Check the summary (coefficients)

#0.
str(suit)

#1.
suit %>%
  ggplot(aes(sample = orchidpa)) +
  geom_qq() +
  geom_qq_line()

# Species explanatory data is categorical so we would not do a correlation test.# Test for association anyway:
t.test(soilom ~ species, data = suit)

#2.
# p threshold = 0.01. Do not know enough to decide which order we want. Rule of thumb: categorical then continuous. Do not re run as not correlated.
# Spec then soilom

#3.
glm.specom <- glm(orchidpa ~ species + soilom, data = suit, family = "binomial")

#4.
autoplot(glm.specom)

#5.
anova(glm.specom, test = "F")

#6.
summary(glm.specom)

value <- -0.90356
exp(value)/(1+exp(value))

glm.specom.predict <- expand_grid(
  species = c("A", "B"),
  soilom = seq(from = 0, to = 40, by = 0.5)
)

glm.specom.predictobj <- predict(glm.specom, glm.specom.predict, type = "response", se.fit = T) # type = response back transforms for us

glm.specom.predict <- glm.specom.predict %>%
  mutate(orchidpa = glm.specom.predictobj$fit,
         orchidpa.se = glm.specom.predictobj$se.fit)

ggplot(mapping = aes(x = soilom, y = orchidpa, col = species)) +
  geom_point(data = suit) +
  geom_line(data = glm.specom.predict) +
  geom_ribbon(data = glm.specom.predict,
              mapping = aes(ymin = orchidpa - orchidpa.se,
                            ymax = orchidpa+orchidpa.se,
                            fill = species),
              col = NA, alpha = 0.4) +
  facet_wrap( ~ species)
# Above was an ANCOVA

# Interactions:
# Hypothesis Temperature decreases with tree density, but the strength of the relationship wil vary with forest type.

glm.int.typden <- glm(temperature ~ foresttype * treedensity, data = suit)
autoplot(glm.int.typden)

anova(glm.int.typden, test = "F") # Can only interpret the interaction in the anova table when fitting for interaction!!!
summary(glm.int.typden) # Must only interpret coefficients of main effects if interactions are significant!!!

suit %>%
  ggplot(aes(x = treedensity, y = temperature, col = foresttype)) +
  geom_point()

# Create dummy dataframe
glm.inttypden.predict <- expand_grid(foresttype = suit$foresttype %>% unique, 
            treedensity = seq(from = 0, to = 0.6, 0.02)) # Resolution of prediction changes standard error view

# Create a prediction object containing our "fit" values (predictions) and 
# a standard error of each of these fit values
glm.inttypden.predictobj <- predict(glm.int.typden, glm.inttypden.predict, se.fit = T)

# Bring these two valyes into our prediction dataframe
glm.inttypden.predict <- glm.inttypden.predict %>%
  mutate(temperature = glm.inttypden.predictobj$fit, se = glm.inttypden.predictobj$se.fit)

# Add these to our plot!! More manual way
ggplot(mapping = aes(x = treedensity, y = temperature, col = foresttype)) + # Note the mapping = aes
  geom_point(data = suit) + # Because both data frames have same column names, only need to specify data, not x and y again.
  geom_line(data = glm.inttypden.predict) +
  geom_ribbon(data = glm.inttypden.predict, mapping = aes(ymin = temperature - se, ymax = temperature + se, fill = foresttype), 
              col = NA, alpha = 0.4)
