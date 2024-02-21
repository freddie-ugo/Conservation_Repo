library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
install.packages("dgof")
library(dgof)

# Load the data
suit <- read.csv("S7_Prac/suitability.csv")

# Hypothesis 1: Increasing tree density improves environmental conditions for a rare orchid


# Check normality

suit %>% 
  ggplot(aes(sample = temperature)) + 
  geom_qq() + 
  stat_qq_line()

suit$temperature %>% ks.test(x = ., 
                 y = "pnorm", #pnorm = normal distribution
                 alternative = "two.sided")

suit$humidity %>% ks.test(x = ., 
                          y = "pnorm", #pnorm = normal distribution
                          alternative = "two.sided")

# Pearson's Correlation
suit %>%
  select(temperature, humidity) %>%
  cor(method = "pearson")

#or if just want a single correlation
cor(suit$temperature, suit$humidity, method = "pearson")

#only know it is meaningful with a staistical test...
#Variables fit assumptions of parametric tess, so can use a correlation test for significance.
cor.test(suit$temperature, suit$humidity, method = "pearson")

# Test our hypothesis by fitting a linear model (REGRESSION):

tempdens.lm <- lm(temperature ~ treedensity, data = suit)
anova(tempdens.lm) # Takes linear model and stick into anova to run the F test. aov would only calculate the sum of squares, and not run the statistical test.
summary(tempdens.lm) # Gives coefficients (intercept and slope). R squared tells you how much of dataset is explained.

autoplot(tempdens.lm) # Scale-location tests for homogeniety of variance. 

# Plot with line of best fit
suit %>%
  ggplot(aes(x = treedensity, y =temperature)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) # Quick way that only works with this example. Often need to do it more rigorous way.

#Rigorous way
tempdens.lm.coef <- coefficients(tempdens.lm)

suit %>%
  ggplot(aes(x = treedensity, y =temperature)) +
  geom_point() +
  geom_abline(intercept = tempdens.lm.coef[1], slope = tempdens.lm.coef[2], colour = "red")


  
# Hypothesis 2: Increasing tree density is associated with increasing orchid observations

# Check normality
suit %>%
  ggplot(aes(sample = orchidobs))+
  geom_qq() +
  stat_qq_line()

# Not normal, so cannot do a parametric test
# Run a Spearman's correlation

cor(suit$treedensity, suit$orchidobs, method = "spearman")

cor.test(suit$treedensity, suit$orchidobs, method = "spearman")


# Own work ----------------------------------------------------------------

# 1. Calculate and test the  correlation between treedensity and humidity

suit$treedensity %>% ks.test(x = ., 
                          y = "pnorm",
                          alternative = "two.sided")

suit$humidity %>% ks.test(x = ., 
                          y = "pnorm",
                          alternative = "two.sided")

suit %>%
  ggplot(aes(sample = treedensity))+
  geom_qq() +
  stat_qq_line()

# Not normal, so spearmans:

suit %>%
  select(treedensity, humidity) %>%
  cor(method = "spearman")

cor.test(suit$treedensity, suit$humidity, method = "spearman")

suit %>%
  ggplot(aes(x = treedensity, y = humidity)) +
  geom_point()


# 2. Fit and plot a linear model testing if treedensity affects humidity

humdens.lm <- lm(humidity ~ treedensity, data = suit)
anova(humdens.lm) 
summary(humdens.lm)

autoplot(humdens.lm) 

# Plot with line of best fit
humdens.lm.coef <- coefficients(humdens.lm)

suit %>%
  ggplot(aes(x = treedensity, y = humidity)) +
  geom_point() +
  geom_abline(intercept = humdens.lm.coef[1], slope = humdens.lm.coef[2], colour = "red")
