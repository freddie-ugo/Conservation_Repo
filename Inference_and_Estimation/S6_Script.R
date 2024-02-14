library(tidyr)
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

trawl <- read.csv("S6_Prac/pairedtt_trawlprocess.csv")

str(trawl)

# Replace 1 and 2 with strings, then rename them
trawl <- trawl %>%
  mutate(group = recode(as.character(group), '1' = 'altered', '2' = 'manual'))

# Check histogram
trawl %>%
  ggplot(aes(x = extra, fill = group)) +
  geom_histogram(bins = 6) +
  facet_wrap(~group)

#QQ Plot

trawl %>%
  ggplot(aes(sample = extra)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~group)

# Kolmogorov - Smirnow
trawl %>% filter(group == "altered") %>% pull(extra) %>%
  ks.test(x = ., 
          y = "pnorm", #pnorm = normal distribution
          alternative = "two.sided")

trawl %>% filter(group == "manual") %>% pull(extra) %>%
  ks.test(x = ., 
          y = "pnorm", #pnorm = normal distribution
          alternative = "two.sided")

# t test

t.test(x = trawl %>% filter(group == "altered") %>% pull(extra), # X must be greater than Y therefore after has to be first
       y = trawl %>% filter(group == "manual") %>% pull(extra),
       alternative = "greater",
       paired = TRUE,
       conf.level = 0.95) 

# Or do with different formula that likes this data format

t.test(extra ~ group, data = trawl,
       paired = TRUE,
       alternative = "two.sided")


#  ANOVA on BCI data ------------------------------------------------------

library(vegan)
data(BCI)
data(BCI.env)

# Add shannon diversity and species richness
BCI.analysis <- BCI.env %>%
  mutate(shannon = diversity(BCI, index = "shannon"),
         richness = specnumber(BCI))

# Skip normality checking, which we should usually do.

# Do ANOVA test

shanhab.aov <- aov(shannon ~ Habitat, data = BCI.analysis)
summary(shanhab.aov) # Get the ANOVA table
coefficients(shanhab.aov) # See the coefficients (Mean of first category followed by +/- of the other means)

install.packages("ggfortify")
library(ggfortify)

# Gives 4 plots useful for interrogating ANOVA results
autoplot(shanhab.aov)

# Remove 'Young' as it is invalidating our ANOVA test

BCI.analysis.exclYoung <- BCI.analysis %>%
  filter(Habitat != "Young")

shanhab.aov.exclYoung <- aov(shannon ~ Habitat, data = BCI.analysis.exclYoung)
autoplot(shanhab.aov.exclYoung) #Interrogate model BEFORE looking at results

summary(shanhab.aov.exclYoung) # Get the ANOVA table
coefficients(shanhab.aov.exclYoung) # See the coefficients (Mean of first category followed by +/- of the other means)

# Questionable Post Hoc Test
TukeyHSD(shanhab.aov.exclYoung) # Compares each pairwise category


# Wilcoxon Signed Rank ----------------------------------------------------

df <- data.frame(
  before = c(42, 34, 33, 28, 28, 57, 24, 47, 32, 30, 36, 32),
  after = c(24, 27, 18, 27, 11, 26, 0, 32, 10, 18, 28, 12)
)

# qq plot

df %>% 
  pivot_longer(cols = c("before", "after"), 
               names_to = "treatment") %>%
  ggplot(aes(sample = value, col = treatment)) + 
  geom_qq() + 
  stat_qq_line()

#wilcox test

wilcox.test(x = df$after, y = df$before, 
            alternative = "less", 
            conf.level = 0.95, 
            paired = TRUE)

df %>%
  pivot_longer(cols = c("before", "after"), names_to = "treatment") %>%
                 group_by(treatment) %>%
  ggplot(aes(x = treatment, y = value)) +
  geom_boxplot()
               
