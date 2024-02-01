library(tidyr)
library(dplyr)
library(stringr)
#install.packages("readxl")
library(readxl)
#install.packages("ggplot2")
library(ggplot2)


# Load Data ---------------------------------------------------------------

dbdata <- read_excel("S3_Prac/dbanalysis.xlsx", sheet = "dbdata")
habitat <- read_excel("S3_Prac/dbanalysis.xlsx", sheet = "habitat")
remotesense <- read_excel("S3_Prac/dbanalysis.xlsx", sheet = "remotesense")


# Fix UTM in habitat ------------------------------------------------------

str(habitat)
str(remotesense)

habitat <- habitat %>%
  mutate(UTM_X = as.numeric(UTM_X),
         UTM_Y = as.numeric(UTM_Y))


# Merging Data ------------------------------------------------------------

main <- left_join(dbdata, habitat, by = c("site" = "site")) %>%
  left_join(remotesense, by = c("UTM_X" = "UTM_X", "UTM_Y" = "UTM_Y"))


# How does aspect affect the number of Copris nubilosus? ------------------

main %>%
  filter(taxon == "Copris nubilosus") %>%
  ggplot(aes(x = aspect, y = count,)) +
  geom_point(shape = 1, col = "green")

# How does elevation affect the abundance of Dichotomius species?
#Separate by colour
main %>%
  filter(str_detect(taxon, "Dichotomius")) %>%
  ggplot(aes(x = dem, y = count, colour = taxon)) +
  geom_point()

#Separate by panel
main %>%
  filter(str_detect(taxon, "Dichotomius")) %>%
  ggplot(aes(x = dem, y = count, colour = taxon)) +
  geom_point() +
  facet_wrap(~taxon)

#Separate by shape
main %>%
  filter(str_detect(taxon, "Dichotomius")) %>%
  ggplot(aes(x = dem, y = count, shape = taxon)) +
  geom_point()


# Line Plot (will look ugly) ----------------------------------------------

main %>%
  filter(str_detect(taxon, "Dichotomius")) %>%
  ggplot(aes(x = dem, y = count, colour = taxon)) +
  geom_point() +
  geom_line()


# Bar Chart ---------------------------------------------------------------

# How many individuals for each species?

main %>% 
  group_by(taxon) %>%
  summarise(total = sum(count)) %>%
  ggplot(aes(x = taxon, y = total)) +
  geom_col() +
  coord_flip()


# Histogram ---------------------------------------------------------------

# What is the distribution of the counts of Eurysternus magnus
main %>%
  filter(taxon == "Eurysternus magnus") %>%
  ggplot(aes(x = count)) +
  geom_histogram(bins = 50, fill = "yellow") +
  labs(y = "frequency")


# Session 4 starts --------------------------------------------------------

#What is the mean species richness for each habitat type?

# Check what our sample units are
main %>%
  select(collectdate, site) %>%
  unique() %>%
  arrange(site)

# Combine site and collect date to make unique sample units
# Make a boxplot
main %>%
  group_by(collectdate, site, main_forest_type) %>% # each group is a sample unit
  summarise(richness = sum(count > 0)) %>%
  ungroup() %>% # ungroup afterwards to make it easier to work with after
  ggplot(aes(x = main_forest_type, y = richness)) +
  geom_boxplot()

# Mean and standard deviation

main %>%
  group_by(collectdate, site, main_forest_type) %>% # each group is a sample unit
  summarise(richness = sum(count > 0)) %>%
  ungroup() %>%
  group_by(main_forest_type) %>%
  summarise(mean = mean(richness),
            sd = sd(richness)) %>%
  ungroup() %>%
  ggplot(aes( x = main_forest_type, y = mean,
              ymin = mean - sd, ymax = mean + sd)) +
  geom_point() +
  geom_errorbar()
