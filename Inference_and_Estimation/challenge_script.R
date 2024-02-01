library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)

# Load Data ---------------------------------------------------------------

dbdata <- read_excel("S3_Prac/dbanalysis.xlsx", sheet = "dbdata")
habitat <- read_excel("S3_Prac/dbanalysis.xlsx", sheet = "habitat")
remotesense <- read_excel("S3_Prac/dbanalysis.xlsx", sheet = "remotesense")

habitat <- habitat %>%
  mutate(UTM_X = as.numeric(UTM_X),
         UTM_Y = as.numeric(UTM_Y))

str(habitat)


# Q1 ----------------------------------------------------------------------

dbdata %>%
  group_by(taxon) %>%
  summarise(total = sum(count)) %>%
  mutate(taxon = reorder(taxon, total)) %>%
  ggplot(aes(x = taxon, y = total)) +
  geom_col() +
  coord_flip()


# Q2 ----------------------------------------------------------------------

main <- left_join(dbdata, habitat, by = c("site" = "site")) %>%
  left_join(remotesense, by = c("UTM_X" = "UTM_X", "UTM_Y" = "UTM_Y"))

main %>%
  filter(taxon == "Copris nubilosus") %>%
  ggplot(aes(x = aspect, y = count)) +
  geom_point()

# Q3 ----------------------------------------------------------------------

main %>%
  filter(taxon == "Copris nubilosus" | taxon == "Dichotomius satanas" | taxon == "Eurysternus magnus") %>%
  ggplot(aes(x = dem, y = count, col = taxon)) +
  geom_point() +
  facet_wrap(~taxon)

# Q4 ----------------------------------------------------------------------

main %>%
  filter(taxon == "Copris nubilosus") %>%
  pull(count) %>%
  range()

# Q5 ----------------------------------------------------------------------

main %>%
  filter(taxon == "Copris nubilosus") %>%
  ggplot(aes(x = count)) +
  geom_histogram(fill = "forest green") +
  labs( y = "frequency")

# Q6 ----------------------------------------------------------------------

main %>%
  group_by(taxon) %>%
  summarise(
    min_count = min(count),
    mean_count = mean(count),
    median_count = median(count),
    max_count = max(count)
  )


# Q7 ----------------------------------------------------------------------

main %>%
  filter(taxon == "Copris nubilosus") %>%
  pivot_longer(cols = c("main_forest_type", "X2013_disturbance_type", "X2013_land_use"), names_to = "x_values") %>%
  ggplot(aes(x = value, y = count)) +
  geom_col() +
  facet_wrap(~x_values, scales = "free_x")


# Q8 ----------------------------------------------------------------------

q8 <- main %>%
  group_by(collectdate, site) %>%
  summarise(
    richness = sum(count>0),
    minimum = min(count),
    maximum = max(count),
    total = sum(count)
  )

