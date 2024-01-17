# Read in CSV
#Fixes header issue with skip
dbdata_DA <- read.csv("S2_Prac/dbdata_DA.csv", skip = 1)

# Load Libraries
library(tidyr)
library(dplyr)
library(stringr)

# Look at the structure
str(dbdata_DA)

# Count up different taxa
dbdata_DA %>% count(taxon)
dbdata_DA %>% count(site)

# A different way to see unique taxa
dbdata_DA %>% pull(taxon) %>% unique() %>% sort()

# Check the numeric data
dbdata_DA %>% pull(number) %>% range()
dbdata_DA %>% pull(number) %>% summary()

# FIX SPECIES MISSPELLING ISSUE
dbdata_DA %>% 
  mutate(taxon = str_replace_all(taxon, "  ", " ")) %>%
  mutate(taxon = str_replace_all(taxon, "[ \t]+", " ")) %>%
  mutate(taxon = str_replace_all(taxon, "Eurystenus", "Eurysternus")) %>%
  mutate(taxon = str_replace_all(taxon, "belorinus", "belhorinus")) %>%
  count(taxon)
# Got whole table, turned taxon into taxon where the strings of two spaces were replaced with one space.
 
#FIX TRANSECT AND SITE
dbdata_DA %>%
  unite("site", transect, site, sep = "_")

