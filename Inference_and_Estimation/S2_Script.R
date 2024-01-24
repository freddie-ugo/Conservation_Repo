setwd('/Users/freddieugo/Documents/GitHub/Conservation_Repo/Inference_and_Estimation')
# Read in CSV
#Fixes header issue with skip
dbdata_DA <- read.csv("S2_Prac/dbdata_DA.csv", skip = 1)

# Load Libraries
install.packages('tidyr')
install.packages('dplyr')
install.packages('stringr')
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


# Fix DA Data -------------------------------------------------------------

# FIX SPECIES MISSPELLING ISSUE
dbdata_DA_fix <- dbdata_DA %>% 
  mutate(taxon = str_replace_all(taxon, "  ", " ")) %>%
  mutate(taxon = str_replace_all(taxon, "[ \t]+", " ")) %>%
  mutate(taxon = str_replace_all(taxon, "Eurystenus", "Eurysternus")) %>%
  mutate(taxon = str_replace_all(taxon, "belorinus", "belhorinus"))
# Got whole table, turned taxon into taxon where the strings of two spaces were replaced with one space.
 
#FIX TRANSECT AND SITE
dbdata_DA %>%
  unite("site", transect, site, sep = "_")

#Session 3

# Fix BA data -------------------------------------------------------------

dbdata_BA <- read.csv("S2_Prac/dbdata_BA.csv")
dbdata_BA

str(dbdata_BA)

#Check unique sites
dbdata_BA %>% pull(site) %>% unique

#Fix data
dbdata_BA_fix <- dbdata_BA %>%
  mutate(camp = "BA",
         #Missing Dates to NA
         setdate = ifelse(setdate == "", NA, setdate),
         collectdate = ifelse(collectdate == "", NA, collectdate)) %>%
  # Fill down missing dates
  fill(setdate, collectdate, .direction = "down") %>%
  # Convert to Long format
  pivot_longer(!c(camp, site, setdate, collectdate, name), names_to = "taxon", values_to = "count") %>%
  # Correct missing counts
  mutate(count = replace_na(count, 0))

# Relocate
dbdata_BA_fix %>% relocate(count, site)

#Filter selects rows
dbdata_BA_fix %>% filter(taxon == "Copris.laeviceps")
dbdata_BA_fix %>% filter(site == "BA2_SS1")
dbdata_BA_fix %>% filter(site != "BA1_SS1")

#Select selects Columns
dbdata_BA_fix %>% select(site, taxon, count)
dbdata_BA_fix %>% select(-collectdate, -camp)

#Joining data
# Fix CO data -------------------------------------------------------------

dbdata_CO_occ <- read.csv("S2_Prac/dbdata_CO_occ.csv")
dbdata_CO_rec <- read.csv("S2_Prac/dbdata_CO_rec.csv")

left_join(dbdata_CO_occ, dbdata_CO_rec, by = c("id" = "id"))


