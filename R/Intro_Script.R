a <- 5
b <- c(243, 12, 5, 1)
mean(b)
mean(c(1,2,3))

x <- 10
y <- 14
x + y
sum2 <- function(a, b) a + b
sum2(x, y)

library(vegan)

sample1 <- c(243, 12, 5, 1)
diversity(sample1, index = "shannon")
sample1shannon <- diversity(sample1, index = "shannon")

diversity(sample1, index = "simpson")
sample1simpson <- diversity(sample1, index = "simpson")

fisher.alpha(sample1)
sample1fisher <- fisher.alpha(sample1)

sample1mat <- matrix(sample1, nrow = 1)
specpool(sample1mat)

sample2 <- c(23, 14, 1, 3, 17, 2, 2, 305, 17, 5, 98)
sample2mat <- matrix(sample2, nrow = 1)
specpool(sample2mat)

rarecurve(sample2mat)
sample2_rc <- rarecurve(sample2mat)

library(dplyr)
library(tidyr)
library(tibble)

dbdata <- read.csv("2023_11-29_ComputationalEcology_DungBeetleData.csv")

mean(c(1, 5, 12)) %>% sum2(5)



dbdata.site <- dbdata %>% pull(site)
dbdata.site
dbdata.site == "BC3_SS4"

bc3ss4.counts <- dbdata %>%
  filter(site == "BC3_SS4") %>%
  pull(count)

sum(bc3ss4.counts)

bc3ss4.subset <- dbdata %>%
  filter(site == "BC3_SS4")


bc3ss4.species <- bc3ss4.subset %>%
  filter(count != 0) %>%
  pull(taxon)

diversity(bc3ss4.counts, index = "shannon")
fisher.alpha(bc3ss4.counts, MARGIN =1)

ba1ss1.counts <- dbdata %>%
  filter( site == "BA1_SS1") %>%
  pull(count)

diversity(ba1ss1.counts, index = "shannon")

dbdata.ba.mat <- dbdata %>%
  filter(camp == "BA") %>%
  pivot_wider(names_from = taxon, values_from = count) %>%
  select(-camp) %>%
  column_to_rownames("site")

diversity(dbdata.ba.mat, index = "shannon")
specpool(dbdata.ba.mat)
rarecurve(dbdata.ba.mat)
plot(specaccum(dbdata.ba.mat))

dbdata.site == "BA3_SS4"
dbdata.site


dbdata
dbdata.ST <- dbdata %>%
  filter(camp == "ST")

dbdata.ST.count <- dbdata.ST %>%
  pull(count)

sum(dbdata.ST.count)

dbdata.ST.mat <- dbdata.ST %>%
  pivot_wider(names_from = taxon, values_from = count) %>%
  select(-camp) %>%
  column_to_rownames("site")

diversity.ST <- dbdata.ST.mat %>%
  diversity(index = "shannon")

ordered_diversity <- order(-diversity.ST)
sorted_diversity <- diversity.ST[ordered_diversity]
sorted_diversity

chao.ST <- estimateR(dbdata.ST.mat, index = "chao")

difference_chao_obs <- chao.ST[1,] - chao.ST[2, ]
ordered_diff <- order(difference_chao_obs)
sorted_diff <- difference_chao_obs[ordered_diff]
sorted_diff

rarecurve(dbdata.ST.mat)