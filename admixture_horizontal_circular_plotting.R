library(BITEV2)
library(tidyverse)
library(RCircos)

vec_2_10 <- c(2,3,4,5,6,7,8,9,10)
vec_2_15 <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
vec_2_20 <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

## Preparing the text file

ordered_txt <- read.table("merged_wild_domestic.fam", sep = " ", header = FALSE, stringsAsFactors = FALSE)
ordered_txt <- ordered_txt %>%
  select(V1)
write.table(ordered_txt, "ordered.txt", quote = FALSE, col.names = FALSE, row.names = FALSE)
  


membercoeff.cv(in.file = "log",
               out.file="test_CV_2_20",
               software="Admixture",
               minK=2, maxK=20,
               plot.format="pdf",
               plot.width=50, plot.height=40)

# Admixture plot
membercoeff.plot(in.file = "merged_wild_domestic", out.file = "plot_", software = "Admixture",
                 maxK = 10, plot.main = "Admixture Plot", plot.format = "pdf", pop.order.file = "ordered_sorted.txt")

# Admixture Circos plot
membercoeff.circos(in.file = "merged_wild_domestic", out.file = "plot_circular_2_20_sorted", software = "Admixture",
                   maxK = 20, K.to.plot = vec_2_20, halfmoon = FALSE,
                   plot.main = "Admixture analysis", pop.order.file = "ordered_sorted.txt",
                   plot.format = "pdf", plot.width = 100, plot.height = 100)

# Admixture Circos plot
membercoeff.circos(in.file = "merged_wild_domestic", out.file = "plot_circular_2_20", software = "Admixture",
                   maxK = 20, K.to.plot = vec_2_20, halfmoon = FALSE,
                   plot.main = "Admixture analysis",
                   plot.format = "pdf", plot.width = 100, plot.height = 100)
