library(tidyverse)
library(tidyr)


bim <- read.table(file = "temp_adaptmap.bim", header = FALSE, stringsAsFactors = FALSE)

manifest <- read.csv("manifest_elena_cleaned.csv", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sub_manifest <- manifest %>%
  select(rs., X.Locus_Name, ARS1_chr, ARS1_pos)

write.table(sub_manifest, "sub_manifest.txt",
            sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)



class(bim$V1)
class(sub_manifest$ARS1_chr)


new_bim <- bim %>%
  select(V1,V2, V3, V4,V5,V6) %>%
  right_join(sub_manifest))
