
library(tidyverse)
library(tidyr)

# MANIFEST 1

manifest <- read.csv("manifest_illumina.csv", sep = ",", skip = 8, header = TRUE, stringsAsFactors = FALSE)
header_manifest <- read.csv("manifest_illumina.csv", skip = 7, header = FALSE, nrows = 1)
colnames(manifest) <- header_manifest

# MANIFEST 2
manifest_2 <- read.csv("manifest_illumina_2.csv", sep = ",", skip = 8, header = TRUE, stringsAsFactors = FALSE)
header_manifest_2 <- read.csv("manifest_illumina_2.csv", skip = 7, header = FALSE, nrows = 1)
colnames(manifest_2) <- header_manifest_2

# MANIFEST 3
manifest_3 <- read.csv("manifest_illumina_3.csv", sep = ",", skip = 8, header = TRUE, stringsAsFactors = FALSE)
header_manifest_3 <- read.csv("manifest_illumina_3.csv", skip = 7, header = FALSE, nrows = 1)
colnames(manifest_3) <- header_manifest_3








new_manifest <- manifest_3 %>%
  select(Chr, Name, MapInfo)

dataset <- read.table("dataset_rawdata_preprocessed/wd_adaptmap.bim", header = FALSE, stringsAsFactors = FALSE)
colnames(dataset) <- c("Chr", "Name", "morgan_pos", "physical_pos", "A1", "A2")


# Numeric conversion
class(x = new_manifest$Chr) # Test if the class is a character or not

new_manifest$Chr <- as.numeric(new_manifest$Chr)

dataset_updated <- dataset %>%
  left_join(new_manifest, by = c("Chr" = "Chr", "physical_pos" = "MapInfo"))

