library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)


fam <- read.table("adaptmap/wd_adaptmap.fam", header = FALSE)
colnames(fam) <- c("PID", "IID", "FID", "MID", "sex", "pheno")

fam <- fam %>%
  group_by(PID) %>%
  summarise(Count = n())

fam <- fam %>%
  group_by(Count) %>%
  summarise(Number_of_PID = n())

# scale_x_continuous(breaks = range_distribution$Count) +
ggplot(fam, aes(x = Count, y = Number_of_PID)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = Number_of_PID), vjust = -1, size = 4) +
  theme_minimal() +
  xlab("Range Distribution") +
  ylab("Number of Breeds within the range size") +
  theme(axis.text.x = element_text(angle = 0, vjust = -0.5, size = 12)) +
  scale_x_continuous(breaks = fam$Count)


#######################"

# Algeria

library(dplyr)

fam <- fam %>%
  mutate(
    PID_group = case_when(
      grepl("^MZ", PID) ~ "MZ",
      grepl("^AR", PID) ~ "AR",
      grepl("^NK", PID) ~ "NK",
      grepl("^MK", PID) ~ "MK",
      TRUE ~ PID  # Keep the original if no match
    )
  )

#######################


#######################

# Adaptmap


fam <- read.table("adaptmap/wd_adaptmap.fam", header = FALSE)
colnames(fam) <- c("PID", "IID", "FID", "MID", "sex", "pheno")

# Extract just the alphabetic part of the IID prefix, e.g. ET_ABR
fam <- fam %>%
  mutate(family_IID = str_extract(IID, "^[A-Z]{2}_[A-Z]+"))

fam <- fam %>%
  group_by(family_IID) %>%
  summarise(Count = n())

fam <- fam %>%
  group_by(Count) %>%
  summarise(Number_of_PID = n())

# scale_x_continuous(breaks = range_distribution$Count) +
ggplot(fam, aes(x = Count, y = Number_of_PID)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = Number_of_PID), vjust = -1, size = 4) +
  theme_minimal() +
  xlab("Range Distribution") +
  ylab("Number of Breeds within the range size") +
  theme(axis.text.x = element_text(angle = 0, vjust = -0.5, size = 12))

