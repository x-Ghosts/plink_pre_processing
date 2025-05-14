library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
#

fam <- read.table("italy_1/wd_italy_1.fam", header = FALSE)
colnames(fam) <- c("PID", "IID", "FID", "MID", "sex", "pheno")

original_signature_list <- c("ET_ABR", "CH_ALP", "IT_ALP", "FR_ALP", "MW_BAW", "IT_BIO",
                             "IE_BLB", "FR_CRS", "PK_DDP", "BF_DJA", "MW_DZD", "ET_GUM", 
                             "ET_KEF", "MW_LGW", "DK_LNR", "FI_LNR", "NL_LNR", "MG_MEN",
                             "ML_NAI", "EG_NBN", "MW_NSJ", "IE_OIG", "IT_ORO", "TZ_PRW",
                             "FR_PTV", "CH_SAA", "TZ_SNJ", "MG_SOF", "IT_VAL", "IT_VSS",
                             "NG_WAD", "CM_WAD", "ET_WYG", "GIR", "ORO")

fam <- fam %>%
  group_by(PID) %>%
  summarise(Count = n())

fam <- fam %>%
  mutate(Label = ifelse(PID %in% original_signature_list, "Original", "Other"))



ggplot(fam, aes(x = PID, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -1, size = 4) +
  scale_fill_manual(values = c("Original" = "darksalmon", "Other" = "aquamarine3")) +
  theme_minimal() +
  xlab("Population ID") +
  ylab("Sample size") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.5, size = 12))


###########################################"
###########################################

## Sideways work per dataset

###########################################"
###########################################

# Algeria - It was needed to execute this line of codes to merge, all existing character within the PID name, so it can be plotted the data
# distribution
fam <- read.table("algeria/algeria.fam", header = FALSE)
colnames(fam) <- c("PID", "IID", "FID", "MID", "sex", "pheno")

original_signature_list <- c("Iberian_ibex", "Cretan_wild", "Nubian_ibex", "Markhor")

fam <- fam %>%
  group_by(PID_new) %>%
  summarise(Count = n())

fam <- fam %>%
  mutate(Label = ifelse(PID_new %in% original_signature_list, "Original", "Other"))

#Edit it a bit

library(dplyr)

df <- df %>%
  mutate(
    PID_group = case_when(
      grepl("^MZ", PID) ~ "MZ",
      grepl("^AR", PID) ~ "AR",
      grepl("^FR", PID) ~ "FR",
      TRUE ~ PID  # Keep the original if no match
    )
  )
#end of edit

ggplot(fam, aes(x = PID_new, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -1, size = 4) +
  scale_fill_manual(values = c("Original" = "darksalmon", "Other" = "aquamarine3")) +
  theme_minimal() +
  xlab("Population ID") +
  ylab("Sample size") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.5, size = 12))

# Angora - Note that you must use the **wd_file**

# Ibex needed the Addon of Original / Non Original signatures

# Adaptmap


fam <- read.table("adaptmap/ADAPTmap_genotypeTOP_20160222_full.fam", header = FALSE)
colnames(fam) <- c("PID", "IID", "FID", "MID", "sex", "pheno")

original_signature_list <- c("ET_ABR", "CH_ALP", "IT_ALP", "FR_ALP", "MW_BAW", "IT_BIO",
                             "IE_BLB", "FR_CRS", "PK_DDP", "BF_DJA", "MW_DZD", "ET_GUM", 
                             "ET_KEF", "MW_LGW", "DK_LNR", "FI_LNR", "NL_LNR", "MG_MEN",
                             "ML_NAI", "EG_NBN", "MW_NSJ", "IE_OIG", "IT_ORO", "TZ_PRW",
                             "FR_PTV", "CH_SAA", "TZ_SNJ", "MG_SOF", "IT_VAL", "IT_VSS",
                             "NG_WAD", "CM_WAD", "ET_WYG")

##########
##########

# Extract just the alphabetic part of the IID prefix, e.g. ET_ABR
fam <- fam %>%
  mutate(family_IID = str_extract(IID, "^[A-Z]{2}_[A-Z]+"))


# Now label each row based on whether the family_IID is in your original list
fam <- fam %>%
  mutate(Label = ifelse(family_IID %in% original_signature_list, "Original", "Other"))

#########
#########

fam <- fam %>%
  group_by(family_IID) %>%
  summarise(Count = n())

fam <- fam %>%
  mutate(Label = ifelse(family_IID %in% original_signature_list, "Original", "Other"))


ggplot(fam, aes(x = family_IID, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -1, size = 4) +
  scale_fill_manual(values = c("Original" = "darksalmon", "Other" = "aquamarine3")) +
  theme_minimal() +
  xlab("Population ID") +
  ylab("Sample size") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.5, size = 12))
