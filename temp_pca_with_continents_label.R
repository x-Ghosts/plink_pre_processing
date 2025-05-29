library(ggplot2)
library(dplyr)

# Load data
pc1 <- 1
pc2 <- 2
pc_val <- read.table(file = "pca/pca_merged.eigenval", header = FALSE)
pc_vec <- read.table(file = "pca/pca_merged.eigenvec", header = FALSE, stringsAsFactors = FALSE)

# Calculate variance
pc_val$V2 <- round(pc_val$V1 / sum(pc_val$V1) * 100, 2)

# Extract IDs and PC scores
pc_vecfam <- pc_vec[, c(1, 2)]
pc_scores <- pc_vec[, -c(1, 2)]

# Initial columns
pc_df <- data.frame(
  FID = pc_vecfam$V1,
  IID = pc_vecfam$V2,
  PC1 = pc_scores[, pc1],
  PC2 = pc_scores[, pc2]
)


pc_df$Color <- "black"
pc_df$Shape <- as.numeric(as.factor(pc_df$FID))

# Assign colors by your logic
africa_pops <- c("ANG", "ZA_ANG", "ABR", "ALB", "AND", "BAW", "BEZ", "BOE", "BOEx", "BRK", "BUR", "CAM", "CRO",
                 "DIA", "DJA", "DRA", "DZD", "GAL", "SEAxGAL", "GAZ", "GHA", "GOG", "GSH", "GUE", "GUM", "KEF",
                 "KAR", "KIG", "LGW", "LND", "MAA", "MAN", "MAU", "MEN", "MLY", "MOR", "MSH", "MTB", "MTBx", "MUB", "MUBx", 
                 "NAI", "NBN", "NDA", "NGD", "NOR", "NRW", "NSJ", "OSS", "PAF", "PEU", "PRW", "RSK", "SAA", "SEAxSAA",
                 "SAH", "SDN", "SEA", "SEAx", "SEAxALP", "SEAxTOG", "SEB", "SHL", "SID", "SNJ", "SOF", "SOU", 
                 "TAR", "TET", "THY", "TOG", "TUN", "WAD", "WYG", "NK_ALG", "MZ_ALG", "AR_ALG", "MK_ALG")
asia_pops <- c("ANK", "BAB", "BAR", "BRI", "BUT", "CHA", "DDP", "IRA", "JAT", "KAC", "KAM", "KES", "KIL", "KLS",
               "LOH", "LOP", "PAH", "PAT", "TAP", "TED", "THA")
europe_pops <- c("FR_ANG", "SP_MGRANA", "ARG", "ASP", "BIA", "CAP", "FAC", "GAR", "IT_GARFAG", "GCI", "LIV", "MES", "MNT_M", "MNT_I",
                 "MON", "MXS", "NVE", "RCC", "RME", "SAA", "TER", "VLS", "ALP", "ASP", "BEY", "BIO", "BOE", "CRP", "CRS", "FSS",
                 "JON", "LNR", "MAL", "MLG", "MLS", "MLT", "NIC", "ORO", "PAL", "PTV", "PVC", "PYR", "RAS", "SAA", "SAR", "VAL", 
                 "VSS", "GK_EGR", "GK_SKP")

pc_df$Color[pc_df$FID %in% africa_pops] <- "red"
pc_df$Color[pc_df$FID %in% asia_pops] <- "orange"
pc_df$Color[pc_df$FID %in% europe_pops] <- "green"
pc_df$Color[pc_df$FID %in% oceania_pops] <- "green"  # If overlap with Europe: will be green



ggplot(pc_df, aes(x = PC1, y = PC2, color = Color, shape = as.factor(Shape)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  scale_color_identity() +  # use the colors directly
  labs(
    title = "VarGoats PCA",
    x = paste0("PC", pc1, " (", pc_val$V2[pc1], "%)"),
    y = paste0("PC", pc2, " (", pc_val$V2[pc2], "%)")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )
)

# Idea number2

# 1️⃣ PCA Plot: Continent Colors
ggplot(pc_df, aes(x = PC1, y = PC2, color = Color)) +
  geom_point(size = 1.5, alpha = 0.7) +
  theme_minimal() +
  scale_color_identity() +
  labs(
    title = "PCA - Merged 8 datasets - Clustered by Continents",
    x = paste0("PC", pc1, " (", pc_val$V2[pc1], "%)"),
    y = paste0("PC", pc2, " (", pc_val$V2[pc2], "%)")
  )

ggsave("pca_continent_plot.pdf", width = 8, height = 8)

# 2️⃣ PCA Plot: Breed Colors + Labels
ggplot(pc_df, aes(x = PC1, y = PC2, color = FID, label = IID)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_text(size = 1.8, vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "PCA - Merged 8 datasets",
    x = paste0("PC", pc1, " (", pc_val$V2[pc1], "%)"),
    y = paste0("PC", pc_val$V2[pc2], "%)")
  ) +
  theme(legend.position = "none")  # Hide long breed legend

