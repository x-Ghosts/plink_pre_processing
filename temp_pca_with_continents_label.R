library(ggplot2)
library(dplyr)

# Load data
pc1 <- 1
pc2 <- 2
pc3 <- 3
pc4 <- 4

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
  PC2 = pc_scores[, pc2],
  PC3 = pc_scores[, pc3]
)


pc_df$Color <- "black"
pc_df$Shape <- as.numeric(as.factor(pc_df$FID))

# Assign colors by your logic
africa_pops <- c("ANG", "ZA_ANG", "ABR", "ALB", "AND", "BAW", "BRK", "BUR", "CAM", "CRO",
                 "DIA", "DJA", "DRA", "DZD", "GAL", "GAZ", "GHA", "GOG", "GSH", "GUE", "GUM", "KEF",
                 "KAR", "KIG", "LGW", "LND", "MAA", "MAN", "MAU", "MEN", "MLY", "MOR", "MSH", "MTB", "MUB", 
                 "NAI", "NBN", "NDA", "NGD", "NOR", "NRW", "NSJ", "OSS", "PAF", "PEU", "PRW", "RSK", "SAA",
                 "SAH", "SDN", "SEA", "SEB", "SHL", "SID", "SNJ", "SOF", "SOU", "BOE", 
                 "TAR", "TET", "THY", "TOG", "TUN", "WAD", "WYG", "NK_ALG", "MZ_ALG", "AR_ALG", "MK_ALG", "Nubian_ibex",
                 "Tagg", "Nubian", "BAR")
asia_pops <- c("ANK", "BAB", "BRI", "BUT", "CHA", "DDP", "IRA", "JAT", "KAC", "KAM", "KES", "KIL", "KLS",
               "LOH", "LOP", "PAH", "PAT", "TAP", "TED", "THA", "Nubian_ibex", "Markhor" , "BEZ")
europe_pops <- c("FR_ANG", "SP_MGRANA", "ARG", "ASP", "BIA", "CAP", "FAC", "GAR", "IT_GARFAG", "GCI", "LIV", "MES", "MNT_I",
                 "MON", "NVE", "RCC", "RME", "SAA", "TER", "VLS", "ALP", "ASP", "BEY", "BIO", "CRP", "CRS", "FSS",
                 "JON", "LNR", "MAL", "MLG", "MLS", "MLT", "NIC", "ORO", "PAL", "PTV", "PVC", "PYR", "RAS", "SAA", "SAR", "VAL", 
                 "VSS", "GK_EGR", "GK_SKP", "Cretan_wild","Peacock", "Chamois-C", "Dreznica",
                 "Skyros", "Chios", "Lesbos", "Peloponnese", "Crete", "GIR", "DDS", "FUL")

pc_df$Color[pc_df$FID %in% africa_pops] <- "red"
pc_df$Color[pc_df$FID %in% asia_pops] <- "orange"
pc_df$Color[pc_df$FID %in% europe_pops] <- "aquamarine3"



# Idea number2

# 1️⃣ PCA Plot: Continent Colors
ggplot(pc_df, aes(x = PC2, y = PC3, color = Color)) +
  geom_point(size = 1.5, alpha = 0.7) +
  theme_minimal() +
  scale_color_identity() +
  labs(
    title = "PCA - Merged 8 datasets Wild and Domestic - Clustered by Continents",
    x = paste0("PC", pc2, " (", pc_val$V2[pc2], "%)"),
    y = paste0("PC", pc3, " (", pc_val$V2[pc3], "%)")
  )

ggsave("pca_continent_plot_wild_domestic.pdf", width = 12, height = 9)

######
######

# 🟢 Add Continent column
pc_df$Continent <- "Other"
pc_df$Continent[pc_df$FID %in% africa_pops] <- "Africa"
pc_df$Continent[pc_df$FID %in% asia_pops] <- "Asia"
pc_df$Continent[pc_df$FID %in% europe_pops] <- "Europe"

# 🟢 Assign colors to each continent (in the plot, no need for a Color column)
continent_colors <- c(
  "Africa" = "red",
  "Asia" = "orange",
  "Europe" = "aquamarine3",
  "Other" = "black"
)

# 🟢 Plot with Continent as color
ggplot(pc_df, aes(x = PC2, y = PC3, color = Continent, label = FID)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_text(size = 1.8, vjust = -0.5) +
  theme_minimal() +
  labs(
    title = "PCA - Merged 8 datasets Wild_Domestic - Labels included",
    x = paste0("PC", pc2, " (", pc_val$V2[pc2], "%)"),
    y = paste0("PC", pc3, " (", pc_val$V2[pc3], "%)")
  ) +
  scale_color_manual(values = continent_colors) +
  guides(color = guide_legend(override.aes = list(label = ""))) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )


ggsave("pca_continent_labelled.pdf", width = 12, height = 9)

#################"
##################


# 🟢 Compute centroids for each breed (FID)
fid_centroids <- pc_df %>%
  group_by(FID) %>%
  summarize(
    PC2 = mean(PC2),
    PC3 = mean(PC3),
    Continent = first(Continent)
  )

# 🟢 Plot only the centroids with breed labels
ggplot(fid_centroids, aes(x = PC2, y = PC3, color = Continent, label = FID)) +
  geom_point(size = 3, alpha = 0.9) +  # Only centroid points
  geom_text(size = 2.5, vjust = -0.5) +  # Labels for each breed
  theme_minimal() +
  scale_color_manual(values = continent_colors) +
  guides(color = guide_legend(override.aes = list(label = ""))) +
  labs(
    title = "PCA - Centroids of Breeds (FID) - Colored by Continent",
    x = paste0("PC", pc2, " (", pc_val$V2[pc2], "%)"),
    y = paste0("PC", pc3, " (", pc_val$V2[pc3], "%)")
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

ggsave("pca_fid_centroids_labeled.pdf", width = 12, height = 9)

