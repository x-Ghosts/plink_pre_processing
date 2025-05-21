
library(ggmap)
library(ggplot2)
library(readr)
library(dplyr)


register_stadiamaps("24dcfc0f-ebaf-4583-bfbc-c87c571ce15b", write = TRUE)
coords <- read_csv("PFE/coordinates.csv")



coords$Longitude <- as.numeric(coords$Longitude)
coords$Latitude <- as.numeric(coords$Latitude)

bbox <- c(
  left = min(coords$Longitude, na.rm = TRUE) - 1,
  bottom = min(coords$Latitude, na.rm = TRUE) - 1,
  right = max(coords$Longitude, na.rm = TRUE) + 1,
  top = max(coords$Latitude, na.rm = TRUE) + 1
)


world_map <- get_stadiamap(bbox, zoom = 4, maptype = "alidade_smooth") #alidade_smooth

ggmap(world_map) +
  geom_point(data = coords, aes(x = Longitude, y = Latitude, color = `Label Name`), size = 2) +
  theme_minimal() +
  labs(title = "Geographic Distribution of Goat Populations",
       x = "Longitude", y = "Latitude", color = "Breed Label") +
  guides(color = guide_legend(ncol = 5))
