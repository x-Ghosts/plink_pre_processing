
library(ggmap)
library(ggplot2)
library(readr)
library(dplyr)


register_stadiamaps("24dcfc0f-ebaf-4583-bfbc-c87c571ce15b", write = TRUE)
coords <- read_csv("PFE/coordinates.csv")


# Define bounding box based on your data
bbox <- c(
  left = min(coords$Longitude) - 5,
  bottom = min(coords$Latitude) - 5,
  right = max(coords$Longitude) + 5,
  top = max(coords$Latitude) + 5
)

world_map <- get_stadiamap(bbox, zoom = 4, maptype = "alidade_smooth") #alidade_smooth

ggmap(world_map) +
  geom_point(data = coords, aes(x = Longitude, y = Latitude, color = Label), size = 2) +
  theme_minimal() +
  labs(title = "Geographic Distribution of Goat Populations",
       x = "Longitude", y = "Latitude", color = "Breed Label") +
  guides(color = guide_legend(ncol = 5))

