library(readr)
library(ggplot2)
library(ggmap)

# Load clean CSV
df <- read_csv("Desktop/PFE/coordinates.csv")
colnames(df) <- c("V1", "lat", "long")

# Convert lat/lon (if needed)
df$Latitude <- as.numeric(df$lat)
df$Longitude <- as.numeric(df$Longitude)

# Register your Google Maps API key
register_google(key = "YOUR_GOOGLE_API_KEY")

# Plot on a base map
map <- get_map(location = c(lon = mean(df$Longitude), lat = mean(df$Latitude)), zoom = 5)

ggmap(map) +
  geom_point(data = df, aes(x = Longitude, y = Latitude, color = `Label Name - Code`), size = 3) +
  theme_minimal()
