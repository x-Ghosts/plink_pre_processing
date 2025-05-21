## Code not working 
## To be improved whenever the possibility is.



# Load required libraries
library(dplyr)
library(readr)

# Read the CSV file
data <- read_csv("uncomputed_centroid_italy.csv")

# Check column names — adjust if needed
# View(names(data))

# Group by breed column (assumed to be FID or IT_NAME) and compute centroid
centroids <- data %>%
  group_by(IT_ARG) %>%  # change to IT_NAME if needed
  summarise(
    centroid_lat = mean(`38.0128274`, na.rm = TRUE),
    centroid_long = mean(`14.5974572`, na.rm = TRUE)
  )

# Save results to new CSV
write_csv(centroids, "centroids_by_breed.csv")

# Optional: print result
print(centroids)
