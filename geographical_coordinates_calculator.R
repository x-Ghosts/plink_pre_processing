## Small Script of Coordinates calculator :


degree_lat <- 48
minute_lat <- 06
second_lat <- 53
direction_lat <- "N"

degree_long <- 1
minute_long <- 40
second_long <- 46
direction_long <- "W"

dir_lat <- ifelse(direction_lat == "S", -1, 1)
dir_long <- ifelse(direction_long == "W", -1, 1)

latitude <- dir_lat * (degree_lat + minute_lat / 60 + second_lat / 3600)
longitude <- dir_long * (degree_long + minute_long / 60 + second_long / 3600)

cat("Latitude  =", latitude)

cat("Longitude =", longitude)