# Load necessary libraries
library(geosphere)  # For the distHaversine function
library(readxl)  # To read Excel files

# Read the dataset
df <- read_excel("clinics.xls")
df <- df[df$locLong >= -180 & df$locLong <= 180 & df$locLat >= -90 & df$locLat <= 90, ]
df$locLong <- as.numeric(df$locLong)
df$locLat <- as.numeric(df$locLat)

# Define a reference point (latitude, longitude)
ref_lat <- 40.671
ref_lon <- -73.985

# Approach 1: Using a for-loop
start_time <- Sys.time()
haversine_loop <- function(df) {
  distances <- numeric(nrow(df))
  for (i in 1:nrow(df)) {
    distances[i] <- distHaversine(c(ref_lon, ref_lat), c(df$locLong[i], df$locLat[i]))
  }
  return(distances)
}
df$distance_loop <- haversine_loop(df)
end_time <- Sys.time()
print(paste("For loop time:", end_time - start_time))

# Approach 2: Using apply()
start_time <- Sys.time()
haversine_apply <- function(df) {
  apply(df, 1, function(row) {
    distHaversine(c(ref_lon, ref_lat), c(as.numeric(row["locLong"]), as.numeric(row["locLat"])))
  })
}
df$distance_apply <- haversine_apply(df)
end_time <- Sys.time()
print(paste("Apply function time:", end_time - start_time))

# Approach 3: Vectorized computation
start_time <- Sys.time()
haversine_vectorized <- function(df) {
  distHaversine(matrix(c(ref_lon, ref_lat), ncol = 2, nrow = nrow(df), byrow = TRUE),
                matrix(c(df$locLong, df$locLat), ncol = 2))
}
df$distance_vectorized <- haversine_vectorized(df)
end_time <- Sys.time()
print(paste("Vectorized time:", end_time - start_time))
