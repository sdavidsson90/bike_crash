
# Kernel Density Estimation (KDE)

# Read in packages
pacman::p_load(tidyverse, tmap, sf, viridis, MASS, raster)

# Reading in data
bike_crash <- read_rds("~/Documents/ARKIV/R/Projects/bike_crash/processed_data/bike_crash.rds")

# Convert to spatial
bike_crash <- st_as_sf(bike_crash, coords = c(x = "longitude", y = "latitude"), crs = "NAD83") 

# _ _ _ _ _ _ _ _
# KDE: Preliminary
kde <- kde2d(bike_crash$x, bike_crash$y, h = 0.5, n = 750)

# Questions:
# 1) How do you determine appropriate bandwith?

kde <- raster(kde)

tm_shape(kde) + 
  tm_raster("layer",  palette = "OrRd") + 
  tm_shape(bike_crash) +
  tm_dots(alpha = 0.01, col = "black")

# First attempt at KDE was succesful - but does not reveal great insights. 
# Next step is to become more local - but first other methods will be used.
