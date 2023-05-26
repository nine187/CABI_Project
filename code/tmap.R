rm(list = ls())
graphics.off()

library(tmap)
library(tmaptools)

# Load country boundaries
my_data <- data("World")

# Subset Hungary from the country boundaries data
hungary <- World[World$iso_a3 == "HUN", ]

# Create a base map with Hungary boundaries
tm_shape(hungary) +
  tm_polygons()

# Add your data on top of the map
tm_shape(my_data) +
  # Specify the appearance of your data (e.g., dots, polygons, etc.)
  tm_dots(size = 0.5, col = "red")
tmap_mode("view")
