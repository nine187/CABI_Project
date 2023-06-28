rm(list = ls())
graphics.off()
getwd()

#check package dependencies later
library(readr) #use for reading csv file
library(sf) #for reading spatial data
library(raster) #similar reason
library(tmap) #mapping
#setwd("Documents/CABI_Project/data/")

#load the raster data files

Irrigation_data <- raster("EI_irrigation.tiff.tiff")
No_irrigation_data <- raster("EI_noirrigation.tiff.tiff")

#load the irrigation data
Irrigated_areas <- raster("CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif")

#load the irrigated areas data from FAO
irrigation_mask <- raster("CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif")

#load the gbif occurences data
gbif <- read.delim("gbif_occurrence.csv")

#load the raster file
gbif_raster <- raster("gbif_raster.tif")

###################PLOTTING########################
tmap_mode("plot")

data(land, World)

map <- tm_shape(irrigation_mask) +
  tm_raster()
tmap_mode("view")
map
#export the file to results

###########################################################
#create a new raster file to reverse the data
#Irrigation_data_reversed <- 250 - Irrigation_data
#No_irrigation_data_reversed <- 250 - Irrigation_data

# Define the old and new ranges
#old_range <- c(0, 250)
#new_range <- c(0, 100)

# Rescale the values in the reversed raster to the range [0, 100]
#Irrigation_data_rescaled <- (Irrigation_data_reversed - old_range[1]) / 
  #diff(old_range) * diff(new_range) + new_range[1]
#No_irrigation_data_rescaled <- (Irrigation_data_reversed - old_range[1]) / 
  #diff(old_range) * diff(new_range) + new_range[1]
#create a map overlapping the irrigation and no irrigation with gbif occurences
#gbif_irrigation_map <- tm_shape(Irrigation_data) +
  #tm_raster(style = "fixed", palette = "Blues", title = "Irrigation Data") +
  #tm_shape(gbif_sp) +
  #tm_dots(col = "red", size = 0.5, title = "GBIF Occurrences") +
  #tm_layout(title = "Irrigation Data with GBIF Occurences")+
  #tm_legend(show = FALSE )

#gbif_irrigation_map

###Map both EI values next to each other using tmap##
#map_1 <- tm_shape(Irrigation_data_rescaled)+
  #tm_raster(style = "cont",palette = "RdYlBu", title = "EI1", max.value = 100)+
  #tm_layout(legend.outside = TRUE, legend.outside.position = "bottom")
#map_1

#map_2 <- tm_shape(No_irrigation_data_rescaled)+
  #tm_raster(style = "cont",palette = "RdYlBu", title = "EI1", max.value = 100)+
  #tm_layout(legend.outside = TRUE, legend.outside.position = "bottom")
#map_2

#Arrange the map side by side
#tmap_arrange(map_1, map_2)

###Map both EI maps on top of each other using overlay function in raster###
#EI_mean <- overlay(Irrigation_data_rescaled, No_irrigation_data_rescaled, 
                   #fun = mean)

#Create tmap for mean EI
#map_EI <- tm_shape(EI_mean) +
  #tm_raster(style = "cont", midpoint = 0, palette = "RdYlBu", title = "Mean EI",
            #contrast = 1) +
  #tm_layout(legend.outside = TRUE, legend.outside.position = "bottom")
#map_EI