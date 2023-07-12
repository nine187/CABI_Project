rm(list=ls())
graphics.off()

#package for loading the github functions
library(remotes)

#function for dealing with CLIMEX data
#remotes::install_github("nowosad/ffipm") 

#download the custom function package for dealing with CLIMEX data
#https://github.com/aniaszy/ffipm
library(ffipm)

#package for mapping the output
library(tmap)

#deal with raster files and create raster files for FAOSTAT data
library(raster)
#tmap alternative for AllYears_raster
library(raster)
library(rasterVis)
library(dplyr)
#define the path for the files

path <- "C:/Users/Pasith/Documents/Dymex/Aflatoxin/"
#1970-2019 (fifty years) annual data irrigation and no irrigation
mod_irri_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_Irr_Annual_1970-2019.nc")
mod_noirri_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_NoIrr_Annual_1970-2019.nc")
mod_irr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Annual.nc")
mod_irr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Weekly.nc")
mod_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Annual.nc")
mod_noirr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Weekly.nc")
og_irr_annual <- paste0(path,"NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Annual.nc" )
og_irr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Weekly.nc")
og_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Annual.nc")
og_noirr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Weekly.nc")

#extract the data from the NetCDF file using the extract_data_list function
#EI
mod_irr_allyear_EI <- extract_data_list(mod_irri_allyear,"EI",n = 1, years = 1970:2019,
                                  step = "Year")
mod_noirr_allyear_EI <- extract_data_list(mod_noirri_allyear, "EI", n = 1, 
                                     years = 1970:2019, step = "Year")

#create a raster stack from the previous outputs
mod_irr_allyear_EI_raster <- create_raster_stack(mod_irr_allyear_EI,
                                                 years = 1970:2019, step = "Year")
mod_noirr_allyear_EI_raster <- create_raster_stack(mod_noirr_allyear_EI,
                                                   years = 1970:2019, step = "Year")

#CS
#DS
#WS
#HS


#1.explore the dataset
#check the name and type of each layer
names(mod_irr_allyear_EI_raster)
class(mod_irr_allyear_EI_raster)

#look at the plot for each year
#irrigation EI
#plot(mod_irr_allyear_EI_raster)
#plot(mod_noirr_allyear_EI_raster)

for (i in 1:length(mod_irr_allyear_EI_raster)) {
  layer <- mod_irr_allyear_EI_raster[[i]]
  year <- names(mod_irr_allyear_EI_raster)[i]
  
  # Plot a new plot for each year
  plot(layer, main = year, zlim =c(0,100))
  
  # Save as .png
  png(filename = paste0("map_", year, ".png"))
  # Add pause to view the plot
  #Sys.sleep(4)
  
  # Clear for the next iteration of the plot
  dev.off()
}

#tmap version of the loop
for (i in 1:length(mod_irr_allyear_EI_raster)) {
  layer <- mod_irr_allyear_EI_raster[[i]]
  year <- names(mod_irr_allyear_EI_raster)[i]
  
  # Create a new tmap plot for each iteration
  tm_map <- tm_shape(layer) +
    tm_raster() +
    tm_format(title = year)
  
  # Save the tmap plot as a PNG file
  tmap_save(tm_map, filename = paste0("map_", year, ".png"))
}

#no irrigation EI
for (i in 1:length(mod_noirr_allyear_EI_raster)) {
  layer <- mod_irr_allyear_EI_raster[[i]]
  year <- names(mod_irr_allyear_EI_raster)[i]
  plot(layer, main = year,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}

#plot(AllYears_raster_2000)

#2.link the map to maize cultivation
#import the data from FAOSTAT maize cultivation by country estimation 
#1970 data (test data)
maize_70 <- read.csv("GitHub/CABI_Project/data/FAOSTAT_data_en_7-12-2023(3).csv")

summary(maize_70)
View(maize_70)

#plot the data on the world map
list(unique(maize_70$Area))
data(World)
list(unique(World$name))

#remove unnessary columns

# name the columns that I want to keep in the dataset
desired_columns <- c("iso_a3", "name", "geometry")

# identify the columns that are not included in above
columns_to_remove <- setdiff(colnames(World), desired_columns)

# Remove the unwanted columns
World <- World[, !(colnames(World) %in% columns_to_remove)]

#match the FAOSTAT data to the country
#add a trial data for 1970 FAOSTAT maize
maize_70 <- read.csv("GitHub/CABI_Project/data/1970_maize.csv")
maize_70


#try to visualize the plot with ggplot2
#world_map <- map_data("world")
#area_data <- data.frame(area_code = c("US", "CA", "GB", "FR", "JP"), value = c(0, 0, 0, 0, 0))
#merged_data <- merge(world_map, area_data, by.x = "region", by.y = "area_code", all.x = TRUE)
#plot(merged_data$long, merged_data$lat, type = "n", xlim = c(-180, 180), ylim = c(-90, 90), xlab = "lat", ylab = "long")
#polygon(merged_data[c("long", "lat")], col = "lightblue", border = "pink")
#points(merged_data$long, merged_data$lat, pch = 19, cex = merged_data$value / max(merged_data$value) * 2)

#3. Classify the suitability with certain threshold  
#4. Try to run the parameters file in future climate