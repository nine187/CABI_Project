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

#use to convert dataframes back to sf class
library(sf)

#deal with raster files
library(raster)

#help visualize tmap package 
library(rnaturalearth) 

#package for downloading rnaturalearthhires
library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires") 

library(rmapshaper) 

#alternative package to read the NetCDF files
#library(ncdf4)

#read the csv files 
maize_70 <- read.csv("GitHub/CABI_Project/data/1970_maize.csv")
#import the data from FAOSTAT maize cultivation by country estimation 
#1970 data (test data)
allyear_maize <- read.csv("GitHub/CABI_Project/data/FAOSTAT_data_en_7-12-2023(3).csv")

#define the path for the files
path <- "C:/Users/Pasith/Documents/Dymex/Aflatoxin/"
#1970-2019 (fifty years) annual data irrigation and no irrigation
mod_irr_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_Irr_Annual_1970-2019.nc")
mod_noirr_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_NoIrr_Annual_1970-2019.nc")
mod_irr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Annual.nc")
mod_irr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Weekly.nc")
mod_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Annual.nc")
mod_noirr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Weekly.nc")
og_irr_annual <- paste0(path,"NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Annual.nc" )
og_irr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Weekly.nc")
og_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Annual.nc")
og_noirr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Weekly.nc")
irr_mask <- paste0(path,"NetCDF/CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif")

#extract the data from the NetCDF file using the extract_data_list function

# Example variables to loop through
#var <- c("WS", "HS", "CS", "DS", "EI", "GI")

###EI###
#extract data list for EI
mod_irr_allyear_EI <- extract_data_list(mod_noirr_week,"EI",
                                        step = "Year")

mod_noirr_allyear_EI <- extract_data_list(mod_noirr_allyear,"EI", years = 1970:2019,
                                        step = "Year")
#create the raster stack, check step = Year/Step (Weekly)
mod_irr_allyear_EI_raster <- create_raster_stack(mod_irr_allyear_EI,
                                                 years = 1970:2019, step = "Year")

mod_noirr_allyear_EI_raster <- create_raster_stack(mod_noirr_allyear_EI,
                                                   years = 1970:2019, step = "Year")

for (i in 1:length(mod_irr_allyear_EI_raster)) {
  layer <- mod_irr_allyear_EI_raster[[i]]
  year <- names(mod_irr_allyear_EI_raster)[i]
  
  # Create the title
  title <- paste("EI_irr", year)
  
  # Plot a new plot for each year
  plot(layer, main = title, zlim =c(0,100))
  
  # Save as .png
  png(filename = paste0("map_", year, ".png"))
  # Add pause to view the plot
  #Sys.sleep(4)
  
  # Clear for the next iteration of the plot
  dev.off()
}

#no irrigation EI
for (i in 1:length(mod_noirr_allyear_EI_raster)) {
  layer <- mod_irr_allyear_EI_raster[[i]]
  year <- names(mod_irr_allyear_EI_raster)[i]
  title <- paste("EI_noirr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}

###CS###
mod_irr_allyear_CS <- extract_data_list(mod_irr_allyear,"CS", years = 1970:2019,
                                        step = "Year")
mod_noirr_allyear_CS <- extract_data_list(mod_noirr_allyear,"CS", years = 1970:2019,
                                          step = "Year")
mod_irr_allyear_CS_raster <- create_raster_stack(mod_irr_allyear_CS,
                                                 years = 1970:2019, step = "Year")
mod_noirr_allyear_CS_raster <- create_raster_stack(mod_noirr_allyear_CS,
                                                   years = 1970:2019, step = "Year")
for (i in 1:length(mod_irr_allyear_CS_raster)) {
  layer <- mod_irr_allyear_CS_raster[[i]]
  year <- names(mod_irr_allyear_CS_raster)[i]
  title <- paste("CS_irr", year)
  plot(layer, main = title,zlim = c(0,999))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_CS_raster)) {
  layer <- mod_noirr_allyear_CS_raster[[i]]
  year <- names(mod_noirr_allyear_CS_raster)[i]
  title <- paste("CS_noirr", year)
  plot(layer, main = title,zlim =c(-100,0))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}

###WS###
mod_irr_allyear_WS <- extract_data_list(mod_irr_allyear,"WS", years = 1970:2019,
                                        step = "Year")
mod_noirr_allyear_WS <- extract_data_list(mod_noirr_allyear,"WS", years = 1970:2019,
                                          step = "Year")
mod_irr_allyear_WS_raster <- create_raster_stack(mod_irr_allyear_WS,
                                                 years = 1970:2019, step = "Year")
mod_noirr_allyear_WS_raster <- create_raster_stack(mod_noirr_allyear_WS,
                                                   years = 1970:2019, step = "Year")
for (i in 1:length(mod_irr_allyear_WS_raster)) {
  layer <- mod_irr_allyear_WS_raster[[i]]
  year <- names(mod_irr_allyear_WS_raster)[i]
  title <- paste("WS_irr", year)
  plot(layer, main = title,zlim =c(0,320))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_WS_raster)) {
  layer <- mod_noirr_allyear_WS_raster[[i]]
  year <- names(mod_noirr_allyear_WS_raster)[i]
  title <- paste("WS_noirr", year)
  plot(layer, main = title,zlim =c(0,320))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}

###HS###
mod_irr_allyear_HS <- extract_data_list(mod_irr_allyear,"HS", years = 1970:2019,
                                        step = "Year")
mod_noirr_allyear_HS <- extract_data_list(mod_noirr_allyear,"HS", years = 1970:2019,
                                          step = "Year")
mod_irr_allyear_HS_raster <- create_raster_stack(mod_irr_allyear_HS,
                                                 years = 1970:2019, step = "Year")
mod_noirr_allyear_HS_raster <- create_raster_stack(mod_noirr_allyear_HS,
                                                   years = 1970:2019, step = "Year")
for (i in 1:length(mod_irr_allyear_HS_raster)) {
  layer <- mod_irr_allyear_HS_raster[[i]]
  year <- names(mod_irr_allyear_HS_raster)[i]
  title <- paste("HS_irr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_HS_raster)) {
  layer <- mod_noirr_allyear_HS_raster[[i]]
  year <- names(mod_noirr_allyear_HS_raster)[i]
  title <- paste("CS_noirr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
###DS###
mod_irr_allyear_DS <- extract_data_list(mod_irr_allyear,"DS", years = 1970:2019,
                                        step = "Year")
mod_noirr_allyear_DS <- extract_data_list(mod_noirr_allyear,"DS", years = 1970:2019,
                                          step = "Year")
mod_irr_allyear_DS_raster <- create_raster_stack(mod_irr_allyear_DS,
                                                 years = 1970:2019, step = "Year")
mod_noirr_allyear_DS_raster <- create_raster_stack(mod_noirr_allyear_DS,
                                                   years = 1970:2019, step = "Year")
for (i in 1:length(mod_irr_allyear_DS_raster)) {
  layer <- mod_irr_allyear_DS_raster[[i]]
  year <- names(mod_irr_allyear_DS_raster)[i]
  title <- paste("DS_irr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_DS_raster)) {
  layer <- mod_noirr_allyear_DS_raster[[i]]
  year <- names(mod_noirr_allyear_DS_raster)[i]
  title <- paste("DS_noirr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
###GI###
mod_irr_allyear_GI <- extract_data_list(mod_irr_allyear,"GI", years = 1970:2019,
                                        step = "Year")
mod_noirr_allyear_GI <- extract_data_list(mod_noirr_allyear,"GI", years = 1970:2019,
                                          step = "Year")
mod_irr_allyear_GI_raster <- create_raster_stack(mod_irr_allyear_GI,
                                                 years = 1970:2019, step = "Year")
mod_noirr_allyear_GI_raster <- create_raster_stack(mod_noirr_allyear_GI,
                                                   years = 1970:2019, step = "Year")
for (i in 1:length(mod_irr_allyear_GI_raster)) {
  layer <- mod_irr_allyear_GI_raster[[i]]
  year <- names(mod_irr_allyear_GI_raster)[i]
  title <- paste("GI_irr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_GI_raster)) {
  layer <- mod_noirr_allyear_GI_raster[[i]]
  year <- names(mod_noirr_allyear_GI_raster)[i]
  title <- paste("GI_noirr", year)
  plot(layer, main = title,zlim =c(0,100))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}

##########2.link the map to maize cultivation##########

#look at the maize yield dataset from FAO in 1970
summary(maize_70)
View(maize_70)

#plot the data on the world map
list(unique(maize_70$Area))
data(World)
list(unique(World$name))

#remove unnecessary columns

# name the columns that I want to keep in the dataset
desired_columns <- c("iso_a3", "name", "geometry")

# identify the columns that are not included in above
columns_to_remove <- setdiff(colnames(World), desired_columns)

# Remove the unwanted columns
World <- World[, !(colnames(World) %in% columns_to_remove)]

#match the FAOSTAT data to the country
#add a trial data for 1970 FAOSTAT maize

#remove unnecessary columns

desired_columns <- c("Area", "Year", "Year.code", "unit", "Value")
columns_to_remove <- setdiff(colnames(maize_70), desired_columns)
maize_70 <- maize_70[, !(colnames(maize_70) %in% columns_to_remove)]
head(maize_70)

data(World)
# Merge the datasets based on matching values in "Area" and "name" columns
#all.y = TRUE is set so that all the matching column w/o Value won't be erased
World <- merge(maize_70, World, by.x = "Area", by.y = "name", all.y = TRUE)
head(World)

######some countries names didn't match, fix this later

#convert World back to sf dataframe class so tmap can read the file
World_sf <- st_as_sf(World, sf_column_name = "geometry")
#convert World_sf to raster
World_raster <- raster(World_sf)
plot(World_raster)

#plot the thing in tmap
tmap_mode("plot")
map <- tm_shape(World_sf) +
  tm_polygons("Value") 

tmap_mode("view")
map

plot(mod_irr_allyear_EI_raster$X1970)

#test by converting the raster for 1970 
raster_irr_70 <- mod_irr_allyear_EI_raster$X1970
plot(raster_70)
raster_noirr_70 <- mod_noirr_allyear_EI_raster$X1970
plot(raster_noirr_70)

#merge the corn map with the irrigation map from before
merged_map <- merge(raster_irr_70, raster_noirr_70, by = "common_id")
plot(merged_map)


#test by doing a regression analysis on both irr and no irr
class(raster_irr_70)
class(raster_noirr_70)
class(World_raster)

#extract the values from the raster files
values1 <- getValues(raster_irr_70)
values2 <- getValues(raster_noirr_70)
values3 <- getValues(World_raster)

#creates a new dataframe from the values
data <- data.frame(values2, values3)
model <- lm(values1 ~ values3, data = data)
summary(model)
plot(model, pch = 1)

#create a regression map from the merged data
#model <- lm(raster_irr_70 ~ raster_noirr_70, data = merged_map)

#3. Classify the suitability with certain threshold

#4. Try to run the parameters file in future climate
#load the aflatoxin distribution map for maize

###############################SANDBOX##########################################

###codes that might be useful later###
#explore the irrigation mask data
mask <- raster(irr_mask)
print(mask)
plot(mask)
summary(mask, forceapply = TRUE)

#create a composite raster of irri and noirri maps
composite_raster <- create_composite_raster(file1 = mod_irr_allyear,
                                            file2 = mod_noirr_allyear,
                                            mask = mask,
                                            years_input_file = 1970:2019,
                                            years_raster_stack = 1970:2019,
                                            dname = "EI")

#load the raster file to modify the variable name
#raster_mask <- raster(raster_file)
#plot(raster_mask)

#cant find EI, going to use regular nc package to explore the file
nc_file <- nc_open(mod_irr_annual)
head(nc_file)
variables <- names(nc_file$var)
variables

#tmap version of the loop
  # Create a new tmap plot for each iteration
  tm_map <- tm_shape(World) +
    tm_raster(mod_noirr_allyear_EI_raster$X1970)+
    tm_dots(col = "blue", palette = "Set1", title = "City") +
    tm_basemap("OpenStreetMap") +
    tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
    tm_layout(legend.position = c("left", "bottom"))+

  # Save the tmap plot as a PNG file
tmap_mode("view")
tm_map

# Create a raster stack for your time-series data for the analysis of the linear trend 

time <- 1:nlayers(composite.stack[[1]])  

fun2 = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }} 

climex.slope <- vector(mode = "list", length=length(dname)) 

for (i in 1:length(dname)){ 
  
  climex.slope[[i]] <- calc(composite.stack[[i]], fun2) 
  
} 

#tmap

World <- rnaturalearth::ne_countries(scale = 10, continent = NULL, returnclass = "sf", type ="countries") 

World <- rmapshaper::ms_simplify(World, keep = 0.1, keep_shapes = TRUE) 

#plot(AllYears_raster_2000)
#try to visualize the plot with ggplot2
#world_map <- map_data("world")
#area_data <- data.frame(area_code = c("US", "CA", "GB", "FR", "JP"), value = c(0, 0, 0, 0, 0))
#merged_data <- merge(world_map, area_data, by.x = "region", by.y = "area_code", all.x = TRUE)
#plot(merged_data$long, merged_data$lat, type = "n", xlim = c(-180, 180), ylim = c(-90, 90), xlab = "lat", ylab = "long")
#polygon(merged_data[c("long", "lat")], col = "lightblue", border = "pink")
#points(merged_data$long, merged_data$lat, pch = 19, cex = merged_data$value / max(merged_data$value) * 2)