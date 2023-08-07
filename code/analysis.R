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
library(ncdf4)

library(leaflet)

#source the functions
source("GitHub/CABI_Project/code/function.R")
#define the path for the files
path <- "C:/Users/Pasith/Documents/Dymex/Aflatoxin/"
#1970-2019 (fifty years) annual data irrigation and no irrigation
mod_irr_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_Irr_Annual_1970-2019.nc")
#time-series data, weekly averages for individual year
mod_noirr_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_NoIrr_Annual_1970-2019.nc")

#modify annual parameter
mod_irr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Annual.nc")
#met data from climex 1995
mod_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Annual.nc")

#modify weekly parameter
mod_irr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Weekly.nc")
#explore the seasonal
mod_noirr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Weekly.nc")
#use the function to create animation for the presentation

#original parameters
og_irr_annual <- paste0(path,"NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Annual.nc" )
og_irr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Weekly.nc")
og_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Annual.nc")
og_noirr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Weekly.nc")

#irrigation mask
irr_mask <- raster("Dymex/Aflatoxin/FAO_Irrigated Areas/CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif")

#extract the data from the NetCDF file using the extract_data_list function

###################1.Explore each variables&visualize the data##################

###EI###
#extract data list for EI
mod_irr_allyear_EI <- extract_data_list(mod_irr_allyear,"EI", years = 1970:2019,
                                        step = "Year")

mod_noirr_allyear_EI <- extract_data_list(mod_noirr_allyear,"EI", years = 1970:2019,
                                        step = "Year")
#create the raster stack, check step = Year/Step (Weekly)
mod_irr_allyear_EI_raster <- create_raster_stack(mod_irr_allyear_EI,
                                                 years = 1970:2019, step = "Year")

mod_noirr_allyear_EI_raster <- create_raster_stack(mod_noirr_allyear_EI,
                                                   years = 1970:2019, step = "Year")

#plot all 50 years of raster data
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
  plot(layer, main = title,zlim =c(0,999))
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
  plot(layer, main = title,zlim =c(0,10))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_WS_raster)) {
  layer <- mod_noirr_allyear_WS_raster[[i]]
  year <- names(mod_noirr_allyear_WS_raster)[i]
  title <- paste("WS_noirr", year)
  plot(layer, main = title,zlim =c(0,10))
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
  plot(layer, main = title,zlim =c(0,10))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_HS_raster)) {
  layer <- mod_noirr_allyear_HS_raster[[i]]
  year <- names(mod_noirr_allyear_HS_raster)[i]
  title <- paste("CS_noirr", year)
  plot(layer, main = title,zlim =c(0,10))
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
  plot(layer, main = title,zlim =c(0,10))
  png(filename = paste0("map_", year, ".png"))
  dev.off()
}
for (i in 1:length(mod_noirr_allyear_DS_raster)) {
  layer <- mod_noirr_allyear_DS_raster[[i]]
  year <- names(mod_noirr_allyear_DS_raster)[i]
  title <- paste("DS_noirr", year)
  plot(layer, main = title,zlim =c(0,10))
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

###################Creating Composite Map######################################

#composite map for yearly data
#define the variables
dname <- c("WS", "TI", "MI", "HS", "GI", "EI", "DS", "DD", "CS", 
           "Core Distribution", "Gen.")

#choose which variable we want to extract
k <- 6

#use nc_open to the dataset
year_irr <- nc_open(mod_irr_annual)
year_noirr <- nc_open(mod_noirr_annual)

#use the custom function get_nc to get the datasets
year_irr_get <- get_nc(year_irr, dname[k])
year_noirr_get <- get_nc(year_noirr, dname[k])

plot(year_irr_get[[1]])

# Crop irrigation layer: 
year_irr_crop <- crop(x = year_irr_get, y = year_noirr_get)

#create a composite map
r_composite <- composite.fun(year_irr_crop, year_irr_get, year_noirr_get)
#plot shows a new range
plot(r_composite)

#use tmap to map the composite map

#read countries shape
World <- rnaturalearth::ne_countries(scale = 10, continent = NULL, 
                                     returnclass = "sf", type = "countries")
World <- rmapshaper::ms_simplify(World, keep = 0.1, keep_shapes = TRUE)
box <- c(-169, -55, 194, 78)
box_sp <- as(extent(box[1],box[3],box[2],box[4]), 'SpatialPolygons')
crs(box_sp) <- "+proj=longlat +datum=WGS84 +no_defs"

max(values(r_composite), na.rm = T)
r_composite_cropped = crop(r_composite, box_sp)
plot(r_composite_cropped)
r_Irr_cropped = crop(year_irr_get, box_sp)
r_NoIrr_cropped = crop(year_noirr_get, box_sp) 

#crop polygon
World_without_antarctica <- World %>% dplyr::filter(sovereignt != "Antarctica")
plot(World_without_antarctica)

cuts_mean=c(0,seq(1,100,1)) # EI
cuts_l_mean <- length(cuts_mean)
cols_brown_yellow_mean <- c("grey", colorRampPalette(c("lightyellow","yellow",
                          "orange","red" ,"red3"))(cuts_l_mean-1))

#composite map
tmap_mode("plot")
composite_map_rescaled <-  tm_shape(r_composite_rescaled,raster.warp = TRUE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 0.8)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_layout(main.title.position = "left",
            main.title.size = 0.9, 
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))
  
tmap_mode("view")
composite_map_rescaled

#irrigated scenario
tmap_mode("plot")
composite_map_irr <-  tm_shape(year_irr_get,raster.warp = TRUE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 0.8)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_layout(main.title.position = "left",
            main.title.size = 0.9, 
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))
tmap_mode("view")
composite_map_irr

#no irrigation map
tmap_mode("plot")
composite_map_noirr <-  tm_shape(year_noirr_get,raster.warp = TRUE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 0.8)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_layout(main.title.position = "left",
            main.title.size = 0.9, 
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))
tmap_mode("view")
composite_map_noirr

####do the same for 1970-2019 datasets###
#choose the variable of interest
dname <- "HS"
dname <- c("EI","TI","MI","CS","HS","DS","WS","GI")
years <- 1970:2019

#use nc_open to open the ncdf file
allyear_irr <- nc_open(mod_irr_allyear)
allyear_noirr <- nc_open(mod_noirr_allyear)

test <- get_nc(allyear_irr, dname = "EI")

# Create a list with variables matrix for each year
AllYears <- vector(mode = "list", length = length(dname))
AllYears.noirr <- vector(mode = "list", length = length(dname))

#loop the data 
#test <- extract_data_list(mod_irr_allyear, dname = dname, years = years, step = "Year")
#only shows -55.75-60.25 for all dname why, need to use get_nc_2 to fix
for (i in 1:length(dname)){
  AllYears[[i]] <- get_nc(allyear_irr,dname = dname[i])
  AllYears.noirr[[i]] <- get_nc(allyear_noirr,dname = dname[i])
}

#preallocate the data
AllYears_r <- vector(mode = "list", length = length(dname))
AllYears_r_noirr <- vector(mode = "list", length = length(dname))

#loop the data
for (i in 1:length(dname)){
  AllYears_r[[i]] <- ffipm::create_raster_stack(AllYears[[i]],years = years, step = "Year")
  AllYears_r_noirr[[i]] <- ffipm::create_raster_stack(AllYears.noirr[[i]],years = years, step = "Year")
}

#remove the first year
for (i in 1:length(dname)){
  AllYears[[i]] <- AllYears[[i]][-c(1)]
  AllYears_r[[i]] <- AllYears_r[[i]][[-c(1)]]
  
  AllYears.noirr[[i]] <- AllYears.noirr[[i]][-c(1)]
  AllYears_r_noirr[[i]] <- AllYears_r_noirr[[i]][[-c(1)]]
}

years <- years[-c(1)]
years_l <- length(years)


climex.sum <- vector(mode = "list", length = length(dname))
climex.sum.noirr <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
   climex.sum[[i]] <- calc(AllYears_r[[i]], fun1)
   climex.sum.noirr[[i]] <- calc(AllYears_r_noirr[[i]], fun1) 
}

plot(climex.sum[[1]][[45:48]])
plot(climex.sum.noirr[[1]][[45:48]])

climex.sum <- vector(mode = "list", length = length(dname))
climex.sum.noirr <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  climex.sum[[i]] <- calc(AllYears_r[[i]], fun1)
  climex.sum.noirr[[i]] <- calc(AllYears_r_noirr[[i]], fun1) 
}

#check the irr mask
plot(irr_mask, main = "FAO Irrigation layer")

# Crop irrigation layer: 
#Irr.r.c <- crop(x = irr_mask, y = climex.sum[[1]])
composite.fun <- function(Irr, Ri, Rn){
  Irr2 <- Irr+1
  Irr2 <- reclassify(Irr2, cbind(2, 0))
  Irr.l <- list(Irr,Irr2)
  Final <- Ri*Irr.l[[1]]+Rn*Irr.l[[2]]
  return(Final)
}

composite.stack <- vector(mode = "list", length = length(dname))
AllYears.composite <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  composite.stack[[i]] <- composite.fun(irr_mask, climex.sum[[i]], climex.sum.noirr[[i]])
  AllYears.composite[[i]] <- composite.fun(irr_mask, AllYears_r[[i]], AllYears_r_noirr[[i]])
}

######modify Dr Anna's code later################

#plot
plot(composite.stack[[1]])
plot(AllYears.composite[[1]])

#create animation using raster_animation

rasters2 = raster::subset(AllYears_r[[1]], 1:40)
rasters_test = raster::subset(mod_irr_allyear_EI_raster, 1:40)
#only 1:40 works not higher why??
world_bbox <- c(-180, -90, 180, 90)

#creating the animation 
raster_animation(raster = rasters_test, filename= "test.mp4", bbox = world_bbox)

###########2.link the map to maize cultivation##########

#load the yield data from MAPSPAM
#check what the rest are later
MAPSPAM_maize <- paste0(path,"NetCDF/MAPSPAM/spam2010V2r0_global_Y_MAIZ_A.tif")
MAPSPAM_maize <- raster(MAPSPAM_maize)
plot(MAPSPAM_maize)
print(MAPSPAM_maize)

# Set the CRS to EPSG:4326 (WGS 84)
projection(MAPSPAM_maize) <- CRS("+init=EPSG:4326")

#getvalues from the cells
maize_val <- getValues(MAPSPAM_maize)

# Filter out NA and zero values
maize_val_filtered <- maize_val[!is.na(maize_val) & maize_val != 0]
print(maize_val_filtered)

# Get the cell indices of non-NA and non-zero cells in the raster
maize_non_zero <- which(!is.na(maize_val) & maize_val != 0)

# Get the associated coordinates for the non-NA and non-zero cells
maize_coord <- as.data.frame(xyFromCell(MAPSPAM_maize, maize_non_zero))

# Add the values as a new column in the data frame
maize_coord$value <- maize_val_filtered

# Print the result
print(maize_coord)

#investigate where there is a high overlap in yield and EI then look at the trend
#AZ, USA


#####3.Fit the linear trend of yearly data to a location to predict the trend###

#inspect the pixels
print(mod_noirr_allyear_EI_raster$X1970)

# Assuming you have already loaded the 'raster' package and read your raster file
# Let's call your raster object 'raster_data'

# Create a matrix of coordinates for all points you want to extract values from
# In this example, we'll use a grid of latitude and longitude values for Bangkok

#test one datapoint
# Latitude and Longitude coordinates for Bangkok, Thailand
latitude <- 13.7562
longitude <- 100.5018

# Create a SpatialPoints object with the coordinates
points <- SpatialPoints(matrix(c(longitude, latitude), ncol = 2), 
                        proj4string = CRS("+proj=longlat +datum=WGS84"))

#preallocate the dataset

# Extract pixel values at the specified locations
extracted_values <- extract(mod_irr_allyear_EI_raster, points)
plot(extracted_values[1:50])

# Assuming you have already extracted the pixel values into 'extracted_values'
# Let's say 'time' represents the corresponding years or time points for the extracted_values

# Subset the first 50 values and corresponding time points
subset_extracted <- extracted_values[1:50]
subset_time <- 1:50

# Create a linear regression model
lm_model <- lm(subset_extracted ~ subset_time)

# Get the coefficients (slope and intercept) of the linear model
slope <- coef(lm_model)[2]  # Slope
intercept <- coef(lm_model)[1]  # Intercept

# Create the linear trend line
trend_line <- slope * subset_time + intercept

# Plot the extracted values and the linear trend line
plot(subset_time, subset_extracted, type = "l", col = "blue", xlab = "Time", ylab = "Extracted Values",
     main = "Linear Trend of Extracted Values", ylim = c(min(subset_extracted), max(subset_extracted)))
lines(subset_time, trend_line, col = "red")
legend("bottomright", legend = c("Extracted Values", "Linear Trend"), col = c("blue", "red"), lty = c(1, 1))
summary(trend_line)

#calculate slope and composite stack
time <- 1:nlayers(composite.stack[[1]]) 

climex.slope <- vector(mode = "list", length=length(dname))

for (i in 1:length(dname)){
  
  climex.slope[[i]] <- calc(composite.stack[[i]], fun2)
  
}

plot(climex.slope[[1]])

#plot the p-value
p <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  
  p[[i]] <- calc(composite.stack[[i]], fun=fun3)
  
}

#plot(p[[1]])

#then mask all values >0.05 to get a confidence level of 95%:#Dr Anna's comment

m = c(0, 0.05, 1, 0.05, 1, 0)

rclmat = matrix(m, ncol=3, byrow=TRUE)

p.mask <- vector(mode = "list", length = length(dname))

p.mask.NA <- vector(mode = "list", length = length(dname))

trend.sig <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  
  p.mask[[i]] = reclassify(p[[i]], rclmat)
  
  p.mask.NA[[i]] = calc(p.mask[[i]], fun4)
  
  trend.sig[[i]] = mask(climex.slope[[i]], p.mask.NA[[i]])
  
}

plot(trend.sig[[3]])

#test extracting values from different raster layer

projection(composite.stack) <- CRS("+init=EPSG:4326")

test <- getValues(trend.sig[[1]])[!is.na(getValues(trend.sig[[1]]))]
p_value_list <- getValues(mod_irr_allyear_EI_raster[[1]])[!is.na(getValues(mod_irr_allyear_EI_raster[[1]]))]


#test
# Get the raster layer
raster_layer <- AllYears.composite[[1]]$layer.1

# Get all values from the raster
EI_value_list_test <- getValues(raster_layer)

# Get all cell indices 
all_cells <- 1:length(EI_value_list_test) #167760 cells

# Get the associated coordinates for all cells (including NA and zero values)
coordinates_df <- as.data.frame(xyFromCell(raster_layer, all_cells))

# Add the values as a new column in the data frame
coordinates_df$value <- EI_value_list_test
coordinates_df

#loop this for all data
#preallocate the dataset
all_composite_EI<- list()

# Loop through all the layers
for (layer_name in names(composite.stack[[1]])) {
  # get the current raster layer
  raster_layer <- composite.stack[[1]][[layer_name]]
  
  # extract all the values
  EI_value_list_test <- getValues(raster_layer)
  
  # get all the cells
  all_cells <- 1:length(EI_value_list_test)
  
  # get all the assosiated coordinates
  coordinates_df <- as.data.frame(xyFromCell(raster_layer, all_cells))
  
  # rename the columns
  colnames(coordinates_df) <- c("longitude", "latitude")
  
  # add the values to the column
  coordinates_df$value <- EI_value_list_test
  
  # add it to the df
  all_composite_EI[[layer_name]] <- coordinates_df
}

#p-values
all_composite_p <- list()
for (i in 1:8) {
  raster_layer <- p[[i]]
  test <- getValues(raster_layer)
  all_cells <- 1:length(test)
  coordinates_df <- as.data.frame(xyFromCell(raster_layer, all_cells))
  colnames(coordinates_df) <- c("longitude", "latitude")
  coordinates_df$value <- test
  all_composite_p[[i]] <- coordinates_df
}

#trend sig
trend_all <- list()
for (i in 1:8) {
  raster_layer <- trend.sig[[i]]
  test <- getValues(raster_layer)
  all_cells <- 1:length(test)
  coordinates_df <- as.data.frame(xyFromCell(raster_layer, all_cells))
  colnames(coordinates_df) <- c("longitude", "latitude")
  coordinates_df$value <- test
  trend_all[[i]] <- coordinates_df
}

#plot the highest trend significant coordinate's EI value which is 
#lon -24.25 and lat 16.75

# Preallocate the list
extracted_values_list <- list()

# Loop through all the layers
for (layer_name in names(composite.stack[[1]])) {
  #get current raster layer
  raster_layer <- composite.stack[[1]][[layer_name]]
  # extract the value at the specific longitude and latitude
  extracted_value <- extract(raster_layer, cbind(-24.25, 16.75))
  # store the extracted value in the list
  extracted_values_list[[layer_name]] <- extracted_value
}

#plotting
plot(1:nlayers(composite.stack[[1]]), unlist(extracted_values_list),
     type = "b", xlab = "Layer", ylab = "EI Value",
     main = "EI Values for lon -24.25 and lat 16.75", col = "blue", pch = 16)
