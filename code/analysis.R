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

################################################################################
###################1.Explore each variables&visualize the data##################
################################################################################

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

#use the custom function get_nc to get the dataset 
year_irr_get <- get_nc(year_irr, dname[k])
year_noirr_get <- get_nc(year_noirr, dname[k])

plot(year_irr_get[[1]])

# Crop irrigation layer: 
year_irr_crop <- crop(x = year_irr_get, y = year_noirr_get)

#create a composite map
r_composite <- composite.fun(year_irr_crop, year_irr_get, year_noirr_get)
#plot shows a new range
plot(r_composite)

#rescale into 0-60
# Original range
old_min <- 0
old_max <- 10000

# New range
new_min <- 0
new_max <- 60

# Rescale the raster data
r_composite_rescaled <- ((r_composite - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min
plot(r_composite_rescaled)

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

#create a composite map for the 1970-2019 year data
#choose the variable of interest
dname <- "EI"
dname <- c("EI","TI","MI","CS","HS","DS","WS","GI")
years <- 1970:2019

# Create a list with variables matrix for each year
AllYears <- vector(mode = "list", length = length(dname))
AllYears.noirr <- vector(mode = "list", length = length(dname))

#loop the data 
for (i in 1:length(dname)){
  AllYears[[i]] <- ffipm::extract_data_list(mod_irr_allyear,dname = dname[i],years = years, step = "Year")
  AllYears.noirr[[i]] <- ffipm::extract_data_list(mod_noirr_allyear,dname = dname[i],years = years, step = "Year")
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
Irr.r.c <- crop(x = irr_mask, y = climex.sum[[1]])
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
  composite.stack[[i]] <- composite.fun(Irr.r.c, climex.sum[[i]], climex.sum.noirr[[i]])
  AllYears.composite[[i]] <- composite.fun(Irr.r.c, AllYears_r[[i]], AllYears_r_noirr[[i]])
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

###############################################################################
###########2.link the map to maize cultivation##########
###############################################################################

#load the yield data from MAPSPAM
#check what the rest are later
MAPSPAM_maize <- paste0(path,"NetCDF/MAPSPAM/spam2010V2r0_global_Y_MAIZ_A.tif")
MAPSPAM_maize <- raster(MAPSPAM_maize)
plot(MAPSPAM_maize)

#investigate where there is a high overlap in yield and EI then look at the trend

################################################################################
#####3.Fit the linear trend of yearly data to a location to predict the trend###
################################################################################

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

#############
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

plot(p[[1]])

#plot(p, main="p-Value")



# p_points = rasterToPoints(p)

# p_df = data.frame(p_points)

# head(p_df) #breaks will be set to column "layer"

# p_df$cuts=cut(p_df$layer,breaks=c(0,0.05,0.10,0.2,0.3,0.4,0.5,1)) #set breaks

# 

# ggplot(data=p_df) + 

#     geom_tile(aes(x=x,y=y,fill=cuts)) + 

#     scale_fill_brewer("p-value",type = "seq", palette = "RdBu") +

#     coord_equal() +

#     theme_bw() +

#     theme(panel.grid.major = element_blank()) +

#     xlab("Longitude") + ylab("Latitude")



#then mask all values >0.05 to get a confidence level of 95%:

m = c(0, 0.05, 1, 0.05, 1, 0)

rclmat = matrix(m, ncol=3, byrow=TRUE)



fun4=function(x) { x[x<1] <- NA; return(x)}



p.mask <- vector(mode = "list", length = length(dname))

p.mask.NA <- vector(mode = "list", length = length(dname))

trend.sig <- vector(mode = "list", length = length(dname))



for (i in 1:length(dname)){
  
  p.mask[[i]] = reclassify(p[[i]], rclmat)
  
  p.mask.NA[[i]] = calc(p.mask[[i]], fun4)
  
  trend.sig[[i]] = mask(climex.slope[[i]], p.mask.NA[[i]])
  
}



###############################################################################
##########4. Try to run the parameters file in future climate model############
###############################################################################


