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

#source the functions
source("GitHub/CABI_Project/code/function.R")
#define the path for the files
path <- "C:/Users/Pasith/Documents/Dymex/Aflatoxin/"
#1970-2019 (fifty years) annual data irrigation and no irrigation
mod_irr_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_Irr_Annual_1970-2019.nc")
mod_noirr_allyear <- paste0(path, "NetCDF/A.flavus_modified-param-file_NoIrr_Annual_1970-2019.nc")

#modify annual parameter
mod_irr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Annual.nc")
mod_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Annual.nc")

#modify weekly parameter
mod_irr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Weekly.nc")
mod_noirr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Weekly.nc")

#original parameters
og_irr_annual <- paste0(path,"NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Annual.nc" )
og_irr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_Irr_Weekly.nc")
og_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Annual.nc")
og_noirr_weekly <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Weekly.nc")

#irrigation mask
irr_mask <- paste0(path,"NetCDF/CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif")

#extract the data from the NetCDF file using the extract_data_list function

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

#define the variables
dname <- c("WS", "TI", "MI", "HS", "GI", "EI", "DS", "DD", "CS", 
           "Core Distribution", "Gen.")
#choose which variable we want to extract
k <- 6

#use nc_open to the dataset
year_irr <- nc_open(mod_irr_annual)
year_noirr <- nc_open(mod_noirr_annual)

#use the custom function get_nc to get the datset 
year_irr_get <- get_nc(year_irr, dname[k])
year_noirr_get <- get_nc(year_noirr, dname[k])

plot(year_irr_get[[1]])

# Crop irrigation layer: 
year_irr_crop <- crop(x = year_irr_get, y = year_noirr_get)

#create a composite map
r_composite <- composite.fun(year_irr_crop, year_irr_get, year_noirr_get)

plot(r_composite)

#use tmap to map the composite map

#read countries shape
World <- rnaturalearth::ne_countries(scale = 10, continent = NULL, returnclass = "sf", type = "countries")
World <- rmapshaper::ms_simplify(World, keep = 0.1, keep_shapes = TRUE)
box <- c(-169, -55, 194, 78)
box_sp <- as(extent(box[1],box[3],box[2],box[4]), 'SpatialPolygons')
crs(box_sp) <- "+proj=longlat +datum=WGS84 +no_defs"

max(values(r_composite), na.rm = T)
#crop raster
r_composite_cropped = crop(r_composite, box_sp)
plot(r_composite_cropped)
r_Irr_cropped = crop(year_irr_get, box_sp)
r_NoIrr_cropped = crop(year_noirr_get, box_sp) 

#crop polygon
World_without_antarctica <- World %>% dplyr::filter(sovereignt != "Antarctica")
#plot(World_without_antarctica)

cuts_mean=c(0,seq(1,100,1)) # EI
cuts_l_mean <- length(cuts_mean)
cols_brown_yellow_mean <- c("grey", colorRampPalette(c("lightyellow","yellow",
                          "orange","red" ,"red3"))(cuts_l_mean-1))

map_robin <- tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey") +
  tm_shape(r_composite_cropped,raster.warp =  FALSE, projection="+proj=longlat") +
  tm_raster(style = 'cont', palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI",  breaks = c(seq(0,100,5)), alpha = 0.8) +
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

library(leaflet)

pal <- colorNumeric(c("grey","lightyellow","yellow","orange","red" ,"red3"), c(0,seq(1,100,1)),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(r_composite_cropped, colors = cols_brown_yellow_mean, opacity = 0.8) 

#leaflet map

tmap_mode("view")
map_robin

###weekly data###
week_irr <- nc_open(mod_irr_week)
week_noirr <- nc_open(mod_noirr_week)

#use the custom function get_nc to get the datset 
year_irr_get <- get_nc(week_irr, dname[k])
year_noirr_get <- get_nc(week_noirr, dname[k])

########FITTING LINEAR TRENDS##########################

# Create a raster stack for your time-series data for the analysis of the linear trend 
time <- 1:nlayers(mod_irr_allyear_EI_raster[[1]])  

#function for calculating linear trend in the time series data
fun2 = function(x) { if (is.na(x[1])){ NA } 
  else { m = lm(x ~ time); summary(m)$coefficients[2] }} 

climex.slope <- vector(mode = "list", length=length(dname)) 

for (i in 1:length(dname)){ 
  
  climex.slope[[i]] <- calc(mod_irr_allyear_EI_raster[[i]], fun2) 
  
} 

#####################################################################

#preallocate a list for the data in each year and week
AllYears <- vector(mode = "list", length = length(dname))
AllYears.noirr <- vector(mode = "list", length = length(dname))

#loop the yearly data
for (i in 1:length(dname)){
  AllYears[[i]] <- ffipm::extract_data_list(mod_irr_annual,dname = dname[i],years = years, step = "Year")
  AllYears.noirr[[i]] <- ffipm::extract_data_list(mod_noirr_annual, dname = dname[i],years = years, step = "Year")
}

AllYears_r <- vector(mode = "list", length = length(dname))
AllYears_r_noirr <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  AllYears_r[[i]] <- ffipm::create_raster_stack(AllYears[[i]],years = years, step = "Year")
  AllYears_r_noirr[[i]] <- ffipm::create_raster_stack(AllYears.noirr[[i]],years = years, step = "Year")
}

# As a rule, remove the first year, as results are dubious:
for (i in 1:length(dname)){
  AllYears[[i]] <- AllYears[[i]][-c(1)]
  AllYears_r[[i]] <- AllYears_r[[i]][[-c(1)]]
  
  AllYears.noirr[[i]] <- AllYears.noirr[[i]][-c(1)]
  AllYears_r_noirr[[i]] <- AllYears_r_noirr[[i]][[-c(1)]]
}

years <- years[-c(1)]
years_l <- length(years)

fun1 <- function(x) { 
  climex.ts = ts(x, start=c(min(years)+1), end=c(max(years)), frequency=1)
  x <- aggregate(climex.ts) 
}

climex.sum <- vector(mode = "list", length = length(dname))
climex.sum.noirr <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  climex.sum[[i]] <- calc(AllYears_r[[i]], fun1)
  climex.sum.noirr[[i]] <- calc(AllYears_r_noirr[[i]], fun1) 
}

plot(climex.sum[[1]][[45:48]])
plot(climex.sum.noirr[[1]][[40:43]])

#create a composite layer
plot(irr_mask, main = "FAO Irrigation layer")
irr_r_c <- crop(x = irr_mask, y = climex.sum[[1]])

plot(irr_r_c)

composite.stack <- vector(mode = "list", length = length(dname))
AllYears.composite <- vector(mode = "list", length = length(dname))

for (i in 1:length(dname)){
  composite.stack[[i]] <- composite.fun(irr_r_c, climex.sum[[i]], climex.sum.noirr[[i]])
  AllYears.composite[[i]] <- composite.fun(irr_r_c, AllYears_r[[i]], AllYears_r_noirr[[i]])
}

plot(composite.stack[[1]][[1:4]])

#do the same but for weekly data

###load the weekly data
week_irr <- nc_open(mod_irr_week)
week_noirr <- nc_open(mod_noirr_week)

#use get nc to get the variables (error in one of the files)

##########2.link the map to maize cultivation##########

#load the yield data from MAPSPAM
#check what the rest are later
MAPSPAM <- paste0(path,"NetCDF/MAPSPAM/spam2010V2r0_global_Y_MAIZ_A.tif")

#3.Fit the linear trend of yearly data to a location to predict the trend
###Compare location
path <- "NetCDF/"
setwd(path)

#A.flavus files
file1="A.flavus_CM30_1995H_V2_orig-param-file_Irr_Annual.nc"
file2="A.flavus_CM30_1995H_V2_orig-param-file_NoIrr_Annual.nc"
mask="CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif"

#define which dname will be used
k <- 1

#load the irrigation mask
irr_mask <- raster("Dymex/Aflatoxin/NetCDF/CM30_1995H_V2_gmia_v5_aei_h_classified10ha.tif")

#load the NetCDFfile

#modified parameters
afla_ncdf1 <- nc_open(file1)
afla_ncdf2 <- nc_open(file2)

#Use the get_nc function to fix the lat and lon
r_noirr <- get_nc(afla_ncdf1, dname_2[k])
r_irr <- get_nc(afla_ncdf2, dname_2[k])
plot(r_noirr)
plot(r_irr)

#crop the irrigation layer (no antarctica)
r_irr_crop <- crop(x = irr_mask, y = r_irr)
plot(r_irr_crop)

#create composite raster
r_composite <- composite.fun(r_irr_crop, r_irr, r_noirr)
plot(r_composite, zlim = c(0,100))

#load the country shape data
test <- raster(file, layer = 1)

World <- rnaturalearth::ne_countries(scale = 10, continent = NULL, 
                                     returnclass = "sf", type ="countries") 
World <- rmapshaper::ms_simplify(World, keep = 0.1, keep_shapes = TRUE) 
plot(World)


###try to fit the linear trend to a location in the data

#extract the time-series data rasterstack

# Create a raster stack for your time-series data for the analysis of the linear trend 

###Compare location year

#4. Try to run the parameters file in future climate