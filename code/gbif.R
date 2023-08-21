graphics.off()
rm(list = ls())
#package for gathering data from the database
library(rgbif)
#package for mapping
library(tmap)
#package for logging into gbif database
library(usethis)
#mapping coordinate system 
library(sf)
#cleaning gbif dataframe
library(CoordinateCleaner)
#manipulating dataframes .csv
library(dplyr)

source("CABI_Project/code/function.R")

maize_lit <- read.csv("CABI_Project/data/maize_coordinates.csv")

#search for aspergillus flavus 
#occ_search(scientificName = "Aspergillus flavus")
#occ_data(scientificName = "Aspergillus flavus")

#get the unique GBIFtaxonkey
taxon_key <- name_backbone(name="Aspergillus flavus")$usageKey
#the unique ID is 5259820
#name_backbone(name = "Aspergillus flavus")

#login the the gbif database
getOption("pasith", default = NULL)

#download the data
occ_download(pred("taxonKey", 5259820),format = "SIMPLE_CSV")
#why some data said the scientific name is aspergillus oryzae?

#check download time
occ_download_wait('0241261-230224095556074')

#retreive the download data (7886 data points)
gbif <- occ_download_get('0241261-230224095556074') %>%
  occ_download_import()

#explore the data and how it is organized
#names(d$countryCode== "AQ")
#check the different lat&long data
list(gbif$decimalLatitude)

################DATA CLEANING#################################################

#remove the datapoints that doesn't have full data of the coordinate
gbif <- subset(gbif, !is.na(decimalLatitude) | !is.na(decimalLongitude))
#2313 datapoints are left

#remove the datapoints in impossible continents (ex.Antarctica and North pole)

# Filter out datapoints in Antarctica and the North Pole
gbif <- gbif[!is_in_antarctica_or_north_pole(gbif$decimalLatitude, gbif$decimalLongitude), ]

###Further clean the database with CoordinateCleaner package

#remove common degree minute to decimal degree conversion error
cd_ddmm(gbif,  lon = "decimalLongitude", lat = "decimalLatitude")

#clean geographic coordinate by multiple empirical tests
clean_coordinates(gbif,  lon = "decimalLongitude", lat = "decimalLatitude")

#remove duplicated records
cc_dupl(gbif, lon = "decimalLongitude", lat = "decimalLatitude", 
        species = "species", additions = NULL, value = "clean", verbose = TRUE)

#remove records with identical lattitude/longitude
cc_equ(gbif,lon = "decimalLongitude", lat = "decimalLatitude", test = "absolute",
       verbose = TRUE)

#in vicinity of country capital (poorly geo-referenced
#occurrence records in biological databases are often erroneously geo-referenced to capitals.)
cc_cap(gbif, lon = "decimalLongitude", lat = "decimalLatitude")
#53 records removed 

#remove flag records within geographical centroids of politcal countries, often are poor geo-referenced
#occruence data
cc_cen(gbif, lon = "decimalLongitude", lat = "decimalLatitude")
#1 record removed

#remove flagged rcords within 0.5 degree radius around GBIF headquarters
cc_gbif(gbif,  lon = "decimalLongitude", lat = "decimalLatitude")

#remove flagged records near biodiversity institution
cc_inst(gbif,  lon = "decimalLongitude", lat = "decimalLatitude")
#1 record removed

#removed identify invalid lat/lon coordinates
cc_val(gbif,  lon = "decimalLongitude", lat = "decimalLatitude")

#remove zero coordinates
cc_zero(gbif,  lon = "decimalLongitude", lat = "decimalLatitude")

#remove the datapoints with aspergillus oryzae ones (later) & remove unnessary columns
#d <- d[d$scientificName == "Aspergillus flavus", ]
# get the columns that matter for mapping and cleaning the occurrence data:
#myspecies_coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references")]
#head(myspecies_coords)

#convert the dataframe into an sf object which can be used by tmap
gbif_tmap <- st_as_sf(gbif, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
st_crs(gbif_tmap) <- 4326

#export the gbif occurences dataset to
write.csv(gbif, file = "gbif_occurrence.csv")

#####################################MAPPING###################################

#change the mode to plotting
tmap_mode("plot")

data(World, land)

#plot the datapoints
aflavus_map <- tm_shape(World) +
  tm_borders("black", lwd = 1) +
  qtm(World, fill="lightgrey", projection=4326, inner.margins=0) +
tm_shape(gbif_tmap)+
  tm_dots(col = "red", palette = "Set1") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_legend(position = c("left", "bottom"))+
  tm_scale_bar(
    width = 0.1,
    text.size = 0.5,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = c("0.01", "0.08"),
    bg.color = NA,
    bg.alpha = NA,
    just = NA)+
  tm_grid( col = "gray70", alpha = 0) +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")+
  tm_compass(position = c("0.01","0.15"), size = 1, type = "arrow")

#https://www.ebi.ac.uk/ena/browser/view/OL334750 evidence of A.flavus in Antarctica
#tmap_mode("view")
#check the map
#aflavus_map

#export the map
tmap_save(aflavus_map, filename = "aflavus_map.png")

#save the data as a shape file 

#create a spatial object from the GBIF map
#set the output file path

A.flavus_shape <- "file.shp"

#save the spatial object as a shapefile

#preallocate a new df for lattitude and longitude, loop this later
gbif_coords <- data.frame(longitude = rep(0,2287), latitude = rep(0, 2287))

#convert the dataframe into 2 columns of lattitude and longitute
gbif_coords$latitude <- gbif$decimalLatitude
gbif_coords$longitude <- gbif$decimalLongitude

#Create Spatial point objects
gbif_sp <- SpatialPoints(gbif_coords)

#convert to spatial point dataframe
gbif_spdf <- SpatialPointsDataFrame(gbif_sp, gbif_coords)

#convert the SpatialPointsDataFrame into sf
gbif_sf <-st_as_sf(gbif_spdf)

#save the output file
output_file <- "gbif.shp"
st_write(gbif_sf, output_file)
write.csv(gbif_coords, file = "gbif_coordinates.csv")

###############################################################################
#explore the data
print(maize_lit)

# Replace maize_lit data into lon and lat
maize_lit <- maize_lit %>%
  mutate(Longitude = as.numeric(sub(".*\\((.*?)\\s.*", "\\1", WKT)),
         Latitude = as.numeric(sub(".*\\s(.*?)\\)", "\\1", WKT)))

# check the updated tables
print(maize_lit)

#create a spatial object for maize literature
maize.shp <- "file.shp"
st_write(maize_lit, maize.shp)
maize_coords <- data.frame(longitude = rep(0,71), latitude = rep(0, 71))

#convert the dataframe into 2 columns of lattitude and longitute
maize_coords$latitude <- maize_lit$Latitude
maize_coords$longitude <- maize_lit$Longitude

#Create Spatial point objects
maize_sf <- st_as_sf(maize_lit, coords = c("longitude", "latitude"), crs = 4326)

#save the output file
output_file <- "maize.shp"
st_write(maize_sf, output_file)

#map
maize_map <- tm_shape(World) +
  tm_borders("black", lwd = 1) +
  qtm(World, fill="lightgrey", projection=4326, inner.margins=0) +
  tm_shape(maize_sf)+
  tm_dots(col = "red", palette = "Set1") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_legend(position = c("left", "bottom"))+
  tm_scale_bar(
    width = 0.1,
    text.size = 0.5,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = c("0.01", "0.08"),
    bg.color = NA,
    bg.alpha = NA,
    just = NA)+
  tm_grid( col = "gray70", alpha = 0) +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")+
  tm_compass(position = c("0.01","0.15"), size = 1, type = "arrow")
maize_map
tmap_save(maize_map, filename = "maize_map.png")
