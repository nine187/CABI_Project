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
#put this in a seperate function file later
is_in_antarctica_or_north_pole <- function(latitude, longitude) {
  in_antarctica <- latitude <= -60 & longitude >= -180 & longitude <= 180
  in_north_pole <- latitude >= 60 & longitude >= -180 & longitude <= 180
  in_antarctica | in_north_pole
}

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
  tm_borders("white", lwd = .5) +
  #check the land cover/cover_cls & modify the legends later
tm_shape(land) +
  tm_raster("cover", palette = terrain.colors(10))+
tm_shape(gbif_tmap)+
  tm_dots(col = "blue", palette = "Set1", title = "City") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))+
#https://www.ebi.ac.uk/ena/browser/view/OL334750 evidence of A.flavus in Antarctica
tmap_mode("view")
#check the map
aflavus_map

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