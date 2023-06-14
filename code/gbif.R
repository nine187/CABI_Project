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
library(rgdal)
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
d <- occ_download_get('0241261-230224095556074') %>%
  occ_download_import()

#explore the data and how it is organized
#names(d$countryCode== "AQ")
#check the different lat&long data
list(d$decimalLatitude)

#remove the datapoints that doesn't have full data of the coordinate
d <- d[!is.na(d$decimalLatitude) & !is.na(d$decimalLongitude), ]
#2313 datapoints are left

#remove the datapoints with aspergillus oryzae ones (later) & remove unnessary columns
#d <- d[d$scientificName == "Aspergillus flavus", ]
# get the columns that matter for mapping and cleaning the occurrence data:
#myspecies_coords <- gbif_data$data[ , c("decimalLongitude", "decimalLatitude", "individualCount", "occurrenceStatus", "coordinateUncertaintyInMeters", "institutionCode", "references")]
#head(myspecies_coords)

#####map the data obtained from GBIF using tmap#####

#convert the dataframe into an sf object which can be used by tmap
d<- st_as_sf(d, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

#####################################MAPPING###################################

#change the mode to plotting
tmap_mode("plot")

data(d, World, land)

#plot the datapoints
aflatoxin_map <- tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA")+
  #check the land cover/cover_cls & modify the legends later
tm_shape(land) +
  tm_raster("cover", palette = terrain.colors(10))+
tm_shape(d)+
  tm_dots(col = "blue", palette = "Set1", title = "City") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))
#https://www.ebi.ac.uk/ena/browser/view/OL334750 evidence of A.flavus in Antarctica

#export the map
tmap_save(aflatoxin_map, filename = "aflatoxin_map.png")