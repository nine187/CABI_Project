#https://geomoer.github.io/moer-bsc-project-seminar-SDM/unit99/student_tutorials-05b_RF_Heyer.html
rm(list = ls())
graphics.off()
#use to create random forest prediction model
library(SSDM)
#handling raster objects
library(raster)
#rgdal is for similar purposes as raster
library(rgdal)

#load the climate data from bioclim
home <- "/home/nine187/Documents/Aflatoxin_project/data/"
climate <- paste0(home, "wc2.1_10m_bio/")
occ <- paste0(home, "occ/")

pred <- load_var(path=climate)
# Initialize plot window
dev.new()

plot(pred)

#load the species data
OCC <- load_occ(path=home, pred, Xcol = 'decimalLatitude', Ycol = 'decimalLongitude', Spcol = 'species',
                file = 'gbif_a.flavus.csv', sep = ',')
#error check later

#load additional data
#borders <- raster::getData("GADM", country="Pakistan",level=0)

#Model the species
SDM <- modelling('RF', subset(OCC, OCC$species == 'Aspergillus flavus'), pred, Xcol = 'decimalLatitude', Ycol = 'decimalLongitude')
str(SDM)
plot(SDM)
plot(SDM@projection)
SDM@evaluation
SDM@variable.importance

#GLM
GLM <- modelling('GLM', subset(OCC, OCC$species == 'Aspergillus flavus'), pred, Xcol = 'decimalLatitude', Ycol = 'decimalLongitude')
str(GLM)
plot (GLM)

#MAXENT
MAXENT <- modelling('MAXENT', subset(OCC, OCC$species == 'Aspergillus flavus'), pred, Xcol = 'decimalLatitude', Ycol = 'decimalLongitude')
str(MAXENT)
plot(MAXENT)

#Artificial Neural Network
ANN <- modelling('ANN', subset(OCC, OCC$species == 'Aspergillus flavus'), pred, Xcol = 'decimalLatitude', Ycol = 'decimalLongitude')
plot(ANN)

#SVM
SVM <- modelling('SVM', subset(OCC, OCC$species == 'Aspergillus flavus'), pred, Xcol = 'decimalLatitude', Ycol = 'decimalLongitude')
plot(SVM)


