#https://geomoer.github.io/moer-bsc-project-seminar-SDM/unit99/student_tutorials-05b_RF_Heyer.html
rm(list = ls())
graphics.off()
#use to create random forest prediction model
library(SSDM)
#handling raster objects
library(raster)
#rgdal is for similar purposes as raster
library(rgdal)
#mapping package
#library(tmap)

#load the predictor variables
pred <- load_var(path="Documents/CABI_Project/data/wc2.1_10m_bio/")

# Initialize plot window
#plot(pred)

#load the species data
OCC <- load_occ(path="Documents/CABI_Project/data/", pred, 
                Xcol = 'decimalLatitude', Ycol = 'decimalLongitude',
                Spcol = 'species', file = 'gbif_occurrence.csv', sep = ',')
#some occurrences are removed, check why

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
