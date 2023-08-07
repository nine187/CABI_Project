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
library(tmap)
library(randomForest)

#load the predictor variables
pred <- load_var(path="",
                 categorical = NULL, format = ".tif", Norm = TRUE, verbose = TRUE,GUI = FALSE)
plot(pred)

#load the species data, lat and long??
occ <- load_occ(path="GitHub/CABI_Project/data/",Env = pred,
                Xcol = 'decimalLongitude', Ycol = 'decimalLatitude', Spcol ='species',
                file = 'gbif_occurrence.csv',header = TRUE,sep = ",", dec = ".", 
                GeoRes = FALSE, verbose = TRUE)

#Spatial Thinning, should geo reso be turned off ?

#load additional data
#borders <- raster::getData("GADM", country="Pakistan",level=0)

#Model the species
#SDM <- modelling('RF', subset(OCC, OCC$species == 'Aspergillus flavus'), pred, 
                # Xcol = 'decimalLatitude', Ycol = 'decimalLongitude')

SDM <- modelling('RF', Occurrences = occ, Env = pred, 
                 Xcol = 'decimalLongitude', Ycol = 'decimalLatitude', trees = 10000,
                 verbose = TRUE)
#str(SDM)
plot(SDM)
plot(SDM@projection)
plot(SDM@binary)
SDM@evaluation
SDM@variable.importance

#GLM
GLM <- modelling('GLM', occ, pred, 
                 Xcol = 'decimalLongitude', Ycol = 'decimalLatitude')
str(GLM)
plot (GLM)
plot (GLM@projection)
#MAXENT
MAXENT <- modelling('MAXENT', occ, pred, 
                    Xcol = 'decimalLongitude', Ycol = 'decimalLatitude')
str(MAXENT)
plot(MAXENT)

#Artificial Neural Network
ANN <-  modelling('ANN', occ, pred, 
                  Xcol = 'decimalLongitude', Ycol = 'decimalLatitude')
plot(ANN)
plot(ANN@projection)

#SVM
SVM <-modelling('SVM', occ, pred, 
                Xcol = 'decimalLongitude', Ycol = 'decimalLatitude')
plot(SVM)
plot(SVM@projection)
