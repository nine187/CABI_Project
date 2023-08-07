#https://cmerow.github.io/RDataScience/3_6_Teaching_Ecoinformatics.html

rm(list=ls())
graphics.off()

#MAXENT

library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(dismo)  
library(rJava)  
library(maptools)
library(jsonlite)

#current environment from worldclim
currentEnv=getData("worldclim", var="bio", res=10)

#future environmental climate scenario for 2070 from HADGEM2-ES model
futureEnv=getData('CMIP5', var='bio', res=10, rcp=85, model='HE', year = 70)
#futureEnv=getData('CSIRO', var='bio', res=10, rcp=85, model='HE', year = 70)
names(futureEnv)=names(currentEnv)

#get the data from GBIF database
Aflavus=gbif('aspergillus', 'flavus')

#limit the number of prdictors
currentEnv=dropLayer(currentEnv, c("bio2", "bio3", "bio4", "bio10", "bio11", "bio13", "bio14", "bio15"))
futureEnv=dropLayer(futureEnv, c("bio2", "bio3", "bio4", "bio10", "bio11", "bio13", "bio14", "bio15"))

#get rid of occurences without location information
Aflavus=subset(Aflavus, !is.na(lon) & !is.na(lat))

#find and eliminate duplicate locations
Aflavusdups = duplicated(Aflavus[, c("lon", "lat")])
Aflavus <-Aflavus[!Aflavusdups, ]

#make initial plot for diagnostic purposes, check this later
#plot(wrld_simpl, xlim=c(min(Aflavus$lon)-1,max(Aflavus$lon)+1),ylim=c(min(Aflavus$lat)-1, max(Aflavus$lat)+1), axes=TRUE, col="light yellow")

####SDM####

# first crop environment to the local species range +/- 10 degrees
model.extent<-extent(min(Aflavus$lon)-10,max(Aflavus$lon)+10,min(Aflavus$lat)-10,max(Aflavus$lat)+10)
modelEnv=crop(currentEnv,model.extent)
modelFutureEnv=crop(futureEnv, model.extent)
#modelMitigatedEnv=crop(mitigatedEnv, model.extent)

# withold 20% of the data for testing the model
Aflavusocc=cbind.data.frame(Aflavus$lon,Aflavus$lat)
fold <- kfold(Aflavusocc, k=5)
Aflavustest <- Aflavusocc[fold == 1, ]
Aflavustrain <- Aflavusocc[fold != 1, ]

#fit the maxent model
Aflavus.me <- maxent(modelEnv, Aflavustrain)

# plot showing importance of each variable
plot(Aflavus.me)

# response curves
response(Aflavus.me)
# predict to entire dataset
Aflavus.pred <- predict(Aflavus.me, modelEnv)

#plot predictions
plot(Aflavus.pred, main="Predicted Suitability")
#map
#map('worldHires', fill=FALSE, add=TRUE)
points(Aflavus$lon, Aflavus$lat, pch="+", cex=0.2)

# make predictions with the future environment data
Aflavus.2070 = predict(Aflavus.me, modelFutureEnv)
#used to be model Mitigated change
Aflavus.mitigated = predict(Aflavus.me, modelFutureEnv)

#plot predictions
plot(Aflavus.2070, main="Predicted Future Suitability")
map('worldHires', fill=FALSE, add=TRUE)
points(Aflavus$lon, Aflavus$lat, pch="+", cex=0.5)

Aflavus.change=Aflavus.2070-Aflavus.pred
Aflavus.mit.change=Aflavus.mitigated-Aflavus.pred
plot(Aflavus.change, main="Predicted Change in Suitability")
map('worldHires', fill=FALSE, add=TRUE)
points(Aflavus$lon, Aflavus$lat, pch="+", cex=0.2)

#testing the model
# background data
bg <- randomPoints(modelEnv, 1000) #background "pseudoabsences"

#simplest way to use 'evaluate'
e1 <- evaluate(Aflavus.me, p=Aflavustest, a=bg, x=modelEnv)

plot(e1, 'ROC')
AflavusChangePoints = extract(Aflavus.change, Aflavusocc)
hist(AflavusChangePoints, main="", xlab="Change in habitat suitability")
abline(v=0, col="red")

AflavusMitChangePoints = extract(Aflavus.mit.change, Aflavusocc)
hist(AflavusChangePoints, main="", x)
abline(v=0, col="red")