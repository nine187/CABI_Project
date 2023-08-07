#https://davidorme.github.io/Masters_GIS/practical_2/practical_2.html#the-bioclim-model
rm(list=ls())
graphics.off()

library(terra)
library(geodata)

library(raster)
library(sf)
library(sp)
library(tmap)

library(dismo)

#load the occurrences data
A.flavus_GBIF <- read.csv("GitHub/CABI_Project/data/maize_coordinates.csv")

# Convert to an sf object and set the projection
A.flavus_GBIF <- st_as_sf(A.flavus_GBIF, coords=c('decimalLongitude', 'decimalLatitude'))
#A.flavus_GBIF <- st_as_sf(A.flavus_GBIF, coords=c('longitude', 'latitude'))
st_crs(A.flavus_GBIF) <- 4326

#load the environmental data
bioclim_hist <- worldclim_global(var='bio', res=10, path='data')

#look at the future distribution later
bioclim_future <- cmip6_world(var='bioc', res=10, ssp="585", 
                              model='HadGEM3-GC31-LL', time="2041-2060", path='data')

# Relabel the variables to match between the two dataset
bioclim_names <- paste0('bio', 1:19)
names(bioclim_future) <- bioclim_names
names(bioclim_hist) <- bioclim_names

# Look at the data structure
print(bioclim_hist)

print(bioclim_future)

par(mfrow=c(2,2), mar=c(1,1,1,1))

# Create a shared colour scheme
breaks <- seq(-30, 35, by=2)
cols <- hcl.colors(length(breaks) - 1)

# Plot the historical and projected data
plot(bioclim_hist[[1]], breaks=breaks, col=cols, 
     type='continuous', plg=list(ext=c(190,200,-90,90)))
plot(bioclim_future[[1]], breaks=breaks, col=cols, 
     type='continuous', plg=list(ext=c(190,200,-90,90)))

# Plot the temperature difference
plot(bioclim_future[[1]] - bioclim_hist[[1]], 
     col=hcl.colors(20, palette='Inferno'), breakby='cases',
     type='continuous', plg=list(ext=c(190,200,-90,90)))

#test
#test <-  project(bioclim_hist, 'EPSG:32718')
#ext(test)
#res(test)  # Resolution in metres on X and Y axes

# How many points to create? We'll use the same as number of observations
n_pseudo <- nrow(A.flavus_GBIF)

# Sample the points, change bioclim_hist
#pseudo_dismo <- randomPoints(mask=as(bioclim_hist, 'Raster'), n=n_pseudo, 
                             #p=st_coordinates(A.flavus_GBIF))

pseudo_dismo <- randomPoints(mask=as(bioclim_hist, 'Raster'), n=n_pseudo)

# Convert this data into an sf object, for consistency with the
# next example.
pseudo_dismo <- st_as_sf(data.frame(pseudo_dismo), coords=c('x','y'), crs=32718)
st_transform(pseudo_dismo)

# Create buffers around the observed points
#check what the nearby and too_close are again
#nearby <- st_buffer(A.flavus_GBIF, dist=100000)
#too_close <- st_buffer(A.flavus_GBIF, dist=20000)

# merge those buffers
#nearby <- st_union(nearby)
#too_close <- st_union(too_close)

#map the datapoints and the pseudoabsence data

tmap_mode("plot")
data(World, land)
map_1 <- tm_shape(World) +
  tm_borders("white", lwd = .5) +
  #check the land cover/cover_cls & modify the legends later
  tm_shape(land) +
  tm_raster("cover", palette = terrain.colors(10))+
  tm_shape(pseudo_dismo)+
  tm_dots(col = "blue", palette = "Set1", title = "City") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))
  
map_2 <- tm_shape(World) +
  tm_borders("white", lwd = .5) +
  #check the land cover/cover_cls & modify the legends later
  tm_shape(land) +
  tm_raster("cover", palette = terrain.colors(10))+
  tm_shape(A.flavus_GBIF)+
  tm_dots(col = "blue", palette = "Set1", title = "City") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))

tmap_mode("view")
#pseduo distribution map
map_1
#original gbif map
map_2

#check errors within the map
# Use kfold to add labels to the data, splitting it into 5 parts
A.flavus_GBIF$kfold <- kfold(A.flavus_GBIF, k=5)

# Do the same for the pseudo-random points
pseudo_dismo$kfold <- kfold(pseudo_dismo, k=5)
#pseudo_nearby$kfold <- kfold(pseudo_nearby, k=5)

#get the coordinate of 80 percent of the training data
train_locs <- st_coordinates(subset(A.flavus_GBIF, kfold != 1))

# Fit the model, check later
bioclim_model <- bioclim(as(bioclim_hist, 'Raster'), train_locs)

#check later
bioclim_pred <- predict(bioclim_hist, bioclim_model)

# Create a copy removing zero scores to focus on within envelope locations
bioclim_non_zero <- bioclim_pred
bioclim_non_zero[bioclim_non_zero == 0] <- NA

plot(land, col='grey', legend=FALSE)
plot(bioclim_non_zero, col=hcl.colors(20, palette='Blue-Red'), add=TRUE)

test_locs <- st_coordinates(subset(A.flavus_GBIF, kfold == 1))
test_pseudo <- st_coordinates(subset(pseudo_dismo, kfold == 1))
bioclim_eval <- evaluate(p=test_locs, a=test_pseudo, 
                         model=bioclim_model, x=bioclim_hist)
print(bioclim_eval)
par(mfrow=c(1,2))

# Plot the ROC curve
plot(bioclim_eval, 'ROC', type='l')

# Find the maximum kappa and show how kappa changes with the model threshold
max_kappa <- threshold(bioclim_eval, stat='kappa')
plot(bioclim_eval, 'kappa', type='l')
abline(v=max_kappa, lty=2, col='blue')

# Apply the threshold to the model predictions
A.flavus_range <- bioclim_pred >= max_kappa
plot(A.flavus_range, legend = FALSE, col=c('grey','red'))
plot(st_geometry(A.flavus_GBIF),add = TRUE, pch=0.2, col='#00000088')

# Create a single sf object holding presence and pseudo-absence data.
# - reduce the GBIF data and pseudo-absence to just kfold and a presence-absence value
present <- subset(A.flavus_GBIF, select='kfold')
present$pa <- 1
absent <- pseudo_dismo
absent$pa <- 0
st_crs(present) <- 4326
st_transform(absent)
# - rename the geometry column of absent to match so we can stack them together.
names(absent) <- c('geometry','kfold','pa')
st_geometry(absent) <- 'geometry'

# - stack the two dataframes
pa_data <- rbind(present, absent)
print(pa_data)

envt_data <- extract(bioclim_hist, pa_data)
pa_data <- cbind(pa_data, envt_data)
print(pa_data)

##########################GLM###############################################

glm_model <- glm(pa ~ bio2 + bio4 + bio3 + bio1 + bio12, data=pa_data, 
                 family=binomial(link = "logit"),
                 subset=kfold != 1)

# Look at the variable significances - which are important
summary(glm_model)

# Response plots
par(mar=c(3,3,1,1), mgp=c(2,1,0))
response(glm_model, fun=function(x, y, ...) predict(x, y, type='response', ...))

glm_pred <- predict(bioclim_hist, glm_model, type='response')

# Extract the test presence and absence
test_present <- st_coordinates(subset(pa_data, pa == 1 & kfold == 1))
test_absent <- st_coordinates(subset(pa_data, pa == 0 & kfold == 1))
glm_eval <- evaluate(p=test_present, a=test_absent, model=glm_model, 
                     x=bioclim_hist)
print(glm_eval)

max_kappa <- plogis(threshold(glm_eval, stat='kappa'))
print(max_kappa)

par(mfrow=c(1,2))
# ROC curve and kappa by model threshold
plot(glm_eval, 'ROC', type='l')
plot(glm_eval, 'kappa', type='l')
abline(v=max_kappa, lty=2, col='blue')

par(mfrow=c(2,2))

# Modelled probability
GLM <- plot(glm_pred, col=hcl.colors(20, 'Blue-Red'))
#save the current distribution as .tif file
writeRaster(glm_map, filename = "data/GLM.tif", format = "GTiff")

# Threshold map
glm_map <- glm_pred >= max_kappa
plot(glm_map, legend=FALSE, col=c('grey','red'))

# Future predictions
glm_pred_future <- predict(bioclim_future, glm_model, type='response')
plot(glm_pred_future, col=hcl.colors(20, 'Blue-Red'))

# Threshold map
glm_map_future <- glm_pred_future >= max_kappa
plot(glm_map_future, legend=FALSE, col=c('grey','red'))

table(values(glm_map), values(glm_map_future), dnn=c('hist', '2050'))
b 