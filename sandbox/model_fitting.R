rm(list=ls())
graphics.off()

#dealing with raster
library(raster)
#SDM
library(dismo)

#load the occurrences data
A.flavus_GBIF <- read.csv("data/maize_coordinates.csv")

#convert data to sf object
A.flavus_GBIF <- st_as_sf(A.flavus_GBIF, coords=c('longitude', 'latitude'))
sp_points <- do.call("rbind", A.flavus_GBIF)

#load the current bioclim data
read_bioclim_file <- function(file_number) {
  # Construct the file path for each Bioclim file
  file_path <- paste0("CM10_1975H_Bio", file_number,"_V1.2.txt")
  
  # Read the Bioclim file into a raster object
  bioclim_data <- raster(file_path)
  
  return(bioclim_data)
}

#load the future data, modify later
bioclim_future <- cmip6_world(var='bioc', res=10, ssp="585", 
                              model='HadGEM3-GC31-LL', time="2041-2060", path='data')

# Specify the range of Bioclim files to read (1 to 40)
file_numbers <- 1:40

# Use lapply to read all Bioclim files and store them in a list
bioclim_list <- lapply(file_numbers, read_bioclim_file)
output_dir <- "tif/"
# Loop through the bioclim_list and save each raster as a GeoTIFF
for (i in 1:length(bioclim_list)) {
  output_filename <- paste0("bioclim_", i, ".tif")
  output_path <- file.path(output_dir, output_filename)
  writeRaster(bioclim_list[[i]], output_path, format = "GTiff")
}

# Print summaries of the first few raster objects
for (i in 1:min(length(bioclim_list), 5)) {
  print(summary(bioclim_list[[i]]))
}

#change the bioclim_list into raster stack
bioclim_list <- stack(bioclim_list)

#rename the variables
bioclim_names <- paste0('bio', 1:40)
#print bioclims
print(bioclim_names)
print(bioclim_list)

#plot the bioclim data
plot(bioclim_list[[1]])

#plot the study area
land <- bioclim_list[[1]] >= 0
plot(land)

#calculate the number of points needed to be sample
set.seed(123)
n_pseudo <- nrow(A.flavus_GBIF)

#subset the data into different k groups
A.flavus_GBIF$kfold <- kfold(A.flavus_GBIF, k=5)

#get the coordinate of 80 percent of the training data
train_locs <- st_coordinates(subset(A.flavus_GBIF, kfold != 1))

#sample random points
pseudo_dismo <- randomPoints(mask=as(bioclim_list, 'Raster'), n=n_pseudo)
pseudo_dismo <- st_as_sf(data.frame(pseudo_dismo), coords=c('x','y'), crs=32718)
st_transform(pseudo_dismo)
pseudo_dismo$kfold <- kfold(pseudo_dismo, k=5)

# Create buffers around the observed points
#check what the nearby and too_close are again
#nearby <- st_buffer(A.flavus_GBIF, dist=100000)
#too_close <- st_buffer(A.flavus_GBIF, dist=20000)

# merge those buffers
#nearby <- st_union(nearby)
#too_close <- st_union(too_close)

#subset the training data
train_locs <- subset(A.flavus_GBIF, kfold != 1)
train_sf <- st_as_sf(train_locs, coords = c("longitude", "latitude"), crs = 4326)
train_locs <- st_coordinates(subset(A.flavus_GBIF, kfold != 1))

###############################bioclim#########################################
#model fitting
bioclim_list <- rast(bioclim_list)

bioclim_model <- bioclim(as(bioclim_list, 'Raster'), train_locs)
bioclim_pred <- predict(bioclim_list, bioclim_model)

plot(bioclim_pred)

test_locs <- st_coordinates(subset(A.flavus_GBIF, kfold == 1))
test_pseudo <- st_coordinates(subset(A.flavus_GBIF, kfold == 1))

bioclim_eva <- evaluate(p=test_locs, a=test_pseudo, 
                        model=bioclim_model, x=bioclim_hist)

# Create a copy removing zero scores to focus on within envelope locations
bioclim_non_zero <- bioclim_pred
bioclim_non_zero[bioclim_non_zero == 0] <- NA

plot(bioclim_non_zero, col=hcl.colors(20, palette='Blue-Red'), add=TRUE)

#evaluation

print(bioclim_eva)
par(mfrow=c(1,2))

# Plot the ROC curve
plot(bioclim_eva, 'ROC', type='l')

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

##############################glm##############################################
#do this once you have future dataset
glm_model <- glm(A.flavus_GBIF ~ bioclim_list[[1]] + bioclim_list[[2]], data=A.flavus_GBIF, 
                 family=binomial(link = "logit"),
                 subset=kfold != 1)

####################################maxent#####################################
maxent_model <- maxent(bioclim_stack, sp_points)
plot(maxent_model)
#bio19&22 have high percentage 

#response curve
response(maxent_model)

maxent_model_pred <- predict(maxent_model, bioclim_stack)

#plot predictions
plot(maxent_model_pred, main="Predicted Suitability")
#map
#map('worldHires', fill=FALSE, add=TRUE)
points(Aflavus$lon, Aflavus$lat, pch="+", cex=0.2)

######################################rf#######################################

########################################svm####################################

#######################################ann#####################################