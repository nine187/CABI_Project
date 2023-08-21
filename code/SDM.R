rm(list = ls())
graphics.off()

library(SSDM) #model fitting 

#load the predictor variables
pred_hist <- load_var(path="data/tif/",categorical = NULL, format = ".tif", Norm = TRUE, verbose = TRUE,GUI = FALSE)
#plot(pred_hist)

#load the occurrences data for A.flavus gbif
gbif_occ <- load_occ(path="data/",Env = pred_hist,
                Xcol = 'decimalLongitude', Ycol = 'decimalLatitude', Spcol ='species',
                file = 'gbif_occurrence.csv',header = TRUE,sep = ",", dec = ".", 
                GeoRes = FALSE, verbose = TRUE)

#load maize litt data
maize_occ <- load_occ(path="data", Env = pred_hist,Xcol = 'longitude', Ycol = 'latitude',
                file = 'maize_coordinates.csv', Spcol = NULL, header = TRUE, sep = ",", dec = ".")

#load the occurrences data for maize literature
#######################################SDM for maize_occ ######################
#run the ensemble model for GLM, RF, xxx with one occurrences

#ensemble model code 
set.seed(123)
ensemble <- ensemble_modelling(c('GLM', 'RF', 'MAXENT', 'GAM', 'SVM', 'ANN'),
                           Occurrences =occ, Env= pred_hist, rep = 1,
                           Xcol = 'decimalLongitude', Ycol = 'decimalLatitude',
                           PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                           cv = "holdout" , #cross-validation uses holdout method
                           cv.param = c(0.8, 10),# partition 80 percent of the data one time
                           weight = FALSE, #weight using mean of the selection matrix, no fixed threshold as Liu et al., 2005 suggested it's the worst approach
                           uncertainty = TRUE)

ensemble_2 <- ensemble_modelling(c('GLM', 'RF', 'MAXENT', 'GAM', 'SVM', 'ANN'),
                               Occurrences = maize_occ, Env= pred_hist, rep = 1,
                               Xcol = 'longitude', Ycol = 'latitude',
                               PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                               cv = "holdout" , #cross-validation uses holdout method
                               cv.param = c(0.8, 1),# partition 80 percent of the data one time
                               weight = FALSE, #weight using mean of the selection matrix, no fixed threshold as Liu et al., 2005 suggested it's the worst approach
                               uncertainty = TRUE)

ensemble_3 <- ensemble_modelling(c('GLM', 'RF', 'MAXENT', 'GAM', 'SVM', 'ANN'),
                               Occurrences =occ, Env= pred_hist, rep = 1,
                               Xcol = 'decimalLongitude', Ycol = 'decimalLatitude',
                               PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                               cv = "holdout" , #cross-validation uses holdout method
                               cv.param = c(0.8, 1),# partition 80 percent of the data one time
                               weight = TRUE, #weight using mean of the selection matrix, no fixed threshold as Liu et al., 2005 suggested it's the worst approach
                               uncertainty = TRUE)

ensemble_4 <- ensemble_modelling(c('GLM', 'RF', 'MAXENT', 'GAM', 'SVM', 'ANN'),
                                 Occurrences = maize_occ, Env= pred_hist, rep = 1,
                                 Xcol = 'longitude', Ycol = 'latitude',
                                 PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                                 cv = "holdout" , #cross-validation uses holdout method
                                 cv.param = c(0.8, 1),# partition 80 percent of the data one time
                                 weight = TRUE, #weight using mean of the selection matrix, no fixed threshold as Liu et al., 2005 suggested it's the worst approach
                                 uncertainty = TRUE)

ensemble_5 <- ensemble_modelling(c('GLM','SVM','ANN','MARS','RF','GAM','MAXENT'),
                               Occurrences =gbif_occ, Env= pred_hist, rep = 10,
                               Xcol = 'decimalLongitude', Ycol = 'decimalLatitude',
                               PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                               cv = "holdout" , #cross-validation uses holdout method
                               cv.param = c(0.8, 10),# partition 80 percent of the data one time
                               weight = FALSE, #weight using mean of the selection matrix, no fixed threshold as Liu et al., 2005 suggested it's the worst approach
                               uncertainty = TRUE, # calculates uncertainty
                               trees = 1000,replace = TRUE, mtry= sqrt(p),#downsampled the data for RF
                               nprune = 2:20, degree = 1) #downweighted for MARS
#GLM - done
GLM <- modelling('GLM', Occurrences = maize_occ, Env = pred_hist, 
                 Xcol = 'longitude', Ycol = 'latitude',
                 PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                 cv = "holdout" , #cross-validation uses holdout method
                 cv.param = c(0.8, 1),# partition 80 percent of the data one time
                 select.metric = c("AUC"), #AUC
                 verbose = TRUE)
plot(GLM)
plot(GLM@projection)
plot(GLM@binary)
GLM@evaluation
GLM@variable.importance
plot(GLM@algorithm.correlation)
GLM@data

#RF_default - done
RF <- modelling('RF', Occurrences = maize_occ, Env = pred_hist, 
                 Xcol = 'longitude', Ycol = 'latitude',
                PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                cv = "holdout" , #cross-validation uses holdout method
                cv.param = c(0.8, 1),# partition 80 percent of the data one time
                select.metric = c("AUC"), #AUC
                 verbose = TRUE)
plot(RF)
plot(RF@projection)
plot(RF@binary)
RF@evaluation
RF@variable.importance
plot(RF@algorithm.correlation)
RF@data

#RF_tuned 
RF_tuned <- modelling('RF', Occurrences = maize_occ, Env = pred_hist, 
                Xcol = 'longitude', Ycol = 'latitude', trees = 1000,
                PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                cv = "holdout" , #cross-validation uses holdout method
                cv.param = c(0.8, 1),# partition 80 percent of the data one time
                select.metric = c("AUC"), #AUC
                verbose = TRUE)
plot(RF)
plot(RF@projection)
plot(RF@binary)
RF@evaluation
RF@variable.importance
plot(RF@algorithm.correlation)
RF@data

#maxent - done
MAXENT <- modelling('MAXENT', Occurrences = maize_occ, Env = pred_hist, 
                Xcol = 'longitude', Ycol = 'latitude',
                PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                cv = "holdout" , #cross-validation uses holdout method
                cv.param = c(0.8, 1),# partition 80 percent of the data one time
                select.metric = c("AUC"), #AUC
                verbose = TRUE)
plot(MAXENT)
plot(MAXENT@projection)
plot(MAXENT@binary)
MAXENT@evaluation
MAXENT@variable.importance
plot(MAXENT@algorithm.correlation)
MAXENT@data

#ANN - done
ANN <- modelling('ANN', Occurrences = maize_occ, Env = pred_hist, 
                 Xcol = 'longitude', Ycol = 'latitude',
                 PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                 cv = "holdout" , #cross-validation uses holdout method
                 cv.param = c(0.8, 10),# partition 80 percent of the data ten times
                 metric = "CCR", #proportion (Liu et al., 2005) https://www.researchgate.net/publication/230246974_Selecting_Thresholds_of_Occurrence_in_the_Prediction_of_Species_Distributions
                 axes.metric = "prop.correct", 
                 select.metric = c("prop.correct"),
                 verbose = TRUE)
plot(ANN)
plot(ANN@projection)
plot(ANN@binary)
ANN@evaluation
ANN@variable.importance
plot(ANN@algorithm.correlation)
ANN@data

#SVM - done
SVM <- modelling('SVM', Occurrences = maize_occ, Env = pred_hist, 
                 Xcol = 'longitude', Ycol = 'latitude',
                 PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                 cv = "holdout" , #cross-validation uses holdout method
                 cv.param = c(0.8, 1),# partition 80 percent of the data
                 metric = "CCR", #proportion of allmodel as Liu et al., 2005 suggested
                 axes.metric = "prop.correct", 
                 select.metric = c("prop.correct"),
                 verbose = TRUE)
plot(SVM)
plot(SVM@projection)
plot(SVM@binary)
SVM@evaluation
test <- SVM@variable.importance
plot(SVM@algorithm.correlation)
SVM@data

#GAM - done
GAM <- modelling('GAM', Occurrences = maize_occ, Env = pred_hist, 
                 Xcol = 'longitude', Ycol = 'latitude',
                 PA = NULL, #default Barbet-Massin et al. (2012) recommendation
                 cv = "holdout" , #cross-validation uses holdout method
                 cv.param = c(0.8, 1),# partition 80 percent of the data one time
                 select.metric = c("AUC"), #AUC
                 verbose = TRUE)
plot(GAM)
plot(GAM@projection)
plot(GAM@binary)
GAM@evaluation
GAM@variable.importance
plot(GAM@algorithm.correlation)
GAM@data

#binary plot
plot(ANN@binary)
plot(GAM@binary)
plot(GLM@binary)
plot(MAXENT@binary)
plot(RF@binary)
plot(SVM@binary)

#extract evaluation stat
eva <- data.frame(ensemble_5@algorithm.evaluation)
eva$AUC <- round(eva$AUC, digits = 2)
eva$Kappa <- round(eva$Kappa, digits = 2)
eva$threshold <- round(eva$threshold, digits = 2)
eva$sensitivity <- round(eva$sensitivity, digits = 2)
eva$specificity <- round(eva$specificity, digits = 2)
eva$prop.correct <- round(eva$prop.correct, digits = 2)
#extract the AUC, kappa, threshold sens, proportion correct
eva_visual <- data.frame(eva$AUC, eva$Kappa, eva$threshold, eva$sensitivity,
                         eva$specificity, eva$prop.correct)
#round up decimal points
eva_visual <- round(eva_visual, digits = 2)

# Print the formatted data frame
print(eva_visual)
write.csv(eva_visual, "ensemble_eva")
#compile all the data in one table

#convert data to sf object
#A.flavus_GBIF <- st_as_sf(A.flavus_GBIF, coords=c('longitude', 'latitude'))
#sp_points <- do.call("rbind", A.flavus_GBIF)

#load the current bioclim data
#read_bioclim_file <- function(file_number) {
  # Construct the file path for each Bioclim file
  #file_path <- paste0("CM10_1975H_Bio", file_number,"_V1.2.txt")
  
  # Read the Bioclim file into a raster object
  #bioclim_data <- raster(file_path)
  
  #return(bioclim_data)
#}

#load the future data, modify later
#bioclim_future <- cmip6_world(var='bioc', res=10, ssp="585", 
                              #model='HadGEM3-GC31-LL', time="2041-2060", path='data')

# Specify the range of Bioclim files to read (1 to 40)
#file_numbers <- 1:40

# Use lapply to read all Bioclim files and store them in a list
#bioclim_list <- lapply(file_numbers, read_bioclim_file)
#output_dir <- "tif/"
# Loop through the bioclim_list and save each raster as a GeoTIFF
#for (i in 1:length(bioclim_list)) {
  #output_filename <- paste0("bioclim_", i, ".tif")
  #output_path <- file.path(output_dir, output_filename)
  #writeRaster(bioclim_list[[i]], output_path, format = "GTiff")
#}
