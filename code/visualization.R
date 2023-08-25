rm(list=ls())
graphics.off()

#package for mapping the output
library(tmap)

#help visualize tmap package 
library(rnaturalearth) 

#package for downloading rnaturalearthhires
library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires") 

#geospatial operations
library(rmapshaper) 

#modifying EI values
library(leaflet)

#mapping
library(ggplot2)

#load the data from analysis
load("backup.RData")

#crop polygon
World_without_antarctica <- World %>% dplyr::filter(sovereignt != "Antarctica")
plot(World_without_antarctica)

cuts_mean=c(0,seq(1,100,1)) # EI
cuts_l_mean <- length(cuts_mean)
cols_brown_yellow_mean <- c("grey", colorRampPalette(c("lightyellow","yellow",
                                                       "orange","red" ,"red3"))(cuts_l_mean-1))

#composite map 2016
composite_2016 <-  tm_shape(AllYears.composite[[1]]$layer.46,
                            raster.warp = FALSE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 1)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_borders(lwd = 0.5, col = "black") +  # Add continent borders with specified line width and color
  tm_layout(main.title="2016",
            main.title.size = 1.8, 
            main.title.position = "center",
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))+
  tm_scale_bar(
    width = 0.1,
    text.size = 0.5,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = c("0.07", "0.009"),
    bg.color = NA,
    bg.alpha = NA,
    just = NA)+
  qtm(World, fill=NULL, projection=4326, inner.margins=0) +
  tm_grid( col = "gray70", alpha = 0) +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")
tmap_save(composite_2016, filename = "c_2016.png")

#2017
composite_2017 <-  tm_shape(AllYears.composite[[1]]$layer.47,
                            raster.warp = FALSE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 1)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_borders(lwd = 0.5, col = "black") +  # Add continent borders with specified line width and color
  tm_layout(main.title="2017",
            main.title.size = 1.8, 
            main.title.position = "center",
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))+
  tm_scale_bar(
    width = 0.1,
    text.size = 0.5,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = c("0.07", "0.009"),
    bg.color = NA,
    bg.alpha = NA,
    just = NA)+
  qtm(World, fill=NULL, projection=4326, inner.margins=0) +
  tm_grid( col = "gray70", alpha = 0) +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")
tmap_save(composite_2017, filename = "c_2017.png")

#2018
composite_2018 <-  tm_shape(AllYears.composite[[1]]$layer.48,
                            raster.warp = FALSE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 1)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_borders(lwd = 0.5, col = "black") +  # Add continent borders with specified line width and color
  tm_layout(main.title="2018",
            main.title.size = 1.8, 
            main.title.position = "center",
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))+
  tm_scale_bar(
    width = 0.1,
    text.size = 0.5,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = c("0.07", "0.009"),
    bg.color = NA,
    bg.alpha = NA,
    just = NA)+
  qtm(World, fill=NULL, projection=4326, inner.margins=0) +
  tm_grid( col = "gray70", alpha = 0) +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")
tmap_save(composite_2018, filename = "c_2018.png")

#2019
composite_2019 <-  tm_shape(AllYears.composite[[1]]$layer.49,
                            raster.warp = FALSE, projection="+proj=longlat") +
  tm_raster(style = 'cont',palette =  cols_brown_yellow_mean,
            legend.show = TRUE, title = "EI", alpha = 1)+
  tm_shape(st_geometry(World_without_antarctica), projection="+proj=longlat") +
  tm_fill(col = "grey", alpha = 0.1)+
  tm_borders(lwd = 0.5, col = "black") +  # Add continent borders with specified line width and color
  tm_layout(main.title="2019",
            main.title.size = 1.8, 
            main.title.position = "center",
            earth.boundary = FALSE,
            bg.color = "white",
            space.color="white",
            legend.title.size=1,                  
            legend.text.size = 0.8, 
            fontface="bold",#0.6 or 0.7
            frame = FALSE) +
  tm_legend(position = c("left", "bottom"))+
  tm_scale_bar(
    width = 0.1,
    text.size = 0.5,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = c("0.07", "0.009"),
    bg.color = NA,
    bg.alpha = NA,
    just = NA)+
  qtm(World, fill=NULL, projection=4326, inner.margins=0) +
  tm_grid( col = "gray70", alpha = 0) +
  tm_xlab("Longitude") +
  tm_ylab("Latitude")
tmap_save(composite_2019, filename = "c_2019.png")


# Preallocate the list
extracted_values_list <- list()

#trend of 4 maize
#1.Arizona, USA 58812.4T 31.8, -110.8
# Loop through all the layers
for (layer_name in names(composite.stack[[1]])) {
  #get current raster layer
  raster_layer <- composite.stack[[1]][[layer_name]]
  # extract the value at the specific longitude and latitude
  #extracted_value <- extract(raster_layer, cbind(-24.25, 16.75))
  extracted_value <- extract(raster_layer, cbind(-110.8,31.8))
  # store the extracted value in the list
  extracted_values_list[[layer_name]] <- extracted_value
}

years_glm <- 1:nlayers(composite.stack[[1]])
#for visualization
years_glm2 <- 1:nlayers(composite.stack[[1]]) + 1971
EI <- unlist(extracted_values_list)

glinear_model <- glm(EI ~ years_glm)
glinear_model_vis <- glm(EI ~ years_glm2)
summary(glinear_model)
# plot
ggplot(data = glinear_model, aes(x = years_glm, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model)[1], slope = coef(glinear_model)[2],
              color = "red", linetype = "dashed") +
  labs(title = "AZ, USA (58812.4T kg/ha) ", x = "Years", y = "EI Value", ) +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(0, 50) +
  ylim(0, 30)+
  theme(plot.title = element_text(hjust = 0.5)) 

# Create the plot for visualization (+1971)
ggplot(data = glinear_model_vis, aes(x = years_glm2, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model_vis)[1], slope = coef(glinear_model_vis)[2],
              color = "red", linetype = "dashed") +
  labs(title = "Arizona, USA (58812.4T kg/ha)", x = "Years", y = "EI Value") +
  xlim(1971, 2019) +
  ylim(0, 30) +
  theme(plot.title = element_text(hjust = 0.5)) 

#2. Xinjiang, China 36607.2 41.2, 82.8
for (layer_name in names(composite.stack[[1]])) {
  #get current raster layer
  raster_layer <- composite.stack[[1]][[layer_name]]
  # extract the value at the specific longitude and latitude
  #extracted_value <- extract(raster_layer, cbind(-24.25, 16.75))
  extracted_value <- extract(raster_layer, cbind(82.8, 41.2))
  # store the extracted value in the list
  extracted_values_list[[layer_name]] <- extracted_value
}

years_glm <- 1:nlayers(composite.stack[[1]])
#for visualization
years_glm2 <- 1:nlayers(composite.stack[[1]]) + 1971
EI <- unlist(extracted_values_list)

glinear_model <- glm(EI ~ years_glm)
glinear_model_vis <- glm(EI ~ years_glm2)

# plot
ggplot(data = glinear_model, aes(x = years_glm, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model)[1], slope = coef(glinear_model)[2],
              color = "red", linetype = "dashed") +
  labs(title = "Xinjiang, China 36607.2 ", x = "Years", y = "EI Value", ) +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(0, 50) +
  ylim(0, 30)+
  theme(plot.title = element_text(hjust = 0.5)) 

# Create the plot for visualization (+1971)
ggplot(data = glinear_model_vis, aes(x = years_glm2, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model_vis)[1], slope = coef(glinear_model_vis)[2],
              color = "red", linetype = "dashed") +
  labs(title = "Xinjiang, China (36607.2 kg/ha)", x = "Years", y = "EI Value") +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(1971, 2019) +
  ylim(0, 20) +
  theme(plot.title = element_text(hjust = 0.5)) 

#3. North West Badiah District, Jordan 30860.3 36.2, 32.2
for (layer_name in names(composite.stack[[1]])) {
  #get current raster layer
  raster_layer <- composite.stack[[1]][[layer_name]]
  # extract the value at the specific longitude and latitude
  #extracted_value <- extract(raster_layer, cbind(-24.25, 16.75))
  extracted_value <- extract(raster_layer, cbind(36.2, 32.2))
  # store the extracted value in the list
  extracted_values_list[[layer_name]] <- extracted_value
}

years_glm <- 1:nlayers(composite.stack[[1]])
#for visualization
years_glm2 <- 1:nlayers(composite.stack[[1]]) + 1971
EI <- unlist(extracted_values_list)

glinear_model <- glm(EI ~ years_glm)
glinear_model_vis <- glm(EI ~ years_glm2)

# plot
ggplot(data = glinear_model, aes(x = years_glm, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model)[1], slope = coef(glinear_model)[2],
              color = "red", linetype = "dashed") +
  labs(title = "AZ, USA (58812.4T) ", x = "Years", y = "EI Value", ) +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(20, 50) +
  ylim(20, 50)+
  theme(plot.title = element_text(hjust = 0.5)) 

# Create the plot for visualization (+1971)
ggplot(data = glinear_model_vis, aes(x = years_glm2, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model_vis)[1], slope = coef(glinear_model_vis)[2],
              color = "red", linetype = "dashed") +
  labs(title = "North West Badiah District, Jordan (30860.3kg/ha)", x = "Years", y = "EI Value") +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(1971, 2019) +
  ylim(20, 50) +
  theme(plot.title = element_text(hjust = 0.5)) 

#4. Balkhash District, Kazakhstan 28729.1 Balkhash District, Kazakhstan 76.2 44.8
for (layer_name in names(composite.stack[[1]])) {
  #get current raster layer
  raster_layer <- composite.stack[[1]][[layer_name]]
  # extract the value at the specific longitude and latitude
  #extracted_value <- extract(raster_layer, cbind(-24.25, 16.75))
  extracted_value <- extract(raster_layer, cbind(76.2, 44.8))
  # store the extracted value in the list
  extracted_values_list[[layer_name]] <- extracted_value
}

years_glm <- 1:nlayers(composite.stack[[1]])
#for visualization
years_glm2 <- 1:nlayers(composite.stack[[1]]) + 1971
EI <- unlist(extracted_values_list)

glinear_model <- glm(EI ~ years_glm)
glinear_model_vis <- glm(EI ~ years_glm2)

# plot
ggplot(data = glinear_model, aes(x = years_glm, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model)[1], slope = coef(glinear_model)[2],
              color = "red", linetype = "dashed") +
  labs(title = "AZ, USA (58812.4T) ", x = "Years", y = "EI Value", ) +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(0, 100) +
  ylim(0, 100)+
  theme(plot.title = element_text(hjust = 0.5)) 

# Create the plot for visualization (+1971)
ggplot(data = glinear_model_vis, aes(x = years_glm2, y = EI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3, shape = 16) +
  geom_abline(intercept = coef(glinear_model_vis)[1], slope = coef(glinear_model_vis)[2],
              color = "red", linetype = "dashed") +
  labs(title = "Balkhash District, Kazakhstan (28729.1kg/ha)", x = "Years", y = "EI Value") +
  scale_x_continuous(breaks = seq(0, 50, by = 5), labels = adjust_labels) +
  xlim(1971, 2019) +
  ylim(0, 10) +
  theme(plot.title = element_text(hjust = 0.5)) 