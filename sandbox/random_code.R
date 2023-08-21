###############################SANDBOX##########################################

###codes that might be useful later###

#linear trend for the whole map

# Create a raster stack for your time-series data for the analysis of the linear trend 
time <- 1:nlayers(mod_irr_allyear_EI_raster)  

#preallocate the list
climex.slope <- vector(mode = "list", length=length(time)) 

for (i in 1:length(time)){ 
  
  climex.slope[[i]] <- calc(mod_noirr_allyear_EI_raster, linear_fun) 
  
} 

plot(climex.slope[[1]])

#explore the irrigation mask data
mask <- raster(irr_mask)
print(mask)
plot(mask)
summary(mask, forceapply = TRUE)

#create a composite raster of irri and noirri maps
composite_raster <- create_composite_raster(file1 = mod_irr_allyear,
                                            file2 = mod_noirr_allyear,
                                            mask = mask,
                                            years_input_file = 1970:2019,
                                            years_raster_stack = 1970:2019,
                                            dname = "EI")

#load the raster file to modify the variable name
#raster_mask <- raster(raster_file)
#plot(raster_mask)

#cant find EI, going to use regular nc package to explore the file
nc_file <- nc_open(mod_irr_annual)
head(nc_file)
variables <- names(nc_file$var)
variables

#tmap version of the loop
# Create a new tmap plot for each iteration
tm_map <- tm_shape(World) +
  tm_raster(mod_noirr_allyear_EI_raster$X1970)+
  tm_dots(col = "blue", palette = "Set1", title = "City") +
  tm_basemap("OpenStreetMap") +
  tm_layout(bg.color = "white", inner.margins = c(0, .02, .02, .02))+
  tm_layout(legend.position = c("left", "bottom"))+
  
  # Save the tmap plot as a PNG file
  tmap_mode("view")
tm_map

# Create a raster stack for your time-series data for the analysis of the linear trend 

time <- 1:nlayers(composite.stack[[1]])  

fun2 = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }} 

climex.slope <- vector(mode = "list", length=length(dname)) 

for (i in 1:length(dname)){ 
  
  climex.slope[[i]] <- calc(composite.stack[[i]], fun2) 
  
} 

#tmap

World <- rnaturalearth::ne_countries(scale = 10, continent = NULL, returnclass = "sf", type ="countries") 

World <- rmapshaper::ms_simplify(World, keep = 0.1, keep_shapes = TRUE) 

#look at the maize yield dataset from FAO in 1970
#summary(maize_70)
#View(maize_70)

#plot the data on the world map
#list(unique(maize_70$Area))
#data(World)
#list(unique(World$name))

#remove unnecessary columns

# name the columns that I want to keep in the dataset
#desired_columns <- c("iso_a3", "name", "geometry")

# identify the columns that are not included in above
#columns_to_remove <- setdiff(colnames(World), desired_columns)

# Remove the unwanted columns
#World <- World[, !(colnames(World) %in% columns_to_remove)]

#match the FAOSTAT data to the country
#add a trial data for 1970 FAOSTAT maize

#remove unnecessary columns

#desired_columns <- c("Area", "Year", "Year.code", "unit", "Value")
#columns_to_remove <- setdiff(colnames(maize_70), desired_columns)
#maize_70 <- maize_70[, !(colnames(maize_70) %in% columns_to_remove)]
#head(maize_70)

#data(World)
# Merge the datasets based on matching values in "Area" and "name" columns
#all.y = TRUE is set so that all the matching column w/o Value won't be erased
#World <- merge(maize_70, World, by.x = "Area", by.y = "name", all.y = TRUE)
#head(World)

######some countries names didn't match, fix this later

#convert World back to sf dataframe class so tmap can read the file
#World_sf <- st_as_sf(World, sf_column_name = "geometry")
#convert World_sf to raster
#World_raster <- raster(World_sf)
#plot(World_raster)

#plot the thing in tmap
#tmap_mode("plot")
#map <- tm_shape(World_sf) +
#tm_polygons("Value") 

#tmap_mode("view")
#map

#plot(mod_irr_allyear_EI_raster$X1970)

#test by converting the raster for 1970 
#raster_irr_70 <- mod_irr_allyear_EI_raster$X1970
#plot(raster_70)
#raster_noirr_70 <- mod_noirr_allyear_EI_raster$X1970
#plot(raster_noirr_70)

#merge the corn map with the irrigation map from before
#merged_map <- merge(raster_irr_70, raster_noirr_70, by = "common_id")
#plot(merged_map)


#test by doing a regression analysis on both irr and no irr
#class(raster_irr_70)
#class(raster_noirr_70)
#class(World_raster)

#extract the values from the raster files
#values1 <- getValues(raster_irr_70)
#values2 <- getValues(raster_noirr_70)
#values3 <- getValues(World_raster)

#creates a new dataframe from the values
#data <- data.frame(values2, values3)
#model <- lm(values1 ~ values3, data = data)
#summary(model)
#plot(model, pch = 1)

#create a regression map from the merged data
#model <- lm(raster_irr_70 ~ raster_noirr_70, data = merged_map)
#look at the maize yield dataset from FAO in 1970
#summary(maize_70)
#View(maize_70)

#plot the data on the world map
#list(unique(maize_70$Area))
#data(World)
#list(unique(World$name))

#remove unnecessary columns

# name the columns that I want to keep in the dataset
#desired_columns <- c("iso_a3", "name", "geometry")

# identify the columns that are not included in above
#columns_to_remove <- setdiff(colnames(World), desired_columns)

# Remove the unwanted columns
#World <- World[, !(colnames(World) %in% columns_to_remove)]

#match the FAOSTAT data to the country
#add a trial data for 1970 FAOSTAT maize

#remove unnecessary columns

#desired_columns <- c("Area", "Year", "Year.code", "unit", "Value")
#columns_to_remove <- setdiff(colnames(maize_70), desired_columns)
#maize_70 <- maize_70[, !(colnames(maize_70) %in% columns_to_remove)]
#head(maize_70)

#data(World)
# Merge the datasets based on matching values in "Area" and "name" columns
#all.y = TRUE is set so that all the matching column w/o Value won't be erased
#World <- merge(maize_70, World, by.x = "Area", by.y = "name", all.y = TRUE)
#head(World)

######some countries names didn't match, fix this later

#convert World back to sf dataframe class so tmap can read the file
#World_sf <- st_as_sf(World, sf_column_name = "geometry")
#convert World_sf to raster
#World_raster <- raster(World_sf)
#plot(World_raster)

#plot the thing in tmap
#tmap_mode("plot")
#map <- tm_shape(World_sf) +
#tm_polygons("Value") 

#tmap_mode("view")
#map

#plot(mod_irr_allyear_EI_raster$X1970)

#test by converting the raster for 1970 
#raster_irr_70 <- mod_irr_allyear_EI_raster$X1970
#plot(raster_70)
#raster_noirr_70 <- mod_noirr_allyear_EI_raster$X1970
#plot(raster_noirr_70)

#merge the corn map with the irrigation map from before
#merged_map <- merge(raster_irr_70, raster_noirr_70, by = "common_id")
#plot(merged_map)


#test by doing a regression analysis on both irr and no irr
#class(raster_irr_70)
#class(raster_noirr_70)
#class(World_raster)

#extract the values from the raster files
#values1 <- getValues(raster_irr_70)
#values2 <- getValues(raster_noirr_70)
#values3 <- getValues(World_raster)

#creates a new dataframe from the values
#data <- data.frame(values2, values3)
#model <- lm(values1 ~ values3, data = data)
#summary(model)
#plot(model, pch = 1)

#create a regression map from the merged data
#model <- lm(raster_irr_70 ~ raster_noirr_70, data = merged_map)


#MAPSPAM

#plot(AllYears_raster_2000)
#try to visualize the plot with ggplot2
#world_map <- map_data("world")
#area_data <- data.frame(area_code = c("US", "CA", "GB", "FR", "JP"), value = c(0, 0, 0, 0, 0))
#merged_data <- merge(world_map, area_data, by.x = "region", by.y = "area_code", all.x = TRUE)
#plot(merged_data$long, merged_data$lat, type = "n", xlim = c(-180, 180), ylim = c(-90, 90), xlab = "lat", ylab = "long")
#polygon(merged_data[c("long", "lat")], col = "lightblue", border = "pink")
#points(merged_data$long, merged_data$lat, pch = 19, cex = merged_data$value / max(merged_data$value) * 2)