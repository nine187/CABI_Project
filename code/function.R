#####FUNCTION FOR DATA COLLECTION/DATA CLEANING########

#####FUNCTION FOR ANALYSIS############

#function for fixing lat and lon in CLIMEX
get_nc <- function(ncdf,dname){
  lon <- seq(from = -179.75, to = 179.75, by = 0.5) 
  # nlon <- dim(lon)
  # head(lon)
  lat <- seq(from = -89.75, to = 83.75, by = 0.5) 
  r1 <- ncvar_get(ncdf,dname)
  rotate <- function(x) t(apply(x, 2, rev))
  r <- rotate(rotate(rotate(r1)))
  
  vec <- c(339:348)
  # New matrix 'new_r' with all 2, 
  new_r <- matrix(0,nrow=length(lat),ncol=length(lon))  
  # 'new_r' rows getting filled with `r` values
  new_r[-vec,] <- r  
  #Create extent for the raster image
  e <- raster::extent(min(lon) - 0.25, max(lon) + 0.25,
                      min(lat) - 0.25, max(lat) + 0.25)
  
  r_final <- raster::raster(nrow = length(lat), ncol = length(lon),
                            ext = e, crs = "+proj=longlat +datum=WGS84 +no_defs")
  # And assign new_r to the RasterLayer
  raster::values(r_final) <- new_r
  
  return(r_final)
}

#creating composite map
composite.fun <- function(Irr, Ri, Rn){
  Irr2 <- Irr+1
  Irr2 <- reclassify(Irr2, cbind(2, 0))
  Final <- Ri*Irr+Rn*Irr2
  return(Final)
}

#function for calculating linear slope of the entire raster plot
linear_fun <- function(x) {
  if (is.na(x[1])) {
    NA
  } else {
    m = lm(x ~ time)
    summary(m)$coefficients[2]
  }
}

#function for composite map
fun1 <- function(x) { 
  climex.ts = ts(x, start=c(min(years)+1), end=c(max(years)), frequency=1)
  x <- aggregate(climex.ts) 
}

#function for extracting slope?
fun2 = function(x) { if (is.na(x[1])){ NA }
  else { m = lm(x ~ time); summary(m)$coefficients[2] }}


#function for p value
fun3=function(x) { if (is.na(x[1])){ NA }
  else { m = lm(x ~ time); summary(m)$coefficients[8] }}

#function for returning NA to values less than 1
fun4=function(x) { x[x<1] <- NA; return(x)}
