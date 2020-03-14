## read libraries
library (raster)
library (sf)
library (mapview)
library (lubridate)
library (dplyr)

## read points
points <- shapefile ("./tombador/points/TOMBADOR_sample_points_FINAL.shp")
points_sf <- st_as_sf (points)

## list ba files
bas <- list.files ("./tombador/ba/", full.names= T, pattern= ".shp$")

## create recipe
df <- as.data.frame(points$sample_ID); names (df)[1] <- "id"
#df$x <- (points$coords.x1); names (df[2]) <- "x"
#df$y <- (points$coords.x2); names (df[3]) <- "y"

## create list of point ids
idT <- as.data.frame(points$sample_ID); names (idT)[1] <- "id"

## start function
for (i in 1:length(bas)) 
{
  ## read ba
  ba <- shapefile (bas[i])
  
  ## convert it to 'sf'
  ba_sf <- st_as_sf(ba)
  
  ## intersect polygons with points and remove geometries
  inter <- st_intersection (ba_sf, points_sf); 
  #st_geometry (inter) = NULL
  
  ## extract year
  date <- sapply (substr(inter$date, start= 1, stop= 4), function(x) x)
  id <- as.data.frame (inter$sample_ID); names(id)[1] = "id"
  temp <- data.frame (id, date)
  
  # match to recipe and join
  temp <- left_join (idT, temp, by= "id")
  if(is.null(levels(temp$date))) {next}
  names (temp)[2] <- as.character (levels(temp$date))
  
  ## paste to perm df
  points_sf <- bind_cols (points_sf, temp[2])
    rm (ba, date, inter, points, temp, id)
}

st_write (points_sf, dsn= "./tombador.shp")


