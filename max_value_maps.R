#This script make a max_value pixel maps in 'raster' using vector(s) input with one variable.
#Author:  Dhemerson Conciani
#Contact: dh.conciani@gmail.com

#requires
library (rgdal)
library (raster)

#1. read .shp data
#dns is the folder, layer is the shapefile without extension 
focus <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='data')

#2. check spatial projection
print(proj4string(focus))

# 3. plot a simple map to check
plot(focus, axes=TRUE, border="black", col='red')

# 4. create a raster mask with same data lenght/ projection and 30 meters resolution
mask.focus <- raster(crs = projection(focus), ext = extent(focus))
            res(mask.focus)=30.0

# 7. Define $variable value as numeric
year.focus <- as.numeric(paste(focus$year))

# 8. Last fire occurence (max value $year pixel) 
raster.focus.last <- rasterize(focus, field= year.focus , fun = "max", mask.focus,
                              update = TRUE, updateValue = "NA")

#9. Plot last fire diagnostics
plot(raster.focus.last, axes= TRUE)

writeRaster (raster.focus.last, filename='last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)
