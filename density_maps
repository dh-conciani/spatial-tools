#This script make a frequency/density maps in 'raster' using vector(s) input.
#Author:  Dhemerson Conciani
#Contact: dh.conciani@gmail.com

#requires
library (rgdal)
library (raster)

#1. read .shp data
#dns is the folder, layer is the shapefile without extension 
focus <- readOGR(dsn= "H:/.../data", layer='data')

#2. check spatial projection
print(proj4string(focus))

# 3. plot a simple map to check
plot(focus, axes=TRUE, border="black", col='red')

# 4. create a raster mask with same data lenght/ projection and 30 meters resolution
mask.focus <- raster(crs = projection(focus), ext = extent(focus))
            res(mask.focus)=30.0
                        
# 5. Count pixel frequency (polygon to raster) 
raster.focus <- rasterize(focus, mask.focus, field = 30, fun = "count", 
                     update = TRUE, updateValue = "NA")

# 6. Plot frequency maps
plot(raster.focus, axes= TRUE)

#export geotiff raster
writeRaster (raster.focus, filename='count_1985_2016.tif',format='GTiff', overwrite=TRUE)
