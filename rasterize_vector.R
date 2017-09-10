# Dhemerson Conciani (dh.conciani@gmail.com)
# Convert vector to raster and calc map algebra's

# Read .shp data
#dns is the folder, layer is the shapefile without extension 
vector <- readOGR(dsn= "H:/~~/" ,layer='data')

# Create a raster mask with same data lenght/ projection and 30 meters resolution
mask <- raster(crs = projection(vector), ext = extent(vector))
            res(mask)=30.0
                        
# Count pixel frequency (polygon to raster) 
raster <- rasterize(vector, mask, field = 30, fun = "count", 
                     update = TRUE, updateValue = "NA")

# Export Geotiff raster
writeRaster (raster, filename='insert_filename_sum',format='GTiff', overwrite=TRUE)

#########################################################################################
# The same process, but calculating the max value per pixel
# Define your classification $variable as numeric
variable <- as.numeric(paste(vector$variable))

# Calc max value on each pixel 
raster <- rasterize(vector, field= variable , fun = "max", mask,
                              update = TRUE, updateValue = "NA")

# Export Geotiff raster
writeRaster (raster, filename='insert_filename_max',format='GTiff', overwrite=TRUE)
