# Dhemerson Conciani (dh.conciani@gmail.com)
# Reproject list of rasters using reference

# Read packages
library (raster)
library (rgdal)

# Read archive with reference projection (In this example, a shapefile)
reference <- readOGR (dsn='H:/~~/', layer='file_name')

# Get reference projection
ref_proj <- crs(reference)

# Get the list of rasters
raster_list <- list.files(path = 'H:/~~/',
                  pattern = '.tif$',
                  full.names = T)
list_count <- length(raster_list)

# Get only basenames
list_names <- file_path_sans_ext(basename(raster_list))

# Reproject using reference 
for (i in 1:list_count) {
  #read in raster
  r <- raster(raster_list[[i]])
  #perform reprojection
  projected_raster <- projectRaster(r, crs = ref_proj)
  #write each reprojected raster to a new file 
  writeRaster(projected_raster, paste0 ('H:/~~/',raster_list[i],'.tif'), overwrite=TRUE)
}
