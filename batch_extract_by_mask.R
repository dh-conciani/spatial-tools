# Dhemerson Conciani (dh.conciani@gmail.com)
# Batch Extract by mask  

# Read packages
library (rgdal)
library (raster)
library (tools)

# Import shapefile or raster to use mask
reference_mask <- readOGR (dsn='H:/~~/', layer='filename')

# Get list of rasters to crop
raster_list <- list.files(path = 'H:/~~/',
                  pattern = '.tif$',
                  full.names = T)
raster_count <- length(raster_list)

# Get only basenames 
names <- file_path_sans_ext(basename(nbr))

# Batch extract by mask 
for (i in 1:raster_count) {
  #read in raster
  r <- raster(raster_list[[i]])
  #perform extract by mask
  croped <- mask(r, reference_mask)
  #write each reclass to a new file 
  writeRaster(croped, paste0 ('H:/nbr/crop/itirapina/eecl/',nbr_names[i],'.tif'), overwrite=TRUE)
}
