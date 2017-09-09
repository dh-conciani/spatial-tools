# Dhemerson Conciani (dh.conciani@gmail.com)
# Batch reclassify
# INPUT:  A list of Remote Sensing Index Images
# OUTPUT: Classified raster's

# Read packages
library (raster)
library (rgdal)
library(tools)

# Get the list of rasters
files <- list.files(path = 'H:/~~/',
                  pattern = '.tif$',
                  full.names = T)
images.count <- length(files)

# Get only basenames
filenames <- file_path_sans_ext(basename(files))

# Generate a reclassification matrix 
# Values less that -1000 are assigned a new value of '1' (burned area)
# and values greater than -1000 assigned a new value of 0 (unburned area)
m <- c(-10000, -1000, 1,  -1000, 20000, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

# Function to reclassify rasters and write a new reclassified .tif file for each
for (i in 1:images.count) {
    #read in raster
    r <- raster(files[[i]])
    #perform the reclassifcation
    rc <- reclassify(r, rclmat)
    #write each reclass to a new file 
    writeRaster(rc, paste0 ('H:/~~/',filenames[i],'.tif'), overwrite=TRUE)
}
