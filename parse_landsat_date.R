# Dhemerson Conciani (dh.conciani@gmail.com)
# Retrieve date from landsat .tif filenames and use as string 
# INPUT:  Path with raster files
# OUTPUT: Object string

# Read packages
library(stringr)
library(lubridate)
library(tools)

# Get list of raster files
images <- list.files(path = 'H:/~~/',
                  pattern = '.tif$',
                  full.names = T)
images.count <- length(nbr)

# Get names without extension
list_names <- file_path_sans_ext(basename(images))

# To USGS products (TM, ETM+ and OLI) the third separator is the aquisiton date of image, extract this:
parse= sapply(strsplit(list_names, split='_', fixed=TRUE), function(x) (x[3]))

# Convert to date string
date= ymd(parse)

date
