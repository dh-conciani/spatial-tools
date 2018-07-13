## Composite bands for Landsat (OLI scenes) acquired from USGS/ ESPA
## Dhemerson Conciani (dh.conciani@gmail.com)

## Read packages
library (dplyr)
library (lubridate)
library (raster)
library (stringr)
library (tools)
library (plyr)

## Set Work Directory
#setwd ('/media/...')

## Read OLI bands
Aerosol <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band1.tif$')
Blue    <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band2.tif$')
Green   <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band3.tif$')
Red     <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band4.tif$')
NIR     <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band5.tif$')
Swir1   <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band6.tif$')
Swir2   <- list.files ('./imgs/OLI', full.names= TRUE, pattern='sr_band7.tif$')

## Read filenames and extract variables to compose output names
## You can use any band to extract dates, for default band1 are used
filenames <- file_path_sans_ext(basename(Aerosol))
dates = sapply(substr(filenames, start=18, stop=25), function(x) x)
path  = sapply(substr(filenames, start=11, stop=13), function(x) x)
row   = sapply(substr(filenames, start=14, stop=16), function(x) x)

## Compute lenght to batch process
list_count <- length (Aerosol)

## Process "OLI COMPOSITE BANDS" algorithm
for (i in 1:list_count) {
  #read bands
  r_aerosol <- raster (Aerosol[[i]])
  r_blue <- raster (Blue[[i]])
  r_green <- raster (Green[[i]])
  r_red <- raster (Red[[i]])
  r_nir <- raster (NIR[[i]])
  r_swir1 <- raster (Swir1[[i]])
  r_swir2 <- raster (Swir2[[i]])
  temp <- stack (r_aerosol, r_blue, r_green, r_red, r_nir, r_swir1, r_swir2)
  writeRaster (temp, paste0 ('OLI_CDR_',path[[i]],'_',row[[i]],'_',dates[[i]],'_bd1_7_utm'), format='GTiff')
}
