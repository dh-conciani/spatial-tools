library(stringr)
library(lubridate)
library(tools)

#get the list of rasters
images <- list.files(path = 'H:/~~/',
                  pattern = '.tif$',
                  full.names = T)
images.count <- length(nbr)

#get names
list_names <- file_path_sans_ext(basename(images))

#extract the third separator (date in Landsat 5, 7 and 8 filenames)
parse= sapply(strsplit(list_names, split='_', fixed=TRUE), function(x) (x[3]))

#convert to date
date= ymd(parse)

date
