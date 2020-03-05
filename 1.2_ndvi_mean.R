library(raster)
library(rgdal)
library(tidyverse)

# clear environment
rm(list=ls())

# shapeflies for masking
denmark <- readOGR(dsn = "adm", layer = "DNK_adm1")

# directory of source images
ndvi.fold <- ""
# create list of files
file_list = list.files(ndvi.fold, full.names = T)

# raster of one year to create focal weights
ndvi <- raster(file_list[1])
# create focal weights
w_255 <- focalWeight(ndvi, 255, type='circle')
w_255 <- w_255/max(w_255)
w_495 <- focalWeight(ndvi, 495, type='circle')
w_495 <- w_495/max(w_495)
w_1005 <- focalWeight(ndvi, 1005, type='circle')
w_1005 <- w_1005/max(w_1005)

# lists of names for outputs
outputnames_255_m <- paste("ndvi_255_", seq(1985, 2016), "_a.tif", sep = "")
outputnames_495_m <- paste("ndvi_495_", seq(1985, 2016), "_a.tif", sep = "")
outputnames_1005_m <- paste("ndvi_1005_", seq(1985, 2016), "_a.tif", sep = "")

# load NDVI images, do focal calculations at different buffers and write outputs
for(i in seq_along(file_list)){
  temp_rast <- raster(file_list[[i]])
  print(c(i,255, "m"))
  focal(temp_rast, w = w_255, fun = mean, na.rm = T) %>%
    mask(denmark) %>%
    writeRaster(format="GTiff", file=outputnames_255_a[i])
  print(c(i,495, "m"))
  focal(temp_rast, w = w_495, fun = mean, na.rm = T) %>%
    mask(denmark) %>%
    writeRaster(format="GTiff", file=outputnames_495_a[i])
  print(c(i,1005, "m"))
  focal(temp_rast, w = w_1005, fun = mean, na.rm = T) %>%
    mask(denmark) %>%
    writeRaster(format="GTiff", file=outputnames_1005_a[i])
}