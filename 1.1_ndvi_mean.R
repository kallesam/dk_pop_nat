library(raster)
library(rgdal)
library(tidyverse)

# clear environment
rm(list=ls())
gc()

# shapefile for masking
denmark <- readOGR(dsn = "", layer = "")
water <- readOGR(dsn = "", layer = "")

# directory of source images
dk_fold <- ""
bh_fold <- ""
# create list of files
dk_list <- list.files(dk_fold, full.names = T) # denmark
bh_list <- list.files(bh_fold, full.names = T) # bornholm

# raster of one year to create focal weights
ndvi <- raster(bh_list[1])
# create focal weights
w_255 <- focalWeight(ndvi, 255, type='circle')
w_255 <- w_255/max(w_255)
w_495 <- focalWeight(ndvi, 495, type='circle')
w_495 <- w_495/max(w_495)
w_1005 <- focalWeight(ndvi, 1005, type='circle')
w_1005 <- w_1005/max(w_1005)

# lists of names for outputs
outputnames_255_m <- paste("ndvi_255_", seq(1995, 2016), "_m.tif", sep = "")
outputnames_495_m <- paste("ndvi_495_", seq(1995, 2016), "_m.tif", sep = "")
outputnames_1005_m <- paste("ndvi_1005_", seq(1995, 2016), "_m.tif", sep = "")

# load NDVI images, do focal calculations at different buffers and write outputs
for(i in seq_along(dk_list)){
  temp_dk <- raster(dk_list[[i]])
  temp_bh <- raster(bh_list[[i]])
  temp_rast <- mosaic(temp_dk, temp_bh, fun = max) %>%
    mask(denmark) %>%
    mask(water, inverse = T)
  print(c(i,255, "m"))
  focal(temp_rast, w = w_255, fun = mean, na.rm = T) %>%
    writeRaster(format="GTiff", file=outputnames_255_m[i])
  print(c(i,495, "m"))
  focal(temp_rast, w = w_495, fun = mean, na.rm = T) %>%
    writeRaster(format="GTiff", file=outputnames_495_m[i])
  print(c(i,1005, "m"))
  focal(temp_rast, w = w_1005, fun = mean, na.rm = T) %>%
    writeRaster(format="GTiff", file=outputnames_1005_m[i])
}
