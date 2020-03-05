library(raster)
library(rgdal)
library(sp)
library(tidyverse)

# clear environment
rm(list=ls())

# shapefile for masking
water <- readOGR(dsn = "water", layer = "dk_water")

# directory of source images
ls.fold <- ""
# make list of years rasters belong to, six files per year
year <- ceiling(seq_along(list.files(ls.fold))/6)
# list files for computation, in groups of years
file_lists <- list.files(ls.fold, full.names = T) %>%
  split(year)
# list of names for outputs
outputnames <- paste("dk_ndvi_", seq(1985, 2016), ".tif",sep="")

# mosaic scenes from same year, calculate NDVI and write outputs
for(i in seq_along(file_lists)){
  print(i)
  q <- file_lists[[i]] %>% map(brick) %>%
    map(function(x){
      (x[[4]]-x[[3]])/(x[[4]]+x[[3]])
    }) # NDVI calculation
  img <- mosaic(q[[1]], q[[2]], q[[3]], q[[4]], q[[5]], q[[6]], fun = max) %>%
    mask(water, inverse = T)
  writeRaster(img, format="GTiff", file=outputnames[i], overwrite = T)
}