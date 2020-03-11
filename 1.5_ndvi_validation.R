library(raster)
library(rgdal)
library(sp)
library(tidyverse)

rm(list=ls())

zealand <- readOGR(dsn = "adm", layer = "zealand")
# read address data
addr_zeal <- readOGR(dsn = "addr_shp", layer = "addr_zeal")

# REDO THE FOLLOWING FOR EACH DATASET
# directory of source images
ndvi.fold <- ""
# list files for computation
file_list <- list.files(ndvi.fold, full.names = T)
# lists of names for outputs
outputnames_495_m <- paste("ndvi_zl_m_", seq(1995, 2016), ".tif", sep = "")

# create focal weight
temp_rast <- raster(file_list[1])
w_495 <- focalWeight(ndvi, 495, type='circle')
w_495 <- w_495/max(w_495)
#create empty list to store outputs from loop
out_list <- list()

# load NDVI rasters, do focal calculations and write outputs
for(i in seq_along(file_list)){
  print(i)
  temp_rast <- raster(file_list[[i]]) %>%
    mask(zealand)
  focal(temp_rast, w = w_495, fun = mean, na.rm = T) %>%
    mask(zealand) %>%
    writeRaster(format="GTiff", file=outputnames_495_m[i])
}

# NDVI neighbourhood rasters
ndvi.fold <- ""
# create lists of files
file_list = list.files(ndvi.fold, full.names = T)
file_list

out_list <- list() #create empty list to store outputs from loop

# load NDVI data, extract values at address points and write outputs
for(i in seq_along(file_list)){
  print(i)
  temp_rast <- raster(file_list[[i]])
  temp_val <- raster::extract(temp_rast, addr_zeal)
  out <- c(mean(temp_val, na.rm = T)) #remove NAs and compute values
  out_list[[i]] <- c(i+1994, out) #store raster path with values
}

ndvi_mid_lan <- data.frame(do.call(rbind, out_list)) #convert list to data frame
colnames(ndvi_mid_lan) <- c("year", "mean")
write.table(ndvi_mid_lan, file = "", sep = "\t", row.names = F)