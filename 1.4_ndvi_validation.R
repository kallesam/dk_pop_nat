library(raster)
library(rgdal)
library(sp)
library(tidyverse)

rm(list=ls())
gc()

zealand <- readOGR(dsn = "", layer = "")
# read address data
addr_zeal <- readOGR(dsn = "", layer = "")

# REDO THE FOLLOWING FOR EACH DATASET

# directory of source images
ndvi.fold <- ""
# list files for computation
file_list <- list.files(ndvi.fold, full.names = T)
# lists of names for outputs
outputnames_1005_m <- paste("ls_all_m_", seq(1995, 2016), ".tif", sep = "")

# create focal weight
temp_rast <- raster(file_list[1])
w_1005 <- focalWeight(temp_rast, 1005, type='circle')
w_1005 <- w_1005/max(w_1005)
#create empty list to store outputs from loop
out_list <- list()

# load NDVI rasters, do focal calculations and write outputs
for(i in seq_along(file_list)){
  print(i)
  temp_rast <- raster(file_list[[i]]) %>%
    mask(zealand)
  focal(temp_rast, w = w_1005, fun = mean, na.rm = T) %>%
    mask(zealand) %>%
    writeRaster(format="GTiff", file=outputnames_1005_m[i])
}

# NDVI neighbourhood rasters
ndvi.fold <- ""
# create lists of files
file_list = list.files(ndvi.fold, full.names = T)

out_list <- list() #create empty list to store outputs from loop

# load NDVI data, extract values at address points and write outputs
for(i in seq_along(file_list)){
  print(i)
  temp_rast <- raster(file_list[[i]])
  temp_val <- raster::extract(temp_rast, addr_zeal)
  out <- c(mean(temp_val, na.rm = T)) #remove NAs and compute values
  out_list[[i]] <- c(i+1994, out) #store raster path with values
}

ndvi_all_ls <- data.frame(do.call(rbind, out_list)) #convert list to data frame
colnames(ndvi_all_ls) <- c("year", "mean")
#ndvi_mid_mod$mean <- ndvi_mid_mod$mean/10000
write.table(ndvi_all_ls, file = "", sep = "\t", row.names = F)