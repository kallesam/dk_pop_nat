library(sp)
library(tidyverse)
library(raster)

# clear environment
rm(list=ls())

# Define the Proj.4 spatial reference 
crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# read address data and create spatial points data frame
addr <- read.table(".txt",
                   header = T) %>%
  SpatialPointsDataFrame(coords =  .[,c(1:2)], proj4string = CRS(crs))

# NDVI data
ndvi.fold <- ""
# create lists of files
file_list = list.files(ndvi.fold, full.names = T)
# variable names
columnnames <- list.files(ndvi.fold) %>%
  str_replace(".tif", "")

# load NDVI data, extract values at address points and write outputs
for(i in seq_along(file_list)){
  print(i)
  temp_rast <- raster(file_list[[i]])
  temp_cols <- colnames(addr@data)
  temp_val <- raster::extract(temp_rast, addr)
  print("values extracted")
  addr <- data.frame(addr@data, temp_val) %>%
    SpatialPointsDataFrame(coords =  .[,c(1:2)],
                           proj4string = CRS(crs))
  colnames(addr@data) <- c(temp_cols, columnnames[i])
  print("data merged")
  write.table(addr@data, file = ".txt", sep = "\t", row.names = F)
}