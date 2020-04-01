library(sp)
library(tidyverse)
library(raster)

# clear environment
rm(list=ls())
gc()

# Define the Proj.4 spatial reference 
crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# read address data and create spatial points data frame
addr <- read.table(".txt",
                   header = T) %>%
  SpatialPointsDataFrame(coords =  .[,c(1:2)], proj4string = CRS(crs))

# NDVI data
ndvi.fold <- "ndvi"
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
  write.table(addr@data, file = "geokoor_ndvi.txt", sep = "\t", row.names = F)
}

# save in long format instead
n250 <- read_delim(".txt", "\t",
                   escape_double = FALSE, trim_ws = TRUE) %>%
  select(x:bopindex, ndvi_255_1995_m:ndvi_255_2016_m)
colnames(n250)[3:25] <- c("index", seq(1995, 2016, 1))
n250 <- n250 %>% gather("year", "n250", -x, -y, -index)

n500 <- read_delim(".txt", "\t",
                   escape_double = FALSE, trim_ws = TRUE) %>%
  select(x:bopindex, ndvi_495_1995_m:ndvi_495_2016_m)
colnames(n500)[3:25] <- c("index", seq(1995, 2016, 1))
n500 <- n500 %>% gather("year", "n500", -x, -y, -index)

n1000 <- read_delim(".txt", "\t",
                    escape_double = FALSE, trim_ws = TRUE) %>%
  select(x:bopindex, ndvi_1005_1995_m:ndvi_1005_2016_m)
colnames(n1000)[3:25] <- c("index", seq(1995, 2016, 1))
n1000 <- n1000 %>% gather("year", "n1000", -x, -y, -index)

ndvi <- data.frame(n250, n500$n500, n1000$n1000)
colnames(ndvi)[6:7] <- c("n500", "n1000")
ndvi$year <- as.numeric(ndvi$year)
write.table(ndvi, file = ".txt", sep = "\t", row.names = F)
