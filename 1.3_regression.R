library(sp)
library(rgdal)
library(parallel)
library(BiocParallel)
library(tidyverse)

rm(list=ls())
gc()
?ifelse

# Returns coefficient and p-value for each index point and variable
calc_coef <- function(df, ind) {
  df_i <- df[df$index==ind,]
  ifelse(all(is.na(df_i[,3])),
         tmp_fit <- NA,
         tmp_fit <- lm(as.formula(paste(colnames(df_i)[3], "~", # make sure column is right
                                 "year", sep = "")),
                       data = df_i, na.action=na.exclude))
  ifelse(all(is.na(df_i[,3])),
         coef <- NA,
         coef <- summary(tmp_fit)[[4]][2])
  ifelse(all(is.na(df_i[,3])),
         p <- NA,
         p <- summary(tmp_fit)[[4]][8])
  return(c(ind, coef, p))
}
var_calc <- function(var) {
  tmp_data <- data[,c(3:4,var)] # make sure columns are right
  var_names <- names(data)
  out = NULL
  for (i in unique(tmp_data$index)) {
    tmp <- calc_coef(tmp_data, i)
    out <- rbind(out, tmp)
  }
  out <- out[,-1]
  colnames(out) <- c(paste(var_names[var], "c", sep = "_"),
                     paste(var_names[var], "p", sep = "_"))
  rownames(out) <- c()
  return(out)
}

# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cores
multicoreparam <- MulticoreParam(workers = no_cores)

# Define the Proj.4 spatial reference 
crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# read data
pop_dens <- read_csv(".csv") %>%
  filter(year>1994) %>%
  SpatialPointsDataFrame(coords =  .[,c(1:2)], proj4string = CRS(crs))
colnames(pop_dens@data)[4:6] <- c("p250", "p500", "p1000")
ndvi <- read_delim(".txt", "\t",
                   escape_double = FALSE, trim_ws = TRUE) %>%
  SpatialPointsDataFrame(coords =  .[,c(1:2)], proj4string = CRS(crs))

# create list of municipalities from folder
mun.fold <- ""
file_list = list.files(mun.fold, full.names = T)[55:98]
file_names <- file_list %>% str_replace("", "") %>% str_replace(".gpkg", "")

# apply functions to return coefficients to list of municipalities
# pop
for(i in seq_along(file_list)){
  print(i)
  mun <- readOGR(file_list[i], file_names[i])
  pop <- pop_dens[mun,] %>% .@data
  nat <- ndvi[mun,] %>% .@data
  data <- nat[,c(1:4)] %>%
    inner_join(pop, by = c("x", "y", "year"))
  gc(verbose = F)
  # Parallel computing
  values <- bplapply(5:7, var_calc, BPPARAM = multicoreparam)
  coef <- as.data.frame(cbind(unique(data$index),
                              values[[1]],
                              values[[2]],
                              values[[3]]))
  colnames(coef)[1] <- "index"
  coef <- data[,c(1:3)] %>% #check that columns are right
    distinct(index, .keep_all = T) %>%
    right_join(coef, by = "index")
  write.table(coef, file = "dk95_p_coef.txt", append = T, sep = "\t", row.names = F)
}

# nat
for(i in seq_along(file_list)){
  print(i)
  mun <- readOGR(file_list[i], file_names[i])
  data <- ndvi[mun,] %>% .@data
  gc(verbose = F)
  # Parallel computing
  values <- bplapply(5:7, var_calc, BPPARAM = multicoreparam)
  coef <- as.data.frame(cbind(unique(data$index),
                              values[[1]],
                              values[[2]],
                              values[[3]]))
  colnames(coef)[1] <- "index"
  coef <- data[,c(1:3)] %>% #check that columns are right
    distinct(index, .keep_all = T) %>%
    right_join(coef, by = "index")
  write.table(coef, file = "dk95_n_coef.txt", append = T, sep = "\t", row.names = F)
}

# nat/pop
for(i in seq_along(file_list)){
  print(i)
  mun <- readOGR(file_list[i], file_names[i])
  pop <- pop_dens[mun,] %>% .@data
  nat <- ndvi[mun,] %>% .@data
  data <- nat %>%
    inner_join(pop, by = c("x", "y", "year"))
  data$np250 <- data$n250*900*225/data$p250
  data$np500 <- data$n500*900*861/data$p500
  data$np1000 <- data$n1000*900*3521/data$p1000
  data <- data[,c(1:4,11:13)]
  gc(verbose = F)
  # Parallel computing
  values <- bplapply(5:7, var_calc, BPPARAM = multicoreparam)
  coef <- as.data.frame(cbind(unique(data$index),
                              values[[1]],
                              values[[2]],
                              values[[3]]))
  colnames(coef)[1] <- "index"
  coef <- data[,c(1:3)] %>%
    distinct(index, .keep_all = T) %>%
    right_join(coef, by = "index")
  write.table(coef, file = "dk95_np_coef.txt", append = T, sep = "\t", row.names = F)
}