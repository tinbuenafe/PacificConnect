# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au/ibritomorales@gmail.com)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(tibble)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(prioritizr)
library(stringr)

####################################################################################
####### 
####################################################################################    

match1 <- function(data, cost, output) {
  # Cost
  out <- readRDS(cost) %>%
    dplyr::select(cellsID, cost)
  out <- cbind(out, st_coordinates(st_centroid(out)))
  # Conservation features data manipulation
  sps <- readRDS(data)
  sps <- cbind(sps, st_coordinates(st_centroid(sps))) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  # Matching
  dff <- left_join(out, sps, by = c("X" = "X", "Y" = "Y")) %>% 
    na.omit() %>% 
    dplyr::select(-cellsID.y, -cost, -X, -Y) %>% # -area_km2 just for MiCO remove for  IUCN
    dplyr::rename(cellsID = cellsID.x)
  saveRDS(dff, paste0(output, basename(data)))
  return(dff)
}

dirF <- list.files(path = "Input/FeaturesIUCN", pattern = ".rds", full.names = TRUE)
for(i in seq_along(dirF)) {match1(data = dirF[i], cost = "Output/Cost/costlayer.rds", output = "Output/FeaturesIUCNNew/")}




