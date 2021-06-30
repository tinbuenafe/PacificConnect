# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au/ibritomorales@gmail.com)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("zscripts/00a_fCleaningDefining.R")

####################################################################################
####### 1. Running to get the correct input data for prioritizr
####################################################################################    
# Reading the path where features are
dirF <- list.files(path = "Output/FeaturesIUCNNew", pattern = ".rds", full.names = TRUE)
fl <- vector("list", length = length(dirF))
for(i in seq_along(fl)) {fl[[i]] <- iucn_mico(data = dirF[i], cost = "Output/Cost/costlayer.rds")}
#  getting the cost
out <- readRDS("Output/Cost/costlayer.rds") %>%
  dplyr::select(cellsID, cost)
# merge elements and adding ID+Cost
final <- do.call(cbind, fl) %>%
  as_tibble() %>%
  mutate(cellsID = out$cellsID) %>%
  left_join(out, "cellsID")
# Converting the final element into a sf object and save it
Fsf <- final %>%
  st_as_sf(sf_column_name = "geometry")
saveRDS(Fsf, "Prioritisation/InputsFeatures/01a_IUCN.rds")

####################################################################################
####### 2. Targets
#################################################################################### 
tIUCN <- trg_Mico("Prioritisation/InputsFeatures/02b_MiCONodes.rds")
  saveRDS(tMiCO, "Prioritisation/InputsTargets/03b_TRGMiCONodes.rds")

####################################################################################
####### 1. Running to get the correct input data for prioritizr
####################################################################################    
# Reading the path where features are
  dirF <- list.files(path = "Output/FeaturesMiCONew", pattern = ".rds", full.names = TRUE)
  fl <- vector("list", length = length(dirF))
  for(i in seq_along(fl)) {fl[[i]] <- mico2(data = dirF[i], cost = "Output/Cost/costlayer.rds")}
  #  getting the cost
    out <- readRDS("Output/Cost/costlayer.rds") %>%
      dplyr::select(cellsID, cost)
  # merge elements and adding ID+Cost
    final <- do.call(cbind, fl) %>%
      as_tibble() %>%
      mutate(cellsID = out$cellsID) %>%
      left_join(out, "cellsID")
  # Converting the final element into a sf object and save it
    Fsf <- final %>%
      st_as_sf(sf_column_name = "geometry")
    saveRDS(Fsf, "Prioritisation/Inputs/02b_MiCONodes.rds")
    
####################################################################################
####### 2. Targets
#################################################################################### 
  tMiCO <- trg_Mico("Prioritisation/InputsFeatures/02b_MiCONodes.rds")
    saveRDS(tMiCO, "Prioritisation/InputsTargets/03b_TRGMiCONodes.rds")

    
    
    
    