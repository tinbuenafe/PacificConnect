

library(tibble)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(prioritizr)
library(stringr)






####################################################################################
####### 1a. MICO Correct type object (general no NODES)
####################################################################################    
mico1 <- function(data, cost) {
  # Cost
    out <- readRDS(cost) %>%
      dplyr::select(cellsID, cost)
  # Conservation features data manipulation
    sps <- readRDS(data)%>%
      dplyr::mutate(feature = str_remove_all(string = basename(data), pattern = ".rds")) %>% 
      dplyr::select(cellsID, feature, geometry) %>%
      as_tibble() %>%
      dplyr::select(-geometry)
    sps2 <- sps %>% 
      dplyr::mutate_(.dots = setNames(sps, as.character(unique(sps$feature))))
    sps3 <- left_join(out, sps2, "cellsID") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      as.tibble() %>%
      dplyr::arrange(cellsID) %>%
      dplyr::select(-cost, -feature, -geometry, -cellsID)
    return(sps3)
}

####################################################################################
####### 1b. MICO Correct type object (with NODES)
####################################################################################    
mico2 <- function(data, cost) {
  # Cost
    out <- readRDS(cost) %>%
      dplyr::select(cellsID, cost)
  # Conservation features data manipulation
    sps <- readRDS(data) %>%
      dplyr::mutate(feature = str_remove_all(string = basename(data), pattern = ".rds")) %>% 
      dplyr::select(cellsID, feature, Node_Type, geometry) %>%
      as_tibble() %>%
      dplyr::mutate(feature2 = paste(feature, Node_Type, sep = "_")) %>% 
      dplyr::select(-geometry) %>% 
      dplyr::select(cellsID, feature2) %>%
      dplyr::rename(feature = feature2)
    nds <- unique(sps$feature)
    fl <- vector("list", length = length(nds))
    for(i in seq_along(nds)) {
      f1 <- sps %>% 
        dplyr::filter(feature == nds[i])
      f2 <- f1 %>% 
        dplyr::mutate_(.dots = setNames(f1, nds[i]))
      fl[[i]] <- left_join(out, f2, "cellsID") %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        as.tibble() %>%
        dplyr::arrange(cellsID) %>%
        dplyr::select(-cost, -feature, -geometry, -cellsID)
    }
    fll <- do.call(cbind, fl) %>% 
      as_tibble()
    return(fll)
}



# Reading the path where features are
dirF <- list.files(path = "Output/FeaturesMiCO", pattern = ".rds", full.names = TRUE)
fl <- vector("list", length = length(dirF))
for(i in seq_along(fl)) { 
  fl[[i]] <- mico1(data = dirF[i], cost = "Output/Cost/costlayer.rds")
}

out <- readRDS("Output/Cost/costlayer.rds") %>%
  dplyr::select(cellsID, cost)
final <- do.call(cbind, fl) %>%
  as_tibble() %>%
  mutate(cellsID = out$cellsID) %>%
  left_join(out, "cellsID")
# Converting the final element into a sf object
Fsf <- final %>%
  st_as_sf(sf_column_name = "geometry")
saveRDS(Fsf, "Output/PrioritisationInput/02a_MiCO.rds")
plot(st_geometry(Fsf))

