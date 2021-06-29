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

source("zscripts/PacificConnect_HelpR.R")
mpas <- readRDS("Output/MPAs/PacificOcean_MPAs.rds")

####################################################################################
####### 1a. IUCN/MICO Correct type object (general NO Provninces NO Nodes)
####################################################################################    
iucn_mico <- function(data, cost) {
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
    sps2[,3] <- 1
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
      f2[,3] <- 1
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

####################################################################################
####### 2a. tarrgets IUCN (general NO provinces)
####################################################################################
trg_iucn <- function(data, iucn_df, iucn_target, nsp) {
  Fsf <- readRDS(data) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sps <- gather(data = Fsf, "feature", "value", -c(cellsID, cost)) %>% 
    arrange(cellsID)
  features <- unique(sps$feature)
  # Targets 
  targets <- sps %>% 
    dplyr::filter(value == 1) %>% 
    dplyr::group_by(feature) %>%
    dplyr::summarise(cells = n()) %>% 
    dplyr::mutate(targets = (1 - ((cells/max(cells)) * (1 - 0.10)))) %>% 
    dplyr::arrange(targets)
  targets <- targets[order(match(targets$feature, features)),]
  targets$scientific_name <- nsp$name1
  # Reading IUCN .csv and filtering by Threatened categories
  iucn <- fread(iucn_df, stringsAsFactors = FALSE) %>% 
    dplyr::select(scientific_name, category) %>%
    dplyr::filter(scientific_name %in%  unique(targets$scientific_name)) %>% 
    as_tibble()
  iucn <- distinct(iucn, scientific_name, .keep_all = TRUE) %>% 
    dplyr::mutate(target = ifelse(category %in% c("EX","EW","CR","EN","VU"), 1, 0))
  iucn[3,2] <- "LC"
  # 
  final <- left_join(x = targets, y = iucn,  by = "scientific_name")
  df2 <- final %>% 
    dplyr::group_by(feature) %>% 
    dplyr::mutate(targets2 = ifelse(category %in% c("EX","EW","CR","EN","VU"), iucn_target, targets)) %>% # iucn_target
    dplyr::select(feature, targets2) %>%
    dplyr::rename(targets = targets2)
}

####################################################################################
####### 2a. tarrgets MiCO (general NO NODES)
####################################################################################
trg_Mico <- function(data) {
  Fsf <- readRDS(data) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  sps <- gather(data = Fsf, "feature", "value", -c(cellsID, cost)) %>% 
    arrange(cellsID)
  features <- unique(sps$feature)
  # Targets 
  targets <- sps %>% 
    dplyr::filter(value == 1) %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(cells = n()) %>% 
    dplyr::mutate(targets = (1 - ((cells/max(cells)) * (1 - 0.10)))) %>% 
    dplyr::arrange(targets)
  targets <- targets[order(match(targets$feature, features)),]
  nodes_type <- lapply(features, function(x){f <- paste0(unlist(strsplit(x, "_"))[3], collapse = " ")})
  targets$nodes_type <- unlist(nodes_type)
  df3 <- targets %>% 
    dplyr::group_by(feature) %>% 
    dplyr::mutate(targets2 = ifelse(nodes_type == "OBS", 0.1, 
                             ifelse(nodes_type == "MIG", 0.3,
                             ifelse(nodes_type %in% c("FEE", "WIN", "STO"), 0.3,
                             ifelse(nodes_type == "BRE", 0.7,0.7))))) %>% 
    dplyr::select(feature, targets2) %>%
    dplyr::rename(targets = targets2)
  return(df3)
}









