library(tibble)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(prioritizr)
library(stringr)

setwd("/Users/bri273/Downloads/maya/")

####################################################################################
####### 1. Cost
####################################################################################    
  # 
    out <- readRDS("costlayer.rds") %>%
      dplyr::select(cellsID, cost)

####################################################################################
####### 2. Conservation features + tarrgets
####################################################################################    
  # Conservation features data manipulation
    sps <- readRDS("NewMiCOspeciesWithLonghurstProvinces.rds") %>%
      dplyr::select(cellsID, feature, geometry) %>%
      as_tibble() %>%
      dplyr::select(-geometry)
  # Features
    features <- unique(sps$feature)
  # Targets 
    targets <- sps %>% 
      dplyr::group_by(feature) %>%
      dplyr::summarise(cells = n()) %>% 
      dplyr::mutate(targets = (1 - ((cells/max(cells)) * (1 - 0.10)))) %>% 
      dplyr::arrange(targets)
    targets <- targets[order(match(targets$feature, features)),]
    
    test <- lapply(features, function(x){f <- paste0(unlist(strsplit(x, "_"))[2:3], collapse = " ")})
    targets$scientific_name <- unlist(test)
    
   # Reading IUCN .csv and filtering by Threatened categories (Vulberable[VU], Endangered[EN], Critically Endangered[CR])
    iucn <- fread("IUCN_REDLIST_2020.csv", stringsAsFactors = FALSE) %>% 
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
      dplyr::mutate(targets2 = ifelse(category %in% c("EX","EW","CR","EN","VU"), 1, targets)) %>% 
      dplyr::select(feature, targets2) %>%
      dplyr::rename(targets = targets2)

####################################################################################
####### 3. Getting the correct type object
####################################################################################    
  # To save the loop output
    df_list <- vector("list", length(features))
  # Loop
    for(i in 1:length(features)) {
      single <- sps[sps$feature == features[i],]
      single2 <-single %>%
        dplyr::mutate_(.dots = setNames(single, as.character(features[i])))
      df_list[[i]] <- left_join(out, single2, "cellsID") %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        as.tibble() %>%
        dplyr::arrange(cellsID) %>%
        dplyr::select(-cost, -feature, -geometry, -cellsID)
      }
  # getting the final element
    final <- do.call(cbind, df_list) %>%
      as_tibble() %>%
      mutate(cellsID = out$cellsID) %>%
      left_join(out, "cellsID")
  # Converting the final element into a sf object
    final_sf <- final %>%
      st_as_sf(sf_column_name = "geometry")
    # # save it
    #   saveRDS(final_sf, "/Users/bri273/Downloads/maya/object-type.rds")
    
####################################################################################
####### 4. Create the problem
####################################################################################    
  # 
    x <- readRDS("object-type.rds")
  # 
    p2 <- prioritizr::problem(x, features, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(df2$targets) %>%
      add_binary_decisions() %>%
      add_default_solver(verbose = FALSE)
    # saveRDS(p2, "/Users/bri273/Downloads/maya/Problem-NewMiCOspeciesWithLonghurstProvinces.rds")
    
    
    

####################################################################################
####### 5. Solve the problem
####################################################################################  
    
x <- readRDS("object-type.rds")
features <- unique(sps$feature)

p2 <- prioritizr::problem(x, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.5) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)







####################################################################################
####### 5. Solve the problem
####################################################################################  

MicoNodes1 <- readRDS("newdata/NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds") %>%
  dplyr::select(Node_Type, cellsID, feature, geometry) %>%
  as_tibble() %>%
  dplyr::select(-geometry) %>% 
  dplyr::mutate(feature2 = paste(feature, Node_Type, sep = "_")) %>% 
  dplyr::select(cellsID, feature2) %>%
  dplyr::rename(feature = feature2)
plot(st_geometry(MicoNodes1))
MicoNodes2 <- distinct(MicoNodes1, cellsID, .keep_all = TRUE)
plot(st_geometry(MicoNodes2))

test <- MicoNodes[MicoNodes$feature == "SUND_BlackMarlin_MICOdata_SPA", ] %>% 
  as.data.frame()
head(test, 20)

# Features
features_Mico <- unique(MicoNodes$feature)

# Targets 
targets_Nodes <- MicoNodes %>% 
  dplyr::group_by(feature) %>%
  dplyr::summarise(cells = n()) %>% 
  dplyr::mutate(targets = (1 - ((cells/max(cells)) * (1 - 0.10)))) %>% 
  dplyr::arrange(targets)
targets_Nodes <- targets_Nodes[order(match(targets_Nodes$feature, features_Mico)),]

nodes_type <- lapply(features_Mico, function(x){f <- paste0(unlist(strsplit(x, "_"))[4], collapse = " ")})
targets_Nodes$nodes_type <- unlist(nodes_type)

df3 <- targets_Nodes %>% 
  dplyr::group_by(feature) %>% 
  dplyr::mutate(targets2 = ifelse(nodes_type == "OBS", 0.1, 
                           ifelse(nodes_type == "MIG", 0.3,
                           ifelse(nodes_type %in% c("FEE", "WIN", "STO"), 0.3,
                           ifelse(nodes_type == "BRE", 0.7,0.7))))) %>% 
  dplyr::select(feature, targets2) %>%
  dplyr::rename(targets = targets2)



# To save the loop output
df_list <- vector("list", length(features_Mico))
# Loop
for(i in 1:length(features_Mico)) {
  single <- MicoNodes[MicoNodes$feature == features_Mico[i],]
  single2 <-single %>%
    dplyr::mutate_(.dots = setNames(single, as.character(features_Mico[i])))
  df_list[[i]] <- left_join(out, single2, "cellsID") %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    as.tibble() %>%
    dplyr::arrange(cellsID) %>%
    dplyr::select(-cost, -feature, -geometry, -cellsID)
}

# getting the final element
final <- do.call(cbind, df_list) %>%
  as_tibble() %>%
  mutate(cellsID = out$cellsID) %>%
  left_join(out, "cellsID")
# Converting the final element into a sf object
final_sf <- final %>%
  st_as_sf(sf_column_name = "geometry")
# save it
  saveRDS(final_sf, "/Users/bri273/Downloads/maya/X-NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds")
  
  
  p3 <- prioritizr::problem(final_sf, features_Mico, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(df3$targets) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  saveRDS(p3, "/Users/bri273/Downloads/maya/Problem-NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds")
  
  
  
  
  
  
  
  
  
  