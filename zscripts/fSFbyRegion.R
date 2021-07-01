
longh <- st_read("/Users/bri273/Desktop/VoCC_Prioritizr_global/shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp") %>% 
  st_make_valid() %>% 
  fConvert2PacificRobinson(buff = 0.01)
saveRDS(longh, "Input/Boundaries/Longhurst_world_v4_2010.rds")

longh <- readRDS("Input/Boundaries/Longhurst_world_v4_2010.rds")
st_crs(longh) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

ft_Nodes <- readRDS("Prioritisation/InputsFeatures/02b_MiCONodes.rds")
sps <- gather(data = ft_Nodes, "feature", "value", -c(cellsID, cost, geometry))
features <- unique(sps$feature)
for(i in seq_along(features)) {
  sg <- sps %>% 
    filter(feature == features[i]) %>% 
    filter(value > 0)
  st_crs(sg) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  nr <- st_nearest_feature(sg, longh)
  sf <- sg %>% 
    mutate(ProvCode = as.vector(longh$ProvCode[nr]))
  
  ftLong <- unique(sf$ProvCode)
  for(j in seq_along(ftLong)) {
    sf2 <- sf %>% 
      filter(ProvCode == ftLong[j])
    sff <- sf2 %>% 
      dplyr::select(cellsID)
    saveRDS(sff, paste0("Output/FeaturesMiCONewLong/", paste(sf2$feature[1], sf2$ProvCode[1], sep = "_"), ".rds"))
  }
}

ft_iucn <- readRDS("Prioritisation/InputsFeatures/01a_IUCN.rds")
sps2 <- gather(data = ft_iucn, "feature", "value", -c(cellsID, cost, geometry))
features2 <- unique(sps2$feature)
for(i in seq_along(features2)) {
  sg <- sps2 %>% 
    filter(feature == features2[i]) %>% 
    filter(value > 0)
  st_crs(sg) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  nr <- st_nearest_feature(sg, longh)
  sf <- sg %>% 
    mutate(ProvCode = as.vector(longh$ProvCode[nr]))
  
  ftLong <- unique(sf$ProvCode)
  for(j in seq_along(ftLong)) {
    sf2 <- sf %>% 
      filter(ProvCode == ftLong[j])
    sff <- sf2 %>% 
      dplyr::select(cellsID)
    saveRDS(sff, paste0("Output/FeaturesIUCNNewLong/", paste(sf2$feature[1], sf2$ProvCode[1], sep = "_"), ".rds"))
  }
}

