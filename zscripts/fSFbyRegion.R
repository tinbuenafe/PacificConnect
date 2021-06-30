
longh <- st_read("/Users/bri273/Desktop/VoCC_Prioritizr_global/shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp") %>% 
  st_make_valid() %>% 
  fConvert2PacificRobinson(buff = 0.01)
saveRDS(longh, "Input/Boundaries/Longhurst_world_v4_2010.rds")
plot(st_geometry(longh))


ft_Nodes <- readRDS("Prioritisation/InputsFeatures/02b_MiCONodes.rds")
sps <- gather(data = ft_Nodes, "feature", "value", -c(cellsID, cost, geometry))
features <- unique(sps$feature)
sg <- sps %>% 
  filter(feature == features[3]) %>% 
  filter(value > 0)
st_crs(sg) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

nr <- st_nearest_feature(sg, longh)
unique(nr)

LPs <- sg %>% 
  mutate(ProvCode = as.vector(longh$ProvCode[nr])) %>% 
  mutate(feature2 = paste(feature, ProvCode, sep = "_"))
unique(LPs$ProvCode)
unique(LPs$feature2)
plot(LPs)
