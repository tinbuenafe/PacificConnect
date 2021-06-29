# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au/ibritomorales@gmail.com)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(raster)
library(ggplot2)
library(prioritizr)
library(gurobi)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(dplyr)
library(readr)
library(ggtext)
library(stringr)
library(data.table)

source("zscripts/PacificConnect_HelpR.R")
# MPAs sf
mpas <- readRDS("Output/MPAs/PacificOcean_MPAs.rds")


ft_Nodes <- readRDS("Prioritisation/InputsFeatures/02b_MiCONodes.rds")
sps <- gather(data = ft_Nodes, "feature", "value", -c(cellsID, cost, geometry))
features <- unique(sps$feature)

gg_list <- vector("list", length = length(features))
for(i in seq_along(features)) {
  sg <- sps %>% 
    filter(feature == features[i]) %>% 
    filter(value > 0)
  st_crs(sg) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  ggplot() +
    geom_sf(data = sg, color = "#31a354", fill = "#31a354") +
    geom_sf(data = world_robinson, size = 0.05, fill = "grey20") +
    coord_sf(xlim = c(st_bbox(sps)$xmin, st_bbox(sps)$xmax),
             ylim = c(st_bbox(sps)$ymin, st_bbox(sps)$ymax),
             expand = TRUE) +
    theme_bw() + 
      ggsave(paste0("Figures/Features/", paste0(unique(sg$feature), ".png")), width = 15, height = 10, dpi = 300)
}










test <- sps %>% 
  filter(feature == features[1])
st_crs(test) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
q <- test %>% filter(value > 0)
plot(q)

# Color Palette, World borders and Legend
pal <- c("#deebf7", "#31a354")
type <- c("Not present", "Present")
# Plotting
plot_sps <- ggplot() +
  geom_sf(data = test, aes(group = as.factor(value), fill = as.factor(value)), color = NA) +
  geom_sf(data = world_robinson, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(test)$xmin, st_bbox(test)$xmax),
           ylim = c(st_bbox(test)$ymin, st_bbox(test)$ymax),
           expand = TRUE) +
  theme_bw()
  scale_fill_manual(values = pal,
                    name = "",
                    labels = type) +
  ggtitle("")



test2 <- readRDS("Output/FeaturesMiCO/BlackMarlin_MICO.rds")
st_crs(test2) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
ggplot() +
  geom_sf(data = test2, aes(group = as.factor(Node_Type), fill = as.factor(Node_Type)), color = NA) +
  geom_sf(data = world_robinson, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(test)$xmin, st_bbox(test)$xmax),
           ylim = c(st_bbox(test)$ymin, st_bbox(test)$ymax),
           expand = TRUE) +
  theme_bw()
 

