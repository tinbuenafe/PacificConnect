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

for(i in seq_along(features)) {
  sg <- sps %>% 
    filter(feature == features[i])
  st_crs(sg) <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # Color Palette, World borders and Legend
  pal <- c("#deebf7", "#31a354")
  type <- c("NO", "YES")
  # Plotting
  ggplot() +
    geom_sf(data = sg, aes(group = as.factor(value), fill = as.factor(value)), color = NA) +
    geom_sf(data = world_robinson, size = 0.05, fill = "grey20") +
    coord_sf(xlim = c(st_bbox(sps)$xmin, st_bbox(sps)$xmax),
             ylim = c(st_bbox(sps)$ymin, st_bbox(sps)$ymax),
             expand = TRUE) +
    theme_bw() +
    scale_fill_manual(values = pal,
                      name = "",
                      labels = type) +
    ggsave(paste0("Figures/FeaturesMiCO/", paste0(unique(sg$feature), ".png")), width = 15, height = 10, dpi = 300)
}