# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(raster)
library(sf)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)

####################################################################################
####### Create a land shapefile Pacific centered and projected  
####################################################################################
# 
  nsp <- as_tibble(matrix(c("Balaenoptera musculus", "BlueWhale",
                            "Carcharodon carcharias", "GreatWhiteShark",
                            "Caretta caretta", "Loggerhead",
                            "Chelonia mydas", "GreenTurtle",
                            "Dermochelys coriacea", "Leatherback",
                            "Eretmochelys imbricata", "Hawksbill",
                            "Istiompax indica", "BlackMarlin",
                            "Isurus oxyrinchus", "ShortfinMako",
                            "Kajikia audax", "StripedMarlin",
                            "Lepidochelys olivacea", "OliveRidley",
                            "Makaira nigricans", "BlueMarlin",
                            "Megaptera novaeangliae", "HumpbackWhale",
                            "Rhincodon typus", "WhaleShark",
                            "Sphyrna lewini", "ScallopedHammerhead",
                            "Xiphias gladius", "Swordfish"),
                          ncol = 2, byrow = TRUE), .name_repair = "universal") %>%
    rename(name1 = ...1, name2 = ...2) %>% 
    arrange(name2)

####################################################################################
####### Create a land shapefile Pacific centered and projected  
####################################################################################
  # Using land mask for nature earth package to create a projected sf/shapefile object
    world <- ne_countries(scale = "medium", returnclass = "sf")
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match # that of world
    polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                         c(0, 90),
                                         c(0, -90),
                                         c(-0.0001, -90),
                                         c(-0.0001, 90)))) %>%
      st_sfc() %>%
      st_set_crs(4326)
  # Modify world dataset to remove overlapping portions with world's polygons
    world2 <- world %>% 
      st_difference(polygon)
  # Perform transformation on modified version of world dataset
    world_robinson <- world2 %>% 
      st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    # notice that there is a line in the middle of Antarctica. This is because we have
    # split the map after reprojection. We need to fix this:
  # Fix those extra boundaries
    bbox <-  st_bbox(world_robinson)
    bbox[c(1,3)]  <-  c(-1e-5,1e-5)
    polygon2 <- st_as_sfc(bbox)
    crosses <- world_robinson %>%
      st_intersects(polygon2) %>%
      sapply(length) %>%
      as.logical %>%
      which
  # Adding buffer 0
    world_robinson[crosses,] %<>%
      st_buffer(0)