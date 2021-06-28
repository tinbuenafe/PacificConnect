# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

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
