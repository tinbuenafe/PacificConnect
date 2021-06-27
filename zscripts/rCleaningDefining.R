
source("fCleaningDefining.R")
####################################################################################
####### Load generic files to run the PROBLEM
####################################################################################
# ft_iucn <- sps_iucn(data = "NewMiCOspeciesWithLonghurstProvinces.rds", cost = "costlayer.rds")
# saveRDS(ft_iucn, "x-NewMiCOspeciesWithLonghurstProvinces.rds")
# ft_Nodes <- sps_nodes(data = "NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds", cost = "costlayer.rds")
# saveRDS(ft_Nodes, "x-NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds")
ft_iucn <- readRDS("x-NewMiCOspeciesWithLonghurstProvinces.rds")
ft_Nodes <- readRDS("x-NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds")
mpas <- readRDS("PacificOcean_MPAs.rds")

####################################################################################
####### IUCN dataset
####################################################################################
  # Targets
    trgts <- seq(0.10, 0.90, 0.10)
    trgts_ls <- vector("list", length = length(trgts))
    for(i in seq_along(trgts)) {trgts_ls[[i]] <- targets_iucn(data = "NewMiCOspeciesWithLonghurstProvinces.rds", iucn_df = "IUCN_REDLIST_2020.csv", iucn_target = trgts[i])}
  # Problem
    p_list <- vector("list", length = length(trgts))
    for(j in 1:length(p_list)) {
      p_list[[j]] <- prioritizr::problem(ft_iucn, trgts_ls[[j]]$feature, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(trgts_ls[[j]]$targets) %>%
        add_binary_decisions() %>%
        add_locked_in_constraints(mpas$cellsID) %>% 
        add_default_solver(verbose = FALSE)
      }
    names(p_list) <- trgts
    saveRDS(p_list, "p_list-NewMiCOspeciesWithLonghurstProvinces.rds")
    
####################################################################################
####### MiCO dataset
####################################################################################
  # Targets
    trgt_Nodes <- trg_Mico("NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds")
  # Problem
    p1 <- prioritizr::problem(ft_Nodes, trgt_Nodes$feature, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(trgt_Nodes$targets) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(mpas$cellsID) %>% 
      add_default_solver(verbose = FALSE)
    saveRDS(p1, "p-NewMiCOspeciesMiCOnodesWithLonghurstProvinces.rds")
    
    
    