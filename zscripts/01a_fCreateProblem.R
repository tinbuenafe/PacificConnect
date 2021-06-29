# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au/ibritomorales@gmail.com)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("zscripts/00a_fCleaningDefining.R")

####################################################################################
####### IUCN dataset
####################################################################################
  # Targets
    trgts <- seq(0.10, 0.90, 0.10)
    trgts_ls <- vector("list", length = length(trgts))
    for(i in seq_along(trgts)) {trgts_ls[[i]] <- trg_iucn(data = "Output/PrioritisationInput/01a_IUCN.rds", iucn_df = "IUCN_REDLIST_2020.csv", iucn_target = trgts[i], nsp = nsp)}
  # Problem
    p_list <- vector("list", length = length(trgts))
    ft_iucn <- readRDS("Output/PrioritisationInput/01a_IUCN.rds")
    for(j in 1:length(p_list)) {
      p_list[[j]] <- prioritizr::problem(ft_iucn, trgts_ls[[j]]$feature, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(trgts_ls[[j]]$targets) %>%
        add_binary_decisions() %>%
        add_locked_in_constraints(mpas$cellsID) %>% 
        add_default_solver(verbose = FALSE)
      }
    names(p_list) <- trgts
    saveRDS(p_list, "Prioritisation/Problems/01a_ProblemsIUCN.rds")
    
####################################################################################
####### MiCO dataset Nodes
####################################################################################
    ft_Nodes <- readRDS("Output/PrioritisationInput/02b_MiCONodes.rds")
  # Targets
    trgt_Nodes <- trg_Mico("Output/PrioritisationInput/02b_MiCONodes.rds")
  # Problem
    p1 <- prioritizr::problem(ft_Nodes, trgt_Nodes$feature, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(trgt_Nodes$targets) %>%
      add_binary_decisions() %>%
      add_locked_in_constraints(mpas$cellsID) %>% 
      add_default_solver(verbose = FALSE)
    saveRDS(p1, "Prioritisation/Problems/02b_ProblemsMiCONodes.rds")