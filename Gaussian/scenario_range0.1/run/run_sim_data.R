rm(list = ls())

n_Bdata <- 10 # 10x10 grid

library(here)
source(here("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/functions/functions_gaussian.R"))
saveloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/data/"
.libPaths("/rds/general/user/sv20/home/miniforge3/envs/r452/lib/R/library")

library(sf)
library(tidyr)
library(INLA)
library(inlabru)
library(ggplot2)
library(sp)
library(scico)
library(dplyr)
library(purrr)
library(MCMCpack)
library(fields)

sim_params <- expand.grid(maxunif = 20,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.1, # range of matern
                          sigma2 = c(2, 4), # marginal variance of matern
                          sigma2error = 1)

n_sim <- 200

for(j in 1:4){
  
  param_vals <- list(beta0 = 10,
                     beta1 = 1.5,
                     maxunif = sim_params[j,"maxunif"],
                     samp_prop = sim_params[j,"samp_prop"],
                     rho = sim_params[j,"rho"],
                     sigma2 = sim_params[j,"sigma2"],
                     sigma2error = sim_params[j,"sigma2error"])
  
  compile_data <- vector("list", length = n_sim)
  
  for(i in 1:n_sim){
    
    # simulate data
    
    sim_data <- data_gen(maxunif = param_vals$maxunif,
                         samp_prop = param_vals$samp_prop,
                         n_areas = n_Bdata,
                         rho = param_vals$rho,
                         sigma2 = param_vals$sigma2,
                         sigma2error = param_vals$sigma2error)
    
    
    compile_data[[i]] <- sim_data
    
    cat("Done with scenario = ", j, " and iter =", i, "out of ", n_sim,"\n")
    
  }
  
  save(compile_data,
       file = paste0(saveloc,"sim_num_",j,".Rdata"))
  
  
}

