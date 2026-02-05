rm(list = ls())

n_Bdata <- 10 # 10 for 10x10

library(here)
source(here("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/functions/functions.R"))
saveloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/data/"
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

n_sim <- 200

sim_params <- expand.grid(maxunif = 40,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.10, # range of matern
                          sigma2 = c(0.05, 0.15)) # marginal variance of matern

#for(j in 1:nrow(sim_params)){
for(j in 1:5){  
  
  param_vals <- list(beta0 = -2.5,
                     beta1 = .15,
                     maxunif = sim_params[j,"maxunif"],
                     samp_prop = sim_params[j,"samp_prop"],
                     rho = sim_params[j,"rho"],
                     sigma2 = sim_params[j,"sigma2"])
  
  compile_data <- vector("list", length = n_sim)
  
  for(i in 1:n_sim){
    
    # simulate data
    
    sim_data <- data_gen(maxunif = param_vals$maxunif,
                         samp_prop = param_vals$samp_prop,
                         n_areas = n_Bdata,
                         rho = param_vals$rho,
                         sigma2 = param_vals$sigma2)
  
  
    compile_data[[i]] <- sim_data
    
    cat("Done with scenario = ", j, " and iter =", i, "out of ", n_sim,"\n")
    
  }

  save(compile_data,
       file = paste0(saveloc,"sim_num_",j,".Rdata"))
  
  
}

