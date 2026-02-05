
rm(list = ls())

n_Bdata <- 10 # 10 for 10x10

library(here)
source(here("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/functions/functions.R"))
dataloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/data/"
saveloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/results/inlamodels/old_mrf_meancovinput/"
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
library(spdep)

sim_params <- expand.grid(maxunif = 40,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.10, # range of matern
                          sigma2 = c(0.05, 0.15)) # marginal variance of matern


n_sim <- 200
param_id <- 1:8

# POISSON 
# spatial model

for(j in param_id){
  
  param_vals <- list(beta0 = -2.5,
                     beta1 = .15,
                     maxunif = sim_params[j,"maxunif"],
                     samp_prop = sim_params[j,"samp_prop"],
                     rho = sim_params[j,"rho"],
                     sigma2 = sim_params[j,"sigma2"])
  
  
  compile_spatial_model <- vector("list", length = 2)
  
  load(file = paste0(dataloc,"sim_num_",j,".Rdata"))
  
  for(i in 1:n_sim){
    
    # simulate data
    
    sim_data <- compile_data[[i]]

    # fit the spatial model
    
    mesh <- fm_mesh_2d(boundary = sim_data$areas,
                       loc = st_centroid(sim_data$nested_grids_sf_all),
                       max.edge = c(0.05, 0.4))
    
    sim_data$data_Y$covariate <- sim_data$data_Y$covariate_mean
    sim_data$mu_B$covariate <- sim_data$mu_B$covariate_mean
  
    fit_old_spatial <- fit_old_spatial_approach(data = sim_data,
                                                mesh_fit = mesh)     
    
    fit_old_mrf <- fit_old_mrf_approach(shp = sim_data$areas_C,
                                        data = sim_data,
                                        samp_prop = param_vals$samp_prop)
    
    compile_spatial_model[[1]] <- fit_old_spatial
    compile_spatial_model[[2]] <- fit_old_mrf

    names(compile_spatial_model) <- c("old","mrf")
    
    cat("Done with spatial model, scenario = ", j, " and iter =", i, "out of ", n_sim,"\n")
    
    save(compile_spatial_model,
         file = paste0(saveloc,"/sim",j,"/spatial_sim_iter",i,".Rdata"))
    
  }
  
}


