
rm(list = ls())

n_Bdata <- 10 #5 for 5x5, 10 for 10x10
new_prior_matern = T

library(here)
source(here("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/functions/functions_gaussian.R"))
dataloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/data/"
saveloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/inlamodels/"


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

sim_params <- expand.grid(maxunif = 20,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.1, # range of matern
                          sigma2 = c(2, 4), # marginal variance of matern
                          sigma2error = 1)

n_sim <- 200
param_id <- 1:8

# GAUSSIAN 

# spatial model

for(j in param_id){

  param_vals <- list(beta0 = 10,
                     beta1 = 1.5,
                     maxunif = sim_params[j,"maxunif"],
                     samp_prop = sim_params[j,"samp_prop"],
                     rho = sim_params[j,"rho"],
                     sigma2 = sim_params[j,"sigma2"],
                     sigma2error = sim_params[j,"sigma2error"])

  compile_spatial_model <- vector("list", length = 3)

  load(file = paste0(dataloc,"sim_num_",j,".Rdata"))


  for(i in 1:n_sim){

    # import the data

    sim_data <- compile_data[[i]]

    # fit the spatial model

    mesh <- fm_mesh_2d(boundary = sim_data$areas,
                       loc = st_centroid(sim_data$nested_grids_sf_all),
                       max.edge = c(0.05, 0.4))

    fit_block_spatial <- fit_block_spatial_approach(data = sim_data,
                                                    mesh_fit = mesh)

    fit_old_spatial <- fit_old_spatial_approach(data = sim_data,
                                                mesh_fit = mesh)
    
    fit_old_mrf <- fit_old_mrf_approach(shp = sim_data$areas_C,
                                        data = sim_data,
                                        samp_prop = param_vals$samp_prop)
    
    compile_spatial_model[[1]] <- fit_block_spatial
    compile_spatial_model[[2]] <- fit_old_spatial
    compile_spatial_model[[3]] <- fit_old_mrf

    names(compile_spatial_model) <- c("block","old","mrf")

    cat("Done with scenario = ", j, " and iter =", i, "out of ", n_sim,"\n")

    save(compile_spatial_model,
         file = paste0(saveloc,"sim",j,"/spatial_sim_iter",i,".Rdata"))
    
  }

}


