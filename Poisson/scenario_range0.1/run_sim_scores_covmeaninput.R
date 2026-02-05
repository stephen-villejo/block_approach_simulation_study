
rm(list = ls())

n_Bdata <- 10 # 5 for 5x5, 10 for 10x10

library(here)
source(here("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/functions/functions.R"))
dataloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/data/"
model_loc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/results/inlamodels/old_mrf_meancovinput/"
saveloc <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/results/scores/old_mrf_meancovinput/"

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

sim_params <- expand.grid(maxunif = 40,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.10, # range of matern
                          sigma2 = c(0.05, 0.15)) # marginal variance of matern

n_sim <- 200
param_id <- 1:2


# spatial model

for(j in param_id){
  
  param_vals <- list(beta0 = -2.5,
                     beta1 = .15,
                     maxunif = sim_params[j,"maxunif"],
                     samp_prop = sim_params[j,"samp_prop"],
                     rho = sim_params[j,"rho"],
                     sigma2 = sim_params[j,"sigma2"])
  
  
  for(i in 1:n_sim){
    
    scores_compile_spatial_model <- vector("list", length = 6)
    
    compile <- clear_compile()
    list2env(compile, envir = .GlobalEnv)
    
    compile2 <- clear_compile_new_scores(n = param_vals$samp_prop * (n_Bdata^2),
                                         nsim = n_sim)
    list2env(compile2, envir = .GlobalEnv)
    
    load(file = paste0(dataloc,"sim_num_",j,".Rdata"))
    
    # load data
    
    sim_data <- compile_data[[i]]
    
    sim_data$data_Y$covariate <- sim_data$data_Y$covariate_mean
    sim_data$mu_B$covariate <- sim_data$mu_B$covariate_mean
    
    # load inla model
    
    load(file = paste0(model_loc,"sim",j,"/spatial_sim_iter",i,".Rdata"))
    
    fit_old <- compile_spatial_model[[1]]
    fit_mrf <- compile_spatial_model[[2]]

    # compute proper scores
  
    properscores_old <- compute_proper_scores_spatial(data = sim_data,
                                                      fit_res = fit_old,
                                                      block = FALSE)
    properscores_mrf <- compute_proper_scores_mrf(data = sim_data,
                                                  fit_res = fit_mrf)
    
    # compute fixed effs param scores
    
    true_vals = c(param_vals$beta0, param_vals$beta1)
    scores_params_fixed_old <- compute_metrics_params_fixed(fit_res = fit_old$res,
                                                            true_vals = true_vals)
    scores_params_fixed_mrf <- compute_metrics_params_fixed(fit_res = fit_mrf$res,
                                                            true_vals = true_vals)
    
    # compute hyper param scores
    
    true_vals = c(1/param_vals$sigma2error,param_vals$rho, sqrt(param_vals$sigma2))
    scores_params_hyper_old <- compute_metrics_params_hyper(fit_res = fit_old$res,
                                                            true_vals = true_vals)
    
    # compute other scores
    
    otherscores_old <- compute_other_scores_spatial(data = sim_data,
                                                    fit_res = fit_old,
                                                    block = FALSE)
    otherscores_mrf <- compute_other_scores_mrf(data = sim_data,
                                                fit_res = fit_mrf)
    
    
    # compile proper scores
    
    compile_proper_scores$rmse$old <- c(compile_proper_scores$rmse$old, sqrt(mean(properscores_old$scores$SE)))
    compile_proper_scores$rmse$mrf <- c(compile_proper_scores$rmse$mrf, sqrt(mean(properscores_mrf$scores$SE)))
    
    compile_proper_scores$ds$old <- c(compile_proper_scores$ds$old, mean(properscores_old$scores$DS))
    compile_proper_scores$ds$mrf <- c(compile_proper_scores$ds$mrf, mean(properscores_mrf$scores$DS))
    
    compile_proper_scores$logscore$old <- c(compile_proper_scores$logscore$old, sum(properscores_old$scores$LS))
    compile_proper_scores$logscore$mrf <- c(compile_proper_scores$logscore$mrf, sum(properscores_mrf$scores$LS))
    
    
    # compile param scores fixed
    
    compile_fixedparam_scores$relbias$old <- c(compile_fixedparam_scores$relbias$old, scores_params_fixed_old$relbias_fixedparam)
    compile_fixedparam_scores$relbias$mrf <- c(compile_fixedparam_scores$relbias$mrf, scores_params_fixed_mrf$relbias_fixedparam)
    
    compile_fixedparam_scores$rmse$old <- c(compile_fixedparam_scores$rmse$old, sqrt(scores_params_fixed_old$sqerror_fixedparam))
    compile_fixedparam_scores$rmse$mrf <- c(compile_fixedparam_scores$rmse$mrf, sqrt(scores_params_fixed_mrf$sqerror_fixedparam))
    
    compile_fixedparam_scores$coverage$old <- c(compile_fixedparam_scores$coverage$old, scores_params_fixed_old$cov_fixedparam)
    compile_fixedparam_scores$coverage$mrf <- c(compile_fixedparam_scores$coverage$mrf, scores_params_fixed_mrf$cov_fixedparam)
    
    
    # compile param scores hyper
    
    compile_hyperparam_scores$relbias$old <- c(compile_hyperparam_scores$relbias$old, scores_params_hyper_old$relbias_hyperparam)
    #compile_hyperparam_scores$relbias$mrf <- c(compile_hyperparam_scores$relbias$mrf, scores_params_hyper_mrf$relbias_hyperparam)
    
    compile_hyperparam_scores$rmse$old <- c(compile_hyperparam_scores$rmse$old, sqrt(scores_params_hyper_old$sqerror_hyperparam))
    #compile_hyperparam_scores$rmse$mrf <- c(compile_hyperparam_scores$rmse$mrf, sqrt(scores_params_hyper_mrf$sqerror_hyperparam))
    
    compile_hyperparam_scores$coverage$old <- c(compile_hyperparam_scores$coverage$old, scores_params_hyper_old$cov_hyperparam)
    #compile_hyperparam_scores$coverage$mrf <- c(compile_hyperparam_scores$coverage$mrf, scores_params_hyper_mrf$cov_hyperparam)
    
    
    
    # compile other scores
    
    compile_other_scores$rmse_mu_B$old <- c(compile_other_scores$rmse_mu_B$old, sqrt(mean(otherscores_old$scores2$SE_mu_B)))
    compile_other_scores$rmse_mu_B$mrf <- c(compile_other_scores$rmse_mu_B$mrf, sqrt(mean(otherscores_mrf$scores2$SE_mu_B)))
    
    compile_other_scores$rmse_mu_b$old <- c(compile_other_scores$rmse_mu_b$old, sqrt(mean(otherscores_old$scores$SE_mu_b)))
    compile_other_scores$rmse_mu_b$mrf <- c(compile_other_scores$rmse_mu_b$mrf, sqrt(mean(otherscores_mrf$scores$SE_mu_b)))
    

    # compile new scores - posterior uncertainty of Y
    
    compile_post_var$old <- c(compile_post_var$old, mean(properscores_old$scores$post_Var))
    compile_post_var$mrf <- c(compile_post_var$mrf, mean(properscores_mrf$scores$post_Var))
    
    # compile new scores - posterior uncertainty of mu
    
    compile_post_var_mu$old <- c(compile_post_var_mu$old, mean(otherscores_old$scores2$post_Var))
    compile_post_var_mu$mrf <- c(compile_post_var_mu$mrf, mean(otherscores_mrf$scores2$post_Var))
    
    
    scores_compile_spatial_model[[1]] <- compile_proper_scores
    scores_compile_spatial_model[[2]] <- compile_fixedparam_scores
    scores_compile_spatial_model[[3]] <- compile_hyperparam_scores
    scores_compile_spatial_model[[4]] <- compile_other_scores
    scores_compile_spatial_model[[5]] <- compile_post_var
    scores_compile_spatial_model[[6]] <- compile_post_var_mu
    names(scores_compile_spatial_model) <- c("compile_proper_scores","compile_fixedparam_scores","compile_hyperparam_scores",
                                             "compile_other_scores","compile_post_var","compile_post_var_mu")
    
    save(scores_compile_spatial_model,
         file = paste0(saveloc,"sim",j,"/spatial_scores_iter",i,".Rdata"))
    
    cat("Done with spatial model, scenario = ", j, " and iter =", i, "out of ", n_sim,"\n")
    
  }
  
}

