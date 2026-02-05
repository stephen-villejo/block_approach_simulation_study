
rm(list=ls())

library(purrr)
library(dplyr)
library(tidyr)

#main_path <- "Z:/home/simulation_block_approach/New/Gaussian/results/scores"
main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/scores"

#### proper scores ####

extract_proper_scores <- function(compile_proper_scores) {
  
  # approaches: old, mrf, block
  approaches <- names(compile_proper_scores[[1]])
  
  # Build a row for each approach
  map_dfr(approaches, function(app) {
    tibble(
      Approach = app,
      rmse      = compile_proper_scores$rmse[[app]],
      ds        = compile_proper_scores$ds[[app]],
      logscore = compile_proper_scores$logscore[[app]]
    )
  })
}
all_proper_scores <- map_dfr(1:6, function(sim_i) {
  
  cat("Starting sim", sim_i, "\n")
  
  sim_path <- file.path(main_path, paste0("sim", sim_i))
  
  df_sim <- map_dfr(1:200, function(iter_j) {
    
    file_path <- file.path(sim_path, paste0("spatial_scores_iter", iter_j, ".Rdata"))
    
    env <- new.env()
    load(file_path, envir = env)
    
    compile_proper_scores <- env$scores_compile_spatial_model$compile_proper_scores
    
    df <- extract_proper_scores(compile_proper_scores)
    
    df %>%
      mutate(sim = sim_i,
             iter = iter_j)
  })
  
  cat("Finished sim", sim_i, "\n")
  df_sim
})
save(all_proper_scores,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/properscores.Rdata")


#### fixed effects param scores ####

extract_param_scores <- function(param_scores) {
  
  # approaches: old, mrf, block
  approaches <- names(param_scores[[1]])
  
  # Build a row for each approach
  map_dfr(approaches, function(app) {
    tibble(
      Approach = app,
      relbias      = param_scores$relbias[[app]],
      rmse        = param_scores$rmse[[app]],
      coverage = param_scores$coverage[[app]]
    )
  })
}
fixedparam_scores <- map_dfr(1:6, function(sim_i) {
  
  cat("Starting sim", sim_i, "\n")
  
  sim_path <- file.path(main_path, paste0("sim", sim_i))
  
  df_sim <- map_dfr(1:200, function(iter_j) {
    
    file_path <- file.path(sim_path, paste0("spatial_scores_iter", iter_j, ".Rdata"))
    
    env <- new.env()
    load(file_path, envir = env)
    
    param_scores <- env$scores_compile_spatial_model$compile_fixedparam_scores
    
    df <- extract_param_scores(param_scores)
    
    df %>%
      mutate(sim = sim_i,
             iter = iter_j)
  })
  
  cat("Finished sim", sim_i, "\n")
  df_sim
})
save(fixedparam_scores,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/paramscores_fixedeffects.Rdata")



####  hyperparams scores ####

extract_param_scores <- function(param_scores) {
  
  # approaches: old, mrf, block
  approaches <- names(param_scores[[1]])
  
  # Build a row for each approach
  map_dfr(approaches, function(app) {
    tibble(
      Approach = app,
      relbias      = param_scores$relbias[[app]],
      rmse        = param_scores$rmse[[app]],
      coverage = param_scores$coverage[[app]]
    )
  })
}
hyperparam_scores <- map_dfr(1:6, function(sim_i) {
  
  cat("Starting sim", sim_i, "\n")
  
  sim_path <- file.path(main_path, paste0("sim", sim_i))
  
  df_sim <- map_dfr(1:200, function(iter_j) {
    
    file_path <- file.path(sim_path, paste0("spatial_scores_iter", iter_j, ".Rdata"))
    
    env <- new.env()
    load(file_path, envir = env)
    
    param_scores <- env$scores_compile_spatial_model$compile_hyperparam_scores
    
    df <- extract_param_scores(param_scores)
    
    df %>%
      mutate(sim = sim_i,
             iter = iter_j)
  })
  
  cat("Finished sim", sim_i, "\n")
  df_sim
})
save(hyperparam_scores,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/paramscores_hyperparams.Rdata")


####  other scores ####

extract_otherscores <- function(compile_other_scores) {
  
  # approaches: old, mrf, block
  approaches <- names(compile_other_scores[[1]])
  
  # Build a row for each approach
  map_dfr(approaches, function(app) {
    tibble(
      Approach = app,
      rmse_mu_b      = compile_other_scores$rmse_mu_b[[app]],
      rmse_mu_B        = compile_other_scores$rmse_mu_B[[app]],
    )
  })
}
all_other_scores <- map_dfr(1:6, function(sim_i) {
  
  cat("Starting sim", sim_i, "\n")
  
  sim_path <- file.path(main_path, paste0("sim", sim_i))
  
  df_sim <- map_dfr(1:200, function(iter_j) {
    
    file_path <- file.path(sim_path, paste0("spatial_scores_iter", iter_j, ".Rdata"))
    
    env <- new.env()
    load(file_path, envir = env)
    
    compile_other_scores <- env$scores_compile_spatial_model$compile_other_scores
    
    df <- extract_otherscores(compile_other_scores)
    
    df %>%
      mutate(sim = sim_i,
             iter = iter_j)
  })
  
  cat("Finished sim", sim_i, "\n")
  df_sim
})
save(all_other_scores,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/otherscores.Rdata")



####  coverage  mu B ####

#main_path <- "Z:/home/simulation_block_approach/New/Gaussian/results/scores/coverage"
main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/scores/coverage"

extract_coverage_scores <- function(compile_coverage_scores) {
  
  # approaches: old, mrf, block
  approaches <- names(compile_coverage_scores)
  
  # Build a row for each approach
  map_dfr(approaches, function(app) {
    tibble(
      Approach = app,
      coverage      = colMeans(compile_coverage_scores[[app]]),
      iter = 1:ncol(compile_coverage_scores[[app]])
    )
  })
}
all_coverage_scores_mu <- map_dfr(1:6, function(sim_i) {
  
  cat("Starting sim", sim_i, "\n")
  
  sim_path <- file.path(main_path, paste0("sim", sim_i))
  
  file_path <- file.path(sim_path, paste0("spatial_scores_iter", 200, ".Rdata"))
  
  env <- new.env()
  load(file_path, envir = env)
  
  compile_coverage_scores <- env$scores_compile_spatial_model$compile_coverage_mu
  
  df <- extract_coverage_scores(compile_coverage_scores)
  
  df_sim <- df %>%
    mutate(sim = sim_i)
  
  cat("Finished sim", sim_i, "\n")
  df_sim
  
}
)
save(all_coverage_scores_mu,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/all_coverage_scores_mu.Rdata")


####  coverage  mu Y ####

#main_path <- "Z:/home/simulation_block_approach/New/Gaussian/results/scores/coverage"
main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/scores/coverage"

extract_coverage_scores <- function(compile_coverage_scores) {
  
  # approaches: old, mrf, block
  approaches <- names(compile_coverage_scores)
  
  # Build a row for each approach
  map_dfr(approaches, function(app) {
    tibble(
      Approach = app,
      coverage      = colMeans(compile_coverage_scores[[app]]),
      iter = 1:ncol(compile_coverage_scores[[app]])
    )
  })
}
all_coverage_scores_y <- map_dfr(1:6, function(sim_i) {
  
  cat("Starting sim", sim_i, "\n")
  
  sim_path <- file.path(main_path, paste0("sim", sim_i))
  
  file_path <- file.path(sim_path, paste0("spatial_scores_iter", 200, ".Rdata"))
  
  env <- new.env()
  load(file_path, envir = env)
  
  compile_coverage_scores <- env$scores_compile_spatial_model$compile_coverage
  
  df <- extract_coverage_scores(compile_coverage_scores)
  
  df_sim <- df %>%
    mutate(sim = sim_i)
  
  cat("Finished sim", sim_i, "\n")
  df_sim
  
}
)
save(all_coverage_scores_y,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/all_coverage_scores_y.Rdata")


