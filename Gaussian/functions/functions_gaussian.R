
matern_cov <- function(d, nu, rho, sigma2) {
  part1 <- (2^(1 - nu)) / gamma(nu)
  part2 <- (sqrt(2 * nu) * d / rho)^nu
  part3 <- besselK(sqrt(2 * nu) * d / rho, nu)
  cov <- sigma2 * part1 * part2 * part3
  cov[d == 0] <- sigma2  # handle zero distance
  return(cov)
}

clear_compile <- function(){
  
  compile_proper_scores <- vector("list", length = 3)
  for(i in 1:length(compile_proper_scores)){
    compile_proper_scores[[i]] <- vector("list", length = 3)
    names(compile_proper_scores[[i]]) <- c("block","old","mrf")
  }
  names(compile_proper_scores) <- c("rmse","ds","logscore")
  
  compile_fixedparam_scores <- vector("list", length = 3)
  for(i in 1:length(compile_fixedparam_scores)){
    compile_fixedparam_scores[[i]] <- vector("list", length = 3)
    names(compile_fixedparam_scores[[i]]) <- c("block","old","mrf")
  }
  names(compile_fixedparam_scores) <- c("relbias","rmse","coverage")
  
  compile_hyperparam_scores <- vector("list", length = 3)
  for(i in 1:length(compile_hyperparam_scores)){
    compile_hyperparam_scores[[i]] <- vector("list", length = 3)
    names(compile_hyperparam_scores[[i]]) <- c("block","old","mrf")
  }
  names(compile_hyperparam_scores) <- c("relbias","rmse","coverage")
  
  
  compile_other_scores <- vector("list", length = 2)
  for(i in 1:length(compile_other_scores)){
    compile_other_scores[[i]] <- vector("list", length = 3)
    names(compile_other_scores[[i]]) <- c("block","old","mrf")
  }
  names(compile_other_scores) <- c("rmse_mu_b","rmse_mu_B")
  
  return(out = list(compile_proper_scores = compile_proper_scores,
                    compile_fixedparam_scores = compile_fixedparam_scores,
                    compile_hyperparam_scores = compile_hyperparam_scores,
                    compile_other_scores = compile_other_scores))
  
}



clear_compile_mrf <- function(){
  
  compile_proper_scores <- vector("list", length = 3)
  names(compile_proper_scores) <- c("rmse","ds","logscore")
  
  compile_fixedparam_scores <- vector("list", length = 3)
  names(compile_fixedparam_scores) <- c("relbias","rmse","coverage")
  
  compile_other_scores <- vector("list", length = 2)
  names(compile_other_scores) <- c("rmse_mu_b","rmse_mu_B")
  
  return(out = list(compile_proper_scores = compile_proper_scores,
                    compile_fixedparam_scores = compile_fixedparam_scores,
                    compile_other_scores = compile_other_scores))
  
}

data_gen <- function(maxunif,
                     samp_prop = 1,
                     n_areas,
                     rho,
                     sigma2,
                     sigma2error){
  
  # Create study boundary
  areas <- data.frame(lon = c(0,1,1,0),
                      lat = c(0,0,1,1)) %>%
    st_as_sf(coords = c("lon","lat")) %>%
    st_bbox() %>%
    st_as_sfc()
  
  # Create blocks
  
  areas_C <- st_make_grid(areas,
                          what = "polygons",
                          square = TRUE,
                          n = n_areas)
  areas_C <- st_as_sf(data.frame(geometry = areas_C))
  areas_C <- areas_C %>%
    mutate(block = row_number())
  
  # Create the nested grids
  
  nested_grids <- map(areas_C$geometry, ~ st_make_grid(.x, what = "polygons", square = TRUE, n = 5)) %>%
    map(st_as_sf)
  nested_grids <- bind_rows(nested_grids)
  nested_grids_sf <- st_as_sf(nested_grids)
  nested_grids_sf <- st_join(nested_grids_sf, areas_C, join = st_within)
  
  # Generate covariate data (fix?)
  
  set.seed(912009)
  nested_grids_sf$covariate <- runif(nrow(nested_grids_sf),min=0,max=param_vals$maxunif)
  
  
  # Generate Matern field
  
  nested_grids_sf_coords <- st_coordinates(st_centroid(nested_grids_sf))
  D <- rdist(nested_grids_sf_coords)
  Sigma <- matern_cov(D, 1, rho, sigma2)
  set.seed(Sys.time())
  field <- mvrnorm(n = 1, mu = rep(0, nrow(nested_grids_sf_coords)), Sigma = Sigma)
  nested_grids_sf$field <- field
  
  # Compute the latent mu values at the small grids (b)
  
  nested_grids_sf$mu <- param_vals$beta0 + nested_grids_sf$covariate * param_vals$beta1 + nested_grids_sf$field
  
  # Compute the spatial average for each block B
  
  data_Y <- nested_grids_sf %>%
    group_by(block) %>%
    summarise(mu = mean(mu),
              covariate = mean(covariate))
  
  mu_B <- data_Y
  
  # Simulate the observed data
  
  if(samp_prop == 1){
    
    set.seed(Sys.time())
    data_Y$y <- rnorm(n = nrow(data_Y), mean = data_Y$mu, sd = sqrt(sigma2error))
    nested_grids_sf_all <- nested_grids_sf
    
  }else{
    
    set.seed(Sys.time())
    data_Y$y <- rnorm(n = nrow(data_Y), mean = data_Y$mu, sd = sqrt(sigma2error))
    
    set.seed(1920)
    sample_C <- sample(1:nrow(data_Y), size = nrow(data_Y)*param_vals$samp_prop)
    data_Y <- data_Y[sample_C,]
    
    nested_grids_sf_all <- nested_grids_sf
    
    nested_grids_sf <- st_centroid(nested_grids_sf)
    nested_grids_sf <- nested_grids_sf[which(nested_grids_sf$block %in% sample_C),]
    nested_grids_sf$x <- st_sfc(
      lapply(st_geometry(nested_grids_sf), function(pt) {
        st_point(c(pt[[1]], pt[[2]], 0))  # X, Y, Z = 0
      }),
      crs = st_crs(nested_grids_sf)
    )
    
    data_Y <- data_Y[order(data_Y$block),]
    data_Y$.block <- 1:nrow(data_Y)
    
    block_df <- st_drop_geometry(data_Y[,c("block",".block")])
    nested_grids_sf <- left_join(nested_grids_sf,
                                 block_df,
                                 by = c("block"))
    
    data_Y <- data_Y[,-which(names(data_Y) == "block")]
    nested_grids_sf <- nested_grids_sf[,-which(names(nested_grids_sf) == "block")]
    
    names(data_Y)[which(names(data_Y) == ".block")] <- "block"
    names(nested_grids_sf)[which(names(nested_grids_sf) == ".block")] <- "block"
    
  }
  
  return(out = list(data_Y = data_Y,
                    nested_grids_sf = nested_grids_sf,
                    nested_grids_sf_all = nested_grids_sf_all,
                    areas = areas,
                    areas_C = areas_C,
                    mu_B = mu_B))
  
}


fit_block_approach <- function(data){
                      
  
  integration_data <- st_drop_geometry(data$nested_grids_sf)
  integration_data$weight = 1
  
  cmp <-  ~ -1 + intercept(1) +
    covariate(covariate, model = "linear")
  
  fit_block <- bru(cmp,
                   bru_obs(
                     y ~ fm_block_eval(
                       block = block,
                       n_block = nrow(data$data_Y),
                       weights = weight,
                       rescale = TRUE,
                       values = intercept + covariate
                     ),
                     family = "gaussian",
                     data = integration_data,
                     response_data = data$data_Y,
                     allow_combine = TRUE
                   ),
                   options = list(control.inla = list(int.strategy = "eb"),
                                  control.compute = list(dic = TRUE, waic = TRUE),
                                  bru_verbose = FALSE,
                                  bru_max_iter = 10,
                                  bru_method=list(rel_tol = 0.10))) 
  
  return(out = list(res = fit_block,
                    integration_data = integration_data))
  
}



fit_block_spatial_approach <- function(data = sim_data,
                                       mesh_fit = mesh){
  
  integration_data <- st_centroid(data$nested_grids_sf)
  integration_data$weight = 1
  
  max_dist <- max(st_distance(st_cast(st_boundary(data$areas), "POINT")))
  # matern = inla.spde2.pcmatern(mesh = mesh_fit,
  #                              alpha = 2,
  #                              prior.range = c(as.numeric(max_dist/5), 0.5),
  #                              prior.sigma = c(sd(data$data_Y$y), 0.5))
  
  matern = inla.spde2.pcmatern(mesh = mesh_fit,
                               alpha = 2,
                               prior.range = c(.1, 0.5),
                               prior.sigma = c(1.73, 0.5))
  
  cmp <-  ~ -1 + intercept(1) +
    covariate(covariate, model = "linear") +
    sp_field(main = x, model = matern)
  
  fit_block <- bru(cmp,
                   bru_obs(
                     y ~ fm_block_eval(
                       block = block,
                       n_block = nrow(data$data_Y),
                       weights = weight,
                       rescale = TRUE,
                       values = intercept + covariate + sp_field
                     ),
                     family = "gaussian",
                     data = integration_data,
                     response_data = data$data_Y,
                     allow_combine = TRUE
                   ),
                   options = list(control.inla = list(int.strategy = "eb"),
                                  control.compute = list(dic = TRUE, waic = TRUE),
                                  control.family = list(
                                    hyper = list(
                                      prec = list(
                                        prior = "pc.prec",
                                        param = c(1, 0.5)  # (lambda, alpha)
                                      )
                                    )),
                                  bru_verbose = FALSE,
                                  bru_max_iter = 10,
                                  bru_method=list(rel_tol = 0.10))) 
  
  return(out = list(res = fit_block,
                    integration_data = integration_data))
  
}



fit_old_approach <- function(data = sim_data){
  
  data_Y_centroid <- st_centroid(data$data_Y)   
  
  cmp <-  ~ -1 + intercept(1) +
    covariate(covariate, model = "linear")
  
  fit_old = bru(components = cmp,
                formula = y ~ intercept + covariate,
                data = data_Y_centroid,
                family = "gaussian",
                options = list(control.inla = list(int.strategy = "eb"),
                               control.compute = list(dic = TRUE, waic = TRUE),
                               bru_verbose = FALSE,
                               bru_max_iter = 10,
                               bru_method=list(rel_tol = 0.10)))  
  
  return(out = list(res = fit_old))
  
}


fit_old_spatial_approach <- function(data = sim_data,
                                     mesh_fit = mesh){
  
  data_Y_centroid <- st_centroid(data$data_Y)
  
  max_dist <- max(st_distance(st_cast(st_boundary(data$areas), "POINT")))
  # matern = inla.spde2.pcmatern(mesh = mesh_fit,
  #                              alpha = 2,
  #                              prior.range = c(as.numeric(max_dist/5), 0.5),
  #                              prior.sigma = c(sd(data$data_Y$y), 0.5))
  # 
  matern = inla.spde2.pcmatern(mesh = mesh_fit,
                               alpha = 2,
                               prior.range = c(.1, 0.5),
                               prior.sigma = c(1.73, 0.5))
  
  cmp <-  ~ -1 + intercept(1) +
    covariate(covariate, model = "linear") +
    sp_field(main = x, model = matern)
  
  fit_old = bru(components = cmp,
                formula = y ~ intercept + covariate + sp_field,
                data = data_Y_centroid,
                family = "gaussian",
                options = list(control.inla = list(int.strategy = "eb"),
                               control.compute = list(dic = TRUE, waic = TRUE),
                               control.family = list(
                                 hyper = list(
                                   prec = list(
                                     prior = "pc.prec",
                                     param = c(1, 0.5)  # (lambda, alpha)
                                   )
                                 )),
                               bru_verbose = FALSE,
                               bru_max_iter = 10,
                               bru_method=list(rel_tol = 0.10)))  
  
  return(out = list(res = fit_old))
  
}

compute_proper_scores <- function(data = sim_data,
                                  fit_res = fit_block,
                                  block = FALSE){ 
  
  if(block == FALSE){
    
    data_Y_centroid <- st_centroid(data$data_Y)
    
    pred_vals <- predict(fit_res$res, data_Y_centroid,
                         formula = ~ {
                           E <- intercept + covariate
                           V <- 1 / Precision_for_the_Gaussian_observations
                           list(
                             eta = E,
                             sigma_y_2 = V,
                             dens = dnorm(y, mean = E, sd = sqrt(V))
                           )
                         },
                         n.samples = 2000
    )
    
    post_E <- pred_vals$eta$mean
    post_Var <- pred_vals$sigma_y_2$mean + pred_vals$eta$sd^2
    
    scores <- data.frame(
      SE = (data_Y_centroid$y - post_E)^2,
      DS = (data_Y_centroid$y - post_E)^2 / post_Var + log(post_Var),
      LS = -log(pred_vals$dens$mean),
      post_Var = post_Var,
      post_E = post_E
    )
    
  }else{
    
    pred_vals <- predict(fit_res$res, fit_res$integration_data,
                         formula = ~ {
                           E <- fm_block_eval(
                             block = block,
                             n_block = max(fit_res$integration_data$block),
                             weights = weight,
                             rescale = TRUE,
                             values = intercept + covariate
                           )
                           V <- 1 / Precision_for_the_Gaussian_observations
                           list(
                             eta = E,
                             sigma_y_2 = V,
                             dens = dnorm(data$data_Y$y, mean = E, sd = sqrt(V))
                           )
                         },
                         n.samples = 2000
    )
    
    post_E <- pred_vals$eta$mean
    post_Var <- pred_vals$sigma_y_2$mean + pred_vals$eta$sd^2
    
    scores <- data.frame(
      SE = (data$data_Y$y - post_E)^2,
      DS = (data$data_Y$y - post_E)^2 / post_Var + log(post_Var),
      LS = -log(pred_vals$dens$mean),
      post_Var = post_Var,
      post_E = post_E
    )
    
  }
  
  return(list(scores = scores))
  
}


compute_proper_scores_spatial <- function(data = sim_data,
                                          fit_res = fit_block_spatial,
                                          block = FALSE){ 
  
  if(block == FALSE){
    
    data_Y_centroid <- st_centroid(data$data_Y)
    
    pred_vals <- predict(fit_res$res, data_Y_centroid,
                         formula = ~ {
                           E <- intercept + covariate + sp_field
                           V <- 1 / Precision_for_the_Gaussian_observations
                           list(
                             eta = E,
                             sigma_y_2 = V,
                             dens = dnorm(y, mean = E, sd = sqrt(V))
                           )
                         },
                         n.samples = 2000
    )
    
    post_E <- pred_vals$eta$mean
    post_Var <- pred_vals$sigma_y_2$mean + pred_vals$eta$sd^2
    
    scores <- data.frame(
      SE = (data_Y_centroid$y - post_E)^2,
      DS = (data_Y_centroid$y - post_E)^2 / post_Var + log(post_Var),
      LS = -log(pred_vals$dens$mean),
      post_Var = post_Var,
      post_E = post_E
    )
    
  }else{
    
    pred_vals <- predict(fit_res$res, fit_res$integration_data,
                         formula = ~ {
                           E <- fm_block_eval(
                             block = block,
                             n_block = max(fit_res$integration_data$block),
                             weights = weight,
                             rescale = TRUE,
                             values = intercept + covariate + sp_field
                           )
                           V <- 1 / Precision_for_the_Gaussian_observations
                           list(
                             eta = E,
                             sigma_y_2 = V,
                             dens = dnorm(data$data_Y$y, mean = E, sd = sqrt(V))
                           )
                         },
                         n.samples = 2000
    )
    
    post_E <- pred_vals$eta$mean
    post_Var <- pred_vals$sigma_y_2$mean + pred_vals$eta$sd^2
    
    scores <- data.frame(
      SE = (data$data_Y$y - post_E)^2,
      DS = (data$data_Y$y - post_E)^2 / post_Var + log(post_Var),
      LS = -log(pred_vals$dens$mean),
      post_Var = post_Var,
      post_E = post_E
    )
    
  }
  
  return(list(scores = scores))
  
}




compute_metrics_params_fixed <- function(fit_res = fit_old$res,
                                         true_vals){
  
  # Relative bias of param estimates
  relbias_fixedparam <- abs(fit_res$summary.fixed$mean - true_vals)/abs(true_vals) * 100
  
  # Squared error of param estimates
  sqerror_fixedparam <- (fit_res$summary.fixed$mean - true_vals)^2
  
  # coverage of param estimates
  cov_fixedparam <- true_vals > fit_res$summary.fixed$`0.025quant` &
    true_vals < fit_res$summary.fixed$`0.975quant`
  
  return(list(relbias_fixedparam = relbias_fixedparam,
              sqerror_fixedparam = sqerror_fixedparam,
              cov_fixedparam = cov_fixedparam))
  
}


compute_metrics_params_hyper <- function(fit_res = fit_old$res,
                                         true_vals){
  
  # Relative bias of param estimates
  relbias_hyperparam <- abs(fit_res$summary.hyper$mean - true_vals)/abs(true_vals) * 100
  
  # Squared error of param estimates
  sqerror_hyperparam <- (fit_res$summary.hyper$mean - true_vals)^2
  
  # coverage of param estimates
  cov_hyperparam <- true_vals > fit_res$summary.hyper$`0.025quant` &
    true_vals < fit_res$summary.hyper$`0.975quant`
  
  return(list(relbias_hyperparam = relbias_hyperparam,
              sqerror_hyperparam = sqerror_hyperparam,
              cov_hyperparam = cov_hyperparam))
  
}


compute_other_scores <- function(data = sim_data,
                                 fit_res = fit_old,
                                 block){
  
  
  
  if(block == FALSE){
    
    mu_B_centroid <- st_centroid(data$mu_B)
    
    pred_vals <- predict(fit_res$res, mu_B_centroid,
                         formula = ~ {
                           E <- intercept + covariate
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    
    pred_mu_b <- data.frame(block = 1:nrow(pred_vals$eta),
                            pred.mu_ave = pred_vals$eta$mean)
    pred_mu_b <- left_join(data$nested_grids_sf_all,
                           pred_mu_b,
                           by = "block")
    
    pred_vals_new <- predict(fit_res$res, pred_mu_b,
                         formula = ~ {
                           E <- intercept + covariate
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    pred_mu_b$pred.mu <- pred_vals_new$eta$mean
    
    
    scores <- data.frame(
      SE_mu_b_ave = (pred_mu_b$mu - pred_mu_b$pred.mu_ave)^2,
      SE_mu_b = (pred_mu_b$mu - pred_mu_b$pred.mu)^2)
    
    scores2 <- data.frame(
      post_E = pred_vals$eta$mean,
      SE_mu_B = (mu_B_centroid$mu - pred_vals$eta$mean)^2,
      post_Var = pred_vals$eta$sd^2)
    
  }else{
    
    newdata <- st_centroid(data$nested_grids_sf_all)
    pred_vals <- predict(fit_res$res, newdata,
                         formula = ~ {
                           E <- intercept + covariate
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    post_E <- pred_vals$eta$mean
    scores <- data.frame(
      SE_mu_b = (newdata$mu - post_E)^2)
    
    
    nested_grids_sf_all_centroid <- st_centroid(data$nested_grids_sf_all)
    nested_grids_sf_all_centroid$weight <- 1

    pred_vals <- predict(fit_res$res, nested_grids_sf_all_centroid,
                         formula = ~ {
                           E <- fm_block_eval(
                             block = block,
                             n_block = nrow(data$mu_B),
                             weights = weight,
                             rescale = TRUE,
                             values = intercept + covariate
                           )
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    
    scores2 <- data.frame(
      post_E = pred_vals$eta$mean,
      SE_mu_B = (data$mu_B$mu - pred_vals$eta$mean)^2,
      post_Var = pred_vals$eta$sd^2)
    
  }
  
  return(list(scores = scores,
              scores2 = scores2))
  
}



compute_other_scores_spatial <- function(data = sim_data,
                                         fit_res = fit_old,
                                         block){
  
  
  
  if(block == FALSE){
    
    mu_B_centroid <- st_centroid(data$mu_B)
    
    pred_vals <- predict(fit_res$res, mu_B_centroid,
                         formula = ~ {
                           E <- intercept + covariate + sp_field
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    
    pred_mu_b <- data.frame(block = 1:nrow(pred_vals$eta),
                            pred.mu_ave = pred_vals$eta$mean)
    pred_mu_b <- left_join(data$nested_grids_sf_all,
                           pred_mu_b,
                           by = "block")
    
    pred_mu_b_centroid <- st_centroid(pred_mu_b)
    pred_vals_new <- predict(fit_res$res, pred_mu_b_centroid,
                             formula = ~ {
                               E <- intercept + covariate + sp_field
                               list(
                                 eta = E
                               )
                             },
                             n.samples = 2000
    )
    pred_mu_b$pred.mu <- pred_vals_new$eta$mean
    
    scores <- data.frame(
      SE_mu_b_ave = (pred_mu_b$mu - pred_mu_b$pred.mu_ave)^2,
      SE_mu_b = (pred_mu_b$mu - pred_mu_b$pred.mu)^2)
    
    scores2 <- data.frame(
      post_E = pred_vals$eta$mean,
      SE_mu_B = (mu_B_centroid$mu - pred_vals$eta$mean)^2,
      post_Var = pred_vals$eta$sd^2)
    
  }else{
    
    newdata <- st_centroid(data$nested_grids_sf_all)
    pred_vals <- predict(fit_res$res, newdata,
                         formula = ~ {
                           E <- intercept + covariate + sp_field
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    post_E <- pred_vals$eta$mean
    scores <- data.frame(
      SE_mu_b = (newdata$mu - post_E)^2)
    
    
    nested_grids_sf_all_centroid <- st_centroid(data$nested_grids_sf_all)
    nested_grids_sf_all_centroid$weight <- 1

    pred_vals <- predict(fit_res$res, nested_grids_sf_all_centroid,
                         formula = ~ {
                           E <- fm_block_eval(
                             block = block,
                             n_block = nrow(data$mu_B),
                             weights = weight,
                             rescale = TRUE,
                             values = intercept + covariate + sp_field
                           )
                           list(
                             eta = E
                           )
                         },
                         n.samples = 2000
    )
    scores2 <- data.frame(
      post_E = pred_vals$eta$mean,
      SE_mu_B = (data$mu_B$mu - pred_vals$eta$mean)^2,
      post_Var = pred_vals$eta$sd^2)
    
  }
  
  return(list(scores = scores,
              scores2 = scores2))
  
}



clear_compile_new_scores <- function(n, nsim){
  
  compile_post_var <- vector("list", length = 3)
  names(compile_post_var) <- c("block","old","mrf")
  
  compile_coverage <- vector("list", length = 3)
  compile_coverage[[1]] <- matrix(NA, nrow = nsim, ncol = n)  
  compile_coverage[[2]] <- matrix(NA, nrow = nsim, ncol = n)  
  compile_coverage[[3]] <- matrix(NA, nrow = nsim, ncol = n)
  names(compile_coverage) <- c("block","old","mrf")
  
  compile_post_var_mu <- vector("list", length = 3)
  names(compile_post_var_mu) <- c("block","old","mrf")
  
  compile_coverage_mu <- vector("list", length = 3)
  compile_coverage_mu[[1]] <- matrix(NA, nrow = nsim, ncol = 100)  
  compile_coverage_mu[[2]] <- matrix(NA, nrow = nsim, ncol = 100)  
  compile_coverage_mu[[3]] <- matrix(NA, nrow = nsim, ncol = 100)  
  names(compile_coverage_mu) <- c("block","old","mrf")
  
  return(out = list(compile_post_var = compile_post_var,
                    compile_coverage = compile_coverage,
                    compile_post_var_mu = compile_post_var_mu,
                    compile_coverage_mu = compile_coverage_mu))
  
}

clear_compile_new_scores_mrf <- function(n, nsim){
  
  compile_post_var <- vector("list", length = 1)

  compile_coverage <- vector("list", length = 1)
  compile_coverage[[1]] <- matrix(NA, nrow = nsim, ncol = n)  

  compile_post_var_mu <- vector("list", length = 1)

  compile_coverage_mu <- vector("list", length = 1)
  compile_coverage_mu[[1]] <- matrix(NA, nrow = nsim, ncol = 100)  

  return(out = list(compile_post_var = compile_post_var,
                    compile_coverage = compile_coverage,
                    compile_post_var_mu = compile_post_var_mu,
                    compile_coverage_mu = compile_coverage_mu))
  
}


fit_old_mrf_approach <- function(shp = sim_data$areas_C,
                                 data,
                                 samp_prop){
  
  areas_C <- sim_data$areas_C
  names(areas_C) <- c("geometry","area_id")
  
  data_Y_centroid <- st_centroid(data$data_Y)
  
  data_Y_centroid <- st_join(data_Y_centroid, areas_C, join = st_within)
  
  nb <- poly2nb(as(shp, "Spatial"))
  B <- nb2mat(nb, style = "B", zero.policy = TRUE) # binary yes/no neighbours
  x <- mat2listw(B, style = "W", zero.policy = TRUE)
  
  adj_file <- "gaussian.adj"
  
  if (!file.exists(adj_file)) {
    nb2INLA(adj_file, x$neighbours)
  }
  
  g <- inla.read.graph("gaussian.adj")
  
  cmp <-  ~ -1 + intercept(1) +
    covariate(covariate, model = "linear") +
    bym2(area_id, model = "bym2", graph = g)
  
  fit_mrf = bru(components = cmp,
                formula = y ~ intercept + covariate + bym2,
                data = data_Y_centroid,
                family = "gaussian",
                options = list(control.inla = list(int.strategy = "eb"),
                               control.compute = list(dic = TRUE, waic = TRUE),
                               control.family = list(
                                 hyper = list(
                                   prec = list(
                                     prior = "pc.prec",
                                     param = c(1, 0.5)  # (lambda, alpha)
                                   )
                                 )),
                               bru_verbose = FALSE,
                               bru_max_iter = 10,
                               bru_method=list(rel_tol = 0.10)))  
  
  return(out = list(res = fit_mrf))
  
}




compute_proper_scores_mrf <- function(shp = sim_data$areas_C,
                                      data = sim_data,
                                      fit_res = fit_mrf){ 
  
  areas_C <- sim_data$areas_C
  names(areas_C) <- c("geometry","area_id")
  
  data_Y_centroid <- st_centroid(data$data_Y)
  
  data_Y_centroid <- st_join(data_Y_centroid, areas_C, join = st_within)
  
  pred_vals <- predict(fit_res$res, data_Y_centroid,
                       formula = ~ {
                         E <- intercept + covariate + bym2
                         V <- 1 / Precision_for_the_Gaussian_observations
                         list(
                           eta = E,
                           sigma_y_2 = V,
                           dens = dnorm(y, mean = E, sd = sqrt(V))
                         )
                       },
                       n.samples = 2000
  )
  
  post_E <- pred_vals$eta$mean
  post_Var <- pred_vals$sigma_y_2$mean + pred_vals$eta$sd^2
  
  scores <- data.frame(
    SE = (data_Y_centroid$y - post_E)^2,
    DS = (data_Y_centroid$y - post_E)^2 / post_Var + log(post_Var),
    LS = -log(pred_vals$dens$mean),
    post_Var = post_Var,
    post_E = post_E
  )
  
  return(list(scores = scores))
  
}




compute_other_scores_mrf <- function(shp = sim_data$areas_C,
                                     data = sim_data,
                                     fit_res = fit_mrf){
  
  
  data$mu_B$area_id <- data$mu_B$block
  
  pred_vals <- predict(fit_res$res, data$mu_B,
                       formula = ~ {
                         E <- intercept + covariate + bym2
                         list(
                           eta = E
                         )
                       },
                       n.samples = 2000
  )
  
  pred_mu_b <- data.frame(block = 1:nrow(pred_vals$eta),
                          pred.mu_ave = pred_vals$eta$mean)
  pred_mu_b <- left_join(data$nested_grids_sf_all,
                         pred_mu_b,
                         by = "block")
  pred_mu_b$area_id <- pred_mu_b$block
  
  pred_vals_new <- predict(fit_res$res, pred_mu_b,
                           formula = ~ {
                             E <- intercept + covariate + bym2
                             list(
                               eta = E
                             )
                           },
                           n.samples = 2000
  )
  pred_mu_b$pred.mu <- pred_vals_new$eta$mean
  
  scores <- data.frame(
    SE_mu_b_ave = (pred_mu_b$mu - pred_mu_b$pred.mu_ave)^2,
    SE_mu_b = (pred_mu_b$mu - pred_mu_b$pred.mu)^2)
  
  scores2 <- data.frame(
    post_E = pred_vals$eta$mean,
    SE_mu_B = (data$mu_B$mu - pred_vals$eta$mean)^2,
    post_Var = pred_vals$eta$sd^2)
  
  
  return(list(scores = scores,
              scores2 = scores2))
  
}

