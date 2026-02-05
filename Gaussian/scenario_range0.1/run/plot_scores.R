
rm(list=ls())

library(dplyr)
library(ggplot2)

#main_path <- "Z:/home/simulation_block_approach/New/Gaussian/results/compile_scores/"
main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/scenario_range0.1/results/compile_scores/"

sim_params <- expand.grid(maxunif = 20,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.1, # range of matern
                          sigma2 = c(2, 4), # marginal variance of matern
                          sigma2error = 1)
sim_params$sim <- 1:nrow(sim_params)
sim_params$samp_prop <- as.factor(sim_params$samp_prop)

load(file = paste0(main_path,"properscores.Rdata"))
load(file = paste0(main_path,"otherscores.Rdata"))
load(file = paste0(main_path,"paramscores_fixedeffects.Rdata"))
load(file = paste0(main_path,"paramscores_hyperparams.Rdata"))
load(file = paste0(main_path,"all_coverage_scores_mu.Rdata"))
load(file = paste0(main_path,"all_coverage_scores_y.Rdata"))


all_proper_scores <- left_join(all_proper_scores,
                               sim_params,
                               by = "sim")
all_other_scores <- left_join(all_other_scores,
                              sim_params,
                              by = "sim")
fixedparam_scores <- left_join(fixedparam_scores,
                               sim_params,
                               by = "sim")
hyperparam_scores <- left_join(hyperparam_scores,
                               sim_params,
                               by = "sim")
all_coverage_scores_mu <- left_join(all_coverage_scores_mu,
                                    sim_params,
                                    by = "sim")
all_coverage_scores_y <- left_join(all_coverage_scores_y,
                                    sim_params,
                                    by = "sim")

                               
                              
all_proper_scores <- all_proper_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range",
    )
  )
all_proper_scores <- all_proper_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "2" ~ "Low variance",
      sigma2 == "4"  ~ "High variance"
    )
  )
all_other_scores <- all_other_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range",
    )
  )
all_other_scores <- all_other_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "2" ~ "Low variance",
      sigma2 == "4"  ~ "High variance"
    )
  )
fixedparam_scores <- fixedparam_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range",
    )
  )
fixedparam_scores <- fixedparam_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "2" ~ "Low variance",
      sigma2 == "4"  ~ "High variance"
    )
  )
hyperparam_scores <- hyperparam_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range",
    )
  )
hyperparam_scores <- hyperparam_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "2" ~ "Low variance",
      sigma2 == "4"  ~ "High variance"
    )
  )
all_coverage_scores_mu <- all_coverage_scores_mu %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range",
    )
  )
all_coverage_scores_mu <- all_coverage_scores_mu %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "2" ~ "Low variance",
      sigma2 == "4"  ~ "High variance"
    )
  )
all_coverage_scores_y<- all_coverage_scores_y %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range",
    )
  )
all_coverage_scores_y <- all_coverage_scores_y %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "2" ~ "Low variance",
      sigma2 == "4"  ~ "High variance"
    )
  )



save(all_proper_scores,
     all_other_scores,
     fixedparam_scores,
     hyperparam_scores,
     all_coverage_scores_mu,
     all_coverage_scores_y,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/merge_all_scenarios/Rdatafiles/scores_mid_range.Rdata")



