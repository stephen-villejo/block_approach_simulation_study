
rm(list=ls())

library(dplyr)
library(ggplot2)

main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/scenario_range0.1/results/compile_scores/"

sim_params <- expand.grid(maxunif = 40,
                          samp_prop = c(0.3,0.6,1),
                          rho = 0.1, # range of matern
                          sigma2 = c(0.05, 0.15)) # marginal variance of matern
sim_params$sim <- 1:nrow(sim_params)
sim_params$samp_prop <- as.factor(sim_params$samp_prop)


load(file = paste0(main_path,"properscores.Rdata"))
all_proper_scores_sum <- all_proper_scores
load(file = paste0(main_path,"properscores_covmeaninput.Rdata"))
all_proper_scores_mean <- all_proper_scores
all_proper_scores_mean <- all_proper_scores_mean[-which(all_proper_scores_mean$Approach == "block"),]
all_proper_scores_mean <- all_proper_scores_mean %>%
  mutate(Approach = recode(Approach,
                           "old" = "old (mean)",
                           "mrf" = "mrf (mean)"))
all_proper_scores <- rbind(all_proper_scores_sum,
                           all_proper_scores_mean)

load(file = paste0(main_path,"otherscores.Rdata"))
all_other_scores_sum <- all_other_scores
load(file = paste0(main_path,"otherscores_covmeaninput.Rdata"))
all_other_scores_mean <- all_other_scores
all_other_scores_mean <- all_other_scores_mean[-which(all_other_scores_mean$Approach == "block"),]
all_other_scores_mean <- all_other_scores_mean %>%
  mutate(Approach = recode(Approach,
                           "old" = "old (mean)",
                           "mrf" = "mrf (mean)"))
all_other_scores <- rbind(all_other_scores_sum,
                          all_other_scores_mean)

load(file = paste0(main_path,"paramscores_fixedeffects.Rdata"))
fixedparam_scores_sum <- fixedparam_scores
load(file = paste0(main_path,"paramscores_fixedeffects_covmeaninput.Rdata"))
fixedparam_scores_mean <- fixedparam_scores
fixedparam_scores_mean <- fixedparam_scores_mean[-which(fixedparam_scores_mean$Approach == "block"),]
fixedparam_scores_mean <- fixedparam_scores_mean %>%
  mutate(Approach = recode(Approach,
                           "old" = "old (mean)",
                           "mrf" = "mrf (mean)"))
fixedparam_scores <- rbind(fixedparam_scores_sum,
                           fixedparam_scores_mean)


load(file = paste0(main_path,"paramscores_hyperparams.Rdata"))
hyperparam_scores_sum <- hyperparam_scores
load(file = paste0(main_path,"paramscores_hyperparams_covmeaninput.Rdata"))
hyperparam_scores_mean <- hyperparam_scores
hyperparam_scores_mean <- hyperparam_scores_mean[-which(hyperparam_scores_mean$Approach == "block"),]
hyperparam_scores_mean <- hyperparam_scores_mean %>%
  mutate(Approach = recode(Approach,
                           "old" = "old (mean)",
                           "mrf" = "mrf (mean)"))
hyperparam_scores <- rbind(hyperparam_scores_sum,
                           hyperparam_scores_mean)


load(file = paste0(main_path,"all_coverage_scores_mu.Rdata"))
all_coverage_scores_mu_sum <- all_coverage_scores_mu
load(file = paste0(main_path,"all_coverage_scores_mu_covmeaninput.Rdata"))
all_coverage_scores_mu_mean <- all_coverage_scores_mu
all_coverage_scores_mu_mean <- all_coverage_scores_mu_mean[-which(all_coverage_scores_mu_mean$Approach == "block"),]
all_coverage_scores_mu_mean <- all_coverage_scores_mu_mean %>%
  mutate(Approach = recode(Approach,
                           "old" = "old (mean)",
                           "mrf" = "mrf (mean)"))
all_coverage_scores_mu <- rbind(all_coverage_scores_mu_sum,
                                all_coverage_scores_mu_mean)

load(file = paste0(main_path,"all_coverage_scores_y.Rdata"))
all_coverage_scores_y_sum <- all_coverage_scores_y
load(file = paste0(main_path,"all_coverage_scores_y_covmeaninput.Rdata"))
all_coverage_scores_y_mean <- all_coverage_scores_y
all_coverage_scores_y_mean <- all_coverage_scores_y_mean[-which(all_coverage_scores_y_mean$Approach == "block"),]
all_coverage_scores_y_mean <- all_coverage_scores_y_mean %>%
  mutate(Approach = recode(Approach,
                           "old" = "old (mean)",
                           "mrf" = "mrf (mean)"))
all_coverage_scores_y <- rbind(all_coverage_scores_y_sum,
                               all_coverage_scores_y_mean)




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
      rho == "0.1" ~ "Mid range"
    )
  )
all_proper_scores <- all_proper_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "0.05" ~ "Low variance",
      sigma2 == "0.15"  ~ "High variance"
    )
  )
all_other_scores <- all_other_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range"
    )
  )
all_other_scores <- all_other_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "0.05" ~ "Low variance",
      sigma2 == "0.15"  ~ "High variance"
    )
  )
fixedparam_scores <- fixedparam_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range"
    )
  )
fixedparam_scores <- fixedparam_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "0.05" ~ "Low variance",
      sigma2 == "0.15"  ~ "High variance"
    )
  )
hyperparam_scores <- hyperparam_scores %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range"
    )
  )
hyperparam_scores <- hyperparam_scores %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "0.05" ~ "Low variance",
      sigma2 == "0.15"  ~ "High variance"
    )
  )
all_coverage_scores_mu <- all_coverage_scores_mu %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range"
    )
  )
all_coverage_scores_mu <- all_coverage_scores_mu %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "0.05" ~ "Low variance",
      sigma2 == "0.15"  ~ "High variance"
    )
  )
all_coverage_scores_y <- all_coverage_scores_y %>%
  mutate(
    rho_label = case_when(
      rho == "0.1" ~ "Mid range"
    )
  )
all_coverage_scores_y <- all_coverage_scores_y %>%
  mutate(
    sigma2_label = case_when(
      sigma2 == "0.05" ~ "Low variance",
      sigma2 == "0.15"  ~ "High variance"
    )
  )


save(all_proper_scores,
     all_other_scores,
     fixedparam_scores,
     hyperparam_scores,
     all_coverage_scores_mu,
     all_coverage_scores_y,
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/merge_all_scenarios/Rdatafiles/scores_mid_range.Rdata")


