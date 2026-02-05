
rm(list=ls())

library(dplyr)
library(ggplot2)

#main_path <- "Z:/home/simulation_block_approach/New/Poisson/results/compile_scores/"
main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/compile_scores/"

sim_params <- expand.grid(maxunif = 40,
                          samp_prop = c(0.3,0.6,1),
                          rho = c(0.05,0.4), # range of matern
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/merge_all_scenarios/Rdatafiles/scores_highlow_range.Rdata")



theme <- theme(axis.text = element_text(size = 14),
               axis.title = element_text(size = 14, face = "bold"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16, face = "bold"),
               strip.text = element_text(size = 16, face = "bold"),
               plot.title = element_text(size = 18))


library(stringr)
wrapped_labels <- str_wrap(
  c("Proposed", "Centroids", "MRF", "Centroids (mean)", "MRF (mean)"),
  width = 12   # adjust width until it looks good
)


plot_ds <- ggplot(all_proper_scores, aes(x = samp_prop, y = ds   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
    #labels= wrapped_labels
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
                    #labels= wrapped_labels
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/ds_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_ds)                              
dev.off()


plot_rmse <- ggplot(all_proper_scores, aes(x = samp_prop, y = rmse   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/rmse_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_rmse)                              
dev.off()



plot_logscore <- ggplot(all_proper_scores, aes(x = samp_prop, y = logscore   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/logscore_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_logscore)                              
dev.off()



plot_rmse_small_b <- ggplot(all_other_scores, aes(x = samp_prop, y = rmse_mu_b   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/rmse_small_b_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_rmse_small_b)                              
dev.off()


plot_rmse_big_b <- ggplot(all_other_scores, aes(x = samp_prop, y = rmse_mu_B   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/rmse_big_b_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_rmse_small_b)                              
dev.off()



fixedparam_scores$param <- rep(c("beta0","beta1"), length = nrow(fixedparam_scores))

plot_relbias_beta0 <- ggplot(fixedparam_scores[which(fixedparam_scores$param == "beta0"),], 
                             aes(x = samp_prop, y = relbias   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/relbias_beta0_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_relbias_beta0)                              
dev.off()


plot_relbias_beta1 <- ggplot(fixedparam_scores[which(fixedparam_scores$param == "beta1"),], 
                             aes(x = samp_prop, y = relbias   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/relbias_beta1_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_relbias_beta1)                              
dev.off()


plot_relbias_beta0_lines <- ggplot(fixedparam_scores[which(fixedparam_scores$param == "beta0"),], 
                                   aes(x = samp_prop, y = relbias, group = Approach)) +
  # Mean points per x & Approach
  stat_summary(
    aes(color = Approach),
    fun = mean, geom = "point",
    size = 2.5, position = position_dodge(width = 0.1)
  ) +
  # Mean lines across samp_prop per Approach
  stat_summary(
    aes(color = Approach),
    fun = mean, geom = "line",
    linewidth = 1, position = position_dodge(width = 0.1)
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme_bw() +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/relbias_beta0_lines_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_relbias_beta0_lines)                              
dev.off()

plot_relbias_beta1_lines <- ggplot(fixedparam_scores[which(fixedparam_scores$param == "beta1"),], 
                                   aes(x = samp_prop, y = relbias, group = Approach)) +
  # Mean points per x & Approach
  stat_summary(
    aes(color = Approach),
    fun = mean, geom = "point",
    size = 2.5, position = position_dodge(width = 0.1)
  ) +
  # Mean lines across samp_prop per Approach
  stat_summary(
    aes(color = Approach),
    fun = mean, geom = "line",
    linewidth = 1, position = position_dodge(width = 0.1)
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme_bw() +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/relbias_beta1_lines_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_relbias_beta1_lines)                              
dev.off()



plot_coverage_mu_B <- ggplot(all_coverage_scores_mu, aes(x = samp_prop, y = coverage   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/coverage_mu_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_coverage_mu_B)                              
dev.off()



plot_coverage_mu_y <- ggplot(all_coverage_scores_y, aes(x = samp_prop, y = coverage   , fill = Approach)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  theme_bw() +
  geom_point( 
    aes(color = Approach),
    position = position_jitterdodge(
      jitter.width = 0.5, jitter.height = 0,
      dodge.width  = 0.8
    ),
    alpha = 0.5, size = .2,
    show.legend = FALSE  
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  scale_fill_brewer(palette = "Set1",
                    breaks = c("block", "old", "mrf", "old (mean)", "mrf (mean)"),
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF", "old (mean)" = "Centroids\n(mean)",  "mrf (mean)" = "MRF\n(mean)")
  ) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),   # Legend title size
    legend.text  = element_text(size = 18),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Poisson/results/figures/coverage_y_2.png", width=26, height=17, units = 'cm', res = 300, type = "cairo")
print(plot_coverage_mu_y)                              
dev.off()