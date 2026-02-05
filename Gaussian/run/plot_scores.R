
rm(list=ls())

library(dplyr)
library(ggplot2)

main_path <- "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/compile_scores/"

sim_params <- expand.grid(maxunif = 20,
                          samp_prop = c(0.3,0.6,1),
                          rho = c(0.05, 0.4), # range of matern
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
      rho == "0.05" ~ "Low range",
      rho == "0.4"  ~ "High range"
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
     file = "/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/merge_all_scenarios/Rdatafiles/scores_highlow_range.Rdata")




theme <- theme(axis.text = element_text(size = 14),
               axis.title = element_text(size = 14, face = "bold"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16, face = "bold"),
               strip.text = element_text(size = 16, face = "bold"),
               plot.title = element_text(size = 18))



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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/ds.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_ds)                              
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/logscore.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_logscore)                              
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/rmse.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_rmse)                      
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/rmse_small_b.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/rmse_big_b.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_rmse_big_b)    
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/relbias_beta0.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/relbias_beta1.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_relbias_beta1)   
dev.off()


hyperparam_scores_A <- hyperparam_scores[which(hyperparam_scores$Approach %in% c("block","old")),]
hyperparam_scores_A$param <- rep(c("prec","rho","marg.sd"), length = nrow(hyperparam_scores_A))
hyperparam_scores_B <- hyperparam_scores[which(hyperparam_scores$Approach %in% c("mrf")),]

prec_scores <- rbind(hyperparam_scores_A[which(hyperparam_scores_A$param == "prec"),
                                         -which(names(hyperparam_scores_A) == "param")],
                     hyperparam_scores_B)
plot_precerror <- ggplot(prec_scores, 
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
print(plot_precerror)   

rho_scores <- hyperparam_scores_A[which(hyperparam_scores_A$param == "rho"),]
plot_rho <- ggplot(rho_scores[-which(rho_scores$relbias>=200),], 
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
print(plot_rho)   

marg.sd_scores <- hyperparam_scores_A[which(hyperparam_scores_A$param == "marg.sd"),]
plot_marg.sd <- ggplot(marg.sd_scores[-which(marg.sd_scores$relbias>=300),], 
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
print(plot_marg.sd)   





plot_relbias_beta0_lines <- ggplot(fixedparam_scores[which(fixedparam_scores$param == "beta0"),],
                                aes(x = samp_prop, y = relbias, color = Approach, group = Approach)) +
  # geom_point(
  #   position = position_jitter(width = 0.15, height = 0, seed = 1),
  #   alpha = 0.35, size = 0.6, show.legend = FALSE
  # ) +
  # Mean points per x & Approach
  stat_summary(
    fun = mean, geom = "point",
    size = 2.5, position = position_dodge(width = 0.1)
  ) +
  # Mean lines across samp_prop per Approach
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, position = position_dodge(width = 0.1)
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  ) +
  theme(legend.position = "none")
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/relbias_beta0_lines.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_relbias_beta0_lines)
dev.off()

plot_relbias_beta1_lines <- ggplot(fixedparam_scores[which(fixedparam_scores$param == "beta1"),],
                                aes(x = samp_prop, y = relbias, color = Approach, group = Approach)) +
  # geom_point(
  #   position = position_jitter(width = 0.15, height = 0, seed = 1),
  #   alpha = 0.35, size = 0.6, show.legend = FALSE
  # ) +
  # Mean points per x & Approach
  stat_summary(
    fun = mean, geom = "point",
    size = 2.5, position = position_dodge(width = 0.1)
  ) +
  # Mean lines across samp_prop per Approach
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, position = position_dodge(width = 0.1)
  ) +
  facet_grid(sigma2_label~ rho_label) +
  scale_color_brewer(
    palette = "Set1",
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  ) +
  theme(legend.position = "none")
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/relbias_beta1_lines.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/coverage_mu.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
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
    breaks = c("block", "old", "mrf"),
    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")
  ) +
  scale_fill_brewer(palette = "Set1",
                    labels = c("block" = "Proposed", "old" = "Centroids", "mrf" = "MRF")) +
  theme +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  xlab("Sampling proportion") +
  ylab("") +
  theme(
    legend.title = element_text(size = 23),   # Legend title size
    legend.text  = element_text(size = 23),   # Legend labels size
    legend.key.size = unit(1.5, "lines"),      # Size of legend keys
    strip.text = element_text(size = 22),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(face = "bold")
  )
png("/rds/general/user/sv20/home/simulation_block_approach/New/Gaussian/results/figures/coverage_y.png", width=24, height=16, units = 'cm', res = 300, type = "cairo")
print(plot_coverage_mu_y)    
dev.off()