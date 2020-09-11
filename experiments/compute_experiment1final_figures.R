
load("results_experiment1final.rda")
source("experiments/experiment_functions.R")
library(ggplot2)


# Order
ordr <- order(experiment_jobs$kappa, experiment_jobs$seed)
experiment_jobs <- experiment_jobs[ordr,]
results <- results[ordr]


# Order
ordr <- order(experiment_jobs$kappa, experiment_jobs$seed)
experiment_jobs <- experiment_jobs[ordr,]
results <- results[ordr]


for(i in 1:length(results)){
  idx <- i
  ggd <- setup_ggplot_data(results[idx], experiment_jobs[idx, c("C_kappa", "kappa")])

  plt1 <- ggplot(data = ggd, aes(x = iteration, y = log_post)) +
    geom_hline(yintercept = -545.5614, lty = "dotted") +
    geom_hline(yintercept = -648.1314, lty = "dotted") +
    geom_hline(yintercept = -676.8834, lty = "dotted") +
    geom_hline(yintercept = -703.6333, lty = "dotted") +
    geom_line() + #  + ylim(-800, -500)
    geom_vline(xintercept = 6000, lty = 2) +
    ylab("Log Marginal Posterior") + xlab("Iteration")
#    ggtitle(paste0("C_kappa: ", ggd$C_kappa[1], ", kappa: ", ggd$kappa[1]))
  pn <- paste0("lmp_kappa", ggd$kappa[i], "_C", ggd$C_kappa[i], "_seed", experiment_jobs$seed[i])
  ggsave(plt1, filename = paste0("figures/", pn, ".png"), width = 7, height = 3)
  save(plt1, file = paste0("figures/", pn, ".rda"))
  suppressWarnings(print(plt1))

}
