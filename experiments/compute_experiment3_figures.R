# Compute the maximum log marginal posterior

# Compute the bar-chart for 1000 random initializations.
library(ggplot2)

source("experiments/experiment_functions.R")
load("results3random_crp1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19400"

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3_crp1.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3_crp1.rda")



rm(list = ls())

source("experiments/experiment_functions.R")
load("results3random_crp2.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19400"

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3_crp2.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3_crp2.rda")


rm(list = ls())

source("experiments/experiment_functions.R")
load("results3random_crp3.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
plt <- ggplot(data = experiment_jobs, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "mlpd_experiments3_crp3.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "mlpd_experiments3_crp3.rda")





rm(list = ls())

source("experiments/experiment_functions.R")
load("experiment3langinit_crp1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19400"

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3_crp1_initlang.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3_crp1_initlang.rda")


rm(list = ls())

source("experiments/experiment_functions.R")
load("experiment3langinit_crp2.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19400"

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3_crp2_initlang.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3_crp2_initlang.rda")





