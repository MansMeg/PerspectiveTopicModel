# Compute the maximum log marginal posterior

# Compute the bar-chart for 1000 random initializations.
library(ggplot2)

source("experiments/experiment_functions.R")
load("results3random_crp1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19400"

# which(experiment_jobs$max_lmp < -19600)
# results[[1]]$final$count_matrices

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

plt <- ggplot(data = experiment_jobs, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "mlpd_experiments3_crp2.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "mlpd_experiments3_crp2.rda")


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



rm(list = ls())

source("experiments/experiment_functions.R")
load("results3b_crp1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -20200] <- "-20000"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19200"

# which(experiment_jobs$mode == "-20000")
# results[[10]]$final$count_matrices

plt <- ggplot(data = experiment_jobs, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3b_crp1_histogram.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3b_crp1_histogram.rda")

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3b_crp1.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3b_crp1.rda")



rm(list = ls())

source("experiments/experiment_functions.R")
load("results3b_crp2.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-20400"
experiment_jobs$mode[experiment_jobs$max_lmp > -19600] <- "-19200"

plt <- ggplot(data = experiment_jobs, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3b_crp2_histogram.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3b_crp2_histogram.rda")

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3b_crp2.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments3b_crp2.rda")





rm(list = ls())

source("experiments/experiment_functions.R")
load("results3c_crp2_1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- experiment_jobs

rm(experiment_jobs, results)
load("results3c_crp2_2.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

rm(experiment_jobs, results)
load("results3c_crp2_3.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

rm(experiment_jobs, results)
load("results3c_crp2_4.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

rm(experiment_jobs, results)
load("results3b_crp2.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs <- experiment_jobs[experiment_jobs$kappa < 128,]
exp_data <- rbind(exp_data, experiment_jobs)

# Setup variables
exp_data$mode <- "-20400"
exp_data$mode[exp_data$max_lmp > -19600] <- "-19200"

plt <- ggplot(data = exp_data, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3c_crp2_histogram.png", plot = plt, width = 5, height = 3)
save(plt, exp_data, file = "kappa_experiments3c_crp2_histogram.rda")

plt <- ggplot(data = exp_data, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3c_crp2.png", plot = plt, width = 12, height = 3)
save(plt, exp_data, file = "kappa_experiments3c_crp2.rda")








# Experiment 3D

rm(list = ls())

source("experiments/experiment_functions.R")
load("results3d_crp2_1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- experiment_jobs

rm(experiment_jobs, results)
load("results3d_crp2_2.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

rm(experiment_jobs, results)
load("results3d_crp2_3.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

rm(experiment_jobs, results)
load("results3d_crp2_4.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

# Setup variables
exp_data$mode <- "-20400"
exp_data$mode[exp_data$max_lmp > -19600] <- "-19200"

plt <- ggplot(data = exp_data, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3d_crp2_histogram.png", plot = plt, width = 5, height = 3)
save(plt, exp_data, file = "kappa_experiments3d_crp2_histogram.rda")

plt <- ggplot(data = exp_data, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3d_crp2.png", plot = plt, width = 12, height = 3)
save(plt, exp_data, file = "kappa_experiments3d_crp2.rda")







# Experiment 3D

rm(list = ls())

source("experiments/experiment_functions.R")
load("experiment3e1_crp4_1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- experiment_jobs

rm(experiment_jobs, results)
load("experiment3e1_crp4_2.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

# Setup variables
exp_data$mode <- "-20400"
exp_data$mode[exp_data$max_lmp > -19600] <- "-19200"

plt <- ggplot(data = exp_data, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3e_crp4_histogram.png", plot = plt, width = 5, height = 3)
save(plt, exp_data, file = "kappa_experiments3e_crp4_histogram.rda")

plt <- ggplot(data = exp_data, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3e_crp4.png", plot = plt, width = 12, height = 3)
save(plt, exp_data, file = "kappa_experiments3e_crp4.rda")



rm(list = ls())

source("experiments/experiment_functions.R")
load("experiment3e1_crp5_1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- experiment_jobs

rm(experiment_jobs, results)
load("experiment3e1_crp5_2.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
exp_data <- rbind(exp_data, experiment_jobs)

# Setup variables
exp_data$mode <- "-20400"
exp_data$mode[exp_data$max_lmp > -19600] <- "-19200"

plt <- ggplot(data = exp_data, aes(x = max_lmp)) + geom_histogram() + theme_bw() + xlab("Maximum Log Posterior Density") + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3e_crp5_histogram.png", plot = plt, width = 5, height = 3)
save(plt, exp_data, file = "kappa_experiments3e_crp5_histogram.rda")

plt <- ggplot(data = exp_data, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments3e_crp5.png", plot = plt, width = 12, height = 3)
save(plt, exp_data, file = "kappa_experiments3e_crp5.rda")
