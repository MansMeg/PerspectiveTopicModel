# Compute the maximum log marginal posterior

# Compute the bar-chart for 1000 random initializations.
library(ggplot2)

source("experiments/experiment_functions.R")

load("results_exp1sens1.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-1360"
experiment_jobs$mode[experiment_jobs$max_lmp > -1310] <- "-1303"
experiment_jobs$mode[experiment_jobs$max_lmp > -1100] <- "-1095"
#experiment_jobs$kappa <- as.integer(experiment_jobs$kappa)

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments_sens1.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments_sens1.rda")


rm(list = ls())
source("experiments/experiment_functions.R")
load("results_exp1sens2.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-1360"
experiment_jobs$mode[experiment_jobs$max_lmp > -1310] <- "-1303"
experiment_jobs$mode[experiment_jobs$max_lmp > -1100] <- "-1095"
#experiment_jobs$kappa <- as.integer(experiment_jobs$kappa)

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments_sens2.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments_sens2.rda")



rm(list = ls())
source("experiments/experiment_functions.R")
load("results_exp1sens3.rda")

experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$mode <- "-676.9"
experiment_jobs$mode[experiment_jobs$max_lmp > -670] <- "-648.1"
experiment_jobs$mode[experiment_jobs$max_lmp > -640] <- "-545.6"
experiment_jobs$mode <- as.factor(experiment_jobs$mode)

table(experiment_jobs$kappa)
k8 <- experiment_jobs[experiment_jobs$kappa == 8,]
k16 <- experiment_jobs[experiment_jobs$kappa == 16,]

pltk8 <- ggplot(data = k8, aes(x = as.factor(C_kappa), fill = mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(C(kappa))) + ylab("Count") + scale_fill_grey(drop = FALSE)
ggsave(filename = "C_kappa8_experiments_sens3.png", plot = pltk8, width = 5, height = 3)
save(pltk8, k8, file = "C_kappa8_experiments_sens3.rda")

pltk16 <- ggplot(data = k16, aes(x = as.factor(C_kappa), fill = mode)) + geom_bar(position =  position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(C(kappa))) + ylab("Count") + scale_fill_grey(drop = FALSE)
ggsave(filename = "C_kappa16_experiments_sens3.png", plot = pltk16, width = 5, height = 3)
save(pltk16, k16, file = "C_kappa16_experiments_sens3.rda")


