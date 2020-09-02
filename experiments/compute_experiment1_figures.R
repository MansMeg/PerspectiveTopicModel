# Compute the maximum log marginal posterior

# Compute the bar-chart for 1000 random initializations.
source("experiments/experiment_functions.R")
library(ggplot2)

load("results_experiment1f.rda")

df <- data.frame(max_lmp = extract_max_log_marginal_posterior(results))
df$mode <- "-676.9"
df$mode[df$max_lmp > -650] <- "-648.1"
df$mode[df$max_lmp > -600] <- "-545.6"

plt <- ggplot(data = df, aes(x = mode)) + geom_bar() + theme_bw() + xlab("Mode") + ylab("Count")
ggsave(filename = "random_init.png", plot = plt, width = 5, height = 3)
save(plt, df, file = "random_init.rda")


load("results_experiment1e.rda")
experiment_jobs$max_lmp <- extract_max_log_marginal_posterior(results)
experiment_jobs$Mode <- "-676.9"
experiment_jobs$Mode[experiment_jobs$max_lmp > -650] <- "-648.1"
experiment_jobs$Mode[experiment_jobs$max_lmp > -600] <- "-545.6"
#experiment_jobs$kappa <- as.integer(experiment_jobs$kappa)

plt <- ggplot(data = experiment_jobs, aes(x = as.factor(kappa), fill = Mode)) + geom_bar(position = position_dodge(preserve = 'single')) + theme_bw() + xlab(expression(kappa)) + ylab("Count") + scale_fill_grey()
ggsave(filename = "kappa_experiments.png", plot = plt, width = 5, height = 3)
save(plt, experiment_jobs, file = "kappa_experiments.rda")


