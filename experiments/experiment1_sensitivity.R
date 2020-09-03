
# Experiment 1 Sensitivity:
# Will simulated annealing end up in a better mode?
library(PerspectiveTopicModel)
source("experiments/experiment_functions.R")


## Run experiment 1 sensitivity 1 (double sized corpus)
experiment1sens1 <- expand.grid(m = 800,
                                kappa = c(2400, 2^(0:6)),
                                C_kappa = 25,
                                init = "word2",
                                alpha = 1,
                                beta = 1,
                                seed = 4711:4810,
                                stringsAsFactors = FALSE)
results1 <- run_experiment1(experiment_jobs = experiment1sens1,
                           result_file_name = "results_exp1sens1.rda")

## Run experiment 1 sensitivity 2 (duplicate documents)
experiment1sens2 <- expand.grid(m = 400,
                                ndoc = 2,
                                kappa = c(2400, 2^(0:6)),
                                C_kappa = 25,
                                init = "word2",
                                alpha = 1,
                                beta = 1,
                                seed = 4711:4810,
                                stringsAsFactors = FALSE)
results2 <- run_experiment1(experiment_jobs = experiment1sens2,
                            result_file_name = "results_exp1sens2.rda")

## Run experiment 1 sensitivity 3 (Different no of iterations)
experiment1sens3 <- expand.grid(m = 400,
                                kappa = c(8, 16),
                                C_kappa = c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2),
                                init = "word2",
                                alpha = 1,
                                beta = 1,
                                seed = 4711:5210,
                                stringsAsFactors = FALSE)
results3 <- run_experiment1(experiment_jobs = experiment1sens3,
                            result_file_name = "results_exp1sens3.rda")


