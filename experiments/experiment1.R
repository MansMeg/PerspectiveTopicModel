# TODO: Profile

# Experiment 1:
# Will simulated annealing end up in a better mode?
library(PerspectiveTopicModel)
source("experiments/experiment_functions.R")

## Parameters Experiment 1
m <- 400
kappa <- c(1200, 1, 2^(1:3), 2^{-(1:3)}) # 1200 will create tau = 1, hence ordinary gibbs sampling
C_kappa <- c(25, 50, 100, 200)
init <- "word2"
alpha <-  1
beta <- 1
seed <- 4711

## Parameters Experiment 1.2
m2 <- m
D2 <- D
kappa2 <- c(1200, 1) # 1200 will create tau = 1, hence ordinary gibbs sampling
C_kappa2 <- c(1)
init2 <- "random"
priors2 <- list(alpha = 1, beta = 1)
seed2 <- 4711:4720



## Compute log marginal posterior
priors <- list(alpha = 1, beta = 1)
s1 <- generate_experiment1_corpus(400, "word1")
cm1 <- init_count_matrices_lda(s1)
log_marginal_posterior_lda(cm1, priors)

s2 <- generate_experiment1_corpus(400, "word2")
cm2 <- init_count_matrices_lda(s2)
log_marginal_posterior_lda(cm2, priors)

s3 <- generate_experiment1_corpus(400, "word3")
cm3 <- init_count_matrices_lda(s3)
log_marginal_posterior_lda(cm3, priors)

random_lmp <- numeric(100)
set.seed(4711)
for(i in seq_along(random_lmp)){
  s3$topic <- sample(1:2, nrow(s3), TRUE)
  cm3r <- init_count_matrices_lda(s3)
  random_lmp[i] <- log_marginal_posterior_lda(cm3r, priors)
}
mean(random_lmp)



## Run experiment 2
experiment_jobs2 <- expand.grid(m = m2,
                                kappa = kappa2,
                                C_kappa = C_kappa2,
                                init = init2,
                                alpha = alpha,
                                beta = beta,
                                seed = seed2,
                                stringsAsFactors = FALSE)
results <- run_experiment1(experiment_jobs = experiment_jobs2,
                           "results_experiment1b.rda")


## Run experiment 1
experiment_jobs1 <- expand.grid(m = m,
                                kappa = kappa,
                                C_kappa = C_kappa,
                                init = init,
                                alpha = alpha,
                                beta = beta,
                                seed = seed,
                                stringsAsFactors = FALSE)
results <- run_experiment1(experiment_jobs = experiment_jobs1,
                           "results_experiment1a.rda")
