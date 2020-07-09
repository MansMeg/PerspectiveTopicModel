# TODO: Profile

# Experiment 1:
# Will simulated annealing end up in a better mode?

source("experiments/experiment_functions.R")

## Parameters Experiment 1
m <- 400
kappa <- c(1200, 1, 2^(1:3), 2^{-(1:3)}) # 1200 will create tau = 1, hence ordinary gibbs sampling
C_kappa <- c(25, 50, 100, 200)
start_mode <- "word2"
alpha <-  1
beta <- 1
seed <- 4711

## Parameters Experiment 1.2
m2 <- m
D2 <- D
kappa2 <- c(1200, 1) # 1200 will create tau = 1, hence ordinary gibbs sampling
C_kappa2 <- c(1)
start_mode2 <- start_mode
priors2 <- list(alpha = 1, beta = 1)
seed2 <- 4711:4721



## Compute log marginal posterior
s1 <- generate_experiment1_corpus(400, "word1")
cm1 <- init_count_matrices_lda(s1)
log_marginal_posterior_lda(cm1, priors = list(alpha = 1, beta = 1))

s2 <- generate_experiment1_corpus(400, "word2")
cm2 <- init_count_matrices_lda(s2)
log_marginal_posterior_lda(cm2, priors = list(alpha = 1, beta = 1))

s3 <- generate_experiment1_corpus(400, "word3")
cm3 <- init_count_matrices_lda(s3)
log_marginal_posterior_lda(cm3, priors)
s3$topic <- sample(1:2, nrow(s3), TRUE)
cm3r <- init_count_matrices_lda(s3)
log_marginal_posterior_lda(cm3r, priors)


## Run experiment 1
experiment_jobs1 <- expand.grid(m = m,
                                kappa = kappa,
                                C_kappa = C_kappa,
                                start_mode = start_mode,
                                alpha = alpha,
                                beta = beta,
                                seed = seed,
                                stringsAsFactors = FALSE)
results <- run_experiment1(experiment_jobs = experiment_jobs1,
                           "results_experiment1.rda")

## Run experiment 2
results <- list()
for(i in 1:nrow(experiment_jobs2)){
  print(paste(i,":", Sys.time()))
  crp <- generate_experiment1_corpus(experiment_jobs$m[i], experiment_jobs$start_mode[i])
  params <- list(K = 2,
                 tau = rep(experiment_jobs$tau[i], experiment_jobs$sa_iterations[i]),
                 save_state_every = NULL,
                 log_marginal_posterior_every = 100,
                 seed = 4711,
                 verbose = FALSE)
  # Run SA
  res_sa <- collapsed_sampler_simulated_annealing(state = crp, priors, params)
  # Run final N iterations with tau = 1
  params$tau <- rep(1, nrow(crp))
  res_final <- collapsed_sampler_simulated_annealing(state = res_sa$state, priors, params)
  results[[i]] <- list(sa = res_sa, final = res_final)

}
save(results, experiment_jobs, file = "results_experiment1.rda")


