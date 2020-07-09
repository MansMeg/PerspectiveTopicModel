#' Setup experiment 1 corpus
#' @param m document size
#' @param second_topic_word word to set as topic 2
generate_experiment1_corpus <- function(m, second_topic_word = "word3"){
  state_df <- data.frame(doc = factor(c(rep("doc1", m), rep("doc2", m), rep("doc3", m))),
                         type = factor(c(rep("word1", 0.9*m), c(rep("word2", 0.1*m), rep("word2", m), rep("word3", m)))))
  state_df$topic <- 1L
  state_df$topic[state_df$type == second_topic_word] <- 2L
  state_df
}

#' Run experiment 1
#'
#' @description
#' The corpus is generated based on m, then SA is run for N * C_kappa itreations.
#' Then the full Gibbs sampler (tau = 1) for
run_experiment1 <- function(experiment_jobs, result_file_name = NULL){
  checkmate::assert_data_frame(experiment_jobs)
  checkmate::assert_names(names(experiment_jobs), must.include = c("m", "kappa", "C_kappa", "start_mode", "seed", "alpha", "beta"))
  checkmate::assert_path_for_output(result_file_name, overwrite = TRUE)

  if(!is.null(result_file_name) && file.exists(result_file_name)) {
    load(result_file_name, envir = parent.frame())
    message("Results already exist. Return the current results.")
    return(list(results = results, experiment_jobs = experiment_jobs))
  }

  # Setup experiment
  crp <- generate_experiment1_corpus(experiment_jobs$m[1], as.character(experiment_jobs$start_mode[1]))
  N <- nrow(crp)
  experiment_jobs$tau <- experiment_jobs$kappa/N
  experiment_jobs$sa_iterations <- N * experiment_jobs$C_kappa
  experiment_jobs$iterations <- N

  results <- list()
  for(i in 1:nrow(experiment_jobs)){
    print(paste(Sys.time(), ":", i))
    crp <- generate_experiment1_corpus(experiment_jobs$m[i], experiment_jobs$start_mode[i])
    params <- list(K = 2,
                   tau = rep(experiment_jobs$tau[i], experiment_jobs$sa_iterations[i]),
                   save_state_every = NULL,
                   log_marginal_posterior_every = 10,
                   seed = experiment_jobs$seed[i],
                   verbose = FALSE)
    priors <- list(alpha = experiment_jobs$alpha[i], beta = experiment_jobs$beta[i])
    # Run SA
    res_sa <- collapsed_sampler_simulated_annealing(state = crp, priors = priors, params)
    # Run final N iterations with tau = 1
    params$tau <- rep(1, N)
    res_final <- collapsed_sampler_simulated_annealing(state = res_sa$state, priors, params)
    results[[i]] <- list(sa = res_sa, final = res_final)
  }
  save(results, experiment_jobs, file = result_file_name)

  list(results = results, experiment_jobs = experiment_jobs)
}

