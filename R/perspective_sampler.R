#' Sample perspective in R
#'
#' @param state Initial state to start sampling from
#' @param priors Prior settings
#' @param params Parameters to use
#'
#' @export
perspective_sampler <-function(state, priors, params){
  # Assertions
  checkmate::assert_subset(c("doc", "type", "topic", "party", "perspective"), names(state))
  checkmate::assert_integer(state$perspective)
  checkmate::assert_subset(c("alpha", "betax0", "betax1", "alpha_pi", "beta_pi" ), names(priors))

  checkmate::assert_integerish(params$start_iter, lower = 2L)

  # Create constants
  constants <- list(D = length(unique(state$doc)),
                    V = length(unique(state$type)),
                    K = length(unique(state$topic)),
                    P = length(unique(state$party)),
                    N = nrow(state))

  # Remove factors
  state$type <- as.integer(state$type)
  state$party <- as.integer(state$party)

  # Init count matrices
  count_matrices <- init_count_cpp(state, constants)

  # Calculate extra count matrices
  count_matrices[["n_kp"]] <- apply(count_matrices$n_kpx, MARGIN=c(1, 2), sum)
  count_matrices[["n_kx"]] <- apply(count_matrices$n_kpx, MARGIN=c(1, 3), sum)

  # Sanity checks
  stopifnot(all(unlist(lapply(count_matrices, sum)) == constants$N))

  ### Sampler
  results <- per_sampler_cpp(state = state, count_matrices = count_matrices, priors = priors, constants = constants)
  for (step in params$start_iter:params$gibbs_iter){
    print(step)
    results <- per_sampler_cpp(state = results$state, count_matrices = results$count_matrices, priors = priors, constants = constants)
    if(step %% params$save_state_every == 0) save(results, file = paste0(params$state_file_name, "_it", step, ".Rdata"))
  }
  results
}


