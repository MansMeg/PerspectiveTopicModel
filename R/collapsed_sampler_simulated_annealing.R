#' Collapsed Gibbs Sampler in R
#'
#' @param state Initial state to start sampling from
#' @param priors Prior settings
#' @param params Parameters to use
#'
#' @export
collapsed_sampler_simulated_annealing <- function(state, priors, params){
  # Assertions
  assert_lda_state(state)
  assert_lda_prior(priors)
  assert_lda_sa_parameters(params)

  params$gibbs_iter <- length(params$tau)
  params$start_iter <- 1L
  class(params) <- "parameters"

  # Set seed
  set.seed(params$seed)

  # Do a deep copy of topic assignments
  state$topic <- sapply(state$topic, FUN = function(x) x)

  # Extract vocabulary
  vocabulary <- levels(state$type)
  doc_ids <- levels(state$doc)

  # Create constants
  constants <- get_lda_constants(state)

  # Assert
  checkmate::assert_character(vocabulary, len = constants$V)
  checkmate::assert_character(doc_ids, len = constants$D)
  checkmate::assert(max(state$topic) == params$K)

  # Warnings
  throw_state_warnings(state)

  # Remove factors
  state$type <- as.integer(state$type)
  state$doc <- as.integer(state$doc)

  # Init count matrices
  count_matrices <- init_count_matrices_lda(state)

  # Sanity checks before sampling
  checkmate::assert(all(unlist(lapply(count_matrices, sum)) == constants$N))

  # Progressbar
  if(params$verbose) {
    pb <- utils::txtProgressBar(min = params$start_iter, max = params$gibbs_iter, initial = params$start_iter, style = 3)
    utils::setTxtProgressBar(pb, params$start_iter)
  }

  ### Setup log marginal posterior
  lmp <- setup_log_marginal_posterior_table(params)
  lmp[1,2] <- log_marginal_posterior_lda(count_matrices, priors)
  lmp_idx <- 2L

  ### Setup sampler
  results <- collapsed_sampler_sa_cpp(state = state, count_matrices = count_matrices, priors = priors, constants = constants, tau = params$tau[1])
  if(lmp[lmp_idx, 1] == params$start_iter){
    lmp[lmp_idx, 2] <- log_marginal_posterior_lda(results$count_matrices, priors)
    lmp_idx <- lmp_idx + 1L
  }
  lp <- lmp$log_post

  for (step in (params$start_iter + 1):params$gibbs_iter){
    results <- collapsed_sampler_sa_cpp(state = results$state, count_matrices = results$count_matrices, priors = priors, constants = constants, tau = params$tau[step])

    if(step %% params$log_marginal_posterior_every == 0){
      lp[lmp_idx] <- log_marginal_posterior_lda(results$count_matrices, priors)
      lmp_idx <- lmp_idx + 1L
    }

    if(params$verbose) utils::setTxtProgressBar(pb, step)

    if(!is.null(params$save_state_every) && step %% params$save_state_every == 0) {
      state <- results$state
      state$type <- factor(state$type, levels = 1:length(vocabulary), labels = vocabulary)
      save(state, priors, parameters, lmp, file = paste0(params$state_path, "_it", stringr::str_pad(step, nchar(params$gibbs_iter), pad = "0"), ".Rdata"))
    }
  }

  # Store results
  lmp$log_post <- lp

  # Cleanup final model
  results$tmp <- NULL
  results$priors <- priors

  # Final output
  results$state$type <- factor(results$state$type, levels = 1:length(vocabulary), labels = vocabulary)
  results$state$doc <- factor(results$state$doc, levels = 1:length(doc_ids), labels = doc_ids)
  results$lmp <- lmp
  results$parameters <- params
  class(results) <- "lda_topic_model"
  results
}


