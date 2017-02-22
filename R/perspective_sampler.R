#' Sample perspective in R
#'
#' @param state Initial state to start sampling from
#' @param priors Prior settings
#' @param params Parameters to use
#'
#' @export
perspective_sampler <-function(state, priors, params){
  # Converting
  if(!is.factor(state$doc)) {
    state$doc <- as.factor(state$doc)
    warning("state$doc is not a factor")
  }

  # Assertions
  PerspectiveTopicModel:::assert_state(state)
  checkmate::assert_class(priors, "priors")
  checkmate::assert_class(params, "parameters")

  # Extract vocabulary
  vocabulary <- levels(state$type)
  parties <- levels(state$party)
  doc_ids <- levels(state$doc)

  # Create constants
  constants <-   PerspectiveTopicModel:::get_constants(state)

  # Assert
  checkmate::assert_character(vocabulary, len = constants$V)
  checkmate::assert_character(doc_ids, len = constants$D)
  checkmate::assert(max(state$topic) == params$K)

  # Assert non_zero_type_topics and non_zero_doc_topics
  PerspectiveTopicModel:::assert_non_zero(priors, constants, vocabulary, doc_ids)

  # Warnings
  PerspectiveTopicModel:::throw_state_warnings(state)

  # Remove factors
  state$type <- as.integer(state$type)
  state$party <- as.integer(state$party)
  state$doc <- as.integer(state$doc)

  # Init count matrices
  count_matrices <-   PerspectiveTopicModel:::init_count2_cpp(state, constants)

  # Calculate extra count matrices
  count_matrices[["n_pk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 2), sum))
  count_matrices[["n_xk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 3), sum))

  # Sanity checks before sampling
  checkmate::assert(all(unlist(lapply(count_matrices, sum)) == constants$N))

  # Prepare prior on Phi and Theta for sampling
  priors <-   PerspectiveTopicModel:::prepare_prior_for_sampling(priors, constants, vocabulary, doc_ids)

  # Handle defaults in param object
  if(is.null(params$state_path)) {
    state_file_name <- "perspective"
  } else {
    state_file_name <- params$state_path
  }
  if(is.null(params$verbose)) {
    verbose <- FALSE
  } else {
    verbose <- params$verbose
  }

  # Progressbar
  if(verbose) {
    pb <- utils::txtProgressBar(min = params$start_iter, max = params$gibbs_iter, initial = params$start_iter, style = 3)
    utils::setTxtProgressBar(pb, params$start_iter)
  }

  ### Setup log marginal posterior
  lmp <- setup_log_marginal_posterior_table(params)
  lmp[1,2] <- log_marginal_posterior_computation(count_matrices, priors)
  lmp_idx <- 2L

  ### Run sampler
  # Choose sampler to use
  per_sampler <- per_sampler5_cpp

  results <- per_sampler(state = state, count_matrices = count_matrices, priors = priors, constants = constants)
  if(lmp[lmp_idx, 1] == params$start_iter){
    lmp[lmp_idx, 2] <- log_marginal_posterior_computation(results$count_matrices, priors)
    lmp_idx <- lmp_idx + 1L
  }

  for (step in (params$start_iter + 1):params$gibbs_iter){
    results <- per_sampler(state = results$state, count_matrices = results$count_matrices, priors = priors, constants = constants)

    if(lmp[lmp_idx, 1] == step){
      lmp[lmp_idx, 2] <- log_marginal_posterior_computation(results$count_matrices, priors)
      lmp_idx <- lmp_idx + 1L
    }

    if(verbose) utils::setTxtProgressBar(pb, step)

    if(!is.null(params$save_state_every) && step %% params$save_state_every == 0) {
      state <- results$state
      state$type <- factor(state$type, levels = 1:length(vocabulary), labels = vocabulary)
      state$party <- factor(state$party, levels = 1:length(parties), labels = parties)
      save(state, priors, parameters, lmp, file = paste0(state_file_name, "_it", stringr::str_pad(step, nchar(params$gibbs_iter), pad = "0"), ".Rdata"))
    }
  }

  # Cleanup final model
  results$tmp <- NULL
  priors$tmp_prior_types <- NULL
  priors$tmp_prior_types_map <- NULL
  priors$tmp_prior_types_indicator <- NULL
  priors$tmp_prior_doc <- NULL
  priors$tmp_prior_doc_map <- NULL
  priors$tmp_prior_doc_indicator <- NULL
  priors$tmp_perspective_flag <- NULL
  results$priors <- priors

  # Final output
  results$state$type <- factor(results$state$type, levels = 1:length(vocabulary), labels = vocabulary)
  results$state$party <- factor(results$state$party, levels = 1:length(parties), labels = parties)
  results$state$doc <- factor(results$state$doc, levels = 1:length(doc_ids), labels = doc_ids)
  results$lmp <- lmp
  results$parameters <- params
  class(results) <- "perspective_topic_model"
  results
}
