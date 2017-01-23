#' Sample perspective in R
#'
#' @param state Initial state to start sampling from
#' @param priors Prior settings
#' @param params Parameters to use
#'
#' @export
perspective_sampler <-function(state, priors, params){
  # Assertions
  assert_state(state)
  checkmate::assert_class(priors, "priors")
  checkmate::assert_class(params, "parameters")

  # Extract vocabulary
  vocabulary <- levels(state$type)
  parties <- levels(state$party)

  # Create constants
  constants <- get_constants(state)

  # Assert
  checkmate::assert(max(state$topic) == params$K)

  # Assert non_zero_type_topics
  if(!is.null(priors$non_zero_type_topics)){
    checkmate::assert(all(names(priors$non_zero_type_topics) %in% levels(state$type)))
    for(i in seq_along(priors$non_zero_type_topics)){
      checkmate::assert_integerish(priors$non_zero_type_topics[[i]], lower = 1, upper = constants$K)
    }
  }

  # Warnings
  if(max(state$doc) != length(unique(state$doc))) warning("Missing document ids.")
  if(max(state$topic) != length(unique(state$topic))) warning("Missing topic ids.")
  if(length(unique(state$party)) != length(levels(state$party))) warning("Missing party ids.")
  if(length(unique(state$type)) != length(levels(state$type))) warning("Missing type ids.")

  # Remove factors
  state$type <- as.integer(state$type)
  state$party <- as.integer(state$party)

  # Init count matrices
  count_matrices <- init_count2_cpp(state, constants)

  # Calculate extra count matrices
  count_matrices[["n_pk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 2), sum))
  count_matrices[["n_xk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 3), sum))

  # Prepare prior on Phi object
  if(!is.null(priors$non_zero_type_topics)){
    priors$tmp_prior_types <- logical(length(vocabulary))
    priors$tmp_prior_types_map <- integer(length(vocabulary))
    priors$tmp_prior_types_indicator <- list()
    for(i in seq_along(priors$non_zero_type_topics)){
      idx <- which(vocabulary %in% names(priors$non_zero_type_topics)[i])
      priors$tmp_prior_types[idx] <- TRUE
      priors$tmp_prior_types_map[idx] <- i
      priors$tmp_prior_types_indicator[[i]] <- 1:constants$K %in% priors$non_zero_type_topics[[i]]
    }
  }

  # Sanity checks
  checkmate::assert(all(unlist(lapply(count_matrices, sum)) == constants$N))

  # Handle defaults
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
  if(verbose) pb <- utils::txtProgressBar(min = 1, max = params$gibbs_iter, initial = params$start_iter, style = 3)

  ### Run sampler
  if(!is.null(priors$non_zero_type_topics)){
    per_sampler <- per_sampler3_cpp
  } else {
    per_sampler <- per_sampler2_cpp
  }
  results <- per_sampler(state = state, count_matrices = count_matrices, priors = priors, constants = constants)

  for (step in (params$start_iter + 1):params$gibbs_iter){
    results <- per_sampler(state = results$state, count_matrices = results$count_matrices, priors = priors, constants = constants)
    if(verbose) utils::setTxtProgressBar(pb, step)
    if(!is.null(params$save_state_every) && step %% params$save_state_every == 0) {
      state <- results$state
      state$type <- factor(state$type, levels = 1:length(vocabulary), labels = vocabulary)
      state$party <- factor(state$party, levels = 1:length(parties), labels = parties)
      save(state, file = paste0(state_file_name, "_it", stringr::str_pad(step, nchar(params$gibbs_iter), pad = "0"), ".Rdata"))
    }
  }

  # Parse final results
  results$tmp <- NULL
  results$state$type <- factor(results$state$type, levels = 1:length(vocabulary), labels = vocabulary)
  results$state$party <- factor(results$state$party, levels = 1:length(parties), labels = parties)
  results
}
