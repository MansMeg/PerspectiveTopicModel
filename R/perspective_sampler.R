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
  checkmate::assert_integer(state$doc)
  checkmate::assert_factor(state$type)
  checkmate::assert_integer(state$topic)
  checkmate::assert_factor(state$party)
  checkmate::assert_integer(state$perspective)

  checkmate::assert_class(priors, "priors")
  checkmate::assert_class(params, "parameters")

  # Create constants
  constants <- list(D = max(state$doc),
                    V = length(levels(state$type)),
                    K = max(state$topic),
                    P = length(levels(state$party)),
                    N = nrow(state))

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

  ### Sampler
  results <- per_sampler2_cpp(state = state, count_matrices = count_matrices, priors = priors, constants = constants)

  for (step in params$start_iter:params$gibbs_iter){
    results <- per_sampler2_cpp(state = results$state, count_matrices = results$count_matrices, priors = priors, constants = constants)
    if(verbose) utils::setTxtProgressBar(pb, step)
    if(!is.null(params$save_state_every) && step %% params$save_state_every == 0) save(results, file = paste0(state_file_name, "_it", stringr::str_pad(step, nchar(params$gibbs_iter), pad = "0"), ".Rdata"))
  }
  results
}
