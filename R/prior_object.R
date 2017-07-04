#' Constructor for prior object
#'
#' @param x NULL or a list with priors to set
#' @param alpha The alpha parameter
#' @param betax0 The betax0 parameter
#' @param betax1 The betax1 parameter
#' @param alpha_pi The alpha_pi parameter (prior weight on perspective)
#' @param beta_pi The beta_pi parameter (prior weight on non-perspective)
#' @param non_zero_type_topics List of priors on wordtypes. See details.
#' @param non_zero_doc_topics List of priors on documents. See details.
#' @param perspective_topics Topics that contain perspectives. If \code{NULL}, all topics have perspectives. If \code{0}, no topic has perspectives.
#' @param annealing_iterations If simulated annealing is used. Indicate at which iteration the same position in hyper-parameter prior vector that should be used.
#'
#' @details
#' The parameter \code{non_zero_type_topics} is a list named with types with prior settings. Each element is a type that contains a vector of non_zero probability topics.
#' The parameter \code{non_zero_doc_topics} List of priors on documents. See details.
#'
#' @return A \code{priors} object
#'
#' @export
priors <- function(x = NULL, alpha = 0.1, betax0 = 0.01, betax1 = 0.01, alpha_pi = 0.1, beta_pi = 0.1, non_zero_type_topics = NULL, non_zero_doc_topics = NULL, perspective_topics = NULL, annealing_iterations = NULL){
  if(!is.null(x)) {
    checkmate::assert_class(x, "list")
    checkmate::assert_subset(names(x), c("alpha", "betax0", "betax1", "alpha_pi", "beta_pi", "non_zero_type_topics", "non_zero_doc_topics", "perspective_topics", "annealing_iterations"))
    if(!is.null(x$non_zero_type_topics)) {
      checkmate::assert_class(x$non_zero_type_topics, "list")
    }
  }

  prior_obj <- list(alpha = alpha,
                    betax0 = betax0,
                    betax1 = betax1,
                    alpha_pi = alpha_pi,
                    beta_pi = beta_pi,
                    non_zero_type_topics = non_zero_type_topics,
                    non_zero_doc_topics = non_zero_doc_topics,
                    perspective_topics = perspective_topics,
                    annealing_iterations = annealing_iterations)

  if(!is.null(x)){
    for(name in names(x)){
      prior_obj[[name]] <- x[[name]]
    }
  }

  # Check valid parameters
  if(is.null(prior_obj$annealing_iterations)){
    checkmate::assert_number(prior_obj$alpha, lower = 0.000000001)
    checkmate::assert_number(prior_obj$betax0, lower = 0.000000001)
    checkmate::assert_number(prior_obj$betax1, lower = 0.000000001)
    checkmate::assert_number(prior_obj$alpha_pi, lower = 0.000000001)
    checkmate::assert_number(prior_obj$beta_pi, lower = 0.000000001)
  } else {
    checkmate::assert_integerish(prior_obj$annealing_iterations, lower = 0)
    checkmate::assert(prior_obj$annealing_iterations[1] == 0)
    checkmate::assert_numeric(prior_obj$alpha, lower = 0.000000001, len = length(prior_obj$annealing_iterations))
    checkmate::assert_numeric(prior_obj$betax0, lower = 0.000000001, len = length(prior_obj$annealing_iterations))
    checkmate::assert_numeric(prior_obj$betax1, lower = 0.000000001, len = length(prior_obj$annealing_iterations))
    checkmate::assert_numeric(prior_obj$alpha_pi, lower = 0.000000001, len = length(prior_obj$annealing_iterations))
    checkmate::assert_numeric(prior_obj$beta_pi, lower = 0.000000001, len = length(prior_obj$annealing_iterations))
  }

  if(!is.null(prior_obj$non_zero_type_topics)) {
    checkmate::assert_class(prior_obj$non_zero_type_topics, "list")
    checkmate::assert_character(names(prior_obj$non_zero_type_topics),
                                len = length(prior_obj$non_zero_type_topics))
    for(i in seq_along(prior_obj$non_zero_type_topics)) {
      checkmate::assert_integerish(prior_obj$non_zero_type_topics[[i]])
    }
  }
  if(!is.null(prior_obj$non_zero_doc_topics)) {
    checkmate::assert_class(prior_obj$non_zero_doc_topics, "list")
    checkmate::assert_character(names(prior_obj$non_zero_doc_topics),
                                len = length(prior_obj$non_zero_doc_topics))
    for(i in seq_along(prior_obj$non_zero_doc_topics)) {
      checkmate::assert_integerish(prior_obj$non_zero_doc_topics[[i]])
    }
  }

  checkmate::assert_integerish(prior_obj$perspective_topics, null.ok = TRUE, lower = 0L)

  class(prior_obj) <- c("priors", "list")

  prior_obj
}


#' Prepare the prior objects for sampling
#'
#' @param priors a \code{priors} object.
#' @param constants a \code{state_constants} object.
#' @param vocabulary a character vector with the vocabulary.
#' @param doc_ids a character vector with the document ids.
#'
#' @keywords Internal
prepare_prior_for_sampling <- function(priors, constants, vocabulary, doc_ids){
  checkmate::assert_class(priors, "priors")
  checkmate::assert_class(constants, "state_constants")
  checkmate::assert_character(vocabulary, len = constants$V)
  checkmate::assert_character(doc_ids, len = constants$D)

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
  } else {
    priors$tmp_prior_types <- logical(length(vocabulary))
    priors$tmp_prior_types_map <- integer(length(vocabulary))
    priors$tmp_prior_types_indicator <- list()
  }

  if(!is.null(priors$non_zero_doc_topics)){
    priors$tmp_prior_doc <- logical(length(doc_ids))
    priors$tmp_prior_doc_map <- integer(length(doc_ids))
    priors$tmp_prior_doc_indicator <- list()
    for(i in seq_along(priors$non_zero_doc_topics)){
      idx <- which(doc_ids %in% names(priors$non_zero_doc_topics)[i])
      priors$tmp_prior_doc[idx] <- TRUE
      priors$tmp_prior_doc_map[idx] <- i
      priors$tmp_prior_doc_indicator[[i]] <- 1:constants$K %in% priors$non_zero_doc_topics[[i]]
    }
  } else {
    priors$tmp_prior_doc <- logical(length(doc_ids))
    priors$tmp_prior_doc_map <- integer(length(doc_ids))
    priors$tmp_prior_doc_indicator <- list()
  }

  priors$tmp_perspective_flag <- !logical(constants$K)
  if(!is.null(priors$perspective_topics)){
    priors$tmp_perspective_flag <- logical(constants$K)
    priors$tmp_perspective_flag[priors$perspective_topics] <- TRUE
  }

  priors
}


#' Get prior for iteration
#'
#' @details
#' When running simulated annealing we have extract one specific prior per iteration.
#'
#' @param priors a \code{priors} object.
#' @param iteration current iteration.
#'
#' @keywords Internal
get_priors_for_iteration <- function(priors, iteration){
  checkmate::assert_class(priors, "priors")
  checkmate::assert_integerish(iteration, len = 1, lower = 0)

  it_priors <- priors

  if(!is.null(priors$annealing_iterations)){
    pos <- max(which(priors$annealing_iterations <= iteration))
    it_priors$annealing_iterations <- NULL
    it_priors$alpha <- priors$alpha[pos]
    it_priors$betax0 <- priors$betax0[pos]
    it_priors$betax1 <- priors$betax1[pos]
    it_priors$alpha_pi <- priors$alpha_pi[pos]
    it_priors$beta_pi <- priors$beta_pi[pos]
  }

  class(it_priors) <- c("iteration_prior", class(priors))
  it_priors
}
