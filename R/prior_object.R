#' Constructor for prior object
#'
#' @param x NULL or a list with priors to set
#' @param alpha The alpha parameter
#' @param betax0 The betax0 parameter
#' @param betax1 The betax1 parameter
#' @param alpha_pi The alpha_pi parameter
#' @param beta_pi The beta_pi parameter
#' @param non_zero_type_topics List of priors on wordtypes. See details.
#' @param non_zero_doc_topics List of priors on documents. See details.
#' @param perspective_topics Topics that contain perspectives. If \code{NULL}, all topics has perspectives.
#'
#' @details
#' The parameter \code{non_zero_type_topics} is a list named with types with prior settings. Each element is a type that contains a vector of non_zero probability topics.
#' The parameter \code{non_zero_doc_topics} List of priors on documents. See details.
#'
#' @return A \code{priors} object
#'
#' @export
priors <- function(x = NULL, alpha = 0.1, betax0 = 0.01, betax1 = 0.01, alpha_pi = 0.1, beta_pi = 0.1, non_zero_type_topics = NULL, non_zero_doc_topics = NULL, perspective_topics = NULL){
  if(!is.null(x)) {
    checkmate::assert_class(x, "list")
    checkmate::assert_subset(names(x), c("alpha", "betax0", "betax1", "alpha_pi", "beta_pi", "non_zero_type_topics"))
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
                    perspective_topics = perspective_topics)

  if(!is.null(x)){
    for(name in names(x)){
      prior_obj[[name]] <- x[[name]]
    }
  }

  # Check valid parameters
  checkmate::assert_number(prior_obj$alpha, lower = 0.000000001)
  checkmate::assert_number(prior_obj$betax0, lower = 0.000000001)
  checkmate::assert_number(prior_obj$betax1, lower = 0.000000001)
  checkmate::assert_number(prior_obj$alpha_pi, lower = 0.000000001)
  checkmate::assert_number(prior_obj$beta_pi, lower = 0.000000001)
  if(!is.null(prior_obj$non_zero_type_topics)) {
    checkmate::assert_class(prior_obj$non_zero_type_topics, "list")
    checkmate::assert_character(names(prior_obj$non_zero_type_topics),
                                len = length(prior_obj$non_zero_type_topics))
    for(i in seq_along(non_zero_type_topics)) {
      checkmate::assert_integerish(non_zero_type_topics[[i]])
    }
  }
  if(!is.null(prior_obj$non_zero_doc_topics)) {
    checkmate::assert_class(prior_obj$non_zero_doc_topics, "list")
    checkmate::assert_character(names(prior_obj$non_zero_doc_topics),
                                len = length(prior_obj$non_zero_doc_topics))
    for(i in seq_along(non_zero_doc_topics)) {
      checkmate::assert_integerish(non_zero_doc_topics[[i]])
    }
  }

  checkmate::assert_integerish(perspective_topics, null.ok = TRUE)

  class(prior_obj) <- c("priors", "list")

  prior_obj
}
