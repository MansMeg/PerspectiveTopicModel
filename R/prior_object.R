#' Constructor for prior object
#'
#' @param x NULL or a list with priors to set
#' @param alpha The alpha parameter
#' @param betax0 The betax0 parameter
#' @param betax1 The betax1 parameter
#' @param alpha_pi The alpha_pi parameter
#' @param beta_pi The beta_pi parameter
#'
#' @return A \code{priors} object
#'
#' @export
priors <- function(x = NULL, alpha = 0.1, betax0 = 0.01, betax1 = 0.01, alpha_pi = 0.1, beta_pi = 0.1){
  if(!is.null(x)) {
    checkmate::assert_class(x, "list")
    checkmate::assert_subset(names(x), c("alpha", "betax0", "betax1", "alpha_pi", "beta_pi"))
  }

  prior_obj <- list(alpha = alpha,
                    betax0 = betax0,
                    betax1 = betax1,
                    alpha_pi = alpha_pi,
                    beta_pi = beta_pi)

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

  class(prior_obj) <- c("priors", "list")

  prior_obj
}
