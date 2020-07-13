#' Compute the log marginal posterior for an ordinary LDA model
#'
#' @param count_matrices n_dk, n_k, n_vk
#' @param priors alpha, beta
#'
#' @export
log_marginal_posterior_lda <- function(count_matrices, priors){
  assert_count_matrices(count_matrices)
  assert_lda_prior(priors)
  log_marginal_posterior_lda_internal(count_matrices, priors)
}

#' For internal use (without assertions)
log_marginal_posterior_lda_internal <- function(count_matrices, priors){
  K <- ncol(count_matrices$n_dk)
  V <- ncol(count_matrices$n_kv)
  D <- nrow(count_matrices$n_dk)
  n_d <- rowSums(count_matrices$n_dk)
  alpha <- priors$alpha
  beta <- priors$beta

  # See modelLogLikelihood in mallet
  lmp <-
    K * lgamma(V * beta) -
    K * V * lgamma(beta) +
    sum(lgamma(count_matrices$n_kv + beta)) -
    sum(lgamma(count_matrices$n_k + beta)) +
    D * lgamma(K * alpha) -
    D * K * lgamma(alpha) +
    sum(lgamma(count_matrices$n_dk + alpha)) -
    sum(lgamma(n_d + alpha))
  lmp
}
