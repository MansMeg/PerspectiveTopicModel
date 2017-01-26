#' Log-Marginal posterior
#'
#' @description Compute the log marginal posterior for the perspective topic model.
#'
#' @param count_matrices a list of the following count matrices (with the same name): \code{n_dk}, \code{n_kvpx}, \code{n_kpx}, \code{n_pk}, \code{n_xk}
#' @param priors a \code{prior} object.
#'
#' @keywords Internal
log_marginal_posterior_computation <- function(count_matrices, priors){
  # Assertions
  checkmate::assert_class(priors, "priors")
  checkmate::assert_names(names(count_matrices), permutation.of = c("n_dk", "n_kvpx", "n_kpx", "n_pk", "n_xk" ))

  # Create data structures
  n_dk <- count_matrices$n_dk
  n_kvpx <- count_matrices$n_kvpx
  n_kpx <- count_matrices$n_kpx
  n_pk <- count_matrices$n_pk
  n_xk <- count_matrices$n_xk
  n_d <- apply(n_dk, 1, sum)

  V <- dim(n_kvpx)[2]
  K <- dim(n_dk)[2]
  P <- dim(n_pk)[1]
  D <- dim(n_dk)[1]

  betax0 <- priors$betax0
  betax1 <- priors$betax1
  alpha_pi <- priors$alpha_pi
  beta_pi <- priors$beta_pi
  alpha <- priors$alpha

  # Pre-calculations
  beta_x0_sum <- betax0 * V
  beta_x1_sum <- betax1 * V

  lgamma_beta_x0 <- lgamma(betax0)
  lgamma_beta_x0_sum <- lgamma(beta_x0_sum)
  lgamma_beta_x1 <- lgamma(betax1)
  lgamma_beta_x1_sum <- lgamma(beta_x1_sum)

  a <- K * lgamma_beta_x0_sum - K * V * lgamma_beta_x0 + sum(lgamma(n_kvpx[,,1] + betax0)) - sum(lgamma(n_xk[1,] + beta_x0_sum))
  b <- P * K * lgamma_beta_x1_sum - P * K * V * lgamma_beta_x1 + sum(lgamma(n_kvpx[,,-1] + betax1)) - sum(lgamma(n_kpx[,,-1] + beta_x1_sum))
  c <- P * K * lgamma(alpha_pi + beta_pi) - P * K * (lgamma(alpha_pi) + lgamma(beta_pi)) + sum(lgamma(n_kpx[,,1] + beta_pi)) + sum(lgamma(n_kpx[,,-1] + alpha_pi)) - sum(lgamma(n_pk + alpha_pi + beta_pi))
  d <- D * lgamma(K * alpha) - D * K * lgamma(alpha) + sum(lgamma(n_dk + alpha)) - sum(lgamma(n_d + K * alpha))

  a + b + c + d
}

#' Compute log marginal posterior for a state file
#'
#' @param state a \code{state} object.
#' @param priors a \code{priors} object.
#'
#' @export
log_marginal_posterior <- function(state, priors){
  # Assertions
  assert_state(state)

  # Get constants
  constants <- get_constants(state)

  # Compute count matrices
  count_matrices <- init_count2_cpp(state, constants)
  count_matrices[["n_pk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 2), sum))
  count_matrices[["n_xk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 3), sum))

  log_marginal_posterior_computation(count_matrices,priors)
}
