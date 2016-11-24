#' Perspective sampler (implemented in R as a cross-check)
#'
#' @rdname per_sampler_cpp
#'
#' @keywords Internal
per_sampler_r <- function(state, count_matrices, priors, constants){

  # Sanity checks
  DEBUG = FALSE
  if(DEBUG) check_sums <- unlist(lapply(count_matrices, sum))

  # Create data structures
  n_dk <- count_matrices$n_dk
  n_vkpx <- count_matrices$n_vkpx
  n_kpx <- count_matrices$n_kpx
  n_kp <- count_matrices$n_kp
  n_kx <- count_matrices$n_kx

  # Allocate memory (precalculate)
  uprobs <- numeric(constants$K * 2)
  alpha_beta_pi <- priors$alpha_pi + priors$beta_pi
  beta_x0_sum <- priors$betax0 * constants$V
  beta_x1_sum <- priors$betax1 * constants$V


  for(i in 1:nrow(state)){ # i <- 1
    if(DEBUG) print(i)
    # Creat idx
    d <- state$doc[i]
    k <- state$topic[i]
    v <- state$type[i]
    p <- state$party[i]
    x <- state$perspective[i] + 1L
    px <- 1L + (x - 1L) * p

    # Remove current positions
    n_dk[d, k] <- n_dk[d, k] - 1L
    n_vkpx[v, k, px] <- n_vkpx[v, k, px] - 1L
    n_kpx[k, p, x] <- n_kpx[k, p, x] - 1L
    n_kp[k, p] <- n_kp[k, p] - 1L
    n_kx[k, x] <- n_kx[k, x] - 1L

    # Check sanity
    if(DEBUG) unlist(lapply(list(n_dk, n_vkpx, n_kpx, n_kp, n_kx), sum))

    # Calculate unnormalized probs
    for(j in 1:constants$K){ # j <- 2
      njd_alpha <- n_dk[d, j] + priors$alpha
      njpx0_beta <- n_kpx[j, p, 1] + priors$beta_pi
      pi_prob_x0 <- (njpx0_beta / (alpha_beta_pi + n_kp[j,p]))

      # x == 0
      uprobs[j] <-
        pi_prob_x0 *
        ((n_vkpx[v, j, 1] + priors$betax0)/(n_kx[j, 1] + beta_x0_sum)) *
        njd_alpha

      # x == 1
      uprobs[j + constants$K] <-
        (1 - pi_prob_x0) *
        ((n_vkpx[v, j, p + 1] + priors$betax1)/(n_kpx[j, p, 2] + beta_x1_sum)) *
        njd_alpha
    }

    # Sample indicator
    kx <- rcategorical(uprobs)
    new_x <- ifelse(kx > constants$K, yes = 2L, no = 1L)
    new_k <- kx - constants$K * (new_x - 1L)
    new_px <- 1L + (new_x - 1L) * p

    # Set the indicators
    state$topic[i] <- new_k
    state$perspective[i] <- (new_x - 1L)

    # Update the count matrices
    n_dk[d, new_k] <- n_dk[d, new_k] + 1L
    n_vkpx[v, new_k, new_px] <- n_vkpx[v, new_k, new_px] + 1L
    n_kpx[new_k, p, new_x] <- n_kpx[new_k, p, new_x] + 1L
    n_kp[new_k, p] <- n_kp[new_k, p] + 1L
    n_kx[new_k, new_x] <- n_kx[new_k, new_x] + 1L

    if(DEBUG) stopifnot(unlist(lapply(list(n_dk, n_vkpx, n_kpx, n_kp, n_kx), sum)) == check_sums)
  }

  count_matrices$n_dk <- n_dk
  count_matrices$n_vkpx <- n_vkpx
  count_matrices$n_kpx <- n_kpx
  count_matrices$n_kp <- n_kp
  count_matrices$n_kx <- n_kx
  list(state=state, count_matrices=count_matrices, tmp = list(u_prob = uprobs, kx = kx))
}


