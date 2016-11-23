#' Initialize count matrices (R implementation)
#'
#' @details
#' Initialization of n_dk, n_vkpx, n_kpx matrices
#'
#' @param state a state file
#' @param const a list with constants
#'
#' @keywords Internal
init_counts_r <- function(state, const){
  checkmate::assert_subset(c("N", "V", "K", "P", "D"), names(const))
  checkmate::assert_subset(c("doc", "topic", "type", "party", "perspective"), names(state))

  n_dk <- matrix(0L, nrow = const$D, ncol = const$K) # Topic counts by document
  n_vkpx <- array(0L, dim = c(const$V, const$K, const$P + 1))
  n_kpx <- array(0L, dim = c(const$K, const$P, 2))

  for(i in 1:nrow(state)){
    d <- state$doc[i]
    k <- state$topic[i]
    v <- state$type[i]
    p <- state$party[i]
    x <- state$perspective[i] + 1L
    px <- 1L + (x - 1L) * p

    n_dk[d, k] <- n_dk[d, k] + 1L
    n_vkpx[v, k, px] <- n_vkpx[v, k, px] + 1L
    n_kpx[k, p, x] <- n_kpx[k, p, x] + 1L
  }

  # Assert the matrices
  stopifnot(sum(n_dk) == sum(n_vkpx),
            sum(n_dk) == sum(n_kpx),
            all(apply(n_dk, MARGIN=c(2), sum) == apply(n_vkpx, MARGIN=c(2), sum)),
            all(apply(n_dk, MARGIN=c(2), sum) == apply(n_kpx, MARGIN=c(1), sum)),
            all(apply(n_vkpx, MARGIN=c(3), sum)[-1] == apply(n_kpx, MARGIN=c(2,3), sum)[,2])
  )

  return(list(n_dk = n_dk, n_vkpx = n_vkpx, n_kpx = n_kpx))
}
