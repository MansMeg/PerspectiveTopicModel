#' Assert a perspective topic state object
#'
#' @param x a object to assert is a perspective state object.
#'
#' @keywords Internal
assert_lda_prior <- function(x){
  checkmate::assert_list(x)
  checkmate::assert_names(names(x), must.include = c("alpha", "beta"))
  checkmate::assert_number(x$alpha, lower = 0)
  checkmate::assert_number(x$beta, lower = 0)
}

