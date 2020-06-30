#' Assert a perspective topic state object
#'
#' @param x a object to assert is a perspective state object.
#'
#' @keywords Internal
assert_state <- function(x){
  assert_lda_state(x)
  checkmate::assert_subset(c("doc", "type", "topic", "party", "perspective"), names(x))
  checkmate::assert_factor(x$party)
  checkmate::assert_integer(x$perspective)
}


#' @rdname assert_state
#' @keywords Internal
assert_lda_state <- function(x){
  checkmate::assert_subset(c("doc", "type", "topic"), names(x))
  checkmate::assert_factor(x$doc)
  checkmate::assert_factor(x$type)
  checkmate::assert_integer(x$topic)
}
