#' Assert a perspective topic state object
#'
#' @param x a object to assert is a perspective state object.
#'
#' @keywords Internal
assert_state <- function(x){
  checkmate::assert_subset(c("doc", "type", "topic", "party", "perspective"), names(x))
  checkmate::assert_integer(x$doc)
  checkmate::assert_factor(x$type)
  checkmate::assert_integer(x$topic)
  checkmate::assert_factor(x$party)
  checkmate::assert_integer(x$perspective)
}
