#' Extract constants from a state object
#'
#' @param state a \code{state} object.
#'
#' @keywords Internal
get_constants <- function(state){
  # Assert
  assert_state(state)

  # Extract constants
  list(D = max(state$doc),
       V = length(levels(state$type)),
       K = max(state$topic),
       P = length(levels(state$party)),
       N = nrow(state))
}
