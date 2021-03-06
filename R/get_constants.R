#' Extract constants from a state object
#'
#' @param state a \code{state} object.
#'
#' @return a \code{state_constants} object.
#'
#' @keywords Internal
get_constants <- function(state){
  # Assert
  assert_state(state)

  # Extract constants
  res <- list(D = length(levels(state$doc)),
       V = length(levels(state$type)),
       K = max(state$topic),
       P = length(levels(state$party)),
       N = nrow(state))

  class(res) <- c("state_constants", "list")
  res
}
