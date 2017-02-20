#' Checks state file and trow warnings for potential problems
#'
#' @param state a \code{state} object.
#'
#' @keywords Internal
throw_state_warnings <- function(state){
  if(length(unique(state$doc)) != length(levels(state$doc))) warning("Missing document ids.")
  if(max(state$topic) != length(unique(state$topic))) warning("Missing topic ids.")
  if(length(unique(state$party)) != length(levels(state$party))) warning("Missing party ids.")
  if(length(unique(state$type)) != length(levels(state$type))) warning("Missing type ids.")
}
