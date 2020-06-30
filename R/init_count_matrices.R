#' Initialize count matrices for ordinary LDA
#'
#' @param state a lda state object
#' @export
init_count_matrices_lda <- function(state){
  m <- list()
  m$n_k <- as.vector(table(state$topic))
  m$n_dk <- as.matrix(table(state$doc, state$topic))
  m$n_kv <- as.matrix(table(state$topic, state$type))
  class(m$n_dk) <- "matrix"
  class(m$n_kv) <- "matrix"
  m
}
