assert_count_matrices <- function(x){
  checkmate::assert_list(x)
  for (i in seq_along(x)) {
    checkmate::assert_integer(x[[i]])
  }
}
