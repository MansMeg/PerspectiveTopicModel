assert_lda_sa_parameters <- function(x){
  checkmate::assert_list(x)
  checkmate::assert_names(names(x), must.include = c("K", "tau", "save_state_every", "seed", "verbose", "log_marginal_posterior_every"))
  checkmate::assert_int(x$K)
  checkmate::assert_int(x$K)
  checkmate::assert_numeric(x$tau, lower = 0, upper = 1)
  checkmate::assert_int(x$save_state_every, lower = 0)
  checkmate::assert_flag(x$verbose)
}
