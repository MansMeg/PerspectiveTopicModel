#' Set up a table for when to calculate log marginal posterior
#'
#' @param params a \code{parameters} object.
#'
#' @keywords Internal
setup_log_marginal_posterior_table <- function(params){
  checkmate::assert_class(params, "parameters")

  iter_vec <- params$start_iter:params$gibbs_iter
  lmp_bool <- iter_vec %% params$log_marginal_posterior_every == 0
  lmp_length <- params$start_iter:params$gibbs_iter %% params$log_marginal_posterior_every == 0
  dplyr::data_frame(iteration = c(params$start_iter - 1, iter_vec[lmp_bool]), log_post = as.numeric(rep(NA, sum(lmp_bool) + 1)))
}
