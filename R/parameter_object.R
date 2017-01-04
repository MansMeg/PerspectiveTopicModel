#' Constructor for parameter object
#'
#' @param x NULL or a list with priors to set
#'
#' @return A \code{parameter} object
#'
#' @export
parameters <- function(x = NULL,
                       K = 10,
                       start_iter = 2L,
                       gibbs_iter = 10L,
                       save_state_every = NULL,
                       state_path = "perspective_",
                       seed = as.integer(Sys.time()),
                       verbose = TRUE){
  if(!is.null(x)) {
    checkmate::assert_class(x, "list")
    checkmate::assert_subset(names(x), c("K", "start_iter", "gibbs_iter", "save_state_every", "state_path", "seed", "verbose"))
  }

  params_obj <- list(K = K,
                    start_iter = start_iter,
                    gibbs_iter = gibbs_iter,
                    save_state_every = save_state_every,
                    state_path = state_path,
                    seed = seed)

  if(!is.null(x)){
    for(name in names(x)){
      params_obj[[name]] <- x[[name]]
    }
  }

  # Assertions
  checkmate::assert_integerish(params_obj$K, lower = 2L)
  checkmate::assert_integerish(params_obj$gibbs_iter, lower = params$start_iter)
  checkmate::assert_integerish(params_obj$start_iter, lower = 2L, upper = params$gibbs_iter)
  checkmate::assert_integerish(params_obj$save_state_every, lower = 1L, params$gibbs_iter, null.ok = TRUE)
  checkmate::assert_path_for_output(params_obj$state_path)
  checkmate::assert_integerish(params_obj$seed, lower = 1L)
  checkmate::assert_flag(params_obj$verbose)

  # Warnings
  if(is.null(params_obj$save_state_every) & state_path != "perspective_"){
    warning("Parameter state_path is ignored.")
  }

  class(params_obj) <- c("parameters", "list")

  params_obj
}
