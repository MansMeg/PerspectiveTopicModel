context("collapsed_sampler")

test_that("collapsed_sampler", {
  set.seed(4711)
  N <- 1000
  D <- 17
  V <- 31
  K <- 10
  state_df <- data.frame(doc = factor(sample(1:D, size = N, replace = TRUE)),
                         type = factor(sample(1:V, size = N, replace = TRUE)),
                         topic = sample(1:K, size = N, replace = TRUE))

  constants <- list(D = length(unique(state_df$doc)),
                    V = length(unique(state_df$type)),
                    K = length(unique(state_df$topic)),
                    N = nrow(state_df))

  expect_silent(
    stopifnot(constants$D == D,
              constants$V == V,
              constants$K == K,
              constants$N == N)
  )

  priors <- list(alpha = 0.1,
                 beta = 0.1)
  params <- parameters(K = 10,
                       gibbs_iter = 50L,
                       save_state_every = 10,
                       seed = 4711)
  params$verbose <- FALSE
  expect_silent(res <- collapsed_sampler(state = state_df, priors = priors, params))
  file.remove(dir(full.names = TRUE)[grepl(x = dir(), pattern = "it[0-9]{2}\\.Rdata")])

  params$verbose <- TRUE
  expect_output(res <- collapsed_sampler(state_df, priors = priors, params))
  file.remove(dir(full.names = TRUE)[grepl(x = dir(), pattern = "it[0-9]{2}\\.Rdata")])

})


