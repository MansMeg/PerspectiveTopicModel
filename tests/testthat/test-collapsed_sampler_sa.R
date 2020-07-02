context("collapsed_sampler_simulated_annealing")

test_that("collapsed_sampler_simulated_annealing", {
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
  params <- list(K = 10,
                 tau = rep(0, 50),
                 save_state_every = 10,
                 log_marginal_posterior_every = 1,
                 seed = 4711,
                 verbose = FALSE)
  params2 <- parameters(K = 10,
                       gibbs_iter = 50L,
                       save_state_every = 10,
                       seed = 4711,
                       verbose = FALSE)
  set.seed(4711)
  expect_silent(res1 <- collapsed_sampler_simulated_annealing(state = state_df, priors = priors, params))
  file.remove(dir(full.names = TRUE)[grepl(x = dir(), pattern = "it[0-9]{2}\\.Rdata")])
  # Assert that the topic indicators in original data is not modified
  expect_failure(expect_identical(res1$state, state_df))

  params$verbose <- TRUE
  params$tau <- rep(1, 50)
  set.seed(4711)
  expect_output(res2a <- collapsed_sampler_simulated_annealing(state_df, priors = priors, params))
  file.remove(dir(full.names = TRUE)[grepl(x = dir(), pattern = "it[0-9]{2}\\.Rdata")])
  set.seed(4711)
  expect_silent(res2b <- collapsed_sampler(state_df, priors = priors, params2))

  expect_failure(expect_identical(res1$state$topic, res2a$state$topic))
  expect_identical(res2a$lmp$log_post[1], res2b$lmp$log_post[1])
  expect_identical(res2a$lmp$log_post[2], res2b$lmp$log_post[2])

  expect_lte(res1$lmp$log_post[50], res2a$lmp$log_post[50])

  set.seed(4711)
  params$tau <- rep(0.5, 50)
  expect_output(res3 <- collapsed_sampler_simulated_annealing(state = state_df, priors = priors, params))
  file.remove(dir(full.names = TRUE)[grepl(x = dir(), pattern = "it[0-9]{2}\\.Rdata")])

  expect_failure(expect_identical(res1$state$topic, res3$state$topic))
  expect_failure(expect_identical(res2a$state$topic, res3$state$topic))

  expect_lte(res1$lmp$log_post[50], res3$lmp$log_post[50])
  expect_lte(res3$lmp$log_post[50], res2a$lmp$log_post[50])

})


