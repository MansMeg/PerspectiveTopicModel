context("perspective_sampler")

test_that("perspective_sampler", {
  set.seed(4711)
  N <- 1000
  D <- 17
  V <- 31
  K <- 10
  P <- 3
  state_df <- data.frame(doc = factor(sample(1:D, size = N, replace = TRUE)),
                         type = factor(sample(1:V, size = N, replace = TRUE)),
                         topic = sample(1:K, size = N, replace = TRUE),
                         party = factor(sample(1:P, size = N, replace = TRUE)),
                         perspective = sample(0:1, size = N, replace = TRUE))

  constants <- list(D = length(unique(state_df$doc)),
                    V = length(unique(state_df$type)),
                    K = length(unique(state_df$topic)),
                    P = length(unique(state_df$party)),
                    N = nrow(state_df))

  expect_silent(
    stopifnot(constants$D == D,
              constants$V == V,
              constants$K == K,
              constants$P == P,
              constants$N == N)
  )

  priors <- priors(alpha = 0.1,
                 betax0 = 0.01,
                 betax1 = 0.01,
                 alpha_pi = 0.1,
                 beta_pi = 0.1)

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state_df, priors = priors, params))

  params$verbose <- TRUE
  expect_output(res <- perspective_sampler(state_df, priors = priors, params))
})


test_that("perspective_sampler prior on Phi", {
  set.seed(4711)
  N <- 1000
  D <- 17
  V <- 31
  K <- 10
  P <- 3
  state_df <- data.frame(doc = factor(sample(1:D, size = N, replace = TRUE)),
                         type = factor(sample(1:V, size = N, replace = TRUE)),
                         topic = sample(1:K, size = N, replace = TRUE),
                         party = factor(sample(1:P, size = N, replace = TRUE)),
                         perspective = sample(0:1, size = N, replace = TRUE))
  state_df$type[nrow(state_df)] <- 2

  priors <- priors(alpha = 0.1,
                   betax0 = 0.01,
                   betax1 = 0.01,
                   alpha_pi = 0.1,
                   beta_pi = 0.1,
                   non_zero_type_topics = list("2" = c(1,2), "4" = 3, "7" = c(1)))

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_true(all(res$state$topic[res$state$type == 2] %in% c(1,2)))
  expect_true(all(res$state$topic[res$state$type == 4] == 3))
})



test_that("perspective_sampler prior on Theta", {
  set.seed(4711)
  N <- 1000
  D <- 17
  V <- 31
  K <- 10
  P <- 3
  state_df <- data.frame(doc = factor(sample(1:D, size = N, replace = TRUE)),
                         type = factor(sample(1:V, size = N, replace = TRUE)),
                         topic = sample(1:K, size = N, replace = TRUE),
                         party = factor(sample(1:P, size = N, replace = TRUE)),
                         perspective = sample(0:1, size = N, replace = TRUE))
  state_df$type[nrow(state_df)] <- 2
  state <- state_df
  priors <- priors(alpha = 0.1,
                   betax0 = 0.01,
                   betax1 = 0.01,
                   alpha_pi = 0.1,
                   beta_pi = 0.1,
                   non_zero_doc_topics = list("2" = c(1,2), "4" = 3, "7" = c(1)))

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_true(all(res$state$topic[res$state$doc == "2"] %in% c(1,2)))
  expect_true(all(res$state$topic[res$state$doc == "4"] == 3))
  expect_true(all(res$state$topic[res$state$doc == "7"] == 1))
})


