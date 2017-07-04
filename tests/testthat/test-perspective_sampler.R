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



test_that("perspective_sampler prior on perspectives", {
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
                   perspective_topics = c(1,6,9))

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_false(all(res$state$perspective[res$state$topic == 1] == 0))
  expect_true(all(res$state$perspective[res$state$topic == 2] == 0))
  expect_false(all(res$state$perspective[res$state$topic == 6] == 0))
  expect_false(all(res$state$perspective[res$state$topic == 9] == 0))
})




test_that("perspective_sampler prior on Perspectives, Theta and Phi", {

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
                   non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                   non_zero_type_topics = list("2" = 1:5, "6" = 2:4),
                   perspective_topics = c(1,6,9))

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_false(all(res$state$perspective[res$state$topic == 1] == 0))
  expect_true(all(res$state$perspective[res$state$topic == 2] == 0))
  expect_false(all(res$state$perspective[res$state$topic == 6] == 0))
  expect_false(all(res$state$perspective[res$state$topic == 9] == 0))

  expect_true(all(res$state$topic[res$state$doc == "1"] %in% 1:8))
  expect_true(all(res$state$topic[res$state$doc == "4"] %in% 3:9))

  expect_true(all(res$state$topic[res$state$type == "2"] %in% 1:5))
  expect_true(all(res$state$topic[res$state$type == "6"] %in% 2:4))

})


test_that("rerun perspective_sampler with prior on Perspectives, Theta and Phi", {

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
                   non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                   non_zero_type_topics = list("2" = 1:5, "6" = 2:4),
                   perspective_topics = c(1,6,9))

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_silent(priors(res$priors))
  expect_silent(parameters(res$parameters))

  expect_silent(res <- perspective_sampler(state = res$state, priors = res$priors, res$parameters))
})








test_that("perspective_sampler prior without perspectives", {

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
                   non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                   non_zero_type_topics = list("2" = 1:5, "6" = 2:4),
                   perspective_topics = 0L)

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)

  params$verbose <- FALSE
  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_true(all(res$state$perspective == 0))

})




test_that("perspective_sampler with simulated annealing", {

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

  expect_error(
    priors <- priors(annealing_iterations = c(0, 2, 3, 4),
                     alpha = 10 ^ c(3, 2, 1, 0) * 0.1,
                     betax0 = c(4, 3, 2, 1) * 0.01,
                     betax1 = c(4, 3, 2) *  0.01,
                     alpha_pi = c(4, 3, 2, 1) * 0.1,
                     beta_pi = c(4, 3, 2, 1) * 0.1,
                     non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                     non_zero_type_topics = list("2" = 1:5, "6" = 2:4))
  )

  expect_error(
    priors <- priors(annealing_iterations = c(1, 2, 3, 4),
                     alpha = 10 ^ c(3, 2, 1, 0) * 0.1,
                     betax0 = c(4, 3, 2, 1) * 0.01,
                     betax1 = c(4, 3, 2), 1 *  0.01,
                     alpha_pi = c(4, 3, 2, 1) * 0.1,
                     beta_pi = c(4, 3, 2, 1) * 0.1,
                     non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                     non_zero_type_topics = list("2" = 1:5, "6" = 2:4))
  )

  expect_silent(
  priors <- priors(annealing_iterations = c(0, 10, 20, 30),
                   alpha = 10 ^ c(3, 2, 1, 0) * 0.1,
                   betax0 = 10 ^ c(3, 2, 1, 0) * 0.01,
                   betax1 = 10 ^ c(3, 2, 1, 0) *  0.01,
                   alpha_pi = 10 ^ c(3, 2, 1, 0) * 0.1,
                   beta_pi = 10 ^ c(3, 2, 1, 0) * 0.1,
                   non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                   non_zero_type_topics = list("2" = 1:5, "6" = 2:4))
  )

  params <- parameters(K = 10, gibbs_iter = 100L, save_state_every = 10000, seed = 4711)
  params$verbose <- FALSE

  expect_silent(res <- perspective_sampler(state = state_df, priors = priors, params))

  expect_silent(
    priors <- priors(alpha = 0.1,
                     betax0 = 0.01,
                     betax1 = 0.01,
                     alpha_pi = 0.1,
                     beta_pi = 0.1,
                     non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                     non_zero_type_topics = list("2" = 1:5, "6" = 2:4))
  )
  expect_silent(res_nosim <- perspective_sampler(state = state_df, priors = priors, params))

  expect_gt(res_nosim$lmp[1,]$log_post, res$lmp[1,]$log_post)
  expect_equal(res_nosim$lmp[nrow(res_nosim$lmp),]$log_post, res$lmp[nrow(res$lmp),]$log_post, tolerance = 200)

  # plot(res$lmp$iteration, res$lmp$log_post, type = "l")
  # plot(res_nosim$lmp$iteration, res_nosim$lmp$log_post, type = "l")
})





test_that("expect same input priors and params as are returned", {
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
                   betax1 = 0.02,
                   alpha_pi = 0.2,
                   beta_pi = 0.3)

  params <- parameters(K = 10, gibbs_iter = 5L, save_state_every = 10, seed = 4711)
  params$verbose <- FALSE

  expect_silent(res <- perspective_sampler(state_df, priors = priors, params))

  expect_identical(res$priors, priors)
  expect_identical(res$parameters, params)

})

