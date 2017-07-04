context("log_marginal_posterior")


test_that("log_marginal_posterior", {
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
                   betax0 = 0.05,
                   betax1 = 0.01,
                   alpha_pi = 0.15,
                   beta_pi = 0.2)
  priors <- PerspectiveTopicModel:::get_priors_for_iteration(priors, 0L)

  params <- parameters(K = 10, gibbs_iter = 100L, save_state_every = 1000, seed = 4711, verbose = FALSE)

  count_matrices <- PerspectiveTopicModel:::init_count2_cpp(state_df, constants)
  count_matrices[["n_pk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 2), sum))
  count_matrices[["n_xk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 3), sum))

  expect_silent(
    lmp0 <- PerspectiveTopicModel:::log_marginal_posterior_computation(count_matrices, priors)
  )
  expect_silent(
    lmp0b <- PerspectiveTopicModel:::log_marginal_posterior(state_df, priors)
  )
  expect_equal(lmp0, -8260.744, tolerance = 0.01)
  expect_equal(lmp0, lmp0b, tolerance = 0.01)

  # Run 100 iterations and check lmp < lmp0
  expect_silent(res <- perspective_sampler(state_df, priors, params))

  expect_silent(
    lmp100 <- PerspectiveTopicModel:::log_marginal_posterior_computation(res$count_matrices, priors)
  )
  expect_silent(
    lmp100b <- PerspectiveTopicModel:::log_marginal_posterior(res$state, priors)
  )

  expect_equal(lmp100, lmp100b, tolerance = 0.01)
})


test_that("log_marginal_posterior manual check", {
  set.seed(4711)
  N <- 10
  D <- 2
  V <- 2
  K <- 2
  P <- 2
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
                   betax0 = 0.05,
                   betax1 = 0.01,
                   alpha_pi = 0.15,
                   beta_pi = 0.2)

  params <- parameters(K = 10, gibbs_iter = 100L, save_state_every = 1000, seed = 4711, verbose = FALSE)

  count_matrices <- init_count2_cpp(state_df, constants)
  count_matrices[["n_pk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 2), sum))
  count_matrices[["n_xk"]] <- t(apply(count_matrices$n_kpx, MARGIN=c(1, 3), sum))

  # Manual computation
  man_lmp <-
    K * lgamma(V * priors$betax0) -
    K * V * lgamma(priors$betax0) +
    sum(lgamma(count_matrices$n_kvpx[,,1] + priors$betax0)) -
    sum(lgamma(rowSums(count_matrices$n_kvpx[,,1] + priors$betax0))) + # (a)
    P * K * lgamma(V * priors$betax1) -
    P * K * V * lgamma(priors$betax1) +
    sum(lgamma(count_matrices$n_kvpx[,,-1] + priors$betax1)) -
    sum(lgamma(rowSums(count_matrices$n_kvpx[,,2] + priors$betax1))) -
    sum(lgamma(rowSums(count_matrices$n_kvpx[,,3] + priors$betax1))) + # (b)
    P * K * lgamma(priors$alpha_pi + priors$beta_pi) -
    P * K * (lgamma(priors$alpha_pi) + lgamma(priors$beta_pi)) +
    sum(lgamma(count_matrices$n_kpx[,,1] + priors$beta_pi)) +
    sum(lgamma(count_matrices$n_kpx[,,2] + priors$alpha_pi)) -
    sum(lgamma(count_matrices$n_kpx[,,1] + count_matrices$n_kpx[,,2] + priors$alpha_pi + priors$beta_pi)) + # (c)
    D * lgamma(priors$alpha * K) -
    D * K * lgamma(priors$alpha) +
    sum(lgamma(count_matrices$n_dk + priors$alpha)) -
    sum(lgamma(rowSums(count_matrices$n_dk + priors$alpha))) # (d)


  expect_silent(
    lmp0 <- log_marginal_posterior_computation(count_matrices, priors)
  )

  expect_equal(lmp0, man_lmp, tolerance = 0.01)

})
