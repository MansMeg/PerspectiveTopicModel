context("sampler2_cpp")

test_that("sampler_cpp2", {
  set.seed(4711)
  N <- 1000
  D <- 17
  V <- 31
  K <- 10
  P <- 3
  state_df <- data.frame(doc = sample(1:D, size = N, replace = TRUE),
                         type = sample(1:V, size = N, replace = TRUE),
                         topic = sample(1:K, size = N, replace = TRUE),
                         party = sample(1:P, size = N, replace = TRUE),
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

  icrcpp1 <- PerspectiveTopicModel:::init_count_cpp(state_df, constants)
  icrcpp1[["n_kp"]] <- apply(icrcpp1$n_kpx, MARGIN=c(1, 2), sum)
  icrcpp1[["n_kx"]] <- apply(icrcpp1$n_kpx, MARGIN=c(1, 3), sum)

  expect_silent(
    stopifnot(all(icrcpp1$n_dk >= 0),
              all(icrcpp1$n_vkpx >= 0),
              all(icrcpp1$n_kpx >= 0),
              all(icrcpp1$n_kp >= 0),
              all(icrcpp1$n_kx >= 0))
  )

  icrcpp2 <- PerspectiveTopicModel:::init_count2_cpp(state_df, constants)
  icrcpp2[["n_pk"]] <- t(apply(icrcpp2$n_kpx, MARGIN=c(1, 2), sum))
  icrcpp2[["n_xk"]] <- t(apply(icrcpp2$n_kpx, MARGIN=c(1, 3), sum))

  expect_silent(
    stopifnot(all(icrcpp2$n_dk >= 0),
              all(icrcpp2$n_kvpx >= 0),
              all(icrcpp2$n_kpx >= 0),
              all(icrcpp2$n_pk >= 0),
              all(icrcpp2$n_xk >= 0))
  )

  priors <- list(alpha = 0.1,
                 betax0 = 0.01,
                 betax1 = 0.05,
                 alpha_pi = 0.15,
                 beta_pi = 0.20)

  set.seed(4712)
  expect_silent(
    state_it1_r <- PerspectiveTopicModel:::per_sampler_r(state = state_df, count_matrices = icrcpp1, priors = priors, constants = constants)
  )

  set.seed(4712)
  expect_silent(
    state_it1_cpp <- PerspectiveTopicModel:::per_sampler2_cpp(state = state_df, count_matrices = icrcpp2, priors = priors, constants = constants)
  )

  icrcpp_it1 <- PerspectiveTopicModel:::init_count_cpp(state_it1_cpp[[1]], constants)
  icrcpp_it1[["n_kp"]] <- apply(icrcpp_it1$n_kpx, MARGIN=c(1, 2), sum)
  icrcpp_it1[["n_kx"]] <- apply(icrcpp_it1$n_kpx, MARGIN=c(1, 3), sum)

  expect_silent(
    stopifnot(sum(state_it1_cpp$count_matrices$n_dk) == sum(state_it1_cpp$count_matrices$n_vkpx),
              sum(state_it1_cpp$count_matrices$n_vkpx) == sum(state_it1_cpp$count_matrices$n_kpx))
  )

  expect_silent(
    stopifnot(all(icrcpp_it1$n_dk == state_it1_cpp$count_matrices$n_dk),
              all(icrcpp_it1$n_vkpx == state_it1_cpp$count_matrices$n_vkpx),
              all(icrcpp_it1$n_kpx == state_it1_cpp$count_matrices$n_kpx),
              all(icrcpp_it1$n_kp == state_it1_cpp$count_matrices$n_kp),
              all(icrcpp_it1$n_kx == state_it1_cpp$count_matrices$n_kx))
  )

  expect_silent(
    stopifnot(all(state_it1_r$state == state_it1_cpp$state),
              all(state_it1_r$count_matrices$n_dk == state_it1_cpp$count_matrices$n_dk),
              all(state_it1_r$count_matrices$n_vkpx == state_it1_cpp$count_matrices$n_vkpx),
              all(state_it1_r$count_matrices$n_kpx == state_it1_cpp$count_matrices$n_kpx),
              all(state_it1_r$count_matrices$n_kp == state_it1_cpp$count_matrices$n_kp),
              all(state_it1_r$count_matrices$n_kx == state_it1_cpp$count_matrices$n_kx))
  )
})
