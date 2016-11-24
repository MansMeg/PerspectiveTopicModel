context("init_counts2_cpp")

test_that("init_counts2_cpp", {
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

  icr <- PerspectiveTopicModel:::init_counts_r(state_df, constants)
  icrcpp <- PerspectiveTopicModel:::init_count2_cpp(state_df, constants)

  expect_silent(
    stopifnot(all(icrcpp$n_dk == icr$n_dk),
              sum(icrcpp$n_kvpx) == sum(icr$n_vkpx),
              all(icrcpp$n_kpx == icr$n_kpx))
  )
  for(k in 1:constants$K){
    for(v in 1:constants$V){
      for(px in 1:constants$P){
        expect_equal(icrcpp$n_kvpx[k,v,px], icr$n_vkpx[v,k,px])
      }
    }
  }
})
