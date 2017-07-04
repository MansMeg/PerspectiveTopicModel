
context("priors")

test_that("priors constructor", {
  expect_silent(
    x <- priors(alpha = 0.1,
                betax0 = 0.01,
                betax1 = 0.01,
                alpha_pi = 0.1,
                beta_pi = 0.1,
                non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                non_zero_type_topics = list("2" = 1:5, "6" = 2:4),
                perspective_topics = c(1,6,9))
  )
  expect_silent(priors(x))

  expect_silent(
    x <- priors(alpha = 0.1,
                betax0 = 0.01,
                betax1 = 0.01,
                alpha_pi = 0.1,
                beta_pi = 0.1,
                non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                non_zero_type_topics = list("2" = 1:5, "6" = 2:4),
                perspective_topics = integer())
  )
  expect_silent(priors(x))
})



test_that("get_prior_for_iteration", {

  expect_silent(
    priors_sim <- priors(annealing_iterations = c(1, 2, 3, 4),
                         alpha = 10 ^ c(3, 2, 1, 0) * 0.1,
                         betax0 = 10 ^ c(3, 2, 1, 0) * 0.01,
                         betax1 = 10 ^ c(3, 2, 1, 0) *  0.01,
                         alpha_pi = 10 ^ c(3, 2, 1, 0) * 0.1,
                         beta_pi = 10 ^ c(3, 2, 1, 0) * 0.1,
                         non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                         non_zero_type_topics = list("2" = 1:5, "6" = 2:4))
  )

  expect_silent(pr1 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_sim, iteration = 1))
  expect_equal(pr1$alpha, 100)
  expect_equal(pr1$betax0, 10)
  expect_equal(pr1$betax1, 10)
  expect_equal(pr1$alpha_pi, 100)
  expect_equal(pr1$beta_pi, 100)
  expect_null(pr1$annealing_iterations)

  expect_silent(pr2 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_sim, iteration = 2))
  expect_equal(pr2$alpha, 10)
  expect_equal(pr2$betax0, 1)
  expect_equal(pr2$betax1, 1)
  expect_equal(pr2$alpha_pi, 10)
  expect_equal(pr2$beta_pi, 10)
  expect_null(pr2$annealing_iterations)

  expect_silent(pr3 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_sim, iteration = 3))
  expect_equal(pr3$alpha, 1)
  expect_equal(pr3$betax0, 0.1)
  expect_equal(pr3$betax1, 0.1)
  expect_equal(pr3$alpha_pi, 1)
  expect_equal(pr3$beta_pi, 1)
  expect_null(pr3$annealing_iterations)

  expect_silent(pr4 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_sim, iteration = 4))
  expect_equal(pr4$alpha, 0.1)
  expect_equal(pr4$betax0, 0.01)
  expect_equal(pr4$betax1, 0.01)
  expect_equal(pr4$alpha_pi, 0.1)
  expect_equal(pr4$beta_pi, 0.1)
  expect_null(pr4$annealing_iterations)

  expect_silent(pr4 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_sim, iteration = 100000))
  expect_equal(pr4$alpha, 0.1)
  expect_equal(pr4$betax0, 0.01)
  expect_equal(pr4$betax1, 0.01)
  expect_equal(pr4$alpha_pi, 0.1)
  expect_equal(pr4$beta_pi, 0.1)
  expect_null(pr4$annealing_iterations)


  expect_silent(
    priors_nosim <- priors(alpha = 0.1,
                           betax0 = 0.01,
                           betax1 = 0.01,
                           alpha_pi = 0.1,
                           beta_pi = 0.1,
                           non_zero_doc_topics = list("1" = 1:8, "4" = 3:9),
                           non_zero_type_topics = list("2" = 1:5, "6" = 2:4))
  )

  expect_silent(pr4 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_nosim, iteration = 1))
  expect_equal(pr4$alpha, 0.1)
  expect_equal(pr4$betax0, 0.01)
  expect_equal(pr4$betax1, 0.01)
  expect_equal(pr4$alpha_pi, 0.1)
  expect_equal(pr4$beta_pi, 0.1)
  expect_null(pr4$annealing_iterations)

  expect_silent(pr4 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_nosim, iteration = 3))
  expect_equal(pr4$alpha, 0.1)
  expect_equal(pr4$betax0, 0.01)
  expect_equal(pr4$betax1, 0.01)
  expect_equal(pr4$alpha_pi, 0.1)
  expect_equal(pr4$beta_pi, 0.1)
  expect_null(pr4$annealing_iterations)

  expect_silent(pr4 <- PerspectiveTopicModel:::get_priors_for_iteration(priors = priors_nosim, iteration = 1000))
  expect_equal(pr4$alpha, 0.1)
  expect_equal(pr4$betax0, 0.01)
  expect_equal(pr4$betax1, 0.01)
  expect_equal(pr4$alpha_pi, 0.1)
  expect_equal(pr4$beta_pi, 0.1)
  expect_null(pr4$annealing_iterations)
})
