
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
})
