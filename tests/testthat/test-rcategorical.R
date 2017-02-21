context("rcategorical")

test_that("rcategorical", {
  set.seed(4711)
  u_prob <- c(0.7, 1.7, 1)
  prob <- u_prob / sum(u_prob)
  count <- integer(3)
  n <- 100000
  for(i in 1:n){
    x <- PerspectiveTopicModel:::rcategorical(c(0.7, 1.7, 1))
    count[x] <- count[x] + 1
  }

  expect_equal(sum(count), n)

  expect_gt(chisq.test(count, p = prob)$p.value, 0.001)

})



test_that("rcategorical sum(p) <= 0", {
  set.seed(4711)
  prob <- c(0, 0, 0)
  expect_error(x <- PerspectiveTopicModel:::rcategorical(prob))
})
