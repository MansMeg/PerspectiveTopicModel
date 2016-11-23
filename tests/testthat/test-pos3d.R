context("pos3d")

test_that("pos3d", {
  dims <- c(7,3,13)
  x <- 1:(prod(dims))
  y <- array(x, dim = c(7,3,13))

  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      for(k in 1:dims[3]){
        # cat(i,j,k, y[i, j, k], "\n")
        expect_true(x[PerspectiveTopicModel:::pos3d(i - 1, j - 1, k - 1, dims) + 1] == y[i, j, k])
      }
    }
  }
})
