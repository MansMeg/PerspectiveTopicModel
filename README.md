[![Build Status](https://travis-ci.org/MansMeg/PerspectiveTopicModel.svg?branch=master)](https://travis-ci.org/MansMeg/PerspectiveTopicModel)


# PerspectiveTopicModel

## Example run

```
# Create a random dataset
set.seed(4711)
N <- 1000
D <- 17
V <- 31
K <- 10
P <- 3
state_df <- data.frame(doc = sample(1:D, size = N, replace = TRUE),
                       type = sample(1:V, size = N, replace = TRUE),
                       party = sample(1:P, size = N, replace = TRUE))

# Init topic and perspective indicators
state_df$topic <- sample(1:K, size = N, replace = TRUE)
state_df$perspective <- sample(0:1, size = N, replace = TRUE)

# Define priors
priors <- list(alpha = 0.1,
               betax0 = 0.01,
               betax1 = 0.01,
               alpha_pi = 0.1,
               beta_pi = 0.1)

# Define parameters
params <- list(gibbs_iter = 100L, start_iter = 2L, save_state_every = 50, verbose = TRUE)

res <- perspective_sampler(state_df, priors = priors, params)
```
