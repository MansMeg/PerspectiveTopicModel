[![Build Status](https://travis-ci.org/MansMeg/PerspectiveTopicModel.svg?branch=master)](https://travis-ci.org/MansMeg/PerspectiveTopicModel)


# PerspectiveTopicModel

This is a implementation that is currently under development. Use at your own risk.

## Installation

To install the package just use:

```
devtools::install_github("MansMeg/PerspectiveTopicModel")
```


## Example run

```
# Create a random dataset
set.seed(4711)
N <- 1000
D <- 17
V <- 31
K <- 10
P <- 3
state_df <- data.frame(doc = as.factor(sample(1:D, size = N, replace = TRUE)),
                       type = as.factor(sample(1:V, size = N, replace = TRUE)),
                       party = as.factor(sample(1:P, size = N, replace = TRUE)))

# Init topic and perspective indicators
state_df$topic <- sample(1:K, size = N, replace = TRUE)
state_df$perspective <- sample(0:1, size = N, replace = TRUE)

# Define priors
priors <- priors(alpha = 0.1,
                 betax0 = 0.01, # Ordinary prior on Phi
                 betax1 = 0.01, # Prior on perspective Phis
                 alpha_pi = 0.1, # Prior on perspective proportions
                 beta_pi = 0.1 # Prior on perspective proportions
                 )

# Define parameters
params <- parameters(gibbs_iter = 20L, save_state_every = 10, verbose = TRUE)

# Run model
res <- perspective_sampler(state_df, priors = priors, params)
```
### Type priors and document prior

It is also straight forward to include restrictions on the model

```

# Define priors
priors <- priors(alpha = 0.1,
                 betax0 = 0.01, # Ordinary prior on Phi
                 betax1 = 0.01, # Prior on perspective Phis
                 alpha_pi = 0.1, # Prior on perspective proportions
                 beta_pi = 0.1, # Prior on perspective proportions
                 perspective_topics = 0L, # No perspectives used
                 non_zero_type_topics = list("1" = 1, # Word type 1 can only belong to topic 1
                                             "2" = c(1, 3)), # Word type 2 can only belong to topic 1 and 3
                 non_zero_doc_topics = list("1" = c(1,2,3), # Document 1 can only have topics 1,2,3
                                            "4" = c(1:5)) # Document 4 can only have topics 1 to 5
)

# Run model
res <- perspective_sampler(state_df, priors = priors, params)
```


