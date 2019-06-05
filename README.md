[![Build Status](https://travis-ci.org/MansMeg/PerspectiveTopicModel.svg?branch=master)](https://travis-ci.org/MansMeg/PerspectiveTopicModel)


# PerspectiveTopicModel

This is a implementation that is currently under development. Use at your own risk.

## Installation

To install the package just use:

```
devtools::install_github("MansMeg/PerspectiveTopicModel")
library(PerspectiveTopicModel)
```


## Test run of sampler

Below is an example with simulated data, mainly to test that everything works as it should

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

It is also straight forward to include restrictions on the model. 
Setting ```perspective_topics = 0L``` indicate that perspectives are not used (for example) for using only seeding of documents and or word types.

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

## Test with real data

The data format used is that of ```tidytext```. Here is an example of getting the data in the right format.

```

library(janeaustenr)
library(tidytext)
library(dplyr)

data("prideprejudice")
data("stop_words")

# Pride and
dat <- data.frame(text = prideprejudice, paragraph = 0, chapter = 0, stringsAsFactors = FALSE)

# Add paragraphs and chapter sID
paragraph_indication <- as.integer(dat$text == "")
dat$paragraph <- cumsum(paragraph_indication)

chapter_indication <- as.integer(grepl(stringr::str_trim(tolower(dat$text)), pattern = "^chapter [0-9]+$"))
dat$chapter <- cumsum(chapter_indication)

# Remove chapter titles
dat <- dat[!chapter_indication,]

# Tidy data
tidy_pride <- unnest_tokens(dat, word, text)

# Remove punctuations
tidy_pride$word <- stringr::str_replace_all(tidy_pride$word, "[:punct:]", "")

# Remove stopwords 
tidy_pride <- anti_join(tidy_pride, y = stop_words)

# Remove rare words
word_freq <- table(tidy_pride$word)
rare_words <- data.frame(word = names(word_freq[word_freq <= 5]), stringsAsFactors = FALSE)
tidy_pride <- anti_join(tidy_pride, y = rare_words)

# Cleanup for sampler
tidy_pride <- tidy_pride[tidy_pride$chapter>0,]
tidy_pride$paragraph <- NULL
colnames(tidy_pride) <- c("doc", "type")
tidy_pride$party <- 0L


# Set prior
priors <- priors(alpha = 0.1,
                 betax0 = 0.1, # Ordinary prior on Phi
                 betax1 = 0.1, # Prior on perspective Phis
                 alpha_pi = 0.1, # Prior on perspective proportions
                 beta_pi = 0.1, # Prior on perspective proportions
                 perspective_topics = 0L, # No perspectives used
                 non_zero_type_topics = list("darcy" = 1, # Darcy will only belong to topic 1
                                             "jane" = c(1, 2)), # Jane will only belong to topic 1 and 2
                 non_zero_doc_topics = list("1" = c(1,2,3)) # Document 1 can only have topics 1,2,3
)

# Initialize sampler
K <- 10
tidy_pride$topic <- sample(1:K, size = length(tidy_pride$type), replace = TRUE)
tidy_pride$perspective <- 0L
tidy_pride$type <- as.factor(tidy_pride$type)
tidy_pride$doc <- as.factor(tidy_pride$doc)
tidy_pride$party <- as.factor(tidy_pride$party)

# Set parameters
# Note that here we only use 20 Gibbs iterations, for real analysis probably 1000 iterations are needed
# Check res$lmp (log marginal posterior) to asses convergence over time
params <- parameters(gibbs_iter = 20L, K = K, verbose = TRUE)

# Run model
res <- perspective_sampler(tidy_pride, priors = priors, params)

# The resulting draw from the posterior is stored in res$state
# The topic indicators can be used to analyze the results
head(res$state)
```

