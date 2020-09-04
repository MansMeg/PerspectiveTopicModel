
# Experiment 3:
# Will simulated annealing end up in a better mode?
library(PerspectiveTopicModel)
source("experiments/experiment_functions.R")


## Run experiment 3

txt <- readRDS(file = "experiments/data/three-men-all.rds")
crp <- experiment3_corpus(txt, c("en" = 1200, "fr" = 360, "sv" = 420), 10)
names(crp)[4] <- "type"
crp$type <- as.factor(crp$type)

tab <- table(crp$lang)
tab;sum(tab)

N <- nrow(crp)

# Create corpus versions
crp1 <- crp
crp1$chapter[crp1$chapter <= 10] <- 1
crp1$chapter[crp1$chapter > 10] <- 2
crp1$doc <- as.factor(paste0(crp1$lang, crp1$chapter))
table(crp1$doc)

crp2 <- crp
crp2$chapter[crp2$chapter <= 10 & crp2$lang != "en"] <- 1
crp2$chapter[crp2$chapter > 10 & crp2$lang != "en"] <- 2
crp2$doc <- as.factor(paste0(crp2$lang, crp2$chapter))
table(crp2$doc)

crp3 <- crp
crp3$doc <- as.factor(paste0(crp3$chapter))
table(crp3$doc)


experiment3random <- expand.grid(kappa = c(N, 2^(0:6)),
                           C_kappa = 5,
                           alpha = 1,
                           init = "random",
                           beta = 1,
                           seed = 4711:4810,
                           stringsAsFactors = FALSE)

experiment3langinit <- experiment3random
experiment3langinit$init <- "lang"

results1 <- run_experiment3(txt = crp1,
                            experiment_jobs = experiment3random,
                            result_file_name = "results3random_crp1.rda")

results2 <- run_experiment3(txt = crp1,
                            experiment_jobs = experiment3langinit,
                            result_file_name = "experiment3langinit_crp1.rda")

results3 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3random,
                            result_file_name = "results3random_crp2.rda")

results4 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3langinit,
                            result_file_name = "experiment3langinit_crp2.rda")

results5 <- run_experiment3(txt = crp3,
                            experiment_jobs = experiment3random,
                            result_file_name = "results3random_crp3.rda")

results6 <- run_experiment3(txt = crp3,
                            experiment_jobs = experiment3langinit,
                            result_file_name = "experiment3langinit_crp3.rda")

# Experiment 3b ----

experiment3b <- expand.grid(kappa = NA,
                            C_kappa = 25,
                            alpha = 1,
                            init = "random",
                            beta = 1,
                            seed = 4711:4810,
                            stringsAsFactors = FALSE)

experiment3b$kappa <- 4999
results7 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3b,
                            result_file_name = "experiment3b4999.rda")
experiment3b$kappa <- 1
results8 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3b,
                            result_file_name = "experiment3b1.rda")

experiment3b$kappa <- 16
results9 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3b,
                            result_file_name = "experiment3b16.rda")

experiment3b$kappa <- 256
results10 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3b,
                            result_file_name = "experiment3b256.rda")
