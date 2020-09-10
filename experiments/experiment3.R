
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

# Double sized en
crp4 <- crp
crp4$chapter[crp4$chapter <= 10 & crp4$lang != "en"] <- 1
crp4$chapter[crp4$chapter > 10 & crp4$lang != "en"] <- 2
docs <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10)
for(i in seq_along(docs)) crp4$chapter[crp4$lang == "en" & crp4$chapter == i ] <- docs[i]
crp4$doc <- as.factor(paste0(crp4$lang, crp4$chapter))
table(crp4$doc)

crp5 <- crp
crp5$chapter[crp5$chapter <= 10 & crp5$lang != "en"] <- 1
crp5$chapter[crp5$chapter > 10 & crp5$lang != "en"] <- 2
docs <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5)
for(i in seq_along(docs)) crp5$chapter[crp5$lang == "en" & crp5$chapter == i ] <- docs[i]
crp5$doc <- as.factor(paste0(crp5$lang, crp5$chapter))
table(crp5$doc)


# Get LMP MC approximation
priors <- list(alpha = 1, beta = 1)
lpd_crp1 <- lpd_crp2 <- lpd_crp3 <- numeric(100)
for(i in 1:length(lpd_crp1)){
  crp1$topic <- sample(1:3, nrow(crp1), replace = TRUE)
  crp2$topic <- sample(1:3, nrow(crp2), replace = TRUE)
  crp3$topic <- sample(1:3, nrow(crp3), replace = TRUE)
  cm1 <- init_count_matrices_lda(crp1)
  cm2 <- init_count_matrices_lda(crp2)
  cm3 <- init_count_matrices_lda(crp3)
  lpd_crp1[i] <- log_marginal_posterior_lda(cm1, priors)
  lpd_crp2[i] <- log_marginal_posterior_lda(cm2, priors)
  lpd_crp3[i] <- log_marginal_posterior_lda(cm3, priors)
}
mean(lpd_crp1); mean(lpd_crp2); mean(lpd_crp3)

# Init with language
crp1$topic <- as.integer(as.factor(crp1$lang))
crp2$topic <- as.integer(as.factor(crp2$lang))
crp3$topic <- as.integer(as.factor(crp3$lang))
cm1 <- init_count_matrices_lda(crp1)
cm2 <- init_count_matrices_lda(crp2)
cm3 <- init_count_matrices_lda(crp3)
lpd_crp1 <- log_marginal_posterior_lda(cm1, priors)
lpd_crp2 <- log_marginal_posterior_lda(cm2, priors)
lpd_crp3 <- log_marginal_posterior_lda(cm3, priors)

# Experiment 3a:
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

experiment3low_mode <- expand.grid(kappa = c(N, 2^(0:8)),
                                   C_kappa = 5,
                                   alpha = 1,
                                   beta = 1,
                                   init = "sv+fr",
                                   seed = 5711:5810,
                                   stringsAsFactors = FALSE)

results1 <- run_experiment3(txt = crp1,
                            experiment_jobs = experiment3low_mode,
                            result_file_name = "results3b_crp1.rda")

results2 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3low_mode,
                            result_file_name = "results3b_crp2.rda")


# Experiment 3c ----

experiment3c1 <- expand.grid(kappa = c(128 + (0:8)*4),
                             C_kappa = 5,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3c2 <- expand.grid(kappa = c(128 + (9:16)*4),
                             C_kappa = 5,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3c3 <- expand.grid(kappa = c(128 + (17:24)*4),
                             C_kappa = 5,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3c4 <- expand.grid(kappa = c(128 + (25:32)*4),
                             C_kappa = 5,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)

results1 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3c1,
                            result_file_name = "results3c_crp2_1.rda")

results2 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3c2,
                            result_file_name = "results3c_crp2_2.rda")

results3 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3c3,
                            result_file_name = "results3c_crp2_3.rda")

results4 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3c4,
                            result_file_name = "results3c_crp2_4.rda")


# Experiment 3d
experiment3d1 <- expand.grid(kappa = c(160 + (0:1)*4),
                             C_kappa = 25,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3d2 <- expand.grid(kappa = c(160 + (2:3)*4),
                             C_kappa = 25,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3d3 <- expand.grid(kappa = c(160 + (4:5)*4),
                             C_kappa = 25,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3d4 <- expand.grid(kappa = c(160 + (6:7)*4),
                             C_kappa = 25,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)


results1 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3d1,
                            result_file_name = "results3d_crp2_1.rda")

results2 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3d2,
                            result_file_name = "results3d_crp2_2.rda")

results3 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3d3,
                            result_file_name = "results3d_crp2_3.rda")

results4 <- run_experiment3(txt = crp2,
                            experiment_jobs = experiment3d4,
                            result_file_name = "results3d_crp2_4.rda")



# Experiment 3e
experiment3e1 <- expand.grid(kappa = c(N, 2^(4:8), 96, 120),
                             C_kappa = 5,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)
experiment3e2 <- expand.grid(kappa = c(128 + (1:8)*8),
                             C_kappa = 5,
                             alpha = 1,
                             beta = 1,
                             init = "sv+fr",
                             seed = 4711:4810,
                             stringsAsFactors = FALSE)


results1 <- run_experiment3(txt = crp4,
                            experiment_jobs = experiment3e1,
                            result_file_name = "experiment3e1_crp4_1.rda")

results2 <- run_experiment3(txt = crp4,
                            experiment_jobs = experiment3e2,
                            result_file_name = "experiment3e1_crp4_2.rda")

results3 <- run_experiment3(txt = crp5,
                            experiment_jobs = experiment3e1,
                            result_file_name = "experiment3e1_crp5_1.rda")

results4 <- run_experiment3(txt = crp5,
                            experiment_jobs = experiment3e2,
                            result_file_name = "experiment3e1_crp5_2.rda")




