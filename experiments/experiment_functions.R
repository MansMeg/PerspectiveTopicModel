#' Setup experiment 1 corpus
#' @param m document size
#' @param ndoc the number of documents copies/duplicates of the dataset
#' @param init How to init the topic indicators.
generate_experiment1_corpus <- function(m, init = "word3", ndoc = NULL){
  checkmate::assert_number(m, lower = 1)
  checkmate::assert_choice(init, choices = c("word1", "word2", "word3", "random"))
  checkmate::assert_int(ndoc, lower = 1, upper = 10, null.ok = TRUE)

  state_df <- data.frame(doc = factor(c(rep("doc1", m), rep("doc2", m), rep("doc3", m))),
                         type = factor(c(rep("word1", 0.9*m), c(rep("word2", 0.1*m), rep("word2", m), rep("word3", m)))))
  if(init == "random"){
    state_df$topic <- sample(1:2, nrow(state_df), TRUE)
  } else {
    state_df$topic <- 1L
    state_df$topic[state_df$type == init] <- 2L
  }
  if(!is.null(ndoc)){
    state_df <- eval(parse(text = paste0("rbind(", paste(rep("state_df", ndoc), collapse = ", "), ")")))
  }
  state_df
}

#' Run experiment 1
#'
#' @description
#' The corpus is generated based on m, then SA is run for N * C_kappa itreations.
#' Then the full Gibbs sampler (tau = 1) for
run_experiment1 <- function(experiment_jobs, result_file_name){
  assert_experiment_jobs(experiment_jobs)
  checkmate::assert_path_for_output(result_file_name, overwrite = TRUE)

  if(!is.null(result_file_name) && file.exists(result_file_name)) {
    load(result_file_name, envir = parent.frame())
    message("Results already exist. Return the current results.")
    return(list(results = results, experiment_jobs = experiment_jobs))
  }

  # Setup experiment
  crp <- generate_experiment1_corpus(m = experiment_jobs$m[1], init = as.character(experiment_jobs$init[1]), ndoc = experiment_jobs$ndoc[1])
  N <- nrow(crp)
  experiment_jobs$tau <- experiment_jobs$kappa/N
  experiment_jobs$sa_iterations <- N * experiment_jobs$C_kappa
  experiment_jobs$iterations <- N

  results <- list()
  for(i in 1:nrow(experiment_jobs)){
    print(paste(Sys.time(), ":", i))
    crp <- generate_experiment1_corpus(m = experiment_jobs$m[i], init = as.character(experiment_jobs$init[i]), ndoc = experiment_jobs$ndoc[i])

    params <- list(K = 2,
                   tau = rep(experiment_jobs$tau[i], experiment_jobs$sa_iterations[i]),
                   save_state_every = NULL,
                   log_marginal_posterior_every = 10,
                   seed = experiment_jobs$seed[i],
                   verbose = FALSE)
    priors <- list(alpha = experiment_jobs$alpha[i], beta = experiment_jobs$beta[i])
    # Run SA
    res_sa <- collapsed_sampler_simulated_annealing(state = crp, priors = priors, params)
    # Run final N iterations with tau = 1
    params$tau <- rep(1, N)
    res_final <- collapsed_sampler_simulated_annealing(state = res_sa$state, priors, params)
    results[[i]] <- list(sa = res_sa, final = res_final)
  }
  save(results, experiment_jobs, file = result_file_name)

  list(results = results, experiment_jobs = experiment_jobs)
}


#' Setup a list with results as a ggplot2 data
#'
#' @param result a list with sa results
#' @param experiment_jobs a dataframe with parameters per job
#'
setup_ggplot_data <- function(results, experiment_jobs){
  checkmate::assert_list(results)
  checkmate::assert_data_frame(experiment_jobs)
  checkmate::assert_true(length(results) == nrow(experiment_jobs))

  dfs <- list()
  for (i in seq_along(results)) {
    df <- combine_lmp(results[[i]])
    dfs[[i]] <- suppressWarnings(cbind(df, experiment_jobs[i,]))
  }
  do.call(rbind, dfs)
}

#' Get the full LMP from a result list object (with SA and final)
#'
#' @param x a result object
combine_lmp <- function(x){
  assert_result_object(x)

  x$final$lmp$iteration <- x$final$lmp$iteration + x$sa$lmp$iteration[nrow(x$sa$lmp)]
  lmp <- rbind(x$sa$lmp, x$final$lmp)
  lmp
}

#' Extract the maximum log marginal posterior value
#'
#' @param x a result object
extract_max_log_marginal_posterior <- function(x){
  m <- numeric(length(x))
  for (i in seq_along(x)){
    m[i] <- max(x[[i]]$final$lmp$log_post)
  }
  m
}


assert_result_object <- function(x){
  checkmate::assert_list(x)
  checkmate::assert_names(names(x), must.include = c("sa", "final"))
  checkmate::assert_names(names(x$sa), must.include = c("lmp"))
  checkmate::assert_names(names(x$final), must.include = c("lmp"))
  checkmate::assert_true(x$sa$lmp[nrow(x$sa$lmp),]$log_post == x$final$lmp[1,]$log_post)
}

#' @param txt a tidy text corpus with columns [line], [chapter], [text] and [lang]
#' @param no_words_per_chapter a named vector with language as name and the number of tokens to choose per chapter
experiment3_corpus <- function(txt,
                               no_words_per_chapter = c("en" = 700, "fr" = 210, "sv" = 300),
                               rare_word_limit = 10,
                               remove_stopwords = "nltk"){
  checkmate::assert_names(names(txt), must.include = c("line", "chapter", "text", "lang"))
  checkmate::assert_names(names(no_words_per_chapter), subset.of = unique(txt$lang))
  checkmate::assert_int(rare_word_limit, lower = 0)
  checkmate::assert_integerish(no_words_per_chapter, lower = 0)
  checkmate::assert_choice(remove_stopwords, null.ok = TRUE, stopwords::stopwords_getsources())
  checkmate::assert_subset(names(no_words_per_chapter),  stopwords::stopwords_getlanguages(remove_stopwords))

  # Remove it and chapter titles
  txt <- txt[c(TRUE, !(txt$chapter[-length(txt$chapter)] != txt$chapter[-1])),]
  txt <- txt[txt$chapter >= 1,]

  txt <- tidytext::unnest_tokens(txt, word, text, token = "words", to_lower = TRUE)

  # Add chapter row no
  txt <- dplyr::group_by(txt, lang, chapter)
  txt <- dplyr::mutate(txt, chapter_word_no = dplyr::row_number())

  res <- list()
  for(i in seq_along(no_words_per_chapter)){
    lang <- names(no_words_per_chapter)[i]
    tmp <- txt[txt$lang == lang, ]
    # Remove stopwords here
    sw <- stopwords::stopwords(language = lang, source = remove_stopwords)
    if(lang == "fr") sw <- c(sw, "qu’il", "a", "c’est", "d’un")
    swt <- tibble(word = sw)

    tmp <- dplyr::anti_join(tmp, swt, by = "word")

    # Extract top words
    tmp <- get_first_n_words_from_chapter(tmp, lang, no_words_per_chapter[lang])
    res[[i]] <- remove_rare_words(tmp, rare_word_limit)
  }
  res <- do.call(rbind, res)
  res
}


remove_rare_words <- function(txt, rare_word_limit){
  checkmate::assert_names(names(txt), must.include = c("line", "chapter", "word", "lang"))
  checkmate::assert_int(rare_word_limit, lower = 0)

  txt <- dplyr::group_by(txt, word, lang)
  wf <- dplyr::summarise(txt, n = n(), .groups = "keep")
  txt <- dplyr::left_join(txt, wf, by = c("word", "lang"))
  txt <- txt[txt$n >= rare_word_limit,]
  txt$n <- NULL
  txt
}

get_first_n_words_from_chapter <- function(txt, lang, n){
  checkmate::assert_names(names(txt), must.include = c("line", "chapter", "word", "lang", "chapter_word_no"))
  checkmate::assert_choice(lang, choices = unique(txt$lang))
  checkmate::assert_int(n, lower = 1)
  tmp <- txt[txt$lang == lang,]
  tmp <- tmp[tmp$chapter_word_no <= n,]
  tmp
}


#' Run experiment 1
#'
#' @description
#' The corpus is generated based on m, then SA is run for N * C_kappa itreations.
#' Then the full Gibbs sampler (tau = 1) for
run_experiment3 <- function(txt, experiment_jobs, result_file_name){
  assert_experiment_jobs(experiment_jobs)
  checkmate::assert_path_for_output(result_file_name, overwrite = TRUE)
  checkmate::assert_data_frame(txt)
  checkmate::assert_names(names(txt), must.include = c("doc", "type", "lang"))

  if(!is.null(result_file_name) && file.exists(result_file_name)) {
    load(result_file_name, envir = parent.frame())
    message("Results already exist. Return the current results.")
    return(list(results = results, experiment_jobs = experiment_jobs))
  }

  # Setup experiment
  crp <- txt
  N <- nrow(crp)
  experiment_jobs$tau <- experiment_jobs$kappa/N
  experiment_jobs$sa_iterations <- N * experiment_jobs$C_kappa
  experiment_jobs$iterations <- N

  results <- list()
  for(i in 1:nrow(experiment_jobs)){
    print(paste(Sys.time(), ":", i))
    checkmate::assert_choice(experiment_jobs$init[i], choices = c("random", "lang"))
    if(experiment_jobs$init[i] == "random"){
      crp$topic <- sample(1:3, N, TRUE)
    }
    if(experiment_jobs$init[i] == "lang"){
      crp$topic <- as.integer(as.factor(crp$lang))
    }

    params <- list(K = 3,
                   tau = rep(experiment_jobs$tau[i], experiment_jobs$sa_iterations[i]),
                   save_state_every = NULL,
                   log_marginal_posterior_every = 10,
                   seed = experiment_jobs$seed[i],
                   verbose = FALSE)
    priors <- list(alpha = experiment_jobs$alpha[i], beta = experiment_jobs$beta[i])
    # Run SA
    res_sa <- collapsed_sampler_simulated_annealing(state = crp, priors = priors, params)
    # Run final N iterations with tau = 1
    params$tau <- rep(1, N)
    res_final <- collapsed_sampler_simulated_annealing(state = res_sa$state, priors, params)
    results[[i]] <- list(sa = res_sa, final = res_final)
  }
  save(results, experiment_jobs, file = result_file_name)

  list(results = results, experiment_jobs = experiment_jobs)
}


assert_experiment_jobs <- function(experiment_jobs){
  checkmate::assert_data_frame(experiment_jobs)
  must <- c("kappa", "C_kappa", "seed", "alpha", "beta")
  can <- c("m", "init", "ndoc")
  checkmate::assert_names(names(experiment_jobs), must.include = must, subset.of = c(must, can))
}
