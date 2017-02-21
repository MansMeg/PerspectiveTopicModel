#' Assert a non_zero_type_topics list and a non_zero_doc_topics list
#'
#' @param priors a \code{priors} object.
#' @param constants a \code{state_constants} object.
#' @param vocabulary a character vector with the vocabulary.
#' @param doc_ids a character vector with the document ids.
#'
#' @keywords Internal
assert_non_zero<- function(priors, constants, vocabulary, doc_ids){
  checkmate::assert_class(priors, "priors")
  checkmate::assert_class(constants, "state_constants")
  checkmate::assert_character(vocabulary, len = constants$V)
  checkmate::assert_character(doc_ids, len = constants$D)

  if(!is.null(priors$non_zero_type_topics)){
    checkmate::assert(all(names(priors$non_zero_type_topics) %in% vocabulary))
    for(i in seq_along(priors$non_zero_type_topics)){
      checkmate::assert_integerish(priors$non_zero_type_topics[[i]], lower = 1, upper = constants$K)
    }
  }

  if(!is.null(priors$non_zero_doc_topics)){
    checkmate::assert(all(names(priors$non_zero_doc_topics) %in% doc_ids))
    for(i in seq_along(priors$non_zero_doc_topics)){
      checkmate::assert_integerish(priors$non_zero_doc_topics[[i]], lower = 1, upper = constants$K)
    }
  }

  checkmate::assert_integerish(priors$perspective_topics, max.len = constants$K, upper = constants$K, null.ok = TRUE)
}
