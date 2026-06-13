word_ngrams <- function(text, n = 1, stem = FALSE){
  words <- tokenize_text(text, stem = stem)
  if(length(words) < n) return(character(0))
  sapply(seq_len(length(words) - n + 1), function(i) paste(words[i:(i+n-1)], collapse = "_"))
}

ngram_tfidf <- function(docs, n = 1, stem = FALSE){
  grams_by_doc <- lapply(docs, word_ngrams, n = n, stem = stem)
  tfidf_from_grams(grams_by_doc)
}
