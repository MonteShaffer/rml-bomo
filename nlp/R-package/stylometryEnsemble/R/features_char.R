char_ngrams <- function(text, n){
  text <- tolower(text)
  text <- gsub("[^a-z ]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  chars <- strsplit(text, "")[[1]]
  if(length(chars) < n) return(character(0))
  sapply(seq_len(length(chars) - n + 1), function(i) paste(chars[i:(i+n-1)], collapse = ""))
}

char_tfidf <- function(docs, n){
  grams_by_doc <- lapply(docs, char_ngrams, n = n)
  tfidf_from_grams(grams_by_doc)
}
