function_word_freq <- function(docs, vocab){
  vocab <- sort(unique(vocab))
  mat <- matrix(0, nrow = length(docs), ncol = length(vocab), dimnames = list(names(docs), vocab))
  for(i in seq_along(docs)){
    words <- tokenize_text(docs[i], stem = FALSE)
    tab <- table(words)
    common <- intersect(names(tab), vocab)
    if(length(common) > 0) mat[i, common] <- as.numeric(tab[common])
    if(length(words) > 0) mat[i, ] <- mat[i, ] / length(words)
  }
  mat
}

function_word_tfidf <- function(docs, vocab){
  vocab <- sort(unique(vocab))
  words_by_doc <- lapply(docs, tokenize_text, stem = FALSE)
  tf <- matrix(0, nrow = length(docs), ncol = length(vocab), dimnames = list(names(docs), vocab))
  for(i in seq_along(words_by_doc)){
    tab <- table(words_by_doc[[i]])
    common <- intersect(names(tab), vocab)
    if(length(common) > 0) tf[i, common] <- as.numeric(tab[common])
  }
  rs <- rowSums(tf)
  rs[rs == 0] <- NA_real_
  tf <- tf / rs
  tf[is.na(tf)] <- 0
  df <- colSums(tf > 0)
  N <- nrow(tf)
  idf <- log((N + 1) / (df + 1)) + 1
  sweep(tf, 2, idf, "*")
}
