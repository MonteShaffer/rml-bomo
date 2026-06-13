sentence_length_bins <- function(docs){
  bins <- c("sent_lt10", "sent_10_19", "sent_20_29", "sent_30_39", "sent_ge40")
  mat <- matrix(0, nrow = length(docs), ncol = length(bins), dimnames = list(names(docs), bins))
  for(i in seq_along(docs)){
    sents <- unlist(strsplit(docs[i], "[.!?]+"))
    sents <- trimws(sents)
    sents <- sents[sents != ""]
    if(length(sents) == 0){ next }
    lens <- sapply(sents, function(s) length(tokenize_text(s)))
    mat[i, "sent_lt10"]  <- mean(lens < 10)
    mat[i, "sent_10_19"] <- mean(lens >= 10 & lens < 20)
    mat[i, "sent_20_29"] <- mean(lens >= 20 & lens < 30)
    mat[i, "sent_30_39"] <- mean(lens >= 30 & lens < 40)
    mat[i, "sent_ge40"]  <- mean(lens >= 40)
  }
  mat
}

sentence_length_tfidf <- function(docs){
  tf <- sentence_length_bins(docs)
  df <- colSums(tf > 0)
  N <- nrow(tf)
  idf <- log((N + 1) / (df + 1)) + 1
  sweep(tf, 2, idf, "*")
}
