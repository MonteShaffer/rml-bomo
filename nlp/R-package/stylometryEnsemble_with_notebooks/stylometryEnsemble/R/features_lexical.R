lexical_features <- function(docs){
  cols <- c("type_token_ratio", "hapax_ratio", "avg_word_length")
  mat <- matrix(0, nrow = length(docs), ncol = length(cols), dimnames = list(names(docs), cols))
  for(i in seq_along(docs)){
    words <- tokenize_text(docs[i])
    N <- length(words)
    if(N == 0) next
    tab <- table(words)
    mat[i, "type_token_ratio"] <- length(unique(words)) / N
    mat[i, "hapax_ratio"] <- sum(tab == 1) / N
    mat[i, "avg_word_length"] <- mean(nchar(words))
  }
  mat
}
