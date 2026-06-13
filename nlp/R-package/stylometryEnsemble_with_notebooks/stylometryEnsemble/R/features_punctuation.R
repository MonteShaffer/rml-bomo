count_regex <- function(pattern, text){
  hits <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if(length(hits) == 1 && hits[1] == -1) return(0L)
  length(hits)
}

punctuation_features <- function(docs){
  cols <- c("comma_per_word", "semicolon_per_word", "colon_per_word", "quote_per_word", "period_per_word", "punct_total_per_word")
  mat <- matrix(0, nrow = length(docs), ncol = length(cols), dimnames = list(names(docs), cols))
  for(i in seq_along(docs)){
    text <- docs[i]
    words <- tokenize_text(text)
    n_words <- max(length(words), 1)
    comma_n <- count_regex(",", text)
    semicolon_n <- count_regex(";", text)
    colon_n <- count_regex(":", text)
    quote_n <- count_regex("[\"'“”‘’]", text)
    period_n <- count_regex("\\.", text)
    total_n <- comma_n + semicolon_n + colon_n + quote_n + period_n
    mat[i, ] <- c(comma_n, semicolon_n, colon_n, quote_n, period_n, total_n) / n_words
  }
  mat
}
