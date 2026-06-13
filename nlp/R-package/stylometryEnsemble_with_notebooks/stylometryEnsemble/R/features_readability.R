readability_features <- function(docs){
  if(!requireNamespace("quanteda", quietly = TRUE) || !requireNamespace("quanteda.textstats", quietly = TRUE)){
    warning("readability_features requires quanteda and quanteda.textstats. Returning NULL.", call. = FALSE)
    return(NULL)
  }
  corp <- quanteda::corpus(docs)
  r <- quanteda.textstats::textstat_readability(
    corp,
    measure = c("Flesch", "Flesch.Kincaid", "ARI", "Coleman.Liau", "SMOG")
  )
  mat <- as.matrix(r[, setdiff(names(r), "document"), drop = FALSE])
  rownames(mat) <- names(docs)
  mat[is.na(mat)] <- 0
  mat
}
