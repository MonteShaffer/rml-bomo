load_udpipe_model <- function(model_file = NULL, language = "english"){
  if(!requireNamespace("udpipe", quietly = TRUE)){
    stop("Package 'udpipe' is required for POS features.")
  }
  if(is.null(model_file)){
    model_file <- "english-ewt-ud-2.5-191206.udpipe"
    if(!file.exists(model_file)){
      info <- udpipe::udpipe_download_model(language = language)
      model_file <- info$file_model
    }
  }
  udpipe::udpipe_load_model(model_file)
}

pos_ngrams_from_pos <- function(pos, n){
  pos <- pos[!is.na(pos)]
  if(length(pos) < n) return(character(0))
  sapply(seq_len(length(pos) - n + 1), function(i) paste(pos[i:(i+n-1)], collapse = "_"))
}

pos_tfidf_all <- function(docs, n_values = c(3,4,5), model_file = NULL, ud_model = NULL){
  if(is.null(ud_model)) ud_model <- load_udpipe_model(model_file)
 # anno <- udpipe::udpipe_annotate(ud_model, x = docs, doc_id = names(docs))
 anno <- annotate_docs_pos_cached(
    docs = docs,
    ud_model = ud_model
)

  anno <- as.data.frame(anno)
  pos_by_doc <- split(anno$upos, anno$doc_id)
  # Ensure all docs represented
  pos_by_doc <- pos_by_doc[names(docs)]
  names(pos_by_doc) <- names(docs)
  out <- list()
  for(n in n_values){
    grams <- lapply(pos_by_doc, pos_ngrams_from_pos, n = n)
    out[[paste0("pos", n)]] <- tfidf_from_grams(grams)
  }
  out
}
