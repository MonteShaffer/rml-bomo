tokenize_text <- function(text, stem = FALSE, lowercase = TRUE, remove_punct = TRUE){
  if(lowercase) text <- tolower(text)
  if(remove_punct) text <- gsub("[^a-z ]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  words <- unlist(strsplit(text, " "))
  words <- words[words != ""]
  if(stem) words <- SnowballC::wordStem(words, language = "en")
  words
}

docs_from_table <- function(section_docs, text_col = "text", doc_id_col = "doc_id"){
  if(!all(c(text_col, doc_id_col) %in% names(section_docs))){
    stop("section_docs must contain text and doc_id columns.")
  }
  docs <- section_docs[[text_col]]
  names(docs) <- section_docs[[doc_id_col]]
  docs
}
