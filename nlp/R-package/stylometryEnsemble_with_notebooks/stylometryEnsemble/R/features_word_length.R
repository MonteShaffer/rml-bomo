# features_word_length.R
# Word-length distribution features for stylometryEnsemble

tokenize_word_lengths <- function(text){

  tokens <- stringr::str_to_lower(text)

  tokens <- stringr::str_replace_all(
    tokens,
    "[^a-zA-Z']+",
    " "
  )

  tokens <- stringr::str_squish(tokens)

  if(is.na(tokens) || tokens == ""){
    return(character(0))
  }

  tokens <- unlist(
    stringr::str_split(tokens, "\\s+")
  )

  tokens <- tokens[tokens != ""]
  tokens <- stringr::str_replace_all(tokens, "'", "")
  tokens[nchar(tokens) > 0]
}


word_length_summary_one <- function(text, max_len = 15){

  tokens <- tokenize_word_lengths(text)

  if(length(tokens) == 0){

    out <- data.frame(
      mean_word_length = 0,
      sd_word_length = 0,
      median_word_length = 0,
      pct_word_len_1_3 = 0,
      pct_word_len_4_6 = 0,
      pct_word_len_7_9 = 0,
      pct_word_len_10_plus = 0
    )

    for(i in seq_len(max_len)){
      out[[paste0("word_len_", i)]] <- 0
    }

    out[[paste0("word_len_", max_len, "_plus")]] <- 0

    return(out)
  }

  lens <- nchar(tokens)
  n <- length(lens)

  out <- data.frame(
    mean_word_length = mean(lens),
    sd_word_length = ifelse(n > 1, stats::sd(lens), 0),
    median_word_length = stats::median(lens),
    pct_word_len_1_3 = mean(lens >= 1 & lens <= 3),
    pct_word_len_4_6 = mean(lens >= 4 & lens <= 6),
    pct_word_len_7_9 = mean(lens >= 7 & lens <= 9),
    pct_word_len_10_plus = mean(lens >= 10)
  )

  for(i in seq_len(max_len)){
    out[[paste0("word_len_", i)]] <- mean(lens == i)
  }

  out[[paste0("word_len_", max_len, "_plus")]] <- mean(lens >= max_len)

  out
}


build_word_length_features <- function(
    docs,
    text_col = "text",
    doc_id_col = "doc_id",
    max_len = 15){

  if(is.character(docs)){

    doc_ids <- names(docs)

    if(is.null(doc_ids)){
      doc_ids <- paste0("doc_", seq_along(docs))
    }

    rows <- lapply(
      docs,
      word_length_summary_one,
      max_len = max_len
    )

    mat <- dplyr::bind_rows(rows)
    mat <- as.matrix(mat)

    rownames(mat) <- doc_ids

    return(mat)
  }

  if(!text_col %in% names(docs)){
    stop("Missing text column: ", text_col)
  }

  if(!doc_id_col %in% names(docs)){
    stop("Missing doc_id column: ", doc_id_col)
  }

  rows <- lapply(
    docs[[text_col]],
    word_length_summary_one,
    max_len = max_len
  )

  mat <- dplyr::bind_rows(rows)
  mat <- as.matrix(mat)

  rownames(mat) <- docs[[doc_id_col]]

  mat
}


word_length_features <- build_word_length_features
