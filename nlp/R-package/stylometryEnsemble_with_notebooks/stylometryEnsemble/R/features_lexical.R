# features_lexical.R
# Lexical richness features for stylometryEnsemble

tokenize_lexical_words <- function(text){

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


lexical_richness_one <- function(text){

  tokens <- tokenize_lexical_words(text)

  n_tokens <- length(tokens)

  if(n_tokens == 0){

    return(
      data.frame(
        n_tokens = 0,
        n_types = 0,
        type_token_ratio = 0,
        hapax_ratio = 0,
        avg_word_length = 0,
        vocab_entropy = 0,
        yules_k = 0,
        honore_r = 0,
        simpson_concentration = 0,
        simpson_diversity = 0
      )
    )
  }

  freqs <- table(tokens)

  n_types <- length(freqs)
  n_hapax <- sum(freqs == 1)

  probs <- as.numeric(freqs) / sum(freqs)

  vocab_entropy <- -sum(
    probs * log2(probs),
    na.rm = TRUE
  )

  m1 <- n_tokens
  m2 <- sum(as.numeric(freqs)^2)

  yules_k <- 10000 * (m2 - m1) / (m1^2)

  honore_r <- ifelse(
    n_types > 0 && n_hapax < n_types,
    100 * log(n_tokens) / (1 - (n_hapax / n_types)),
    0
  )

  simpson_concentration <- ifelse(
    n_tokens > 1,
    sum(as.numeric(freqs) * (as.numeric(freqs) - 1)) /
      (n_tokens * (n_tokens - 1)),
    0
  )

  simpson_diversity <- 1 - simpson_concentration

  data.frame(
    n_tokens = n_tokens,
    n_types = n_types,
    type_token_ratio = n_types / n_tokens,
    hapax_ratio = n_hapax / n_tokens,
    avg_word_length = mean(nchar(tokens)),
    vocab_entropy = vocab_entropy,
    yules_k = yules_k,
    honore_r = honore_r,
    simpson_concentration = simpson_concentration,
    simpson_diversity = simpson_diversity
  )
}


build_lexical_features <- function(
    docs,
    text_col = "text",
    doc_id_col = "doc_id"){

  if(is.character(docs)){

    doc_ids <- names(docs)

    if(is.null(doc_ids)){
      doc_ids <- paste0("doc_", seq_along(docs))
    }

    rows <- lapply(
      docs,
      lexical_richness_one
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
    lexical_richness_one
  )

  mat <- dplyr::bind_rows(rows)
  mat <- as.matrix(mat)

  rownames(mat) <- docs[[doc_id_col]]

  mat
}


lexical_features <- build_lexical_features
