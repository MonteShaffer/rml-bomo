build_feature_sets <- function(section_docs, config, text_col = "text", doc_id_col = "doc_id"){
  docs <- docs_from_table(section_docs, text_col = text_col, doc_id_col = doc_id_col)
  feature_sets <- list()

  if("char" %in% config$include){
    feature_sets$char <- setNames(
      lapply(config$char_n, function(n) char_tfidf(docs, n)),
      paste0("char", config$char_n)
    )
  }

  if("word" %in% config$include){
    feature_sets$word <- setNames(
      lapply(config$word_n, function(n) ngram_tfidf(docs, n = n, stem = FALSE)),
      paste0("word", config$word_n)
    )
  }

  if("stem" %in% config$include){
    feature_sets$stem <- setNames(
      lapply(config$stem_n, function(n) ngram_tfidf(docs, n = n, stem = TRUE)),
      paste0("stem", config$stem_n)
    )
  }

  if("function_words" %in% config$include){
    all_sets <- default_function_word_sets()
    selected <- config$function_word_sets
    missing_sets <- setdiff(selected, names(all_sets))
    if(length(missing_sets) > 0){
      stop("Unknown function word sets: ", paste(missing_sets, collapse = ", "))
    }
    fw <- list()
    for(set_name in selected){
      fw[[paste0(set_name, "_freq")]] <- function_word_freq(docs, all_sets[[set_name]])
      if(isTRUE(config$use_tfidf_function_words)){
        fw[[paste0(set_name, "_tfidf")]] <- function_word_tfidf(docs, all_sets[[set_name]])
      }
    }
    feature_sets$function_words <- fw
  }

  if("pos" %in% config$include){
    feature_sets$pos <- pos_tfidf_all(
      docs,
      n_values = config$pos_n,
      model_file = config$udpipe_model_file
    )
  }

  if("punctuation" %in% config$include){
    feature_sets$punctuation <- list(punctuation_freq = punctuation_features(docs))
  }

  if("sentence" %in% config$include){
    feature_sets$sentence <- list(
      sentence_freq = sentence_length_bins(docs),
      sentence_tfidf = sentence_length_tfidf(docs)
    )
  }

  if("lexical" %in% config$include){
    feature_sets$lexical <- list(lexical_richness = lexical_features(docs))
  }

  if("readability" %in% config$include){
    read_mat <- readability_features(docs)
    if(!is.null(read_mat)){
      feature_sets$readability <- list(readability = read_mat)
    }
  }

  feature_sets
}

compute_variant_similarity <- function(feature_sets, similarity = "cosine"){
  if(similarity != "cosine") stop("Currently only cosine similarity is implemented.")
  lapply(feature_sets, function(class_group){
    lapply(class_group, cosine_sim)
  })
}

average_matrices <- function(mats){
  if(length(mats) == 1) return(mats[[1]])
  Reduce("+", mats) / length(mats)
}

compute_class_similarity <- function(variant_similarity){
  lapply(variant_similarity, average_matrices)
}

compute_overall_similarity <- function(class_similarity, weights){
  active_weights <- weights[names(class_similarity)]
  active_weights <- active_weights / sum(active_weights)
  Reduce(
    "+",
    Map(function(mat, w) mat * w, class_similarity, active_weights)
  )
}

nearest_neighbors <- function(similarity_matrix, k = 5){
  docs <- rownames(similarity_matrix)
  rows <- lapply(docs, function(doc){
    scores <- similarity_matrix[doc, ]
    scores <- scores[names(scores) != doc]
    scores <- sort(scores, decreasing = TRUE)
    top <- head(scores, k)
    data.frame(
      doc_id = doc,
      neighbor_doc_id = names(top),
      similarity = as.numeric(top),
      rank = seq_along(top),
      row.names = NULL
    )
  })
  dplyr::bind_rows(rows)
}

run_stylometry_ensemble <- function(section_docs, config = default_config(), text_col = "text", doc_id_col = "doc_id", k_neighbors = 5){
  feature_sets <- build_feature_sets(section_docs, config, text_col = text_col, doc_id_col = doc_id_col)
  variant_similarity <- compute_variant_similarity(feature_sets, similarity = config$similarity)
  class_similarity <- compute_class_similarity(variant_similarity)
  weights <- config$weights[names(class_similarity)]
  weights <- weights / sum(weights)
  overall_similarity <- compute_overall_similarity(class_similarity, weights)
  nn <- nearest_neighbors(overall_similarity, k = k_neighbors)

  structure(
    list(
      config = config,
      feature_sets = feature_sets,
      variant_similarity = variant_similarity,
      class_similarity = class_similarity,
      overall_similarity = overall_similarity,
      nearest_neighbors = nn,
      weights_used = weights
    ),
    class = "stylometry_ensemble_result"
  )
}
