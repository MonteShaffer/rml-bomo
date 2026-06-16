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
  
  if("word_length" %in% config$include){

  feature_sets$word_length <- list(
    word_length = build_word_length_features(
      docs,
      text_col = text_col,
      doc_id_col = doc_id_col
    )
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








run_stylometry_ensemble <- function(
    source_docs,
    target_docs = NULL,
    config = default_config(),
    text_col = "text",
    doc_id_col = "doc_id",
    k_neighbors = 5,
    use_cache = TRUE,
    cache_dir = NULL){

  if(is.null(target_docs)){
    target_docs <- source_docs
  }

if(is.null(cache_dir)){

  cache_dir <- file.path(

    getOption(
      "stylometry.cache",
      "."
    ),

    "ensemble"
  )

}


  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cache_hash <- make_ensemble_hash(
    source_docs,
    target_docs,
    config
  )

  cache_file <- file.path(
    cache_dir,
    paste0("ensemble_", cache_hash, ".rds")
  )
  
  print(cache_file);

  if(use_cache && file.exists(cache_file)){
    message("Loading cached ensemble: ", cache_file)
    return(readRDS(cache_file))
  }

  source_ids <- source_docs[[doc_id_col]]
  target_ids <- target_docs[[doc_id_col]]

  combined_docs <- dplyr::bind_rows(
    source_docs %>% dplyr::mutate(.role = "source"),
    target_docs %>% dplyr::mutate(.role = "target")
  )

  combined_docs <- combined_docs %>%
    dplyr::distinct(.data[[doc_id_col]], .keep_all = TRUE)

  feature_sets_combined <- build_feature_sets(
    combined_docs,
    config,
    text_col = text_col,
    doc_id_col = doc_id_col
  )

  variant_similarity <- lapply(
    feature_sets_combined,
    function(class_group){

      lapply(
        class_group,
        function(mat){

          source_mat <- mat[source_ids, , drop = FALSE]
          target_mat <- mat[target_ids, , drop = FALSE]

          cosine_between(source_mat, target_mat)
        }
      )
    }
  )

  class_similarity <- compute_class_similarity(variant_similarity)

  weights <- config$weights[names(class_similarity)]
  weights <- weights / sum(weights)

  overall_similarity <- compute_overall_similarity(
    class_similarity,
    weights
  )

  nn <- nearest_targets(
    overall_similarity,
    k = k_neighbors
  )
  
  feature_rankings <- feature_rankings_from_variants(
  variant_similarity,
  k = k_neighbors
)

feature_votes <- feature_votes_from_rankings(
  feature_rankings,
  weights = weights,
  rank_filter = 1
)


  
  

result <- structure(
  list(
    config = config,
    source_docs = source_docs,
    target_docs = target_docs,
    feature_sets = feature_sets_combined,
    variant_similarity = variant_similarity,
    class_similarity = class_similarity,
    overall_similarity = overall_similarity,

    nearest_targets = nn,
    nearest_neighbors = nn,

    feature_rankings = feature_rankings,
    feature_votes = feature_votes,

    weights_used = weights,
    cache_hash = cache_hash,
    cache_file = cache_file
  ),
  class = "stylometry_ensemble_result"
)
  
  
  if(use_cache){
  saveRDS(result, cache_file)
  message("Saved ensemble cache: ", cache_file)
}


result
}



make_ensemble_hash <- function(source_docs, target_docs, config){

  digest::digest(
    list(
      source_doc_ids = source_docs$doc_id,
      source_text    = source_docs$text,
      target_doc_ids = target_docs$doc_id,
      target_text    = target_docs$text,
      config         = config
    ),
    algo = "xxhash64"
  )
}



feature_rankings_from_variants <- function(
    variant_similarity,
    k = 5){

  out <- list()

  idx <- 1

  for(class_name in names(variant_similarity)){

    for(variant_name in names(variant_similarity[[class_name]])){

      sim <- variant_similarity[[class_name]][[variant_name]]

      ranks <- nearest_targets(
        sim,
        k = k
      )

      ranks$feature_class <- class_name
      ranks$feature_variant <- variant_name

      out[[idx]] <- ranks

      idx <- idx + 1
    }
  }

  dplyr::bind_rows(out) %>%
    dplyr::select(
      feature_class,
      feature_variant,
      doc_id,
      neighbor_doc_id,
      similarity,
      rank
    )
}




feature_votes_from_rankings <- function(feature_rankings, weights, rank_filter = 1){

  feature_rankings %>%
    dplyr::filter(rank <= rank_filter) %>%
    dplyr::mutate(
      class_weight = weights[feature_class]
    ) %>%
    dplyr::group_by(doc_id, neighbor_doc_id) %>%
    dplyr::summarize(
      votes = dplyr::n(),
      weighted_votes = sum(class_weight, na.rm = TRUE),
      avg_similarity = mean(similarity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      doc_id,
      dplyr::desc(weighted_votes),
      dplyr::desc(votes),
      dplyr::desc(avg_similarity)
    )
}




