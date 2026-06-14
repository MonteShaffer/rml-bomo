# display_results.R
# Helper functions for inspecting stylometryEnsemble result objects.
#
# Expected result fields:
# result$overall_similarity
# result$nearest_targets
# result$nearest_neighbors
# result$feature_votes
# result$feature_rankings
# result$class_similarity
# result$variant_similarity
# result$weights_used
# result$source_docs
# result$target_docs


.result_nearest_table <- function(result){

  if("nearest_targets" %in% names(result)){
    return(result$nearest_targets)
  }

  if("nearest_neighbors" %in% names(result)){
    return(result$nearest_neighbors)
  }

  stop("Result does not contain nearest_targets or nearest_neighbors.")
}


show_nearest_targets <- function(result, doc_id = NULL, k = NULL){

  tbl <- .result_nearest_table(result)

  if(!is.null(doc_id)){
    tbl <- tbl %>% dplyr::filter(.data$doc_id == !!doc_id)
  }

  if(!is.null(k)){
    tbl <- tbl %>% dplyr::filter(.data$rank <= !!k)
  }

  tbl %>% dplyr::arrange(.data$doc_id, .data$rank)
}


show_nearest_with_metadata <- function(result, doc_id = NULL, k = NULL){

  tbl <- show_nearest_targets(result, doc_id = doc_id, k = k)

  source_meta <- result$source_docs %>%
    dplyr::select(
      doc_id,
      dplyr::any_of(c("book", "chapter", "section", "macro_genre", "n_verses"))
    )

  target_meta <- result$target_docs %>%
    dplyr::select(
      neighbor_doc_id = doc_id,
      dplyr::any_of(c("book", "chapter", "section", "macro_genre", "n_verses", "n_chapters", "books"))
    ) %>%
    dplyr::rename_with(~ paste0("neighbor_", .x), -neighbor_doc_id)

  tbl %>%
    dplyr::left_join(source_meta, by = "doc_id") %>%
    dplyr::left_join(target_meta, by = "neighbor_doc_id") %>%
    dplyr::arrange(.data$doc_id, .data$rank)
}


show_feature_votes <- function(result, doc_id = NULL, k = NULL){

  if(!"feature_votes" %in% names(result)){
    stop("Result does not contain feature_votes.")
  }

  tbl <- result$feature_votes

  if(!is.null(doc_id)){
    tbl <- tbl %>% dplyr::filter(.data$doc_id == !!doc_id)
  }

  if(!is.null(k)){
    tbl <- tbl %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::slice_head(n = k) %>%
      dplyr::ungroup()
  }

  tbl %>%
    dplyr::arrange(
      .data$doc_id,
      dplyr::desc(.data$votes),
      dplyr::desc(.data$avg_similarity)
    )
}


show_feature_votes_with_metadata <- function(result, doc_id = NULL, k = NULL){

  tbl <- show_feature_votes(result, doc_id = doc_id, k = k)

  target_meta <- result$target_docs %>%
    dplyr::select(
      neighbor_doc_id = doc_id,
      dplyr::any_of(c("book", "chapter", "section", "macro_genre", "n_verses", "n_chapters", "books"))
    ) %>%
    dplyr::rename_with(~ paste0("neighbor_", .x), -neighbor_doc_id)

  tbl %>% dplyr::left_join(target_meta, by = "neighbor_doc_id")
}


show_feature_rankings <- function(
    result,
    doc_id = NULL,
    rank_max = NULL,
    feature_class = NULL,
    feature_variant = NULL){

  if(!"feature_rankings" %in% names(result)){
    stop("Result does not contain feature_rankings.")
  }

  tbl <- result$feature_rankings

  if(!is.null(doc_id)){
    tbl <- tbl %>% dplyr::filter(.data$doc_id == !!doc_id)
  }

  if(!is.null(rank_max)){
    tbl <- tbl %>% dplyr::filter(.data$rank <= !!rank_max)
  }

  if(!is.null(feature_class)){
    tbl <- tbl %>% dplyr::filter(.data$feature_class %in% !!feature_class)
  }

  if(!is.null(feature_variant)){
    tbl <- tbl %>% dplyr::filter(.data$feature_variant %in% !!feature_variant)
  }

  tbl %>%
    dplyr::arrange(
      .data$doc_id,
      .data$feature_class,
      .data$feature_variant,
      .data$rank
    )
}


show_top_feature_rankings <- function(result, doc_id = NULL){

  show_feature_rankings(result, doc_id = doc_id, rank_max = 1) %>%
    dplyr::select(
      feature_class,
      feature_variant,
      doc_id,
      neighbor_doc_id,
      similarity,
      rank
    )
}


show_feature_winners <- function(result, doc_id = NULL){

  show_top_feature_rankings(result, doc_id = doc_id) %>%
    dplyr::count(
      .data$doc_id,
      .data$neighbor_doc_id,
      name = "wins"
    ) %>%
    dplyr::arrange(
      .data$doc_id,
      dplyr::desc(.data$wins)
    )
}


show_overall_similarity <- function(result, docs = NULL, targets = NULL, digits = 3){

  sim <- result$overall_similarity

  if(!is.null(docs)){
    sim <- sim[intersect(docs, rownames(sim)), , drop = FALSE]
  }

  if(!is.null(targets)){
    sim <- sim[, intersect(targets, colnames(sim)), drop = FALSE]
  }

  round(sim, digits)
}


show_class_similarity <- function(result, class_name, docs = NULL, targets = NULL, digits = 3){

  if(!class_name %in% names(result$class_similarity)){
    stop(
      "Class not found. Available classes: ",
      paste(names(result$class_similarity), collapse = ", ")
    )
  }

  sim <- result$class_similarity[[class_name]]

  if(!is.null(docs)){
    sim <- sim[intersect(docs, rownames(sim)), , drop = FALSE]
  }

  if(!is.null(targets)){
    sim <- sim[, intersect(targets, colnames(sim)), drop = FALSE]
  }

  round(sim, digits)
}


show_variant_similarity <- function(result, class_name, variant_name, docs = NULL, targets = NULL, digits = 3){

  if(!class_name %in% names(result$variant_similarity)){
    stop(
      "Class not found. Available classes: ",
      paste(names(result$variant_similarity), collapse = ", ")
    )
  }

  if(!variant_name %in% names(result$variant_similarity[[class_name]])){
    stop(
      "Variant not found. Available variants for ",
      class_name,
      ": ",
      paste(names(result$variant_similarity[[class_name]]), collapse = ", ")
    )
  }

  sim <- result$variant_similarity[[class_name]][[variant_name]]

  if(!is.null(docs)){
    sim <- sim[intersect(docs, rownames(sim)), , drop = FALSE]
  }

  if(!is.null(targets)){
    sim <- sim[, intersect(targets, colnames(sim)), drop = FALSE]
  }

  round(sim, digits)
}


show_weights <- function(result){

  data.frame(
    feature_class = names(result$weights_used),
    weight = as.numeric(result$weights_used),
    row.names = NULL
  ) %>%
    dplyr::arrange(dplyr::desc(.data$weight))
}


show_result_summary <- function(result){

  data.frame(
    item = c(
      "n_source_docs",
      "n_target_docs",
      "n_feature_classes",
      "n_variant_similarity_matrices",
      "cache_hash",
      "cache_file"
    ),
    value = c(
      nrow(result$source_docs),
      nrow(result$target_docs),
      length(result$class_similarity),
      sum(vapply(result$variant_similarity, length, integer(1))),
      ifelse("cache_hash" %in% names(result), result$cache_hash, NA),
      ifelse("cache_file" %in% names(result), result$cache_file, NA)
    ),
    row.names = NULL
  )
}


explain_doc <- function(result, doc_id, k = 10){

  list(
    nearest_targets =
      show_nearest_with_metadata(result, doc_id = doc_id, k = k),

    feature_votes =
      show_feature_votes_with_metadata(result, doc_id = doc_id, k = k),

    top_feature_rankings =
      show_top_feature_rankings(result, doc_id = doc_id),

    all_feature_rankings =
      show_feature_rankings(result, doc_id = doc_id, rank_max = k),

    overall_similarity =
      show_overall_similarity(result, docs = doc_id)
  )
}


explain_isaiah_chapter <- function(result, chapter, k = 10){

  doc_id <- paste("Isaiah", chapter, sep = "_")

  explain_doc(result, doc_id = doc_id, k = k)
}


show_isaiah_section <- function(
    result,
    section){

  show_feature_votes(
    result
  ) %>%

  dplyr::left_join(

    result$source_docs %>%

      dplyr::select(
        doc_id,
        section
      ),

    by="doc_id"

  ) %>%

  dplyr::filter(
    section == !!section
  )

}



show_consensus <- function(result, doc_id = NULL){

  if(!"feature_votes" %in% names(result)){
    stop("Result does not contain feature_votes.")
  }

  tbl <- result$feature_votes

  if(!is.null(doc_id)){
    tbl <- tbl %>%
      dplyr::filter(.data$doc_id == !!doc_id)
  }

  tbl <- tbl %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(
      dplyr::desc(.data$votes),
      dplyr::desc(.data$weighted_votes),
      dplyr::desc(.data$avg_similarity),
      .by_group = TRUE
    ) %>%
    dplyr::mutate(
      pct_votes =
        .data$votes /
        sum(.data$votes, na.rm = TRUE),

      weighted_pct_votes =
        .data$weighted_votes /
        sum(.data$weighted_votes, na.rm = TRUE),

      vote_margin =
        .data$pct_votes -
        dplyr::lead(
          .data$pct_votes,
          default = 0
        ),

      weighted_vote_margin =
        .data$weighted_pct_votes -
        dplyr::lead(
          .data$weighted_pct_votes,
          default = 0
        )
    ) %>%
    dplyr::ungroup()

  summary_attr <- tbl %>%
  dplyr::group_by(.data$doc_id) %>%
  dplyr::summarize(
    winner = dplyr::first(.data$neighbor_doc_id),

    winner_pct_votes = dplyr::first(.data$pct_votes),

    winner_weighted_pct_votes =
      dplyr::first(.data$weighted_pct_votes),

    vote_margin =
      dplyr::first(.data$vote_margin),

    weighted_vote_margin =
      dplyr::first(.data$weighted_vote_margin),

    second_pct_votes =
      dplyr::nth(.data$pct_votes, 2, default = NA_real_),

    winner_dominance_ratio =
      dplyr::first(.data$pct_votes) /
      dplyr::nth(.data$pct_votes, 2, default = NA_real_),

    doc_entropy =
      -sum(
        .data$pct_votes * log2(.data$pct_votes),
        na.rm = TRUE
      ),

    effective_targets =
      2 ^ doc_entropy,

    consensus_strength =
      dplyr::first(.data$pct_votes) /
      (1 / effective_targets),

    elbow_rank =
      which.max(.data$vote_margin),

    n_targets =
      dplyr::n(),

    .groups = "drop"
  )

  attr(tbl, "summary") <- summary_attr

  tbl
}




consensus_summary <- function(x){

  attr(x, "summary")

}

show_consensus_summary <- function(x){

  s <- attr(x, "summary")

  if(is.null(s)){
    stop("Run show_consensus() first.")
  }

  s <- s[1, ]

  cat(sprintf("%-28s : %s\n", "Document", s$doc_id))
  cat(sprintf("%-28s : %s\n\n", "Consensus Winner", s$winner))

  cat(sprintf("%-28s : %.1f%%\n", "Vote Share", 100 * s$winner_pct_votes))
  cat(sprintf("%-28s : %.1f%%\n\n", "Weighted Vote Share", 100 * s$winner_weighted_pct_votes))

  cat(sprintf("%-28s : %.1f%%\n", "Vote Margin", 100 * s$vote_margin))
  cat(sprintf("%-28s : %.1f%%\n\n", "Weighted Vote Margin", 100 * s$weighted_vote_margin))

  cat(sprintf("%-28s : %.2fx\n", "Winner Dominance Ratio", s$winner_dominance_ratio))
  cat(sprintf("%-28s : %.2fx\n", "Consensus Strength", s$consensus_strength))
  cat(sprintf("%-28s : %s\n\n", "Elbow Rank", s$elbow_rank))

  cat(sprintf("%-28s : %.2f bits\n", "Entropy", s$doc_entropy))
  cat(sprintf("%-28s : %.2f\n", "Effective Targets", s$effective_targets))
  cat(sprintf("%-28s : %d\n", "Targets Receiving Votes", s$n_targets))

  invisible(s)
}



plot_consensus_scree <- function(
    x,
    title = NULL){

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Package 'ggplot2' is required.")
  }

  tbl <- x

  s <- attr(tbl, "summary")

  if(is.null(s)){
    stop("Run show_consensus() first.")
  }

  if(is.null(title)){
    title <- paste0(
      "Consensus scree plot: ",
      unique(tbl$doc_id)
    )
  }

  tbl <- tbl %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      label = paste0(
        .data$rank,
        ". ",
        .data$neighbor_doc_id
      )
    )

  ggplot2::ggplot(
    tbl,
    ggplot2::aes(
      x = reorder(.data$label, -.data$pct_votes),
      y = .data$pct_votes
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_line(
      ggplot2::aes(
        group = 1
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Winner: ", s$winner,
        " | Margin: ", round(100 * s$vote_margin, 1), "%",
        " | Dominance: ", round(s$winner_dominance_ratio, 2), "x"
      ),
      x = "Ranked target",
      y = "Vote share"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )
}





summarize_findings <- function(result, doc_id, k = 10, plot = TRUE){

  .safe_print <- function(x){
    print(
      x,
      width = max(
        getOption("width", 120),
        120
      )
    )
  }

  cat("\n")
  cat("============================================================\n")
  cat("Stylometry ensemble findings\n")
  cat("============================================================\n\n")

  cat("Document:", doc_id, "\n\n")

  cons <- show_consensus(
    result,
    doc_id = doc_id
  )

  cat("------------------------------------------------------------\n")
  cat("1. Consensus summary\n")
  cat("------------------------------------------------------------\n\n")

  show_consensus_summary(cons)

  cat("\n")

  cat("------------------------------------------------------------\n")
  cat("2. Consensus vote table\n")
  cat("------------------------------------------------------------\n\n")

  .safe_print(
    cons %>%
      dplyr::select(
        doc_id,
        neighbor_doc_id,
        votes,
        weighted_votes,
        avg_similarity,
        pct_votes,
        weighted_pct_votes,
        vote_margin,
        weighted_vote_margin
      )
  )

  cat("\n")

  cat("------------------------------------------------------------\n")
  cat("3. Overall nearest targets\n")
  cat("------------------------------------------------------------\n\n")

  .safe_print(
    show_nearest_targets(
      result,
      doc_id = doc_id,
      k = k
    )
  )

  cat("\n")

  cat("------------------------------------------------------------\n")
  cat("4. Feature winners by feature variant\n")
  cat("------------------------------------------------------------\n\n")

  .safe_print(
    show_top_feature_rankings(
      result,
      doc_id = doc_id
    ) %>%
      dplyr::arrange(
        feature_class,
        feature_variant
      )
  )

  cat("\n")

  cat("------------------------------------------------------------\n")
  cat("5. Full feature rankings, top k by feature\n")
  cat("------------------------------------------------------------\n\n")

  .safe_print(
    show_feature_rankings(
      result,
      doc_id = doc_id,
      rank_max = k
    )
  )

  cat("\n")

  cat("------------------------------------------------------------\n")
  cat("6. Ensemble class weights\n")
  cat("------------------------------------------------------------\n\n")

  .safe_print(
    show_weights(result)
  )

  cat("\n")

  if(plot){

    cat("------------------------------------------------------------\n")
    cat("7. Consensus scree plot\n")
    cat("------------------------------------------------------------\n\n")

    print(
      plot_consensus_scree(cons)
    )
  }

  invisible(
    list(
      consensus = cons,
      consensus_summary = attr(cons, "summary"),
      nearest_targets =
        show_nearest_targets(
          result,
          doc_id = doc_id,
          k = k
        ),
      feature_winners =
        show_top_feature_rankings(
          result,
          doc_id = doc_id
        ),
      feature_rankings =
        show_feature_rankings(
          result,
          doc_id = doc_id,
          rank_max = k
        ),
      weights =
        show_weights(result)
    )
  )
}