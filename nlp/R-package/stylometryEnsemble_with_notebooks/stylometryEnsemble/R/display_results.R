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









show_false_matches <- function(attribution_tbl){

  attribution_tbl %>%

    dplyr::filter(
      winner != actual_section
    ) %>%

    dplyr::select(

      chapter,

      doc_id,

      actual_section,

      winner,

      winner_pct_votes_pct,

      winner_weighted_pct_votes_pct,

      vote_margin_pct,

      weighted_vote_margin_pct,

      winner_dominance_ratio,

      consensus_strength,

      doc_entropy,

      effective_targets

    ) %>%

    dplyr::arrange(chapter)

}


plotPCA <- function(
    scores,
    variance_tbl,
    x = 1,
    y = 2,
    book = "Isaiah",
    include_books = NULL,
    color = "section",
    label = "chapter",
    show_labels = TRUE,
    show_centroids = TRUE,
    show_hulls = TRUE,
    show_other_books = TRUE,
    aggregate_other_books = FALSE,
    show_other_centroids = TRUE,
    other_alpha = 0.35,
    other_size = 2,
    point_size = 2.8,
    centroid_size = 6,
    other_centroid_size = 5,
    title = NULL
){

  require(dplyr)
  require(ggplot2)
  require(ggrepel)

  x_col <- paste0("PC", x)
  y_col <- paste0("PC", y)

  if(!(x_col %in% names(scores))){
    stop("Missing column: ", x_col)
  }

  if(!(y_col %in% names(scores))){
    stop("Missing column: ", y_col)
  }

  # ------------------------------------------------------------
  # Select focus book and optional comparison books
  # ------------------------------------------------------------

  if(!is.null(book)){

    plot_data <- scores %>%
      dplyr::filter(.data$book == !!book)

    if(!is.null(include_books)){

      other_data <- scores %>%
        dplyr::filter(.data$book %in% include_books)

      plot_data <- dplyr::bind_rows(
        plot_data,
        other_data
      )
    }

  } else {

    plot_data <- scores
  }

  # ------------------------------------------------------------
  # Display grouping
  # ------------------------------------------------------------

  plot_data <- plot_data %>%
    dplyr::mutate(
      .is_focus = dplyr::if_else(
        !is.null(book) & .data$book == !!book,
        TRUE,
        FALSE
      ),
      .display_group = dplyr::if_else(
        .is_focus,
        as.character(.data[[color]]),
        as.character(.data$book)
      ),
      .label_value = as.character(.data[[label]])
    )

  # ------------------------------------------------------------
  # Aggregate comparison books to centroids only
  # ------------------------------------------------------------

  if(aggregate_other_books && !is.null(include_books)){

    other_centroids <- plot_data %>%
      dplyr::filter(!.is_focus) %>%
      dplyr::group_by(book) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(c(x_col, y_col)),
          ~ mean(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        .is_focus = FALSE,
        .display_group = book,
        .label_value = book
      )

    # Preserve columns that may be expected downstream.
    missing_cols <- setdiff(names(plot_data), names(other_centroids))

    for(mc in missing_cols){
      other_centroids[[mc]] <- NA
    }

    other_centroids <- other_centroids %>%
      dplyr::select(dplyr::all_of(names(plot_data)))

    plot_data <- plot_data %>%
      dplyr::filter(.is_focus) %>%
      dplyr::bind_rows(other_centroids)
  }

  # ------------------------------------------------------------
  # Variance labels
  # ------------------------------------------------------------

  x_pct <- variance_tbl %>%
    dplyr::filter(.data$dimension == x) %>%
    dplyr::pull(.data$pct_variance)

  y_pct <- variance_tbl %>%
    dplyr::filter(.data$dimension == y) %>%
    dplyr::pull(.data$pct_variance)

  if(length(x_pct) == 0) x_pct <- NA_real_
  if(length(y_pct) == 0) y_pct <- NA_real_

  if(is.null(title)){

    if(!is.null(book) && is.null(include_books)){
      title <- paste0(book, ": PC", x, " vs PC", y)
    } else if(!is.null(book) && !is.null(include_books)){
      title <- paste0(
        book,
        " with comparison books: PC",
        x,
        " vs PC",
        y
      )
    } else {
      title <- paste0("PCA: PC", x, " vs PC", y)
    }
  }

  subtitle <- paste0(
    "PC",
    x,
    " = ",
    round(x_pct, 2),
    "% variance; PC",
    y,
    " = ",
    round(y_pct, 2),
    "% variance"
  )

  # ------------------------------------------------------------
  # Base plot
  # ------------------------------------------------------------

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data[[y_col]]
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "grey60",
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "grey60",
      linewidth = 0.3
    )

  # ------------------------------------------------------------
  # Hulls for focus book only
  # ------------------------------------------------------------

  if(show_hulls){

    hull_data <- plot_data %>%
      dplyr::filter(.is_focus) %>%
      dplyr::group_by(.display_group) %>%
      dplyr::filter(dplyr::n() >= 3) %>%
      dplyr::slice(chull(.data[[x_col]], .data[[y_col]])) %>%
      dplyr::ungroup()

    if(nrow(hull_data) > 0){

      p <- p +
        ggplot2::geom_polygon(
          data = hull_data,
          ggplot2::aes(
            fill = .display_group,
            group = .display_group
          ),
          alpha = 0.13,
          color = NA
        )
    }
  }

  # ------------------------------------------------------------
  # Other books
  # ------------------------------------------------------------

  if(show_other_books && any(!plot_data$.is_focus)){

    other_plot_data <- plot_data %>%
      dplyr::filter(!.is_focus)

    if(aggregate_other_books){

      p <- p +
        ggplot2::geom_point(
          data = other_plot_data,
          ggplot2::aes(color = .display_group),
          shape = 4,
          stroke = 1.4,
          size = other_centroid_size,
          alpha = 0.95
        )

    } else {

      p <- p +
        ggplot2::geom_point(
          data = other_plot_data,
          ggplot2::aes(color = .display_group),
          alpha = other_alpha,
          size = other_size
        )
    }
  }

  # ------------------------------------------------------------
  # Focus book points
  # ------------------------------------------------------------

  if(any(plot_data$.is_focus)){

    p <- p +
      ggplot2::geom_point(
        data = plot_data %>% dplyr::filter(.is_focus),
        ggplot2::aes(color = .display_group),
        size = point_size,
        alpha = 0.95
      )
  }

  # If book is NULL, draw all points normally.
  if(is.null(book)){

    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(color = .display_group),
        size = point_size,
        alpha = 0.9
      )
  }

  # ------------------------------------------------------------
  # Centroids for focus book
  # ------------------------------------------------------------

  if(show_centroids && any(plot_data$.is_focus)){

    centroid_data <- plot_data %>%
      dplyr::filter(.is_focus) %>%
      dplyr::group_by(.display_group) %>%
      dplyr::summarize(
        x_val = mean(.data[[x_col]], na.rm = TRUE),
        y_val = mean(.data[[y_col]], na.rm = TRUE),
        .groups = "drop"
      )

    if(nrow(centroid_data) > 0){

      p <- p +
        ggplot2::geom_point(
          data = centroid_data,
          ggplot2::aes(
            x = x_val,
            y = y_val,
            color = .display_group
          ),
          shape = 4,
          stroke = 1.4,
          size = centroid_size
        )
    }
  }

  # ------------------------------------------------------------
  # Labels
  # ------------------------------------------------------------

  if(show_labels){

    label_data <- plot_data

    p <- p +
      ggrepel::geom_text_repel(
        data = label_data,
        ggplot2::aes(
          label = .label_value,
          color = .display_group
        ),
        size = 2.7,
        max.overlaps = Inf,
        show.legend = FALSE
      )
  }

  # ------------------------------------------------------------
  # Final styling
  # ------------------------------------------------------------

  p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = paste0("PC", x, " (", round(x_pct, 2), "%)"),
      y = paste0("PC", y, " (", round(y_pct, 2), "%)"),
      color = "Group",
      fill = "Group"
    ) +
    ggplot2::theme_minimal()
}

plotPCA3d <- function(
    scores,
    variance_tbl,
    x = 1,
    y = 2,
    z = 3,
    color = "section",
    label = "chapter",
    book = NULL,
    section_filter = NULL,
    show_labels = TRUE,
    show_centroids = TRUE,
    point_size = 8,
    centroid_size = 12,
    label_adj = c(1.2, 1.2),
    title = NULL){

  if(!requireNamespace("rgl", quietly = TRUE)){
    stop("Package 'rgl' is required. Install with install.packages('rgl').")
  }

  plot_data <- scores

  if(!is.null(book) && "book" %in% names(plot_data)){
    plot_data <- plot_data %>%
      dplyr::filter(.data$book == !!book)
  }

  if(!is.null(section_filter) && "section" %in% names(plot_data)){
    plot_data <- plot_data %>%
      dplyr::filter(.data$section %in% !!section_filter)
  }

  xname <- paste0("PC", x)
  yname <- paste0("PC", y)
  zname <- paste0("PC", z)

  groups <- as.factor(plot_data[[color]])
  group_levels <- levels(groups)

  cols <- grDevices::rainbow(length(group_levels))
  point_cols <- cols[as.integer(groups)]

  xvar <- variance_tbl$pct_variance[variance_tbl$dimension == x]
  yvar <- variance_tbl$pct_variance[variance_tbl$dimension == y]
  zvar <- variance_tbl$pct_variance[variance_tbl$dimension == z]

  if(is.null(title)){
    title <- paste0(xname, " vs ", yname, " vs ", zname)
  }

  rgl::open3d()

  rgl::plot3d(
    x = plot_data[[xname]],
    y = plot_data[[yname]],
    z = plot_data[[zname]],
    col = point_cols,
    size = point_size,
    #type = "s",
    xlab = paste0(xname, " (", round(xvar, 1), "%)"),
    ylab = paste0(yname, " (", round(yvar, 1), "%)"),
    zlab = paste0(zname, " (", round(zvar, 1), "%)"),
    main = title
  )

  if(show_labels){

    rgl::texts3d(
      x = plot_data[[xname]],
      y = plot_data[[yname]],
      z = plot_data[[zname]],
      texts = plot_data[[label]],
      adj = label_adj
    )
  }

  if(show_centroids){

    centroids <- plot_data %>%
      dplyr::group_by(.data[[color]]) %>%
      dplyr::summarize(
        x = mean(.data[[xname]], na.rm = TRUE),
        y = mean(.data[[yname]], na.rm = TRUE),
        z = mean(.data[[zname]], na.rm = TRUE),
        .groups = "drop"
      )

    centroid_groups <- as.factor(centroids[[color]])
    centroid_cols <- cols[match(as.character(centroid_groups), group_levels)]

    rgl::points3d(
      x = centroids$x,
      y = centroids$y,
      z = centroids$z,
      col = centroid_cols,
      size = centroid_size
    )

    rgl::texts3d(
      x = centroids$x,
      y = centroids$y,
      z = centroids$z,
      texts = paste0(centroids[[color]], " centroid"),
      adj = c(1.2, 1.2)
    )
  }

  rgl::legend3d(
    "topright",
    legend = group_levels,
    pch = 16,
    col = cols,
    cex = 1
  )

  #invisible(plot_data)
  rgl::aspect3d(1, 1, 1)

  rgl::rglwidget()
}



section_overlap <- function(
    scores,
    dims = c("PC1","PC2","PC3","PC4")
){

    require(dplyr)

    dat <- scores %>%
        dplyr::filter(
            book == "Isaiah"
        )

    sections <- unique(dat$section)

    out <- list()

    k <- 1

    for(i in 1:(length(sections)-1)){

        for(j in (i+1):length(sections)){

            s1 <- sections[i]
            s2 <- sections[j]

            X1 <- dat %>%
                dplyr::filter(section == s1) %>%
                dplyr::select(dplyr::all_of(dims)) %>%
                as.matrix()

            X2 <- dat %>%
                dplyr::filter(section == s2) %>%
                dplyr::select(dplyr::all_of(dims)) %>%
                as.matrix()

            mu1 <- colMeans(X1)
            mu2 <- colMeans(X2)

            pooled_cov <- cov(rbind(X1,X2))

            md <- sqrt(
                t(mu1-mu2) %*%
                solve(pooled_cov) %*%
                (mu1-mu2)
            )

            euclid <- sqrt(
                sum(
                    (mu1-mu2)^2
                )
            )

            out[[k]] <- data.frame(

                section_a = s1,

                section_b = s2,

                euclidean_distance =
                    as.numeric(euclid),

                mahalanobis_distance =
                    as.numeric(md)

            )

            k <- k + 1

        }

    }

    dplyr::bind_rows(out) %>%

        dplyr::arrange(

            mahalanobis_distance

        )

}




plot_sentence_features_by_section <- function(
    chapter_docs,
    sentence_features,
    feature = "sentence_length_mean"
){

    require(dplyr)
    require(ggplot2)

    stopifnot(
        feature %in% colnames(sentence_features)
    )

    dat <- sentence_features %>%

        dplyr::left_join(

            chapter_docs %>%

                dplyr::select(

                    doc_id,
                    book,
                    chapter,
                    section

                ),

            by = "doc_id"

        ) %>%

        dplyr::filter(

            book == "Isaiah"

        )

    ggplot(

        dat,

        aes(

            x = chapter,

            y = .data[[feature]],

            color = section

        )

    ) +

    geom_line() +

    geom_point() +

    labs(

        title = paste(

            "Isaiah:",

            feature

        ),

        x = "Chapter",

        y = feature

    ) +

    theme_minimal()

}



show_sentence_summary <- function(
    chapter_docs,
    sentence_features
){

    sentence_features %>%

        left_join(

            chapter_docs %>%

                select(

                    doc_id,
                    book,
                    section

                ),

            by="doc_id"

        ) %>%

        filter(

            book=="Isaiah"

        ) %>%

        group_by(

            section

        ) %>%

        summarise(

            across(

                where(is.numeric),

                list(

                    mean=mean,

                    sd=sd

                ),

                na.rm=TRUE

            ),

            .groups="drop"

        )

}


plot_sentence_distributions <- function(

    chapter_docs,

    sentence_features,

    feature="sentence_length_mean"

){

    require(dplyr)

    require(ggplot2)

    dat <- sentence_features %>%

        left_join(

            chapter_docs %>%

                select(

                    doc_id,

                    book,

                    section

                ),

            by="doc_id"

        ) %>%

        filter(

            book=="Isaiah"

        )

    ggplot(

        dat,

        aes(

            x=.data[[feature]],

            fill=section

        )

    ) +

    geom_density(

        alpha=.35

    ) +

    labs(

        title=paste(

            "Distribution:",

            feature

        ),

        x=feature,

        y="Density"

    ) +

    theme_minimal()

}

