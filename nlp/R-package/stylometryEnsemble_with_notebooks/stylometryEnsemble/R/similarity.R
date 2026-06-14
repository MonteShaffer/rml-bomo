cosine_sim <- function(mat){
  mat <- as.matrix(mat)
  norm <- sqrt(rowSums(mat^2, na.rm = TRUE))
  norm[norm == 0 | is.na(norm)] <- NA_real_
  mat2 <- mat / norm
  sim <- mat2 %*% t(mat2)
  sim[is.na(sim)] <- 0
  diag(sim) <- 1
  sim
}

top_features <- function(mat, doc, k = 10){
  if(!doc %in% rownames(mat)){
    stop("Document '", doc, "' not found. Available docs: ", paste(rownames(mat), collapse = ", "))
  }
  sort(mat[doc, ], decreasing = TRUE)[1:min(k, ncol(mat))]
}

tfidf_from_grams <- function(grams_by_doc){
  vocab <- sort(unique(unlist(grams_by_doc)))
  if(length(vocab) == 0){
    stop("No features found. Check input documents.")
  }
  tf <- matrix(0, nrow = length(grams_by_doc), ncol = length(vocab), dimnames = list(names(grams_by_doc), vocab))
  for(i in seq_along(grams_by_doc)){
    tab <- table(grams_by_doc[[i]])
    if(length(tab) > 0) tf[i, names(tab)] <- as.numeric(tab)
  }
  rs <- rowSums(tf)
  rs[rs == 0] <- NA_real_
  tf <- tf / rs
  tf[is.na(tf)] <- 0
  df <- colSums(tf > 0)
  N <- nrow(tf)
  idf <- log((N + 1) / (df + 1)) + 1
  sweep(tf, 2, idf, "*")
}


cosine_between <- function(source_mat, target_mat){

  source_norm <- sqrt(rowSums(source_mat^2))
  target_norm <- sqrt(rowSums(target_mat^2))

  source_norm[source_norm == 0] <- 1
  target_norm[target_norm == 0] <- 1

  source_scaled <- source_mat / source_norm
  target_scaled <- target_mat / target_norm

  source_scaled %*% t(target_scaled)
}


nearest_targets <- function(sim, k = 5){

  source_ids <- rownames(sim)

  out <- lapply(source_ids, function(id){

    vals <- sim[id, ]

    if(id %in% names(vals)){
      vals[id] <- NA
    }

    vals <- sort(vals, decreasing = TRUE, na.last = NA)
    vals <- head(vals, k)

    data.frame(
      doc_id = id,
      neighbor_doc_id = names(vals),
      similarity = as.numeric(vals),
      rank = seq_along(vals),
      row.names = NULL
    )
  })

  dplyr::bind_rows(out)
}




