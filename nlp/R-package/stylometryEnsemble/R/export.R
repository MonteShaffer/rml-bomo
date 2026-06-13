write_matrix_csv <- function(mat, path){
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(mat)
  df <- tibble::rownames_to_column(df, var = "doc_id")
  readr::write_csv(df, path)
}

export_ensemble_results <- function(result, output_dir = "output"){
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  for(class_name in names(result$feature_sets)){
    for(var_name in names(result$feature_sets[[class_name]])){
      write_matrix_csv(
        result$feature_sets[[class_name]][[var_name]],
        file.path(output_dir, "feature_matrices", paste0(class_name, "__", var_name, ".csv"))
      )
    }
  }
  for(class_name in names(result$class_similarity)){
    write_matrix_csv(
      result$class_similarity[[class_name]],
      file.path(output_dir, "similarity_matrices", paste0("class__", class_name, ".csv"))
    )
  }
  write_matrix_csv(result$overall_similarity, file.path(output_dir, "similarity_matrices", "overall_similarity.csv"))
  readr::write_csv(result$nearest_neighbors, file.path(output_dir, "nearest_neighbors", "nearest_neighbors.csv"))
  readr::write_csv(data.frame(class = names(result$weights_used), weight = as.numeric(result$weights_used)), file.path(output_dir, "summaries", "weights_used.csv"))
}
