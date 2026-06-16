# ------------------------------------------------------------
# POS annotation cache
# ------------------------------------------------------------

get_stylo_cache <- function(){

  cache <- getOption(
    "stylometry.cache",
    default = file.path(
      getOption("stylometry.root", "."),
      "cache"
    )
  )

  dir.create(
    cache,
    recursive = TRUE,
    showWarnings = FALSE
  )

  cache
}


get_pos_cache_dir <- function(){

  cache_dir <- file.path(
    get_stylo_cache(),
    "pos_annotations"
  )

  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  cache_dir
}


digest_text_pos <- function(
    text,
    doc_id,
    model_id = "udpipe",
    extra = NULL
){

  digest::digest(
    list(
      doc_id = doc_id,
      text = text,
      model_id = model_id,
      extra = extra
    ),
    algo = "xxhash64"
  )
}


read_pos_cache <- function(cache_file){

  if(file.exists(cache_file)){
    readRDS(cache_file)
  } else {
    NULL
  }
}


write_pos_cache <- function(x, cache_file){

  dir.create(
    dirname(cache_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  saveRDS(
    x,
    cache_file
  )

  invisible(cache_file)
}


annotate_doc_pos_cached <- function(
    text,
    doc_id,
    ud_model,
    model_id = NULL,
    force = FALSE
){

  if(is.null(model_id)){

    model_id <- paste(
      class(ud_model),
      collapse = "_"
    )
  }

  cache_key <- digest_text_pos(
    text = text,
    doc_id = doc_id,
    model_id = model_id
  )

  cache_file <- file.path(
    get_pos_cache_dir(),
    paste0(
      doc_id,
      "__",
      cache_key,
      ".rds"
    )
  )

  if(!force){

    cached <- read_pos_cache(cache_file)

    if(!is.null(cached)){
      return(cached)
    }
  }

  ann <- udpipe::udpipe_annotate(
    ud_model,
    x = text,
    doc_id = doc_id
  )

  ann <- as.data.frame(ann)

  write_pos_cache(
    ann,
    cache_file
  )

  ann
}


annotate_docs_pos_cached <- function(
    docs,
    text_col = "text",
    doc_id_col = "doc_id",
    ud_model,
    model_id = "english-ewt",
    force = FALSE
){

  if(is.character(docs)){

    if(is.null(names(docs))){
      stop("If docs is a character vector, it must be named with doc_id values.")
    }

    docs_df <- tibble::tibble(
      doc_id = names(docs),
      text = as.character(docs)
    )

  } else {

    docs_df <- docs

    if(!(text_col %in% names(docs_df))){
      stop("Missing text column: ", text_col)
    }

    if(!(doc_id_col %in% names(docs_df))){
      stop("Missing doc_id column: ", doc_id_col)
    }

    docs_df <- docs_df %>%
      dplyr::transmute(
        doc_id = .data[[doc_id_col]],
        text = .data[[text_col]]
      )
  }

  out <- lapply(
    seq_len(nrow(docs_df)),
    function(i){

      annotate_doc_pos_cached(
        text = docs_df$text[i],
        doc_id = docs_df$doc_id[i],
        ud_model = ud_model,
        model_id = model_id,
        force = force
      )
    }
  )

  dplyr::bind_rows(out)
}