# prepare_data.R
# Data preparation helpers for stylometryEnsemble
#
# Expected raw schema:
# corpus_id, translation, book, chapter, verse, text
#
# Main prepared outputs:
# 1. chapter_docs:
#    doc_id = book_chapter, e.g. Isaiah_40
#    section = interpretive grouping, e.g. Isaiah_40_55
#
# 2. section_docs:
#    doc_id = section, e.g. Isaiah_40_55
#    text = all text in that section/book combined


# -------------------------------------------------------------------------
# Default target books
# -------------------------------------------------------------------------

default_target_books <- function(){
  c(
    "Isaiah",
    "Jeremiah", "Ezekiel", "Hosea", "Amos",
    "1 Samuel", "2 Samuel", "1 Kings", "2 Kings",
    "Psalms", "Job", "Proverbs"
  )
}


# -------------------------------------------------------------------------
# Validate raw input
# -------------------------------------------------------------------------

validate_kjv_raw <- function(df){

  required <- c(
    "corpus_id",
    "translation",
    "book",
    "chapter",
    "verse",
    "text"
  )

  missing <- setdiff(required, names(df))

  if(length(missing) > 0){
    stop(
      "Missing required raw columns: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}


# -------------------------------------------------------------------------
# Assign sections and macro genres
# -------------------------------------------------------------------------

assign_kjv_sections <- function(df){

  validate_kjv_raw(df)

  df %>%
    dplyr::mutate(

      chapter = as.integer(chapter),
      verse   = as.integer(verse),

      section = dplyr::case_when(
        book == "Isaiah" & chapter <= 35 ~ "Isaiah_1_35",
        book == "Isaiah" & chapter <= 39 ~ "Isaiah_36_39",
        book == "Isaiah" & chapter <= 55 ~ "Isaiah_40_55",
        book == "Isaiah" & chapter <= 66 ~ "Isaiah_56_66",
        TRUE ~ book
      ),

      macro_genre = dplyr::case_when(
        book %in% c(
          "1 Samuel", "2 Samuel",
          "1 Kings", "2 Kings"
        ) ~ "Narrative",

        book %in% c(
          "Psalms", "Job", "Proverbs"
        ) ~ "Poetic/Wisdom",

        TRUE ~ "Prophetic"
      )
    )
}


# -------------------------------------------------------------------------
# Validate section-assigned data
# -------------------------------------------------------------------------

validate_assigned_docs <- function(df){

  required <- c(
    "corpus_id",
    "translation",
    "book",
    "chapter",
    "verse",
    "text",
    "section",
    "macro_genre"
  )

  missing <- setdiff(required, names(df))

  if(length(missing) > 0){
    stop(
      "Missing required assigned columns: ",
      paste(missing, collapse = ", "),
      "\nRun assign_kjv_sections() first."
    )
  }

  invisible(TRUE)
}


# -------------------------------------------------------------------------
# Prepare chapter-level documents
# -------------------------------------------------------------------------
#
# This is the primary stylometry unit.
#
# Example:
# doc_id     = Isaiah_40
# section    = Isaiah_40_55
# text       = all verses in Isaiah chapter 40 combined

prepare_chapter_docs <- function(df, target_books = NULL){

  validate_assigned_docs(df)

  if(!is.null(target_books)){
    df <- df %>%
      dplyr::filter(book %in% target_books)
  }

  df %>%
    dplyr::arrange(
      corpus_id,
      translation,
      book,
      chapter,
      verse
    ) %>%
    dplyr::group_by(
      corpus_id,
      translation,
      book,
      chapter,
      section,
      macro_genre
    ) %>%
    dplyr::summarize(
      text = stringr::str_c(text, collapse = " "),
      n_verses = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      doc_id = paste(book, chapter, sep = "_"),
      n_chapters = 1L,
      books = book
    ) %>%
    dplyr::select(
      corpus_id,
      translation,
      doc_id,
      book,
      chapter,
      section,
      macro_genre,
      n_verses,
      n_chapters,
      books,
      text
    )
}


# -------------------------------------------------------------------------
# Prepare section-level documents
# -------------------------------------------------------------------------
#
# This is the reference / aggregation unit.
#
# Example:
# doc_id     = Isaiah_40_55
# section    = Isaiah_40_55
# text       = all verses in Isaiah chapters 40-55 combined

prepare_section_docs <- function(df, target_books = NULL){

  validate_assigned_docs(df)

  if(!is.null(target_books)){
    df <- df %>%
      dplyr::filter(book %in% target_books)
  }

  df %>%
    dplyr::arrange(
      corpus_id,
      translation,
      book,
      chapter,
      verse
    ) %>%
    dplyr::group_by(
      corpus_id,
      translation,
      section,
      macro_genre
    ) %>%
    dplyr::summarize(
      text = stringr::str_c(text, collapse = " "),
      n_verses = dplyr::n(),
      n_chapters = dplyr::n_distinct(paste(book, chapter, sep = "_")),
      books = stringr::str_c(unique(book), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      doc_id = section
    ) %>%
    dplyr::select(
      corpus_id,
      translation,
      doc_id,
      section,
      macro_genre,
      n_verses,
      n_chapters,
      books,
      text
    )
}


# -------------------------------------------------------------------------
# KJV convenience wrappers
# -------------------------------------------------------------------------

prepare_kjv_chapters <- function(
    kjv_raw,
    target_books = default_target_books()){

  kjv_raw %>%
    assign_kjv_sections() %>%
    prepare_chapter_docs(
      target_books = target_books
    )
}


prepare_kjv_sections <- function(
    kjv_raw,
    target_books = default_target_books()){

  kjv_raw %>%
    assign_kjv_sections() %>%
    prepare_section_docs(
      target_books = target_books
    )
}


prepare_kjv_docs <- function(
    kjv_raw,
    target_books = default_target_books()){

  list(
    chapter_docs = prepare_kjv_chapters(
      kjv_raw,
      target_books = target_books
    ),

    section_docs = prepare_kjv_sections(
      kjv_raw,
      target_books = target_books
    )
  )
}


# -------------------------------------------------------------------------
# Optional helper: save prepared docs
# -------------------------------------------------------------------------

save_prepared_docs <- function(
    prepared,
    out_dir = "data/prepared"){

  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  if("chapter_docs" %in% names(prepared)){
    readr::write_csv(
      prepared$chapter_docs,
      file.path(out_dir, "kjv_chapter_docs.csv")
    )
  }

  if("section_docs" %in% names(prepared)){
    readr::write_csv(
      prepared$section_docs,
      file.path(out_dir, "kjv_section_docs.csv")
    )
  }

  invisible(TRUE)
}
