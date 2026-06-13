assign_kjv_sections <- function(df){
  df %>%
    mutate(
      section = case_when(
        book == "Isaiah" & chapter <= 35 ~ "Isaiah_1_35",
        book == "Isaiah" & chapter <= 39 ~ "Isaiah_36_39",
        book == "Isaiah" & chapter <= 55 ~ "Isaiah_40_55",
        book == "Isaiah" & chapter <= 66 ~ "Isaiah_56_66",
        TRUE ~ book
      ),
      macro_genre = case_when(
        book %in% c("1 Samuel", "2 Samuel", "1 Kings", "2 Kings") ~ "Narrative",
        book %in% c("Psalms", "Job", "Proverbs") ~ "Poetic/Wisdom",
        TRUE ~ "Prophetic"
      )
    )
}

prepare_section_docs <- function(df, target_books = NULL){
  required <- c("corpus_id", "translation", "book", "chapter", "verse", "text", "section", "macro_genre")
  missing <- setdiff(required, names(df))
  if(length(missing) > 0){
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  if(!is.null(target_books)){
    df <- df %>% filter(book %in% target_books)
  }
  df %>%
    group_by(corpus_id, translation, section, macro_genre) %>%
    summarize(
      text = str_c(text, collapse = " "),
      n_verses = n(),
      n_chapters = n_distinct(chapter),
      books = str_c(unique(book), collapse = ";"),
      .groups = "drop"
    ) %>%
    mutate(doc_id = paste(corpus_id, translation, section, sep = "__")) %>%
    select(corpus_id, translation, doc_id, section, macro_genre, n_verses, n_chapters, books, text)
}

prepare_chapter_docs <- function(df, target_books = NULL){
  required <- c("corpus_id", "translation", "book", "chapter", "verse", "text")
  missing <- setdiff(required, names(df))
  if(length(missing) > 0){
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  if(!is.null(target_books)){
    df <- df %>% filter(book %in% target_books)
  }
  df %>%
    group_by(corpus_id, translation, book, chapter) %>%
    summarize(
      text = str_c(text, collapse = " "),
      n_verses = n(),
      .groups = "drop"
    ) %>%
    mutate(
      section = paste(book, chapter, sep = "_"),
      macro_genre = case_when(
        book %in% c("1 Samuel", "2 Samuel", "1 Kings", "2 Kings") ~ "Narrative",
        book %in% c("Psalms", "Job", "Proverbs") ~ "Poetic/Wisdom",
        TRUE ~ "Prophetic"
      ),
      n_chapters = 1,
      books = book,
      doc_id = paste(corpus_id, translation, section, sep = "__")
    ) %>%
    select(corpus_id, translation, doc_id, section, macro_genre, n_verses, n_chapters, books, text)
}

prepare_kjv_sections <- function(kjv_raw, target_books = c(
  "Isaiah", "Jeremiah", "Ezekiel", "Hosea", "Amos",
  "1 Samuel", "2 Samuel", "1 Kings", "2 Kings",
  "Psalms", "Job", "Proverbs"
)){
  kjv_raw %>%
    assign_kjv_sections() %>%
    prepare_section_docs(target_books = target_books)
}
