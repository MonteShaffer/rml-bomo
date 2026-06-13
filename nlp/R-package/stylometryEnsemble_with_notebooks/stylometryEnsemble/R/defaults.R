default_function_word_sets <- function(){
  list(
    basic = c(
      "the","and","of","to","in","that","which","for","with","by",
      "on","at","from","as","if","or","but","not","is","are",
      "was","were","be","been","being"
    ),
    kjv = c(
      "the","and","of","to","in","that","which","shall","unto",
      "ye","thou","thee","thy","thine","hath","hast","had",
      "doth","didst","saith","yea","nay","verily","for","with","by","from"
    ),
    pronouns = c(
      "i","me","my","mine","thou","thee","thy","thine",
      "he","him","his","she","her","hers","we","us","our","ours",
      "ye","you","your","yours","they","them","their","theirs"
    ),
    conjunctions = c(
      "and","or","but","nor","for","yet","so","if","because","therefore",
      "though","although","wherefore","whereas"
    ),
    prepositions = c(
      "of","to","in","unto","by","with","from","for","on","upon","at",
      "into","through","over","under","before","after","against","among","between"
    ),
    auxiliaries = c(
      "shall","will","should","would","may","might","can","could","must",
      "is","are","was","were","be","been","being","hath","has","had",
      "doth","does","did"
    )
  )
}

new_stylometry_config <- function(
    include = c("char", "word", "stem", "function_words", "pos", "punctuation", "sentence", "lexical", "readability"),
    weights = NULL,
    char_n = c(3,4,5),
    word_n = c(1,2,3),
    stem_n = c(1,2,3),
    pos_n = c(3,4,5),
    function_word_sets = c("basic", "kjv", "pronouns", "conjunctions", "prepositions", "auxiliaries"),
    use_tfidf_function_words = TRUE,
    similarity = "cosine",
    udpipe_model_file = NULL
){
  if(is.null(weights)){
    weights <- c(
      char = 0.25,
      function_words = 0.20,
      pos = 0.15,
      word = 0.10,
      stem = 0.10,
      sentence = 0.07,
      punctuation = 0.05,
      lexical = 0.04,
      readability = 0.04
    )
  }
  missing_weights <- setdiff(include, names(weights))
  if(length(missing_weights) > 0){
    stop("Missing weights for: ", paste(missing_weights, collapse = ", "))
  }
  weights <- weights[include]
  weights <- weights / sum(weights)
  structure(
    list(
      include = include,
      weights = weights,
      char_n = char_n,
      word_n = word_n,
      stem_n = stem_n,
      pos_n = pos_n,
      function_word_sets = function_word_sets,
      use_tfidf_function_words = use_tfidf_function_words,
      similarity = similarity,
      udpipe_model_file = udpipe_model_file
    ),
    class = "stylometry_config"
  )
}

default_config <- function(...){
  new_stylometry_config(...)
}
