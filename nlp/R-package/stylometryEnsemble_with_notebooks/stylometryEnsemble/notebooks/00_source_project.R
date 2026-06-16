# Helper loaded by notebooks.
# Run this from inside the notebooks/ folder.

# ------------------------------------------------------------
# Project root
# ------------------------------------------------------------

STYLO_ROOT <- normalizePath("..")

options(
  stylometry.root = STYLO_ROOT
)


# ------------------------------------------------------------
# Cache location
# ------------------------------------------------------------

# Default:
# STYLO_CACHE <- file.path(STYLO_ROOT, "cache")

# Alternative drive:
STYLO_CACHE <- "C:/-git-/stylometry_cache"

STYLO_CACHE <- normalizePath(
  STYLO_CACHE,
  winslash = "/",
  mustWork = FALSE
)

dir.create(
  STYLO_CACHE,
  recursive = TRUE,
  showWarnings = FALSE
)

options(
  stylometry.cache = STYLO_CACHE
)


message(
  "STYLO_ROOT  = ",
  STYLO_ROOT
)

message(
  "STYLO_CACHE = ",
  STYLO_CACHE
)






source(file.path(STYLO_ROOT, "R", "00_dependencies.R"))
source(file.path(STYLO_ROOT, "R", "defaults.R"))
source(file.path(STYLO_ROOT, "R", "prepare_data.R"))
source(file.path(STYLO_ROOT, "R", "tokenizer.R"))
source(file.path(STYLO_ROOT, "R", "similarity.R"))
source(file.path(STYLO_ROOT, "R", "features_char.R"))
source(file.path(STYLO_ROOT, "R", "features_word.R"))
source(file.path(STYLO_ROOT, "R", "features_function_words.R"))
source(file.path(STYLO_ROOT, "R", "features_pos.R"))
source(file.path(STYLO_ROOT, "R", "features_punctuation.R"))
source(file.path(STYLO_ROOT, "R", "features_sentence.R"))
source(file.path(STYLO_ROOT, "R", "features_lexical.R"))
source(file.path(STYLO_ROOT, "R", "features_word_length.R"))
source(file.path(STYLO_ROOT, "R", "features_readability.R"))
source(file.path(STYLO_ROOT, "R", "ensemble.R"))
source(file.path(STYLO_ROOT, "R", "display_results.R"))
source(file.path(STYLO_ROOT, "R", "cache_pos_annotations.R"))
source(file.path(STYLO_ROOT, "R", "export.R"))
