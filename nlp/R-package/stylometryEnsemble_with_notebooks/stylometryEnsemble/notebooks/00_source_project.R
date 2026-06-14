# Helper loaded by notebooks.
# Run this from inside the notebooks/ folder.

STYLO_ROOT <- normalizePath("..")
options(stylometry.root = STYLO_ROOT)

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
source(file.path(STYLO_ROOT, "R", "features_readability.R"))
source(file.path(STYLO_ROOT, "R", "ensemble.R"))
source(file.path(STYLO_ROOT, "R", "display_results.R"))
source(file.path(STYLO_ROOT, "R", "export.R"))
