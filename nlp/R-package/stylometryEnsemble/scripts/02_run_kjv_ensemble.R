source("R/00_dependencies.R")
source("R/defaults.R")
source("R/tokenizer.R")
source("R/similarity.R")
source("R/features_char.R")
source("R/features_word.R")
source("R/features_function_words.R")
source("R/features_pos.R")
source("R/features_punctuation.R")
source("R/features_sentence.R")
source("R/features_lexical.R")
source("R/features_readability.R")
source("R/ensemble.R")
source("R/export.R")

prepared_path <- "data/prepared/kjv_section_docs.csv"
if(!file.exists(prepared_path)){
  stop("Missing data/prepared/kjv_section_docs.csv. Run scripts/01_prepare_kjv_data.R first.")
}

section_docs <- readr::read_csv(prepared_path, show_col_types = FALSE)

config <- default_config(
  include = c("char", "word", "stem", "function_words", "pos", "punctuation", "sentence", "lexical", "readability")
)

result <- run_stylometry_ensemble(section_docs, config = config, k_neighbors = 5)

print(round(result$overall_similarity, 3))
print(result$nearest_neighbors)

export_ensemble_results(result, output_dir = "output")
message("Results exported to output/.")
