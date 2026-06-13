# stylometryEnsemble

A reusable R codebase for stylometry experiments. It accepts verse/chapter/text data, prepares section-level documents, extracts multiple feature classes, computes cosine similarity, and aggregates weighted ensemble similarity.

## Expected raw data

Place your source file here:

```text
data/raw/kjv.csv
```

Expected header:

```text
corpus_id,translation,book,chapter,verse,text
```

Example row:

```text
KJV,KJV,Isaiah,1,1,"THE vision of Isaiah the son of Amoz..."
```

## Workflow

```r
source("R/00_dependencies.R")
source("R/prepare_data.R")

kjv_raw <- readr::read_csv("data/raw/kjv.csv", show_col_types = FALSE)
kjv_sections <- prepare_kjv_sections(kjv_raw)
readr::write_csv(kjv_sections, "data/prepared/kjv_section_docs.csv")
```

Then run:

```r
source("scripts/02_run_kjv_ensemble.R")
```

## Output

Results are written to:

```text
output/feature_matrices/
output/similarity_matrices/
output/nearest_neighbors/
output/summaries/
```

## Main feature classes

- Character n-grams: char3, char4, char5
- Word n-grams: word1, word2, word3
- Stem n-grams: stem1, stem2, stem3
- Function words: basic, KJV, pronouns, conjunctions, prepositions, auxiliaries
- POS n-grams: pos3, pos4, pos5
- Punctuation frequencies
- Sentence-length frequencies and TF-IDF
- Lexical richness
- Readability metrics

## Notebooks

The `notebooks/` folder contains interactive R Markdown walkthroughs:

- `01_Introduction.Rmd` — import `data/raw/kjv.csv`, prepare section documents, run a lightweight ensemble.
- `02_KJV_Isaiah_Example.Rmd` — run the KJV Isaiah comparison and inspect class-level matrices.
- `03_Experiment_With_Weights.Rmd` — change feature inclusion and class weights.
- `04_Feature_Exploration.Rmd` — inspect top features for char, word, function-word, punctuation, sentence, and lexical classes.
- `05_Author_Identification.Rmd` — nearest neighbors, class agreement, and vote counts.

Run notebooks from the project root directory so paths like `R/defaults.R` and `data/raw/kjv.csv` resolve correctly.
