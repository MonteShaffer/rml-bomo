source("R/00_dependencies.R")
source("R/prepare_data.R")

kjv_raw_path <- "data/raw/kjv.csv"
if(!file.exists(kjv_raw_path)){
  stop("Missing data/raw/kjv.csv. Place your raw KJV file there first.")
}

kjv_raw <- readr::read_csv(kjv_raw_path, show_col_types = FALSE)
kjv_sections <- prepare_kjv_sections(kjv_raw)
kjv_chapters <- prepare_chapter_docs(kjv_raw)

dir.create("data/prepared", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(kjv_sections, "data/prepared/kjv_section_docs.csv")
readr::write_csv(kjv_chapters, "data/prepared/kjv_chapter_docs.csv")

message("Prepared data written to data/prepared/.")
