suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(SnowballC)
})

optional_require <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    warning(sprintf("Optional package '%s' is not installed.", pkg), call. = FALSE)
    return(FALSE)
  }
  TRUE
}
