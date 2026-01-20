# Small helper functions used across multiple scripts

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x, na.rm = TRUE)
}


list.dirs(recursive = FALSE)
list.files("aggregates")
