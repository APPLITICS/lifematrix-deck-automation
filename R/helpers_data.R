# ------ NORMALIZE NAs ---------------------------------------------------------
#' Normalize NA-like values in a data frame.
#'
#' This helper replaces user-defined values with NA after loading. Unlike the
#' `na` parameter in `readr::read_csv()`, this function applies consistent NA 
#' cleaning across any data source (`fread()`, `read.csv()`, or in-memory 
#' data frames).
#'
#' @param data A data frame to clean.
#' @param na_equivalent Character vector of values to be converted to NA
#'
#' @return The input data frame with normalized missing values.
normalize_na_tbl <- function(
    data,
    na_equivalent
) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      # Replace +/- Inf with NA for numeric columns
      data[[col]][is.infinite(data[[col]])] <- NA_real_
    } else if (is.character(data[[col]]) || is.factor(data[[col]])) {
      # Trim whitespace and normalize NA-like values (case-insensitive)
      x <- trimws(as.character(data[[col]]))
      x[tolower(x) %in% tolower(na_equivalent)] <- NA_character_
      data[[col]] <- x
    }
  }
  
  return(data)
}