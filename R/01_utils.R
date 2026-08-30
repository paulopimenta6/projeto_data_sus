`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

normalize_text <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  gsub("[^a-z0-9]+", "_", x)
}

format_pt_number <- function(x, accuracy = 1) {
  scales::number(
    x,
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ",",
    trim = TRUE
  )
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

safe_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion(package))
}
