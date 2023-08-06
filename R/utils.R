`%||%` <- function(l, r) if (is.null(l)) r else l

vlapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, logical(1L), ..., USE.NAMES = USE.NAMES)
}

vcapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, character(1L), ..., USE.NAMES = USE.NAMES)
}