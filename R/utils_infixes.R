#' If not null else
#'
#' Return the left-hand-side if it is not `NULL`, otherwise return the result of
#' the right-hand-side.
#'
#' @export
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

#' If not error else
#'
#' Return the result of the left-hand-side if it does not produce an error,
#' otherwise return the result of the right-hand-side.
#'
#' @export
`%?%` <- function(lhs, rhs) {
  tryCatch(lhs, error = function(...) rhs)
}

#' Capture and coerce conditions
#'
#' Should the left-hand-side emit a condition (for example, an error, warning
#' or message), capture it and pass it to a right-hand-side function
#'
#' @examples
#' (1 + "a") %!% warning
#'
#' @export
`%!%` <- function(lhs, rhs) {
  tryCatch(lhs, condition = rhs)
}
