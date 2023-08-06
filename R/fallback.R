#' @export
fallback <- function(x, ...) {
  xcall <- substitute(x)

  if (inherits(xcall, "call") &&
        (xcall[[1L]] == "::" || xcall[[1L]] == "$" || xcall[[1L]] == "[[")) {
    pkgname <- as.character(xcall[[2L]])
    pkgobj <- as.character(xcall[[3L]])
    value <- ..1
  } else {
    pkgname <- x
    pkgobj <- ..1
    value <- ..2
  }

  register_fallback(pkgname, pkgobj, value, parent.frame())
}
