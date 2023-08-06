#' @export
`$.suggests_package` <- function(x, name) {
  `[[`(x, as.character(name))
}

#' @export
`[[.suggests_package` <- function(x, name, ...) {
  # suggests namespace loaded, pull loaded value
  if (suggests_loaded(x) && name %in% names(x)) {
    return(get(name, envir = x, inherits = FALSE))
  }

  # suggests namespace not loaded, but a fallback is provided
  if (name %in% names(x)) {
    # first try to load actual suggested package version
    load_suggests_package(x, suppress = TRUE)
    return(get(name, envir = x, inherits = FALSE))
  }

  if (!suggests_load_attempted(x)) {
    load_suggests_package(x)
    return(x[[name]])
  }

  # if namespace is loaded, but does not contain value
  if (suggests_loaded(x)) {
    stop(sprintf(
      "Suggested package '%s' does not provide function '%s'",
      attr(x, "pkg"),
      name
    ))
  }

  # namespace could not be loaed
  suggests_callback_error(x)
}

#' @export
format.suggests_package <- function(x, ...) {
  ver <- if (suggests_loaded(x)) {
    packageVersion(attr(x, "pkg"))
  } else if (length(version_requirements(x)) > 0) {
    format_version_requirements(x, collapse = ",")
  } else {
    "any"
  }

  paste0(
    "<", "suggests:", attr(x, "pkg"), "[", ver, "]", ">",
    if (!"success" %in% attr(x, "load_status")) {
      paste0(" (", paste0(names(attr(x, "load_status")), collapse = ", "), ")")
    }
  )
}

#' @export
print.suggests_package <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
.DollarNames.suggests_package <- function(x, pattern) {
  load_suggests_package(x)
  grep(pattern, names(x), value = TRUE)
}

#' @export
double_colon.suggests_package <- `[[.suggests_package`

#' @export
triple_colon.suggests_package <- `[[.suggests_package`
