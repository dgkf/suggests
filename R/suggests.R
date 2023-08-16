# NOTE: changing this will require re-building of packages, avoid changes
SUGGESTS_METADATA_NAME <- ".__SUGGESTS__."  # nolint

#' @export
new_suggests <- function(
  pkg,
  unavailable_callback = suggests_callback_error,
  ...
) {
  metadata <- list(
    pkg = pkg,
    ver = NULL, # to be derived lazily after package is built
    parent = package_env(get_suggests_env()),
    callback = unavailable_callback
  )

  env <- new.env(parent = emptyenv())
  env[[SUGGESTS_METADATA_NAME]] <- metadata

  structure(env, class = "suggests_package")
}

#' @describeIn new_suggests
#' A stub of a suggests package, which will reference stored suggests data.
#' This initializer is used to provide the absolute minimum necessary
#' information to initialize a new suggests object - which package it refers to
#' and where the package was imported. Because this might be built into a
#' package, we want to avoid any parts of the `suggests_package` object
#' structure that might change from version to version.
#'
#' Once accessed, the stub is materialized and saved in the parent package's
#' hidden suggests environment, as well as copied into the stub object.
#'
#' @family suggests-lazy-loading
#' @export
new_suggests_stub <- function(pkg, where = parent.frame()) {
  structure(
    new.env(parent = emptyenv()),
    pkg = pkg,
    where = where,
    class = c("suggests_package_stub", "suggests_package")
  )
}

get_suggests_metadata <- function(x) {
  if (is_suggests_stub(x)) x <- try_get_suggests(x)
  get0(SUGGESTS_METADATA_NAME, envir = x, inherits = FALSE, ifnotfound = list())
}

is_suggests <- function(x) {
  inherits(x, "suggests_package")
}

is_suggests_stub <- function(x) {
  inherits(x, "suggests_package_stub")
}

suggests_parent_name <- function(x) {
  getNamespaceName(x@parent)
}

suggests_parent_desc <- function(x, ..., keep.white = TRUE) {
  parent_pkg_name <- suggests_parent_name()
  path <- find.package(parent_pkg_name)
  read.dcf(file.path(path, "DESCRIPTION"), ..., keep.white = keep.white)
}

#' Suggests load states
#'
#' The `.__NAMESPACE__.` value, used to represent a loaded package namespace,
#' can contain a few different sentinal values to indicate different loading
#' states.
#'
#' \describe{
#'   \item{`environment` (success)}{
#'     The loaded package namespace.
#'   }
#'   \item{`NA_integer_` (fail delayed)}{
#'     Load was attempted, but functionality had a fallback method, delay
#'     output until a behavior without a fallback is needed.
#'   }
#'   \item{`NULL` (fail)}{
#'     Load was attempted, but failed.
#'   }
#' }
suggests_loaded <- function(sug) {
  ns <- get0(".__NAMESPACE__.", sug, inherits = FALSE, ifnotfound = NULL)
  is.environment(ns)
}

suggests_load_attempted <- function(sug) {
  exists(".__NAMESPACE__.", sug, inherits = FALSE)
}