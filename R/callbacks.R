#' Callbacks To Handle Missing Suggests Packages
#'
#' These callbacks can be provided to \code{suggested} when declaring a
#' suggested dependency. If a Suggested package namespace is accessed, the
#' callback is used when handling the missing dependency. Generally, these are
#' mitigating or communicative steps to handle the missing package in its
#' entirety. By contrast, a package author may also define fallback behaviors
#' for specific namespace objects when the package is otherwise missing.
#'
#' @name suggested_callbacks
#' @rdname suggested_callbacks
NULL

#'
#' @section Callbacks:
#' \describe{
#'   \item{\code{suggested_callback_error}}{
#'     Throw an error if a suggested package is not installed.
#'   }
#' }
#'
#' @export
#' @rdname suggested_callbacks
suggests_callback_error <- function(sug, ...) {
  stop(suggests_condition(sug, ..., cond = errorCondition))
}

suggests_condition <- function(sug, ..., cond = simpleCondition) {
  ip <- installed.packages()
  pkgname <- attr(sug, "pkg")

  subclass <- "suggests_missing"
  however <- ", but is unavailable."

  if (pkgname %in% ip[, "Package"]) {
    inst_ver <- ip[pkgname, "Version"]
    subclass <- "suggests_version_incompatible"
    however <- paste0(
      ", but dependency version requirement is not met. ",
      sprintf(
        "Version %s is installed, but must be %s.",
        inst_ver,
        format_version_requirements(sug, collapse = c(", ", " and "))
      )
    )
  }

  msg <- paste0(
    "Suggested package '", pkgname, "' is required for this functionality",
    however
  )

  res <- cond(msg)
  suggests_subclass <- paste0(packageName(), "_", class(res)[[1]])
  class(res) <- c(subclass, suggests_subclass, class(res))
  res
}

#'
#' @section Callbacks:
#' \describe{
#'   \item{\code{suggested_callback_prompt}}{
#'     Prompt to install a suggested package and resume if installed
#'   }
#' }
#'
#' @export
#' @rdname suggested_callbacks
suggests_callback_prompt <- function(sug, ...) {
  is_available <- sug %in% available.packages()[, "Package"]
  if (interactive() && is_available) {
    prompt_install(sug, ...)
  } else {
    suggests_callback_error(sug, ...)
  }
}

prompt_install <- function(sug, ...) {
  pkgname <- attr(sug, "pkg")
  cond <- suggests_condition(sug, ...)
  cond$msg <- paste(cond$msg, "Would you like to install it now? (Y/n)")

  message(cond)
  res <- toupper(readLines(n = 1L)[[1L]])

  if (nchar(res) == 0L || startsWith(res, "Y")) {
    install.packages(pkgname, ...)
  }
}
