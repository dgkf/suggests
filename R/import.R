#' Import a package as a Suggests dependency
#'
#' Creates an object in the current environment with the same name as the
#' package, which can be used just like a package to access its namespace,
#' throwing an error if the package is not installed.
#'
#' @param pkgname A \code{character} package name to import as a "Suggests"
#'   dependency
#' @param unavailable_callback A \code{function} to be called when the suggested
#'   package is meant to be used, but unavailable.
#' @param env A \code{environment} in which the suggested package object should
#'   be assigned. By default, an object of the same name as the package is
#'   created in the parent environment.
#'
#' @export
import <- function(pkg, ..., create = FALSE) {
  pkg_expr <- substitute(pkg)

  if (is.symbol(pkg_expr)) {
    pkg <- as.character(pkg_expr)
  }

  if (!is.character(pkg)) {
    stop(sprintf(
      "Cannot import a suggested package of type '%s'",
      class(pkg)[[1]]
    ))
  }

  env <- package_env(parent.frame())
  create_suggests_env(env)

  spec <- list(pkg, ..., suggests_version = packageVersion(packageName()))
  register_suggested_package(pkg, spec, env)

  if (is_feature_enabled("create", env) || create) {
    delayedAssign(pkg, instantiate_suggests(pkg, spec, env), assign.env = env)
  }
}

#' @export
new_suggests <- function(
  pkgname,
  unavailable_callback = suggests_callback_error,
  ...
) {
  structure(
    new.env(parent = emptyenv()),
    pkg = pkgname,
    ver = NULL, # to be derived lazily after package is built
    parent = package_env(get_suggests_env()),
    callback = unavailable_callback,
    load_status = c("not yet loaded" = "await"),
    class = "suggests_package"
  )
}

new_lazy_suggests <- function(fn) {
  structure(fn, class = c("lazy_suggests_package", "suggests_package"))
}


is_suggests <- function(x) {
  inherits(x, "suggests_package")
}

load_suggests_package <- function(x, suppress = FALSE) {
  pkg <- attr(x, "pkg")
  callback <- attr(x, "callback")

  if (suggests_load_attempted(x)) {
    if (!suppress && "delayed" %in% attr(x, "load_status")) {
      attr(x, "load_status") <- setdiff(attr(x, "load_status"), "delayed")
      return(invisible(callback(x)))
    }

    return()
  }

  # confirm that the version to be loaded would satisfy version requirements
  version_satisfied <- suggests_version_constraints_satisfied(x)

  # if not loaded, check if required package is available
  # NOTE: versionCheck doesn't actually do anything here despite documentation
  can_load <- requireNamespace(
    pkg,
    versionCheck = version_requirements(x),
    quietly = TRUE
  )

  if (!version_satisfied || !can_load) {
    if (!suggests_load_attempted(x)) {
      assign(".__NAMESPACE__.", NULL, x)
    }

    if (suppress) {
      attr(x, "load_status") <- c(
        "load failed" = "failed", 
        "using fallbacks" = "delayed"
      )

      return()
    } else {
      return(invisible(callback(x)))
    }
  }

  attr(x, "status") <- c("loaded" = "success")
  ns <- loadNamespace(pkg)
  for (n in names(ns)) x[[n]] <- ns[[n]]
}

suggests_loaded <- function(sug) {
  ns <- get0(".__NAMESPACE__.", sug, inherits = FALSE, ifnotfound = list())
  !is.null(ns$path)
}

suggests_load_attempted <- function(sug) {
  exists(".__NAMESPACE__.", sug, inherits = FALSE)
}