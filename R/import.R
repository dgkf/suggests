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

  stub <- new_suggests_stub(pkg, env)
  if (capability_enabled("create", env) || create) {
    delayedAssign(pkg, stub, assign.env = env)
  }

  invisible(stub)
}

load_suggests_package <- function(x, suppress = FALSE) {
  if (suggests_load_attempted(x)) {
    if (!suppress && "delayed" %in% x@status) {
      x@status <- setdiff(x@status, "delayed")
      return(invisible(x@callback(x)))
    }

    return()
  }

  # confirm that the version to be loaded would satisfy version requirements
  version_satisfied <- suggests_version_constraints_satisfied(x)

  # if not loaded, check if required package is available
  # NOTE: versionCheck doesn't actually do anything here despite documentation
  can_load <- requireNamespace(
    x@pkg,
    versionCheck = version_requirements(x),
    quietly = TRUE
  )

  if (!version_satisfied || !can_load) {
    if (!suggests_load_attempted(x)) {
      assign(".__NAMESPACE__.", NULL, x)
    }

    if (suppress) {
      x@status <- c("load failed" = "failed", "using fallbacks" = "delayed")
      return()
    } else {
      return(invisible(x@callback(x)))
    }
  }

  x@status <- c("loaded" = "success")
  ns <- loadNamespace(x@pkg)
  for (n in names(ns)) x[[n]] <- ns[[n]]
}