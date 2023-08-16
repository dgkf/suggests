#' Replacement `::` and `:::` operators
#' 
#' The only difference in behavior from the `base` equivalents is that the
#' left-hand-side is first checked to see if it is a suggested package, which
#' prompts `suggests` to pull registered data about the package from the 
#' encapsulating package namespace before attempting to load a name from 
#' its environment.
#'
#' @inheritParams base::`::`
#' @name colon-ops
#' 
NULL

#' @describeIn colon-ops
#' Double-colon operator
suggests_double_colon <- function(pkg, name) {
  env <- parent.frame()
  pkgsym <- substitute(pkg)
  namesym <- substitute(name)
  sug <- try_get_suggests(pkgsym, env)

  if (exists(pkgsym, envir = env) && is_suggests(sug)) {
    double_colon(sug, as.character(namesym))
  } else {
    do.call(getExportedValue("base", "::"), list(pkgsym, namesym))
  }
}

#' @export
double_colon <- function(pkg, name) UseMethod("double_colon")

#' @export
double_colon.default <- getExportedValue("base", "::")

#' @describeIn colon-ops
#' Triple-colon operator
suggests_triple_colon <- function(pkg, name) {
  env <- parent.frame()
  pkgsym <- substitute(pkg)
  namesym <- substitute(name)
  sug <- try_get_suggests(pkgsym, env)
  if (exists(pkgsym, envir = env) && is_suggests(sug)) {
    triple_colon(sug, as.character(namesym))
  } else {
    do.call(getExportedValue("base", ":::"), list(pkgsym, namesym))
  }
}

#' @export
triple_colon <- function(pkg, name) UseMethod("triple_colon")

#' @export
triple_colon.default <- getExportedValue("base", ":::")

#' Search for a suggests package namespace object in an environment
#'
#' @param x A plausible reference to a suggested package namespace 
#' @param env (`environment`) A location in which to search for package 
#'   namespace information.
#'
#' @return A `suggests_package` object as created with [new_suggests()] or
#'   `NULL` if no suggests package of that name is declared by the 
#'   encapsulating package.
#'
#' @family suggests-lazy-loading
#' @keywords internal
try_get_suggests <- function(x, env = parent.frame()) {
  UseMethod("try_get_suggests")
}

try_get_suggests.default <- function(x, env = parent.frame()) {
  try_get_suggests(as.character(x), env)
}

try_get_suggests.suggests_package <- function(x, env = parent.frame()) {
  x
}

try_get_suggests.suggests_package_stub <- function(x, env = parent.env()) {
  # materialize suggests by package and env
  materialized <- try_get_suggests(attr(x, "pkg"), env = attr(x, "where"))

  # load materialized namespace into existing stub and de-stub package
  for (n in names(materialized)) x[[n]] <- materialized[[n]]
  class(x) <- setdiff(class(x), "suggests_package_stub")

  x
}

try_get_suggests.character <- function(x, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)
  if (!x %in% names(suggests_env$packages)) {
    return(NULL)
  }

  if (!is_suggests(sug <- suggests_env$packages[[x]])) {
    suggests_env$packages[[x]] <- materialize_suggests_stub(x, sug, suggests_env)
  }

  suggests_env$packages[[x]]
}

#' Materialize a package by name
#'
#' A suggests package, upon creation, is a minimalist representation containing
#' only a package name and object location (environment). When it's needed,
#' we must first _materialize_ the object, meaning that we look up the
#' dependency version requirements and load up any registered fallbacks. 
#'
#' Note that we haven't actually loaded the package yet!
#'
#' @param pkgs (`character(1L)`) A package name.
#' @param args (`list`) Arguments to pass to [new_suggests()].
#' @param env (`environment`) Where to call [new_suggests()] (used for
#'   discovering the encpsulating package namespace).
#'
#' @return A materialized `suggests_package` object
#'
#' @keywords internal
#' @family suggests-lazy-loading
materialize_suggests_stub <- function(pkg, args, env = parent.frame()) {
  env <- package_env(env)
  sug <- do.call(new_suggests, args, envir = env)

  # save new suggests object in namespace environment
  suggests_env <- get_suggests_env(env)
  suggests_env$packages[[pkg]] <- sug

  # instantiate with fallbacks until namespace is actually loaded
  sug_fallbacks <- suggests_env$fallbacks[[pkg]]
  for (name in names(sug_fallbacks)) {
    sug[[name]] <- sug_fallbacks[[name]]
  }

  sug
}
