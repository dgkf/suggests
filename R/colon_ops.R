suggests_double_colon <- function(pkg, name) {
  env <- parent.frame()
  pkgsym <- substitute(pkg)
  namesym <- substitute(name)
  if (is_suggests(sug <- try_get_suggests(pkgsym, env))) {
    double_colon(sug, as.character(namesym))
  } else if (exists(pkgsym, envir = env) && is.character(pkg) && is_suggests(sug <- try_get_suggests(pkg, env))) {
    double_colon(sug, as.character(namesym))
  } else {
    do.call(getExportedValue("base", "::"), list(pkgsym, namesym))
  }
}

#' @export
double_colon <- function(pkg, name) UseMethod("double_colon")

#' @export
double_colon.default <- getExportedValue("base", "::")

suggests_triple_colon <- function(pkg, name) {
  env <- parent.frame()
  pkgsym <- substitute(pkg)
  namesym <- substitute(name)
  if (is_suggests(sug <- try_get_suggests(pkgsym, env))) {
    triple_colon(sug, namesym)
  } else if (exists(pkgsym, envir = env) && is.character(pkg) && is_suggests(sug <- try_get_suggests(pkg, env))) {
    triple_colon(sug, as.character(namesym))
  } else {
    do.call(getExportedValue("base", ":::"), list(pkgsym, namesym))
  }
}

#' @export
triple_colon <- function(pkg, name) UseMethod("triple_colon")

#' @export
triple_colon.default <- getExportedValue("base", ":::")

try_get_suggests <- function(pkg, env = parent.frame()) {
  UseMethod("try_get_suggests")
}

try_get_suggests.default <- function(pkg, env = parent.frame()) {
  try_get_suggests(as.character(pkg), env)
}

try_get_suggests.suggests_package <- function(pkg, env = parent.frame()) {
  pkg
}

try_get_suggests.suggests_package_stub <- function(pkg, env = parent.env()) {
  try_get_suggests(unclass(pkg), env = attr(pkg, "where"))
}

try_get_suggests.character <- function(pkg, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)
  if (!pkg %in% names(suggests_env$packages)) {
    return(NULL)
  }

  sug <- suggests_env$packages[[pkg]]
  suggests_env$packages[[pkg]] <- instantiate_suggests(pkg, sug, suggests_env)

  suggests_env$packages[[pkg]]
}

instantiate_suggests <- function(pkg, args, env = parent.frame()) {
  if (is_suggests(args)) return(args)

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
