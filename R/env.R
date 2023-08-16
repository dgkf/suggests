package_env <- function(env = parent.frame()) {
  if (isNamespace(env)) return(env)
  topenv(env)
}

create_suggests_env <- function(env = parent.frame(2L)) {
  env <- package_env(env)
  if (!exists(".suggests", env, inherits = FALSE)) {
    suggests_env <- new.env(parent = env)
    suggests_env$features <- character(0L)
    assign(".suggests", suggests_env, envir = env)
  }
}

get_suggests_env <- function(env = parent.frame(2L)) {
  env <- package_env(env)
  create_suggests_env(env)
  get0(".suggests", env)
}

create_features <- function(env = parent.frame(2L)) {
}

# get_features <- function(env = parent.frame()) {
#   suggests_env <- get_suggests_env(env)
# }

register_suggested_package <- function(pkg, spec, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)
  suggests_env$packages[[pkg]] <- spec
}

register_fallback <- function(pkgname, symbol, fallback, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)

  symbol <- as.character(symbol)
  if (!exists("fallbacks", envir = suggests_env)) {
    suggests_env$fallbacks <- list()
  }

  if (!pkgname %in% suggests_env$fallbacks) {
    pkg_fallbacks <- new.env(parent = env)
    suggests_env$fallbacks[[pkgname]] <- pkg_fallbacks
  }

  suggests_env$fallbacks[[pkgname]][[symbol]] <- fallback
}

register_capability_flags <- function(flags, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)
  suggests_env$capabilities <- unique(c(suggests_env$capabilities, flags))
}