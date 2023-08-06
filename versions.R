#' @export
installed <- function(sug, env = parent.frame()) {
  sug_name <- as.character(substitute(sug))
  if (exists(sug_name, env) && inherits(sug, "suggests_package")) {
    return(suggests_loaded(sug))
  } else {
    # try to find package in libraries
    sug <- find.package(sug_name, quiet = TRUE)
    is_available <- length(sug) > 0
    if (!is_available) return(FALSE)

    # pull its version requirements
    desc <- read.dcf(file.path(sug, "DESCRIPTION"), "Suggests")
    dep_op_ver <- extract_version_requirement(desc, sug_name)

    # we're good if there's no version requirement
    has_ver_req <- !is.null(dep_op_ver$op)
    if (!has_ver_req) return(TRUE)

    # otherwise check to make sure the installed version satisfies req
    inst <- packageVersion(sug_name)
    satisfies_ver_req <- with(dep_op_ver, do.call(op, list(inst, version)))
    if (!satisfies_ver_req) return(FALSE)

    TRUE
  }
}

#' @export
available <- function(sug, env = parent.frame()) {
  sug_avail <- available.packages()[sug, "Version"]
  if (length(sug_avail) < 1) return(FALSE)
  any(vlapply(sug_avail, satisfies_version_requirements, sug = sug))
}

version_requirements <- function(x, ...) {
  UseMethod("version_requirements")
}

version_requirements.suggests_package <- function(x, ...) {
  if (!is.null(attr(x, "ver"))) return(attr(x, "ver"))
  reqs <- version_requirements(attr(x, "pkg"), attr(x, "parent"))
  attr(x, "ver") <- reqs
  reqs
}

version_requirements.character <- function(pkg, env = parent.frame(), ...) {
  default <- extract_version_requirement(t(as.matrix(c(Suggests = pkg))), pkg)

  suggests_env <- get_suggests_env(env)
  parent_pkg <- utils::packageName((suggests_env))
  if (is.null(parent_pkg)) return(default)

  parent_pkg_dir <- find.package(parent_pkg, quiet = TRUE)
  if (length(parent_pkg_dir) < 1) return(default)

  parent_desc <- file.path(parent_pkg_dir[[1]], "DESCRIPTION")
  if (!file.exists(parent_desc)) return(default)

  desc <- read.dcf(parent_desc, "Suggests")
  extract_version_requirement(desc, pkg)
}

extract_version_requirement <- function(desc, pkg) {
  suggests <- trimws(strsplit(desc[[1, "Suggests"]], ",")[[1]])
  deps <- lapply(suggests, tools:::.split_op_version)
  which <- vlapply(deps, function(i) i$name == pkg && !is.null(i$op))
  deps[which] # can be more than 1!!
}

format_version_requirements <- function(x, ..., collapse = NULL) {
  reqs <- version_requirements(x)

  if (length(reqs) == 0) return(NULL)
  fmt_reqs <- vcapply(reqs, function(req) paste0(req$op, req$version))

  if (!is.null(collapse)) {
    if (length(collapse) == 1L) collapse[[2]] <- collapse[[1]]
    seps <- c(
      rep_len(collapse[[1]], max(length(reqs) - 2, 0)),
      if (length(reqs) > 1) collapse[[2]],
      ""
    )
    paste0(fmt_reqs, seps, collapse = "")
  } else {
    fmt_reqs
  }
}

suggests_version_constraints_satisfied <- function(x) {
  pkg <- attr(x, "pkg")
  parent_pkg_env <- attr(x, "parent")

  # first ensure package is actually found in .libPaths
  suggests_pkg_dir <- find.package(pkg, quiet = TRUE)
  if (length(suggests_pkg_dir) < 1) {
    return(FALSE)
  }

  inst_pkg_ver <- packageVersion(pkg)
  satisfies_version_requirements(x, inst_pkg_ver)
}

satisfies_version_requirements <- function(sug, version) {
  reqs <- version_requirements(sug)
  reqs_satisfied <- vlapply(reqs, function(req) {
    if (is.null(req$op)) {
      return(TRUE)
    }
    do.call(req$op, list(version, req$version))
  })
  all(reqs_satisfied)
}