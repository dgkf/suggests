#' Choose how extensively suggests affects a package namespace
#'
#' `suggests` will invariably inject some objects into the package namespace.
#' However, the extent to which this happens may be undesirable. At a minimum,
#' `suggests` will will add a `.suggests` object to your package namespace. From
#' there, `suggests` can mask `base` operators in your namespace to streamline
#' your ' development experience. 
#'
#' To enable these extra capabilities, you'll have to opt-in to them using
#' [capabilities()].
#'
#' @section Capabilitiess:
#' \describe{
#'   \item{"colon"}{
#'     Mask base `::` and `:::` internally within your package namespace. The
#'     masking functions will apply your specified behavior when the namespace
#'     is attempted to be accessed.
#'   }
#'   \item{"create"}{
#'     Creates an object of the packages name in the calling namespace '
#'     automatically. It can be indexed just like a package namespace, using
#'     the `::` and `:::` operators (if feature "colon" is enabled), or
#'     using `$`.
#'   }
#' }
#'
#' @examples
#' \dontrun{capabilities("colon")
#' utils <- suggests("utils")
#' utils::head(1:10)
#'
#' capabilities("create")
#' suggests("not.a.real.package")
#' not.a.real.package::head(1:10)
#' # Error: This feature is unavailable because package 'not.a.real.package'
#' #   is not installed.
#' }
#'
#' @export
capabilities <- function(
  name = c("colon", "create"),
  env = parent.frame()
) {
  name <- match.arg(name, several.ok = TRUE)
  env <- get_suggests_env(env)

  register_capability_flags(name, env)
  if ("colon" %in% env$capabilities) capability_enable_colons(env)

  env
}

capability_enabled <- function(name, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)
  name %in% suggests_env$capabilities
}

capability_enable_colons <- function(env = parent.frame()) {
  suggests_env <- get_suggests_env(env)

  # inject masked `::` and `:::` operators
  assign("::", suggests_double_colon, package_env(env))
  assign(":::", suggests_triple_colon, package_env(env))

  invisible(suggests_env)
}