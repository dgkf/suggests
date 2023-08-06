#' Choose how extensively suggests affects a package namespace
#'
#' `suggests` will invariably inject some objects into the package namespace.
#' However, the extent to which this happens may be undesirable. At a minimum,
#' `suggests` will will add a `.suggests` object to your package namespace. From
#' there, `suggests` can mask `base` operators in your namespace to streamline
#' your ' development experience. 
#'
#' To enable these extra features, you'll have to opt-in to them using
#' [enable_features()].
#'
#' @section Features:
#' \describe{
#'   \item{"colon"}{
#'     Mask base `::` and `:::` internally within your package namespace. The
#'     masking functions will apply your specified behavior when the namespace
#'     is attempted to be accessed.
#'   }
#'   \item{"create"}{
#'     Creates an object of the packages name in the calling namespace '
#'     automatically. It can be indexed just like a package namespace, using
#'     the `::` and `:::` '     operators (if feature "colon" is enabled), or
#'     using `$`.
#'   }
#' }
#'
#' @examples
#' \dontrun{enable(features = "colon")
#' utils <- suggests("utils")
#' utils::head(1:10)
#'
#' enable(features = "create")
#' suggests("not.a.real.package")
#' not.a.real.package::head(1:10)
#' # Error: This feature is unavailable because package 'not.a.real.package'
#' #   is not installed.
#' }
#'
#' @export
enable <- function(features = c("colon", "create"), env = parent.frame()) {
  features <- match.arg(features, several.ok = TRUE)
  env <- get_suggests_env(env)

  register_feature_flags(features, env)
  if ("colon" %in% env$features) enable_colons(env)

  env
}

is_feature_enabled <- function(feature, env = parent.frame()) {
  suggests_env <- get_suggests_env(env)
  feature %in% suggests_env$features
}

enable_colons <- function(env = parent.frame()) {
  suggests_env <- get_suggests_env(env)

  # inject masked `::` and `:::` operators
  assign("::", suggests_double_colon, package_env(env))
  assign(":::", suggests_triple_colon, package_env(env))

  invisible(suggests_env)
}