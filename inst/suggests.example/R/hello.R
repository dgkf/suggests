#' @import suggests
NULL

cli <- suggests::import(cli)

suggests::fallback(cli::cat_line,
  function(..., col, background_col) {
    cat(..., "\n", sep = "", append = TRUE)
  }
)

#' @eval suggests::features_as_roxygen_docs()
#' @export
features <- function() suggests::features()

#' @export
hello <- function(who = "World") {
  # use a function with a callback
  cli::cat_line(paste0("Hello, ", who, "!"))

  # cli fails to load (impossible version contraints)
  cli::cli_alert_danger("Uh oh, no valid version available!") %!% message
}