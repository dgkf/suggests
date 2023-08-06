#' @import suggests
NULL

suggests::enable()  # defaults to all
suggests::import(cli)

suggests::fallback(cli::cat_line,
  function(..., col, background_col) {
    cat(..., "\n", sep = "", append = TRUE)
  }
)

#' @export
hello <- function(who = "World") {
  # inspect suggests package
  print(cli)

  # use a function with a callback
  cli::cat_line(paste0("Hello, ", who, "!"))

  # cli fails to load (impossible version contraints)
  cli::cli_alert_danger("Uh oh, no valid version available!") %!% message
}