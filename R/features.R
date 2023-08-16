#' Declare a region of code as relating to a feature
#'
#' Automatically check for the ability to load suggested packages relating
#' to a specific feature, and choose what to do should the packages be
#' unavailable.
#'
feature <- function(features, fallback, env = parent.frame()) {
  features <- get_features(env)  
}