#' Get path to jstor example
#' 
#' jstor includes several sample files for demonstration purposes. This helper
#' makes them easy to access.
#' 
#' The code for this function was adapted from the package `readr`.
#' 
#' @param path Name of the example file. If `NULL`, the example files will be
#' listed.
#' 
#' @return Either a character vector with the names of example files (if 
#' `jst_example()` is called without supplying an argument), or a character 
#' vector indicating the path to the example file.
#' 
#' @export
#' @examples
#' jst_example()
#' jst_example("article_with_references.xml") 
jst_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("data-raw", package = "jstor"))
  } else {
    system.file("data-raw", path, package = "jstor", mustWork = TRUE)
  }
}
