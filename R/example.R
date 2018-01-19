#' Get path to jstor example
#' 
#' jstor includes several sample files for demonstration purposes. This helper
#' makes them easy to access.
#' 
#' The code for this function was adapted from the package `readr`.
#' 
#' @param path Name of the example file. If `NULL`, the example files will be
#' listed.
#' @export
#' @examples
#' jstor_example()
#' jstor_example("sample_with_references.xml") 
jstor_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "jstor"))
  } else {
    system.file("extdata", path, package = "jstor", mustWork = TRUE)
  }
}
