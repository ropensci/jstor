#' Print results in a pretty way
#' 
#' @param x A jstor object, created via one of the `find_*`-functions.
#' @keywords internal
#' @method print jstor
#' @export
print.jstor <- function(x, ...) {
  out <- tibble::as_tibble(x)
  print(out)
}
