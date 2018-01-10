#' Print results in a pretty way.
#' 
#' This print method simply leverages the functionality of `tibble` for printing,
#' without sacrificing speed when computing results with `find_*`-variants.
#' 
#' @param x A jstor object, created via one of the `find_*`-functions.
#' @param ... Arguments which can be passed down to [tibble::format.tbl].
#' 
#' @method print jstor
#' @export
print.jstor <- function(x, ...) {
  out <- tibble::as_tibble(x)
  
  print(out, ...)
}
