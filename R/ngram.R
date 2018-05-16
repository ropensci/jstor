#' Read ngram data
#' 
#' Read in data on ngrams via [readr::read_tsv].
#' 
#' This function is mainly useful when it is used in together with
#' [jst_import_zip], where you can use it to specify reading in ngram data.
#' 
#' @param file A path to a file.
#' @return A [tibble::tibble] with two columns: 
#' - *ngram*: the ngram term (unigram, bigram, trigram)
#' - *n*: an integer for the number of times the term occured in the original 
#' file
#' @export
jst_read_ngram <- function(file) {
  if (!inherits(file, "jstor_zip") && length(file) > 1) {
    stop("file_path should be length 1, not ", length(file))
  }
  
  if (identical(tools::file_ext(file), "txt")) {
    file <- check_path(file)
    
    out <- readr::read_tsv(file,
                           col_names = c("ngram", "n"), 
                           col_types = c("ci"))
    attr(out, "spec") <- NULL
    
    expand_and_bind(file, out)
    
  } else if (inherits(file, "jstor_zip")) {
    
    con <- unz(file$zip_archive, file$file_path, open = "rb")
    
    on.exit(close(con))
    
    out <- readr::read_tsv(con,
                           col_names = c("ngram", "n"), 
                           col_types = c("ci"))    
    attr(out, "spec") <- NULL
    
    expand_and_bind(file, out)
  } else {
    abort("Unknown input file. Must be a `txt`-file.")
  }
}
