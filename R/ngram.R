#' Read ngram data
#' 
#' Read in data on ngrams via [readr::read_tsv].
#' 
#' This function is mainly useful when it is used in together with
#' [jst_import_zip], where you can use it to specify reading in ngram data.
#' 
#' @param file A path to a file or a zip location from [jst_subset_ngrams()].
#' @return A [tibble::tibble] with two columns: 
#' - *ngram*: the ngram term (unigram, bigram, trigram)
#' - *n*: an integer for the number of times the term occurred in the original 
#' file
#' @export
jst_get_ngram <- function(file) {
  if (!inherits(file, "jstor_zip") && length(file) > 1) {
    stop("file_path should be length 1, not ", length(file))
  }
  
  if (identical(tools::file_ext(file), "txt")) {
    file <- check_path(file)
    
    out <- readr::read_tsv(file,
                           col_names = c("ngram", "n"), 
                           col_types = c("ci"))
    attr(out, "spec") <- NULL
    
    expand_and_bind(file, out, ngram = TRUE)
    
  } else if (inherits(file, "jstor_zip")) {
    
    con <- unz(file$zip_archive, file$file_path, open = "rb")
    
    on.exit(close(con))
    
    out <- readr::read_tsv(con,
                           col_names = c("ngram", "n"), 
                           col_types = c("ci"))    
    attr(out, "spec") <- NULL
    
    expand_and_bind(file, out, ngram = TRUE)
  } else {
    abort("Unknown input file. Must be a `txt`-file or a zip-location.")
  }
}


#' Define a subset of ngrams
#' 
#' This function helps in defining a subset of ngram files which should be 
#' imported, since importing all ngrams at once can be very expensive (in
#' terms of cpu and memory).
#' 
#' @param zip_archives A character vector of one or multiple zip-files.
#' @param ngram_type One of `"ngram1"`, `"ngram2"` or `"ngram3"`
#' @param selection A data.frame with the articles/books which are to be
#' selected.
#' @param by A column name for matching.
#' 
#' @return A list of zip-locations which can be read via [jst_get_ngram()].

#' @export
#' @examples 
#' # create sample output
#' tmp <- tempdir()
#' jst_import_zip(jst_example("pseudo_dfr.zip"),
#'                import_spec = jst_define_import(book = jst_get_book),
#'                out_file = "test", out_path = tmp)
#' 
#' # re-import as our selection for which we would like to import ngrams
#' selection <- jst_re_import(paste0(tmp, 
#'                                   "/test_book_chapter_jst_get_book-1.csv"))
#' 
#' # get location of file
#' zip_loc <- jst_subset_ngrams(jst_example("pseudo_dfr.zip"), "ngram1",
#'                              selection) 
#' 
#' # import ngram
#' jst_get_ngram(zip_loc[[1]])
jst_subset_ngrams <- function(zip_archives, ngram_type, selection,
                              by = file_name) {
  by <- rlang::enquo(by)
  
  zip_content <- zip_archives %>% 
    purrr::map_df(get_zip_content, .id = "id")
  
  flagship_ids <- dplyr::select(selection, !!by)
  
  to_import <- zip_content %>% 
    dplyr::mutate(file_name = jst_get_file_name(Name),
                  file_name = stringr::str_remove(file_name, "-ngram\\d")) %>% 
    dplyr::filter(type == ngram_type) %>% 
    dplyr::right_join(flagship_ids, by = rlang::quo_name(by)) %>% 
    dplyr::mutate(id = factor(id, labels = zip_archives))
  
  to_import %>% 
    dplyr::mutate(zip_loc = purrr::map2(id, Name, specify_zip_loc)) %>% 
    dplyr::pull(zip_loc)
}
