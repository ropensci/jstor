#' Batch import data
#'
#' This function helps you import a large number of files.
#' 
#'
#' @param in_paths A character vector of file-paths.
#' @param chunk_number An integer, specifying the number of the chunk. Will be
#' appended to the output-file.
#' @param out_path The path where the files should be written to. Should include
#' the basename for the files too, i.e. `path/to/outfiles/meta_data`.
#' @param fun The function to use when importing files, i.e. `find_meta`,
#' `find_authors` and `read_full_text`.
#' @param col_names Should `col_names` be printed when exporting results? For
#'   errors, col_names are always written to file.
#' @param cores Number of cores to use for parallel processing.
#'
#' @export
jstor_convert_to_file <- function(in_paths, chunk_number, out_path, fun,
                                  col_names = FALSE,
                                  cores = getOption("mc.cores", 1L)) {
  if (.Platform$OS.type == "windows" & cores > 1) {
    cores <- 1L
    message("Parallel processing is currently not supported on windows.",
            "Computing with single core.")
  }
  
  safe_fun <- purrr::safely(fun)

  raw_result <- parallel::mclapply(in_paths, safe_fun, mc.cores = cores) %>%
    purrr::transpose()

  is_ok <- raw_result[["error"]] %>%
    purrr::map_lgl(purrr::is_null)

  res <- raw_result[["result"]]
  error <- raw_result[["error"]]

  res_ok <- res[is_ok] %>%
    dplyr::bind_rows()

  write_csv(res_ok, path = paste0(out_path, "-", chunk_number, ".csv"),
            na = "", col_names = col_names)

  # in case we have errors, write them to file too
  if (sum(!is_ok) > 0) {

    # find error-id
    error_ids <- data_frame(id = seq_along(is_ok),
                            is_ok = is_ok) %>%
      dplyr::filter(!is_ok) %>%
      dplyr::select(-is_ok)

    # extract the error messages
    error_message <- error[!is_ok] %>%
      map(1, `[`, "message") %>% # I don't really get why this works
      flatten_chr()

    # combine ids with error messages
    res_error <- dplyr::bind_cols(error_ids, error_message = error_message)

    write_csv(res_error,
              path = paste0(out_path, "_broken-", chunk_number, ".csv"),
              na = "", col_names = TRUE)

  } # end of "in case we have errors"

}


#' Wrapper for file import
#'
#' This function applies an import function to a list of `xml`-files and saves
#' them in batches of `.csv`-files to disk.
#'
#' Along the way, we wrap three functions, which make the process of converting 
#' many files easier:
#' 
#' - [purrr::safely()]
#' - [parallel::mclapply()]
#' - [readr::write_csv()]
#' 
#' When using one of the `find_*` functions, there should usually be no errors.
#' To avoid the whole computation to fail in the unlikely event that an error
#' occurs, we use `safely()` which let's us
#' continue the process, and catch the error along the way.
#' 
#' If you have many files to import, you might benefit from using
#' parallelization. On Linux and Mac, this can be achieved via `mclapply()`,
#' which we use here.
#' 
#' After importing all files, they are written to disk with
#' [readr::write_csv()].
#' 
#' Since you might run out of memory when importing a large quantity of files,
#' the files to import are split up into batches. Each batch is being treated
#' separately, therefore for each batch multiple processes from `mclapply()` are
#' spawned. For this reason, it is not recommended to have very small batches,
#' as there is an overhead for starting and ending the processes. On the other
#' hand, the batches should not be too large, to not exceed memory limitations.
#' A value of 10000 to 20000 for `files_per_batch` should work fine on most
#' machines.
#' 
#' 
#' @param in_paths A character vector to the `xml`-files which should be 
#' imported
#' @param out_file Name of files to export to. Each batch gets appended by an
#' increasing number.
#' @param out_path Path to export files to (combined with filename).
#' @param .f Function to use for import. Can be one of `find_article`,
#' `find_authors`, `find_references`, `find_footnotes`, `find_book` or
#' `find_chapter`.
#' @param files_per_batch Number of files for each batch.
#' @param cores Number of cores to use for parallel processing.
#'
#' @return Writes `.csv`-files to disk.
#'
#' @export
jstor_import_wrapper <- function(in_paths, out_file, out_path = NULL, .f,
                                 files_per_batch = 10000,
                                 cores = getOption("mc.cores", 1L)) {

  file_list <- split(in_paths, ceiling(seq_along(in_paths)/files_per_batch))
  chunk_numbers <- unique(names(file_list)) %>% as.list()

  if (!is.null(out_path)) {
    out_file <- file.path(out_path, out_file)
  }

  purrr::pwalk(
    list(file_list, chunk_numbers, out_file, list(.f), cores = cores),
    jstor_convert_to_file
  )
}
