#' Batch import data
#'
#' This function does the heavy lifting of importing all data.
#'
#' @param in_paths A character vector of file-paths.
#' @param chunk_number An integer, specifying the number of the chunk. Will be
#' appended to the output-file.
#' @param out_path The path where the files should be written to. Should include the
#' basename for the files too, i.e. `path/to/outfiles/meta_data`.
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
    error_ids <- data_frame(id = 1:length(is_ok),
                            is_ok = is_ok) %>%
      dplyr::filter(!is_ok) %>%
      dplyr::select(-is_ok)

    # extract the error messages
    error_message <- error[!is_ok] %>%
      map(1, `[`, "message") %>% # I don't really get why this works
      flatten_chr()

    # combine ids with error messages
    res_error <- dplyr::bind_cols(error_ids, error_message = error_message)

    write_csv(res_error, path = paste0(out_path, "_broken-", chunk_number, ".csv"),
              na = "", col_names = TRUE)

  } # end of "in case we have errors"

}


#' Wrapper for file conversion
#'
#' This function applies an import function to a list of `xml`-files and saves
#' them in batches of .csv-files to disk.
#'
#' @param file_paths A character vector to the `xml`-files which should be imported
#' @param out_file Name of files to export to. Each batch gets appended by an
#' increasing number.
#' @param path Path to export files to (combined with filename).
#' @param .f Function to use for import. Can be one of `find_meta`, `find_authors`,
#' `find_references` or `find_footnotes`.
#' @param files_per_batch Number of files for each batch.
#' @param cores Number of cores to use for parallel processing.
#'
#' @return Writes .csv-files to disk.
#'
#' @export
jstor_import_wrapper <- function(file_paths, out_file, path = NULL, .f,
                                 files_per_batch = 4000,
                                 cores = getOption("mc.cores", 1L)) {

  file_list <- split(file_paths, ceiling(seq_along(file_paths)/files_per_batch))
  chunk_numbers <- unique(names(file_list)) %>% as.list()

  if (!is.null(path)) {
    out_file <- file.path(path, out_file)
  }

  purrr::pwalk(list(file_list, chunk_numbers, out_file, list(.f), cores = cores),
               jstor_convert_to_file)
}
