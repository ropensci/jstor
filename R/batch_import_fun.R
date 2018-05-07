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
#' @param fun The function to use when importing files, i.e. `find_article`,
#' `find_authors` and `read_full_text`.
#' @param col_names Should `col_names` be printed when exporting results? For
#'   errors, col_names are always written to file.
#' @param n_batches Total number of batches (for progress bar).
#' @param cores Number of cores to use for parallel processing.
#' @noRd
jstor_convert_to_file <- function(in_paths, chunk_number, out_path, fun,
                                  col_names = FALSE, n_batches,
                                  cores = getOption("mc.cores", 1L),
                                  show_progress = TRUE) {
  message("Processing chunk ", chunk_number, "/", n_batches)

  safe_fun <- purrr::safely(fun)
  
  
  # start cluster
  cl <- snow::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  
  # create progress bar if we are in console
  if (interactive() && show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = length(in_paths), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

  
  # compute results in parallel
  parallel_result <- foreach::foreach(i = in_paths, .packages = "jstor",
                                 .options.snow = opts) %dopar%
    safe_fun(i)
  
  # stop cluster and progress bar
  snow::stopCluster(cl)
  
  if (interactive() && show_progress) close(pb)
  
  
  res_transposed <- purrr::transpose(parallel_result)

  is_ok <- res_transposed[["error"]] %>%
    purrr::map_lgl(purrr::is_null)

  res <- res_transposed[["result"]]
  error <- res_transposed[["error"]]

  if (any(is_ok)) {
    res_ok <- res[is_ok] %>%
      dplyr::bind_rows()
    
    write_csv(res_ok, path = paste0(out_path, "-", chunk_number, ".csv"),
              na = "", col_names = col_names)
  }


  # in case we have errors, write them to file too
  if (any(!is_ok)) {

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
#' - [foreach::foreach()]
#' - [readr::write_csv()]
#' 
#' When using one of the `find_*` functions, there should usually be no errors.
#' To avoid the whole computation to fail in the unlikely event that an error
#' occurs, we use `safely()` which let's us
#' continue the process, and catch the error along the way.
#' 
#' If you have many files to import, you might benefit from executing the
#' function in parallel. We use
#' \code{\link[snow:snow-startstop]{snow::createCluster()}} to setup a cluster
#' and
#' then compute the results via `foreach` and \code{\%dopar\%}. The type of
#' cluster is determined by `getClusterOption("type")`.
#' 
#' After importing all files, they are written to disk with
#' [readr::write_csv()].
#' 
#' Since you might run out of memory when importing a large quantity of files,
#' the files to import are split up into batches. Each batch is being treated
#' separately, therefore for each batch multiple processes from
#' \code{\link[snow:snow-startstop]{snow::createCluster()}} are
#' spawned. For this reason, it is not recommended to have very small batches,
#' as there is an overhead for starting and ending the processes. On the other
#' hand, the batches should not be too large, to not exceed memory limitations.
#' A value of 10000 to 20000 for `files_per_batch` should work fine on most
#' machines. If the session is interactive and `show_progress` is `TRUE`, a
#' progress bar is displayed for each batch.
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
#' @param col_names Should column names be written to file? Defaults to `TRUE`.
#' @param n_batches Number of batches, defaults to 1.
#' @param files_per_batch Number of files for each batch. Can be used instead of
#' n_batches, but not in conjunction.
#' @param cores Number of cores to use for parallel processing.
#' @param show_progress Displays a progress bar for each batch, if the session
#' is interactive.
#'
#' @return Writes `.csv`-files to disk.
#'
#' @export
#' @examples 
#' \dontrun{
#' # find all files
#' meta_files <- list.files(pattern = "xml", full.names = T)
#' 
#' # import them via `find_article`
#' jstor_import(meta_files, out_file = "imported_metadata", .f = find_article,
#'              files_per_batch = 25000, cores = 4)
#' } 
jstor_import <- function(in_paths, out_file, out_path = NULL, .f,
                         col_names = TRUE, n_batches = NULL,
                         files_per_batch = NULL,
                         cores = getOption("mc.cores", 1L),
                         show_progress = TRUE) {
  
  if (!is.null(n_batches) && !is.null(files_per_batch)) {
    stop("Either n_batches or files_per_batch needs to be specified, ",
         "not both.", call. = FALSE)
  }
  start_time <- Sys.time()
  
  message("Starting to import ", length(in_paths), " file(s).")

  # set n_batches to 1 for default
  if (is.null(n_batches) && is.null(files_per_batch)) {
    n_batches <- 1
  }
  
  if (!is.null(files_per_batch)) {
    file_list <- split(in_paths, ceiling(seq_along(in_paths) / files_per_batch))
  } else if (identical(as.integer(n_batches), 1L)) {
    file_list <- list(`1` = in_paths)
  } else {
    file_list <- split(in_paths, as.integer(cut(seq_along(in_paths), n_batches)))
  }
  
  chunk_numbers <- unique(names(file_list)) %>% as.list()

  if (!is.null(out_path)) {
    out_file <- file.path(out_path, out_file)
  }
  
  n_batches <- length(chunk_numbers)

  purrr::pwalk(
    list(file_list, chunk_numbers, out_file, list(.f), cores = cores,
         col_names = col_names, n_batches = n_batches,
         show_progress = show_progress),
    jstor_convert_to_file
  )
  
  end_time <- Sys.time()
  run_time <- end_time - start_time
  message("Finished importing ", length(in_paths), " file(s) in ",
          format(round(run_time, 2)), ".")
}

