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
#' @noRd
jstor_convert_to_file <- function(in_paths, chunk_number, out_path, fun,
                                  col_names = FALSE, n_batches,
                                  show_progress = TRUE) {
  if (n_batches > 1) {
    message("Processing chunk ", chunk_number, "/", n_batches)
  }

  safe_fun <- purrr::safely(fun)
  
  # create progress bar if we are in console
  progress <- ifelse(interactive() && show_progress, TRUE, FALSE)
  
  # compute results in parallel
  parallel_result <- furrr::future_map(in_paths, safe_fun, .progress = progress)
  
  
  res_transposed <- purrr::transpose(parallel_result)

  is_ok <- res_transposed[["error"]] %>%
    purrr::map_lgl(purrr::is_null)

  res <- res_transposed[["result"]]
  error <- res_transposed[["error"]]

  if (any(is_ok)) {
    res_ok <- res[is_ok] %>%
      dplyr::bind_rows()
    
    # check for list columns and unnest
    col_types <- res_ok %>% map_chr(class)
    
    if (any(col_types %in% "list")) {
      res_ok <- tidyr::unnest(res_ok)
    }
    
    write_csv(res_ok, file = paste0(out_path, "-", chunk_number, ".csv"),
              na = "", col_names = col_names)
  }


  # in case we have errors, write them to file too
  if (any(!is_ok)) {

    # find error-id
    error_ids <- tibble::tibble(id = seq_along(is_ok),
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
              file = paste0(out_path, "_broken-", chunk_number, ".csv"),
              na = "", col_names = TRUE)

  } # end of "in case we have errors"

}



#' Wrapper for file import
#'
#' This function applies an import function to a list of `xml`-files
#' or a .zip-archive in case of `jst_import_zip` and saves
#' the output in batches of `.csv`-files to disk.
#'
#' Along the way, we wrap three functions, which make the process of converting 
#' many files easier:
#' 
#' - [purrr::safely()]
#' - [furrr::future_map()]
#' - [readr::write_csv()]
#' 
#' When using one of the `find_*` functions, there should usually be no errors.
#' To avoid the whole computation to fail in the unlikely event that an error
#' occurs, we use `safely()` which let's us
#' continue the process, and catch the error along the way.
#' 
#' If you have many files to import, you might benefit from executing the
#' function in parallel. We use futures for this to give you maximum 
#' flexibility. By default the code is executed sequentially. If you want to
#' run it in parallel, simply call [future::plan()] with
#' [future::multiprocess()] as an argument before
#' running `jst_import` or `jst_import_zip`. 
#' 
#' After importing all files, they are written to disk with
#' [readr::write_csv()].
#' 
#' Since you might run out of memory when importing a large quantity of files,
#' you can split up the files to import  into batches. Each batch is being 
#' treated separately, therefore for each batch multiple processes from
#' [future::multiprocess()] are spawned, if you added this plan.
#' For this reason, it is not recommended to have very small batches,
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
#' @param .f Function to use for import. Can be one of `jst_get_article`,
#' `jst_get_authors`, `jst_get_references`, `jst_get_footnotes`, `jst_get_book` 
#' or `jst_get_chapter`.
#' @param col_names Should column names be written to file? Defaults to `TRUE`.
#' @param n_batches Number of batches, defaults to 1.
#' @param files_per_batch Number of files for each batch. Can be used instead of
#' n_batches, but not in conjunction.
#' @param show_progress Displays a progress bar for each batch, if the session
#' is interactive.
#' @param zip_archive A path to a .zip-archive from DfR
#' @param import_spec A specification from [jst_define_import]
#' for which parts of a .zip-archive should be imported via which functions. 
#' @param rows Mainly used for testing, to decrease the number of files which
#' are imported (i.e. 1:100).
#'
#' @return Writes `.csv`-files to disk.
#'
#' @seealso [jst_combine_outputs()] 
#' @export
#' @examples 
#' \dontrun{
#' # read from file list --------
#' # find all files
#' meta_files <- list.files(pattern = "xml", full.names = T)
#' 
#' # import them via `jst_get_article`
#' jst_import(meta_files, out_file = "imported_metadata", .f = jst_get_article,
#'            files_per_batch = 25000)
#'            
#' # do the same, but in parallel
#' library(future)
#' plan(multiprocess)
#' jst_import(meta_files, out_file = "imported_metadata", .f = jst_get_article,
#'            files_per_batch = 25000)
#'
#' # read from zip archive ------ 
#' # define imports
#' imports <- jst_define_import(article = c(jst_get_article, jst_get_authors))
#' 
#' # convert the files to .csv
#' jst_import_zip("my_archive.zip", out_file = "my_out_file", 
#'                  import_spec = imports)
#' } 
jst_import <- function(in_paths, out_file, out_path = NULL, .f,
                       col_names = TRUE, n_batches = NULL,
                       files_per_batch = NULL,
                       show_progress = TRUE) {

  if (!is.null(n_batches) && !is.null(files_per_batch)) {
    stop("Either n_batches or files_per_batch needs to be specified, ",
         "not both.", call. = FALSE)
  }
  start_time <- Sys.time()
  
  message("Starting to import ", format(length(in_paths), big.mark = ","),
          " file(s).")
  
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
    list(file_list, chunk_numbers, out_file, list(.f),
         col_names = col_names, n_batches = n_batches,
         show_progress = show_progress),
    jstor_convert_to_file
  )
  
  end_time <- Sys.time()
  run_time <- end_time - start_time
  message("Finished importing ", format(length(in_paths), big.mark = ","),
          " file(s) in ", format(round(run_time, 2)), ".")
}



#' @rdname jst_import
#' @export
jst_import_zip <- function(zip_archive, import_spec, 
                             out_file, out_path = NULL,
                             col_names = TRUE, n_batches = NULL,
                             files_per_batch = NULL,
                             show_progress = TRUE,
                             rows = NULL) {
  
  if (!is.null(n_batches) && !is.null(files_per_batch)) {
    stop("Either n_batches or files_per_batch needs to be specified, ",
         "not both.", call. = FALSE)
  }
  
  
  tagged_files <- get_zip_content(zip_archive) 

  combined_spec <- import_spec %>% 
    dplyr::left_join(tagged_files, by = "meta_type")
  
  if (is.null(rows)) {
    # all rows are being selected
    combined_spec <- combined_spec %>% 
      mutate(path = purrr::map2(zip_archive, Name, specify_zip_loc))
  } else {
    if (max(rows) > nrow(combined_spec)) {
      # if the selection does not fit the rows which are present, raise an error
      actual_rows <- nrow(combined_spec)
      stop("The selected rows do not exist within the .zip-file. ",
           "The highest count for rows with the current specification is ",
           actual_rows, " rows.", call. = FALSE)
    } else {
      combined_spec <- combined_spec %>% 
        dplyr::slice(rows) %>% # select rows to read by position
        mutate(path = purrr::map2(zip_archive, Name, specify_zip_loc))
    }
  }
  
  
  # warn if specified import is missing in tagged file
  missing_types <- import_spec %>% 
    dplyr::left_join(tagged_files, by = "meta_type") %>% 
    dplyr::select(meta_type, Name) %>%
    dplyr::filter(is.na(Name)) %>% 
    dplyr::pull(meta_type)
  
  if (length(missing_types) > 0) {
    stop("The following types of documents are not available in the .zip-file:",
         " ", paste(missing_types, collapse = ", "), call. = FALSE)
  }
  
  
  if (!is.null(out_path)) {
    out_file <- file.path(out_path, out_file)
  }
  
  enhanced_spec <- compute_batches(combined_spec, n_batches, files_per_batch)
  
  n_batches <- enhanced_spec$n_batches
  chunk_number <- enhanced_spec$chunk_number
  
  enhanced_spec %>% 
    split(.$meta_type) %>% 
    purrr::walk(walk_spec, n_batches = n_batches,
                chunk_number = chunk_number, out_path = out_file,
                show_progress = show_progress, col_names = col_names)
  
}


compute_batches <- function(spec, n_batches, files_per_batch) {
  if (is.null(n_batches) && is.null(files_per_batch)) {
    n_batches <- 1
  }
  
  if (!is.null(files_per_batch)) {
    spec <- spec %>% 
      dplyr::group_by_("meta_type") %>% 
      mutate(chunk_number = ceiling(seq_along(Name) / files_per_batch)) %>% 
      dplyr::ungroup()
  } else if (identical(as.integer(n_batches), 1L)) {
    spec <- spec %>% 
      mutate(chunk_number = 1)
  } else {
    spec <- spec %>% 
      dplyr::group_by_("meta_type") %>% 
      mutate(chunk_number = as.integer(cut(seq_along(Name), n_batches))) %>% 
      dplyr::ungroup()
  }
  
  spec <- spec %>% 
    mutate(n_batches = max(chunk_number))

  spec
}
