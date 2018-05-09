#' Define an import specification
#' 
#' Define which parts of a zip file should be converted via which functions.
#' 
#' The function accepts the following names: article, book, report, pamphlet.
#' The corresponding files from a .zip-archive will be imported via the supplied
#' functions. 
#' 
#' @param ... Named arguments with bare function names.
#' 
#' @return A specification of imports which is necessary for
#'         [jstor_import_zip()].
#' @examples 
#' # articles will be imported via `find_article()` and `find_authors()`
#' jst_define_import(article = c(find_article, find_author))
#' 
#' # import all four types with one function each
#' jst_define_import(article = find_article,
#'                   book = find_book,
#'                   report = find_book,
#'                   pamphlet = find_article)
#'                   
#' # import all four types with multiple functions
#' jst_define_import(article = c(find_article, find_authors, find_references),
#'                   book = c(find_book, find_chapters),
#'                   report = find_book,
#'                   pamphlet = find_article)
#'
#' \dontrun{
#' # define imports
#' imports <- jst_define_import(article = c(find_article, find_author))
#' 
#' # convert the files to .csv
#' jstor_import_zip("my_archive.zip", out_file = "my_out_file", 
#'                  import_spec = imports)
#' }
#' @export
jst_define_import <- function(...) {
  import_spec <- capture_functions(...)
  
  type <- names(import_spec)
  
  # check input types
  possible_types <- c("book", "article", "report", "pamphlet")
  types_checked <- type %in% possible_types
  
  if (!all(types_checked)) {
    stop("Input type must be one of ", paste(possible_types, collapse = ", "),
         ", `", type[!types_checked], "` is not.")
  }
  
  
  type <- dplyr::recode(type, book = "book_chapter",
                        article = "journal_article",
                        report = "research_report")
  
  
  fun_names <- import_spec %>% 
    map(get_expr) %>%
    as.character() %>% 
    str_split(pattern = ", ") %>% 
    map(str_replace_all, "^c\\(|\\)$", "") %>% 
    map(str_replace_all, "^list\\(|\\)$", "")
  
  
  # check input functions
  expressions <- import_spec %>%
    map(get_expr) %>%
    as.character()

  not_bare_funs <- expressions %>%
    str_detect("\\(\\)")

  if (any(not_bare_funs)) {
    stop("All inputs must be bare functions or a vector of bare functions, `",
         expressions[not_bare_funs], "` is not.", call. = FALSE)
  }

  evaled_funs <- import_spec %>% map(eval_tidy) 
  
  if (fun_list_depth(evaled_funs) > 2) {
    funs_checked <- evaled_funs %>% 
      purrr::flatten() %>% 
      map_lgl(purrr::is_function)
  } else {
    funs_checked <- evaled_funs %>% 
      map_lgl(purrr::is_function)
  }
  
  
  if (!all(funs_checked)) {
    stop("All inputs must be bare functions or a vector of bare functions, `",
         fun_names[[1]][!funs_checked], "` is not.", call. = FALSE)
  }
  
  # check namespaces of functions
  if (fun_list_depth(evaled_funs) > 2) {
    namespaces <- evaled_funs %>% 
      purrr::flatten() %>% 
      map(rlang::get_env)
  } else {
    namespaces <- evaled_funs %>% 
      map(rlang::get_env) 
  }
  
  correct_ns <- rlang::get_env(find_article)
  
  matching_namespaces <- namespaces %>% 
    unname() %>% 
    map_lgl(identical, correct_ns)
  
  if (!all(matching_namespaces)) {
    stop("All supplied functions must come from the `jstor` package, `",
         fun_names[[1]][!matching_namespaces], "` does not.",
         call. = FALSE)
  }
  

  out <- tibble::tibble(meta_type = type, fun_names = fun_names,
                        evaled_funs = evaled_funs, bare_funs = import_spec)
  
  class(out) <- c("jstor_import_spec", class(out))
  out
}

capture_functions <- function(...) {
  dots <- enquos(..., .named = T)
  dots
}

walk_spec <- function(spec_df, chunk_number, n_batches, out_path, cores,
                      show_progress, col_names) {
  message("Processing files for ", paste(unique(spec_df$meta_type), collapse = " and "),
          " with functions ", unique(spec_df$fun_names))
  
  funs <- spec_df$evaled_funs
  
  if (any(lengths(funs) > 1)) {
    funs <- funs %>% 
      purrr::transpose() %>% 
      map(unique) %>% 
      purrr::flatten()
  } else {
    funs <- unique(funs)
  }
  
  
  fun_spec <- spec_df %>% 
    tidyr::unnest(fun_names) %>% 
    dplyr::distinct(meta_type, type, fun_names)
    
  out_paths <- fun_spec %>% 
    mutate(out_paths = paste(out_path, meta_type, fun_names, sep = "_")) %>% 
    split(.$fun_names) %>% 
    map(dplyr::pull, out_paths)
  
  # reorder out_paths according to initial order, because split sorts them
  # alphabetically
  out_paths <- out_paths[fun_spec$fun_names]
  
  in_paths <- split(spec_df$path, spec_df$chunk_number)

  n_batches <- unique(n_batches)
  chunk_number <- unique(chunk_number)
  
  
  # recycle arguments to fit all combinations of cases
  path_length <- length(in_paths)
  fun_length <- length(funs)
  
  funs_recycled <- rep(funs, each = path_length)
  in_paths_recycled <- rep(in_paths, times = fun_length)
  out_paths_recycled <- rep(out_paths, each = path_length)
  chunk_number_recycled <- rep(chunk_number, fun_length)

  
  # in the following part, neither out_path nor in_path can be first arguments,
  # due to some quirks in transpose():
  # https://github.com/tidyverse/purrr/issues/474
  
  purrr::pwalk(
    list(
      n_batches = n_batches, 
      out_path = out_paths_recycled, in_paths = in_paths_recycled,
      chunk_number = chunk_number_recycled,
      fun = funs_recycled, cores = cores,
      show_progress = show_progress, col_names = col_names
    ),
    jstor_convert_to_file
  )
  
}
