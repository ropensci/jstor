#' Define an import specification
#' 
#' Define which parts of a zip file should be converted via which functions.
#' 
#' The function accepts the following names: article, book, report, pamphlet,
#' ngram1, ngram2, ngram3.
#' The corresponding files from a .zip-archive will be imported via the supplied
#' functions. 
#' 
#' @param ... Named arguments with bare function names.
#' 
#' @return A specification of imports which is necessary for
#'         [jst_import_zip()].
#' @examples 
#' # articles will be imported via `jst_get_article()` and `jst_get_authors()`
#' jst_define_import(article = c(jst_get_article, jst_get_authors))
#' 
#' # define a specification for importing article metadata and unigrams (ngram1)
#' jst_define_import(article = jst_get_article,
#'                   ngram1 = jst_get_ngram)
#'                   
#'                   
#' # import all four types with one function each
#' jst_define_import(article = jst_get_article,
#'                   book = jst_get_book,
#'                   report = jst_get_book,
#'                   pamphlet = jst_get_article)
#'                   
#' # import all four types with multiple functions
#' jst_define_import(article = c(jst_get_article, jst_get_authors, jst_get_references),
#'                   book = c(jst_get_book, jst_get_chapters),
#'                   report = jst_get_book,
#'                   pamphlet = jst_get_article)
#'
#' # if you want to import chapters with authors, you can use an anonymous
#' # function
#' 
#' chapters_w_authors <- function(x) jst_get_chapters(x, authors = TRUE)
#' jst_define_import(book = chapters_w_authors)
#' 
#'
#' \dontrun{
#' # define imports
#' imports <- jst_define_import(article = c(jst_get_article, jst_get_authors))
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
  possible_types <- c("book", "article", "report", "pamphlet",
                      paste0("ngram", 1:3))
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
    map(stringr::str_remove_all, "^c\\(|\\)$") %>% 
    map(stringr::str_remove_all, "^list\\(|\\)$") %>% 
    map(stringr::str_remove_all, "^[:alpha:]*?\\:{2,3}")
  
  
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
  
  funs_checked <- evaled_funs %>% 
    unlist() %>% 
    map_lgl(purrr::is_function)
  
  
  if (!all(funs_checked)) {
    stop("All inputs must be bare functions or a vector of bare functions, `",
         fun_names[[1]][!funs_checked], "` is not.", call. = FALSE)
  }
  
  # check namespaces of functions
  matching_namespaces <- evaled_funs %>% 
    unlist() %>% 
    map_lgl(is_jstor)
  
  
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


is_jstor <- function(fun) {
  if (identical(environment(fun), environment(find_article))) {
    TRUE
  } else {
    fun %>% 
      check_env() %>% 
      identical(environment(find_article))
  }
}


# the code for the following function came from 
# https://stackoverflow.com/users/3063910/rich-scriven
# from SO:
# https://stackoverflow.com/questions/50410811/determine-the-namespace-of-a-function-within-an-anonymous-function/
check_env <- function(fun) {
  fun %>% 
    pryr::fun_calls() %>% 
    pryr::fget() %>% 
    environment()
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
