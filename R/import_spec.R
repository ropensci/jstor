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
#' jst_import_zip("my_archive.zip", out_file = "my_out_file", 
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
    stop("Input type must be one of ", 
         paste(crayon::green(possible_types), collapse = ", "),
         ".\n", crayon::blue(type[!types_checked]), " is not one of them.",
         call. = FALSE)
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
    stop("All inputs must be bare functions or a vector of bare functions, ",
         crayon::blue(expressions[not_bare_funs]), " is not.", call. = FALSE)
  }

  evaled_funs <- import_spec %>% map(eval_tidy) 
  
  funs_checked <- evaled_funs %>% 
    unlist() %>% 
    map_lgl(purrr::is_function)
  
  
  if (!all(funs_checked)) {
    stop("All inputs must be bare functions or a vector of bare functions, ",
         crayon::blue(fun_names[[1]][!funs_checked]), " is not.", call. = FALSE)
  }
  
  # check namespaces of functions
  matching_namespaces <- evaled_funs %>% 
    unlist() %>% 
    map_lgl(is_jstor)
  
  
  if (!all(matching_namespaces)) {
    stop("All supplied functions must come from the ", crayon::green("jstor"),
         " package, but ",
         crayon::blue(fun_names[[1]][!matching_namespaces]), " does not.",
         call. = FALSE)
  }
  
  # # check if functions match source type
  # this is quite hacky, using the way `walk_spec` works to raise a general
  # error if something is not fitting. Ideally we would catch the error
  # exactly where it occurs and display an informative error message, but this
  # is a bit complex since we need to map over several inputs and possibly
  # several functions to deal with each input.
  correct_types <- tibble::tribble(
    ~meta_type, ~path,
    "journal_article", jst_example("article_with_references.xml"),
    "book_chapter", jst_example("book.xml"),
    "research_report", jst_example("book.xml"),
    "pamphlet", jst_example("article_with_references.xml"),
    "ngram1", jst_example("ngram.txt"),
    "ngram2", jst_example("ngram.txt"),
    "ngram3", jst_example("ngram.txt")
  )
  
  matched_types <- tibble::tibble(meta_type = type, evaled_funs = evaled_funs,
                              fun_names = fun_names, chunk_number = 1) %>% 
    dplyr::left_join(correct_types, by = "meta_type") 
  
  
  tryCatch(
    book_function_for_article = function(cnd) {
      abort(paste0(
        "Your import specification seems to be incorrect, since you are using ",
        "a book function on an article. Please make sure ",
        "that all import functions correspond to the right data type."),
        .subclass = "import_spec")
      },
    article_function_for_book = function(cnd) {
      abort(paste0(
        "Your import specification seems to be incorrect, since you are using ",
        "an article function on a book. Please make sure ",
        "that all import functions correspond to the right data type."),
        .subclass = "import_spec")
    },
    matched_types %>% 
      split(.$meta_type) %>% 
      purrr::map(walk_spec, n_batches = 1,
                 chunk_number = 1, out_path = "",
                 show_progress = FALSE, col_names = "",
                 test_mode = T)
    )
  
 
 
  
  # if everything is ok, combine results and return them
  out <- tibble::tibble(meta_type = type, fun_names = fun_names,
                        evaled_funs = evaled_funs, bare_funs = import_spec)
  
  class(out) <- c("jstor_import_spec", class(out))
  out
}

#' @export
print.jstor_import_spec <- function(x, ...) {
  cli::cat_rule(left = crayon::bold("Import specification"))
  
  cli::cat_line(
    crayon::col_align(paste0("  ", crayon::underline("Source")), width = 20),
    crayon::col_align(crayon::underline("Import functions"))
  )

  print_part <- function(type, functions) {
    type_formatted <- type %>% stringr::str_to_title() %>% str_replace("_", " ") 
    
    imports <- paste0(
      crayon::col_align(
        paste(crayon::green(cli::symbol$bullet), type_formatted), width = 20
      ),
      crayon::col_align(
        paste(crayon::blue(functions), collapse = ", "), width = 20
      ))
    
    cat(imports)
    cli::cat_line()
  }
  
  purrr::walk2(x$meta_type, x$fun_names, print_part)
}


capture_functions <- function(...) {
  dots <- enquos(..., .named = T)
  dots
}


is_jstor <- function(fun) {
  if (identical(environment(fun), environment(jst_get_article))) {
    TRUE
  } else {
    fun %>% 
      check_env() %>% 
      identical(environment(jst_get_article))
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


walk_spec <- function(spec_df, chunk_number, n_batches, out_path,
                      show_progress, col_names, test_mode = FALSE) {
  if (!test_mode) {
    message(
      "Processing files for ", 
      paste(
        crayon::green(unique(spec_df$meta_type)),
        collapse = " and "
      ), 
      " with functions ", 
      crayon::blue(
        paste(
          unlist(spec_df$fun_names),
          collapse = ", ")
        )
      )
  }

  
  funs <- spec_df$evaled_funs
  
  if (any(lengths(funs) > 1)) {
    funs <- funs %>% 
      purrr::transpose() %>% 
      map(unique) %>% 
      purrr::flatten()
  } else {
    funs <- unique(funs)
  }
  
  # quick hack to fix error
  if (tidyr_new_interface()) {
    fun_spec <- spec_df %>% 
      tidyr::unnest_legacy(fun_names) %>% 
      dplyr::distinct(meta_type, fun_names)
  } else {
    fun_spec <- spec_df %>% 
      tidyr::unnest(fun_names) %>% 
      dplyr::distinct(meta_type, fun_names)
  }
    
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
  
  if (!test_mode) {
    purrr::pwalk(
      list(
        n_batches = n_batches, 
        out_path = out_paths_recycled, in_paths = in_paths_recycled,
        chunk_number = chunk_number_recycled,
        fun = funs_recycled,
        show_progress = show_progress, col_names = col_names
      ),
      jstor_convert_to_file
    )
  } else {
    
    mapper <- function(x, fun) map(x, fun)

    purrr::pmap(list(x = in_paths_recycled, fun = funs_recycled), mapper)
    
  }
  
}
