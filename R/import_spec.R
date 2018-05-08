capture_spec <- function(...) {
  import_spec <- capture_functions(...)
  
  # make some checks regarding what functions are captured
  
  type <- names(import_spec)
  type <- recode(type, book = "book_chapter", article = "journal_article")
  fun_names <-  import_spec %>% 
    map(get_expr) %>%
    as.character() %>% 
    str_split(pattern = ", ") %>% 
    map(str_replace_all, "^c\\(|\\)$", "")
  evaled_funs <- import_spec %>% map(eval_tidy) 
  
  tibble(meta_type = type, fun_names = fun_names, evaled_funs = evaled_funs,
         bare_funs = import_spec)
}

capture_functions <- function(...) {
  dots <- enquos(..., .named = T)
  dots
}

walk_spec <- function(spec_df, chunk_number, n_batches, out_path, cores) {
  message("Processing files for ", paste(unique(spec_df$meta_type), collapse = " and "),
          " with functions ", unique(spec_df$fun_names))
  
  funs <- spec_df$evaled_funs
  
  if (any(lengths(funs) > 1)) {
    funs <- funs %>% 
      transpose() %>% 
      map(unique) %>% 
      flatten()
  } else {
    funs <- unique(funs)
  }
  
  
  out_paths <- spec_df %>% 
    unnest(fun_names) %>% 
    mutate(out_paths = paste(out_path, meta_type, fun_names, sep = "_")) %>% 
    select(fun_names, out_paths) %>% 
    split(.$fun_names) %>% 
    map(distinct) %>% 
    map(pull, out_paths)
  
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
  
  pwalk(list(n_batches = n_batches, 
             out_path = out_paths_recycled, in_paths = in_paths_recycled,
             chunk_number = chunk_number_recycled,
             fun = funs_recycled, cores = cores), jstor_convert_to_file)
  
}
