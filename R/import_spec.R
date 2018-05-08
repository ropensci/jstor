capture_spec <- function(...) {
  import_spec <- capture_functions(...)
  
  # make some checks regarding what functions are captured
  
  type <- names(import_spec)
  type <- recode(type, book = "book_chatper", article = "journal_article")
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
  message("Processing files for ", paste(spec_df$type, collapse = " and "),
          " with functions ", spec_df$fun_names)
  
  funs <- spec_df$evaled_funs %>% flatten()
  
  out_paths <- spec_df %>% 
    unnest(fun_names) %>% 
    mutate(out_paths = paste(out_path, type, fun_names, sep = "_")) %>% 
    select(-fun_names) %>% 
    pull(out_paths)
  
  walk2(funs, out_paths, ~jstor_convert_to_file(
    spec_df$path, fun = .x, chunk_number = chunk_number, n_batches = n_batches,
    out_path = .y, cores = cores))

  
}
