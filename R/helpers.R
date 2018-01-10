validate_file_path <- function(file_path, type) {
  if (identical(stringr::str_detect(file_path, type), FALSE)) {
    stop("`file_path` must be a `*.", type, "` file", call. = FALSE)
  }
}

extract_basename <- function(file_path, type) {
  basename(file_path) %>%
    stringr::str_extract(paste0(".*?(?=\\.", type, ")"))
}


as_jstor <- function(x) {
  stopifnot(is.data.frame(x))
  
  structure(x, class = c("jstor", "data.frame"))
}
