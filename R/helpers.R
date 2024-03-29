#' Extract the basename of a path
#' 
#' This helper simply extracts the basename of a path and removes the extension,
#' e.g. `foo/bar.txt` is shortened to `bar`.
#' 
#' @param file_path A path to a file
#' @return A character vector, containing the basename of the file without an
#' extension.
#' 
#' @export
jst_get_file_name <- function(file_path) {
  if (inherits(file_path, "jstor_zip")) {
    file_path[["file_path"]] %>% 
      basename() %>% 
      tools::file_path_sans_ext()
  } else {
    basename(file_path) %>%
      tools::file_path_sans_ext()
  }
}

validate_file_path <- function(file_path, type) {
  if (identical(stringr::str_detect(file_path, type), FALSE)) {
    stop("`file_path` must be a `*.", type, "` file", call. = FALSE)
  }
}

validate_article <- function(xml_file) {
  if (identical(xml2::xml_name(xml_file), "book")) {
    original_call <- as.character(sys.call(-1)[[1]])
    if (any(original_call %in% c("jst_get_references", "jst_get_footnotes"))) {
      abort(paste0(
        "You are using `", original_call, "` on a book. ",
        "Neither footnotes nor references are available for books."),
        .subclass = "article_function_for_book")
    } else {
      abort(paste0(
        "You are using `", original_call, "` on a book. ",
        "Please use `jst_get_book` or `jst_get_chapters` instead."),
        .subclass = "article_function_for_book")
    }
  } else if (!identical(xml2::xml_name(xml_file), "article")) {
    abort("Unknown input file.")
  }
}

validate_book <- function(xml_file) {
  if (identical(xml2::xml_name(xml_file), "article")) {
    abort(
      paste0("You are using `", sys.call(-1)[[1]], "` on an article. ",
             "Please use `jst_get_article`, `jst_get_authors`, `jst_get_references` ",
             "or `jst_get_footnotes` instead."),
      .subclass = "book_function_for_article")
  } else if (!identical(xml2::xml_name(xml_file), "book")) {
    abort("Unknown input file.")
  }
}

# general helper to extract children from an XML-file
# should be be used with a string like "volume", or with an XPATH specification
# like ".//meta-value"
extract_child <- function(doc, element) {
  doc %>%
    xml_child(element) %>%
    xml_text()
}

extract_first <- function(doc, element) {
  doc %>%
    xml_find_first(element) %>%
    xml_text()
}

extract_all <- function(doc, element) {
  res <- doc %>%
    xml_find_all(element)

  if (is_empty(res)) {
    return(NA_character_)
  } else {
    res %>%
      xml_text() %>%
      paste0(collapse = "; ")
  }
}


expand_and_bind <- function(file_path, individual_part, ngram = FALSE) {
  if (ngram) {
    list(
      file_name = jst_get_file_name(file_path) %>%
        stringr::str_remove("-ngram\\d") %>% 
        rep(times = NROW(individual_part))
    ) %>%
      dplyr::bind_cols(individual_part)
  } else {
    list(
      file_name = jst_get_file_name(file_path) %>%
        rep(times = NROW(individual_part))
    ) %>%
      dplyr::bind_cols(individual_part)
  }
}

#' simple helper to suppress warnings from invalid URIs. see issue #33
#' @noRd
read_jstor <- function(file) {
  if (!inherits(file, "jstor_zip") && length(file) > 1) {
    stop("file_path should be length 1, not ", length(file))
  }

  if (identical(tools::file_ext(file), "xml")) {
    file <- check_path(file)
    
    suppressWarnings(xml2::read_xml(file))
  } else if (inherits(file, "jstor_zip")) {
    
    con <- unz(file$zip_archive, file$file_path, open = "rb")
    
    on.exit(close(con))
    
    suppressWarnings(xml2::read_xml(con))
    
  } else {
    stop("Unknown input file. Must be a `xml`-file.", 
         call. = FALSE)
  }
  
}

# copied from readr
check_path <- function(path) {
  if (file.exists(path))
    return(normalizePath(path, "/", mustWork = FALSE))
  
  stop("'", path, "' does not exist",
       if (!is_absolute_path(path))
         paste0(" in current working directory ('", getwd(), "')"),
       ".",
       call. = FALSE
  )
}

# copied from readr
is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

# This function checks if the input of the tibble is valid by checking the 
# lengths of the input.
# It returns the number of rows, since that is what tibble::new_tibble needs.
validate_tibble <- function(x) {
  
  if (is.atomic(x)) {
    return(length(x))
  } else {
    lengths <- lengths(x)
    
    if (length(unique(lengths)) > 1) {
      abort("There is a problem.")
    }
    
    lengths[1]
  }
}
