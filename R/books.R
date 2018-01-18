find_book <- function(file_path) {
  validate_file_path(file_path, "xml")
  
  xml_file <- xml2::read_xml(file_path)
  
  validate_book(xml_file)
  
  book <- xml_find_all(xml_file, "book-meta")
  
  out <- data.frame(
    book_id = extract_child(book, ".//book-id"),
    discipline = extract_all(
      book, ".//subj-group[@subj-group-type='discipline']"
    ),
    book_title = extract_child(book, ".//book-title"),
    book_subtitle = extract_child(book, ".//book-title-group/subtitle"),
    pub_day = extract_child(book, ".//day") %>% as.integer(),
    pub_month = extract_child(book, ".//month") %>% as.integer(),
    pub_year = extract_child(book, ".//year") %>% as.integer(),
    isbn = extract_all(book, "isbn"),
    publisher_name = extract_child(book, ".//publisher-name"),
    publisher_location = extract_child(book, ".//publisher-loc"),
    n_pages = extract_book_pages(book),
    language = extract_child(book, ".//meta-value"),
    stringsAsFactors = FALSE
  )
  
  structure(out, class = c("jstor", "data.frame"))
}


extract_book_pages <- function(book) {
  xml_child(book, ".//counts/page-count") %>% 
    xml2::xml_attr("count") %>%
    as.integer()
}

