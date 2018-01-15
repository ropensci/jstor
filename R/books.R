find_book <- function(file_path) {
  validate_file_path(file_path, "xml")
  
  xml_file <- xml2::read_xml(file_path)
  
  validate_doc_type(xml_file, correct_type = "book", wrong_type = "article")
  
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
    stringsAsFactors = FALSE
  )
  
  structure(out, class = c("jstor", "data.frame"))
}
