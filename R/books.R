find_book <- function(file_path) {
  validate_file_path(file_path, "xml")
  
  xml_file <- xml2::read_xml(file_path)
  
  validate_doc_type(xml_file, correct_type = "book", wrong_type = "article")
  
  book <- xml_find_all(xml_file, "book-meta")
  
  out <- data.frame(
    book_id = extract_element(book, ".//book-id"),
    discipline = extract_element(
      book, ".//subj-group[@subj-group-type='discipline']"
    ),
    stringsAsFactors = FALSE
  )
  
  structure(out, class = c("jstor", "data.frame"))
}
