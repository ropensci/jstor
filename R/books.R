find_book <- function(file_path) {
  validate_file_path(file_path, "xml")
  
  xml_file <- xml2::read_xml(file_path)
  
  # validate type
  if (identical(xml2::xml_name(xml_file), "article")) {
    stop(paste("You are using `find_book` on an article. Please use", 
               "`find_article` instead."), call. = FALSE)
  }
  
  book <- xml_find_all(xml_file, "book-meta")
  
  out <- data.frame(
    book_id = extract_element(book, ".//book-id"),
    stringsAsFactors = FALSE
  )
  
  structure(out, class = c("jstor", "data.frame"))
}
