#' Extract meta information for books
#'
#' `find_book()` extracts meta-data from JSTOR-XML files for book chapters.
#'
#' @param file_path A `.xml`-file for a book or research report.
#' 
#' @return A `data.frame` containing the extracted meta-data with the following
#' columns:
#' - book_id *(chr)*: The jcode or a DOI. If both are present, the jcode
#'  (=publisher-id) is extracted.
#' - basename_id *(chr)*: The filename of the original .xml-file. Can be used 
#'   for joining with other data for the same file.
#' - discipline *(chr)*: The discipline from the discipline names used on JSTOR.
#' - book_id *(chr)*: The book id, either a combination of digits 
#'   (`pub-id-type="jstor"`) or a DOI (`pub-id-type="doi"`).
#' - book_title *(chr)*: The title of the book.
#' - book_subtitle *(chr)*: The subtitle of the book.
#' - pub_day *(int)*: Publication day, if specified.
#' - pub_month *(int)*: Publication month, if specified.
#' - pub_year *(int)*: Year of publication.
#' - isbn *(chr)*: One or more entries for the book's ISBN. If two or more, 
#'   separated by `"; "`.
#' - publisher_name *(int)*: The name of the publisher.
#' - publisher_loc *(int)*: The location of the publisher.
#' - n_pages *(int)*: The number of pages.
#' - language *(chr)*: The language of the book.
#'
#' A note about publication dates: always the first entry is being extracted,
#' which should correspond to the oldest date, in case there is more than one
#' date.
#' @export
#' @examples 
#' find_book(jstor_example("sample_book.xml"))
find_book <- function(file_path) {
  validate_file_path(file_path, "xml")
  
  xml_file <- xml2::read_xml(file_path)
  
  validate_book(xml_file)
  
  book <- xml_find_all(xml_file, "book-meta")
  
  out <- data.frame(
    book_id = extract_child(book, ".//book-id"),
    basename_id = extract_basename(file_path, "xml"),
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

