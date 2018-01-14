context("test-helpers.R")


test_that("Files other than `book` and `article` raise an error", {
  file <- xml2::read_xml("testfiles/wrong_doc_type.xml")
  expect_error(validate_doc_type(file, "book", "article"), "Unknown input file")
})
