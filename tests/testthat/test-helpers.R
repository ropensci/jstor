context("test-helpers.R")

invalid_file <- "testfiles/invalid_file.xml"

# tests ------
test_that("Files other than `book` and `article` raise an error", {
  file <- xml2::read_xml("testfiles/wrong_doc_type.xml")
  expect_error(validate_article(file), "Unknown input file")
  expect_error(validate_book(file), "Unknown input file")
})


test_that("Warnings for invalid URI are suppressed", {
  expect_silent(read_jstor(invalid_file))
})

