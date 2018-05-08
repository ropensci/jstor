context("test-helpers.R")

invalid_file <- "testfiles/invalid_file.xml"
standard_case <- "testfiles/standard_case.xml"

# tests ------
test_that("Files other than `book` and `article` raise an error", {
  file <- xml2::read_xml("testfiles/wrong_doc_type.xml")
  expect_error(validate_article(file), "Unknown input file")
  expect_error(validate_book(file), "Unknown input file")
})


test_that("Warnings for invalid URI are suppressed", {
  expect_silent(read_jstor(invalid_file))
})


test_that("read_jstor works", {
  expect_equal(read_jstor(standard_case), xml2::read_xml(standard_case))
  expect_error(read_jstor("test.txt"))
  expect_error(read_jstor(rep(standard_case, 2)), "file_path should be")
  
  
  zip_loc <- specify_zip_loc("testfiles/standard_case.zip", 
                             "standard_case.xml")
  expect_equal(read_jstor(zip_loc), xml2::read_xml(standard_case))
  
})


test_that("Construction of zip location works", {
  zip_loc <- specify_zip_loc("testfiles/standard_case.zip", 
                             "standard_case.xml")
  expect_s3_class(zip_loc, "jstor_zip")
})

