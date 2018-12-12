context("test-helpers.R")

invalid_file <- "testfiles/invalid_file.xml"
standard_case <- "testfiles/standard_case.xml"

# tests ------
test_that("Files other than `book` and `article` raise an error", {
  file <- xml2::read_xml("testfiles/wrong_doc_type.xml")
  expect_error(validate_article(file), "Unknown input file")
  expect_error(validate_book(file), "Unknown input file")
})

test_that("unvalid file paths raise an error", {
  expect_error(validate_file_path("abc.txt", "xml"))
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
  
  expect_identical(zip_loc$zip_archive, "testfiles/standard_case.zip")
  expect_identical(zip_loc$file_path, "standard_case.xml")
})

test_that("jst_get_file_name works", {
  standard_xml_file <- "testfiles/standard_case.xml"
  expect_identical(jst_get_file_name(standard_xml_file), "standard_case")
  
  zip_loc <- specify_zip_loc("testfiles/standard_case.zip", 
                             "standard_case.xml")
  
  expect_identical(jst_get_file_name(zip_loc), "standard_case")
  
})

test_that("unavailable path raises error", {
  expect_error(check_path("abc"))
})

test_that("getting footnotes or references on book raises informative error", {
  expect_error(jst_get_references(jst_example("book.xml")), 
               "Neither footnotes nor references")
})

test_that("validating input for tibbles works", {
  expect_error(validate_tibble(list(1:3, 1:2)))
  expect_equal(validate_tibble(list(1:5, 1:5)), 5L)
  expect_equal(validate_tibble(letters), 26L)
})
