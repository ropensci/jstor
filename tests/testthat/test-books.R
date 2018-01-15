context("test-books.R")
library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/standard_book.xml" %>%
  find_book()

# tests -----
test_that("Input data is checked", {
  expect_error(find_book("my_path.txt"))
  expect_error(find_book("testfiles/standard_case.xml"), "You are using")
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
  expect_s3_class(result, "data.frame")
})


test_that("book-meta fields are correct", {
  expect_identical(result[["book_id"]], "j.ctt24hdz7")
  expect_identical(result[["discipline"]], "Political Science")
})
