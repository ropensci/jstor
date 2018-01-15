context("test-books.R")
library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/standard_book.xml" %>%
  find_book()

alternative <- "testfiles/book-alternative-case.xml" %>% 
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
  expect_identical(
    find_book("testfiles/book-alternative-case.xml")[["discipline"]],
    "Political Science; Sociology"
  )
  expect_identical(result[["book_title"]], "The 2006 Military Takeover in Fiji")
  expect_identical(result[["book_subtitle"]], "A Coup to End All Coups?")
  expect_identical(result[["pub_day"]], 30L)
  expect_identical(result[["pub_month"]], 4L)
  expect_identical(result[["pub_year"]], 2009L)
  expect_identical(result[["isbn"]], "9781921536502; 9781921536519")
  expect_identical(alternative[["isbn"]], NA_character_)
  expect_identical(result[["publisher_name"]], "ANU E Press")
  expect_identical(result[["publisher_location"]], "Canberra")
  expect_identical(alternative[["n_pages"]], 271L)
  expect_identical(result[["n_pages"]], NA_integer_)
})
