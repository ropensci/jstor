context("test-books.R")
library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/standard_book.xml" %>%
  find_book()

alternative <- "testfiles/book-alternative-case.xml" %>% 
  find_book()

empty <- "testfiles/book-empty.xml" %>% 
  find_book()

empty_chapters <- "testfiles/book-empty.xml" %>% 
  find_chapters()

chapter <- "testfiles/standard_book.xml" %>% 
  find_chapters()

# tests -----
test_that("Input data is checked", {
  expect_error(find_book("my_path.txt"))
  expect_error(find_chapters("my_path.txt"))
  expect_error(find_book("testfiles/standard_case.xml"), "You are using")
  expect_error(find_chapters("testfiles/standard_case.xml"), "You are using")
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
  expect_s3_class(result, "data.frame")
  expect_s3_class(chapter, "tbl_df")
})


test_that("book-meta fields are correct", {
  expect_identical(result[["book_id"]], "j.ctt24hdz7")
  expect_identical(result[["basename_id"]], "standard_book")
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
  expect_identical(alternative[["isbn"]], "9781921536502")
  expect_identical(result[["publisher_name"]], "ANU E Press")
  expect_identical(result[["publisher_location"]], "Canberra")
  expect_identical(alternative[["n_pages"]], 271L)
  expect_identical(result[["language"]], "eng")
})

test_that("missing fields are of correct type", {
  expect_identical(empty[["book_id"]], NA_character_)
  expect_identical(empty[["discipline"]], NA_character_)
  expect_identical(empty[["book_title"]], NA_character_)
  expect_identical(empty[["book_subtitle"]], NA_character_)
  expect_identical(empty[["pub_day"]], NA_integer_)
  expect_identical(empty[["pub_month"]], NA_integer_)
  expect_identical(empty[["pub_year"]], NA_integer_)
  expect_identical(empty[["isbn"]], NA_character_)
  expect_identical(empty[["publisher_name"]], NA_character_)
  expect_identical(empty[["publisher_location"]], NA_character_)
  expect_identical(empty[["n_pages"]], NA_integer_)
  expect_identical(empty[["language"]], NA_character_)
  
  expect_identical(empty_chapters[["book_id"]], NA_character_)
  expect_identical(empty_chapters[["part_id"]], NA_character_)
  expect_identical(empty_chapters[["part_label"]], NA_character_)
  expect_identical(empty_chapters[["part_title"]], NA_character_)
  expect_identical(empty_chapters[["part_subtitle"]], NA_character_)
  expect_identical(empty_chapters[["authors"]], NA_character_)
  expect_identical(empty_chapters[["abstract"]], NA_character_)
  expect_identical(empty_chapters[["part_first_page"]], NA_character_)
})
