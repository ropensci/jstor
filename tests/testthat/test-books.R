context("test-books.R")
library(xml2)
library(magrittr)
library(stringr)

# import files -----
result <- "testfiles/standard_book.xml" %>%
  jst_get_book()

alternative <- "testfiles/book-alternative-case.xml" %>%
  jst_get_book()

empty <- "testfiles/book-empty.xml" %>%
  jst_get_book()

empty_chapters <- "testfiles/book-empty.xml" %>%
  jst_get_chapters()

chapters <- "testfiles/standard_book.xml" %>%
  jst_get_chapters()

chap_auth <- "testfiles/standard_book.xml" %>%
  jst_get_chapters(authors = T)

# tests -----
test_that("Input data is checked", {
  expect_error(jst_get_book("my_path.txt"))
  expect_error(jst_get_chapters("my_path.txt"))
  expect_error(jst_get_book("testfiles/standard_case.xml"), "You are using")
  expect_error(jst_get_chapters("testfiles/standard_case.xml"), "You are using")
})

test_that("class is correct", {
  expect_s3_class(result, "tbl_df")
  expect_s3_class(chapters, "tbl_df")
})


test_that("book-meta fields are correct", {
  expect_identical(result[["book_id"]], "j.ctt24hdz7")
  expect_identical(result[["file_name"]], "standard_book")
  expect_identical(result[["discipline"]], "Political Science")
  expect_identical(
    jst_get_book("testfiles/book-alternative-case.xml")[["discipline"]],
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


test_that("chapters are correct", {
  expect_equal(dim(chapters), c(36, 9))
  expect_identical(chapters[[1, "book_id"]], "j.ctt24hdz7")
  expect_identical(chapters[[1, "file_name"]], "standard_book")
  expect_identical(chapters[[1, "part_id"]], "j.ctt24hdz7.1")
  expect_identical(chapters[[1, "part_label"]], NA_character_)
  expect_identical(chapters[[5, "part_label"]], "1.")
  expect_identical(chapters[[5, "part_title"]],
                   "The enigmas of Fiji’s good governance coup")
  expect_identical(chapters[[5, "part_subtitle"]], NA_character_)
  expect_identical(chapters[[6, "part_subtitle"]],
                   "Fiji’s road to military coup, 2006")
  expect_false(any(!is.na(chapters[["authors"]])))
  expect_identical(chapters[[1, "abstract"]], NA_character_)
  expect_identical(chapters[[5, "abstract"]] %>% str_trunc(25),
                   "Fiji’s December 2006 c...")
  expect_identical(chapters[[1, "part_first_page"]], "i")
  expect_identical(chapters[[5, "part_first_page"]], "3")
})


# authors ----
correct_authors <- tribble(
  ~prefix, ~given_name, ~surname, ~string_name, ~suffix, ~author_number,
  NA_character_, "Jon", "Fraenkel",  NA_character_, NA_character_,  1L,
  NA_character_, "Stewart", "Firth", NA_character_, NA_character_,  2L
)

test_that("authors are correct", {
  expect_type(chap_auth[["authors"]], "list")
  expect_identical(chap_auth[[5, "authors"]], correct_authors)
})
