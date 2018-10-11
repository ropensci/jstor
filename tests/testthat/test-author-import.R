context("author-import")
library(xml2)
library(magrittr)
library(tibble)

# import files
test_file_no_author <- "testfiles/erratum.xml" %>%
  jst_get_authors()

# contrib group but no authors
test_file_no_author2 <- "testfiles/no-authors.xml" %>%
  jst_get_authors()

test_file_single_author <- "testfiles/standard_case.xml" %>%
  jst_get_authors()

test_file_multiple_authors <- "testfiles/multiple-authors.xml" %>%
  jst_get_authors()

test_file_author_string <- "testfiles/author-string.xml" %>%
  jst_get_authors()

test_file_author_prefix <- "testfiles/author-prefix.xml" %>%
  jst_get_authors()

test_file_author_suffix <- "testfiles/author-suffix.xml" %>%
  jst_get_authors()

test_file_book <- "testfiles/standard_book.xml" %>%
  jst_get_authors()

# nolint start

# expected output
single_author <- tribble(
  ~file_name,              ~prefix,       ~given_name, ~surname,     ~string_name, ~suffix,        ~author_number,
  "standard_case", NA_character_, "N. L.",  "Bor", NA_character_, NA_character_,  1L
)

multiple_authors <- tribble(
  ~file_name,                 ~prefix,       ~given_name, ~surname,  ~string_name, ~suffix,        ~author_number,
  "multiple-authors", NA_character_, "Louis",     "Kaplow",  NA_character_, NA_character_,  1L,
  "multiple-authors", NA_character_, "Steven",    "Shavell", NA_character_, NA_character_,  2L
)

multiple_given_names <- tribble(
  ~file_name,                    ~prefix,       ~given_name,    ~surname, ~string_name,  ~suffix,        ~author_number,
  "multiple-given-names", NA_character_, "Seung Ho",      "Park", NA_character_, NA_character_,  1L,
  "multiple-given-names", NA_character_, "Roger",         "Chen", NA_character_, NA_character_,  2L,
  "multiple-given-names", NA_character_, "Scott",    "Gallagher", NA_character_, NA_character_,  3L
)

author_string <- tribble(
  ~file_name,              ~prefix,      ~given_name,    ~surname,     ~string_name,               ~suffix,        ~author_number,
  "author-string", NA_character_, NA_character_, NA_character_, " Michèle de la Pradelle ", NA_character_,  1L,
  "author-string", NA_character_, NA_character_, NA_character_, "Emmanuelle Lallement",     NA_character_,  2L
)

no_authors <- tribble(
  ~prefix,      ~given_name,    ~surname,     ~string_name,   ~suffix,       ~author_number,
  NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_real_
)

book_authors <- tribble(
  ~file_name, ~prefix, ~given_name, ~surname, ~string_name, ~suffix, ~author_number,
  "standard_book", NA_character_, "Jon", "Fraenkel",  NA_character_, NA_character_,  1L,
  "standard_book", NA_character_, "Stewart", "Firth", NA_character_, NA_character_,  2L,
  "standard_book", NA_character_, "Brij V.",  "Lal",  NA_character_, NA_character_,  3L
)

# nolint end

test_that("class is correct", {
  expect_s3_class(test_file_single_author, "tbl_df")
})

test_that("extracting authors works", {
  expect_identical(single_author, test_file_single_author)
  expect_identical(multiple_authors, test_file_multiple_authors)
  expect_identical(author_string, test_file_author_string)
  expect_identical(book_authors, test_file_book)

  expect_identical(no_authors, test_file_no_author[-1])
  expect_identical(no_authors, test_file_no_author2[-1])
})

test_that("prefixes and suffixes are recognized", {
  expect_identical(test_file_author_prefix[["prefix"]], "M.")
  expect_identical(test_file_author_suffix[["suffix"]], "Jr.")
})

test_that("trailing comma is removed from surnames", {
  expect_identical(test_file_author_suffix[["surname"]], "Elder")
})

test_that("multiple given-names are handled", {
  res <- jst_get_authors("testfiles/multiple-given-names.xml")
  expect_identical(res, multiple_given_names)
})
