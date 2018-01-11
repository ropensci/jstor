context("extraction of references")

library(xml2)
library(magrittr)

# import files -----
result <- "testfiles/test-file-single-author.xml" %>%
  find_references()

result_erratum <- "testfiles/test-file-erratum.xml" %>%
  find_references()

weird_pages <- "testfiles/test-file-weird-pages.xml"

weird_references <- "testfiles/test-file-weird-references.xml"

single_and_multiple_references <- "testfiles/test-file-multiple-and-single-references.xml" %>%
  find_references()

random_problem <- "testfiles/test-file-random-problem-reference.xml"

new_format <- "testfiles/test-file-new-format-nov-17.xml" %>%
  find_references()


# tests -----
test_that("Input data is checked", {
  expect_error(find_references("my_path.txt"))
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
  expect_s3_class(result, "data.frame")
})

test_that("catching errors works", {
  expect_silent(find_references(weird_references))
  expect_silent(find_references(weird_pages))
  expect_silent(find_references(random_problem))
})

test_that("null results work", {
  expect_identical(result_erratum[["author_names"]], NA_character_)
  expect_identical(result_erratum[["full_reference"]], NA_character_)
  expect_identical(result[["author_names"]], rep(NA_character_, 43))
})

test_that("extracting references works", {
  skip_on_cran()
  expect_known_output(result[["full_reference"]], "testfiles/correct_references.txt", print = T)
  expect_known_output(new_format[["author_names"]], "testfiles/correct_new_format_authors.txt", print = T)
  expect_known_output(new_format[["full_reference"]], "testfiles/correct_new_format_refs.txt", print = T)
})


