context("footnotes")

library(xml2)
library(magrittr)

# import files -----
result_erratum <- "testfiles/test-file-erratum.xml" %>%
  find_footnotes()

result <- "testfiles/test-file-single-author.xml" %>%
  find_footnotes()

single_and_multiple_footnotes <- "testfiles/test-file-footnotes.xml" %>%
  find_footnotes()

# tests -----
test_that("Input data is checked", {
  expect_error(find_references("my_path.txt"))
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
  expect_s3_class(result, "data.frame")
})

test_that("extracting footnotes works", {
  expect_identical(result_erratum[["footnotes"]][[1]], NA_character_)
  expect_known_output(print(result[["footnotes"]]), "testfiles/correct_footnotes.txt")
  expect_known_output(print(single_and_multiple_footnotes[["footnotes"]]), "testfiles/correct_footnotes_mangled.txt")
})
