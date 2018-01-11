context("extraction of page numers")
library(xml2)

# import files -----
result <- "testfiles/standard_case.xml" %>%
  find_metadata()

result_weird_pages <- "testfiles/weird-pages.xml" %>%
  find_metadata()

result_no_pages <- "testfiles/no-pages.xml" %>%
  find_metadata()

 
# tests -----
test_that("pages are extracted correctly", {
  expect_identical(result[["first_page"]], 187L)
  expect_identical(result[["last_page"]], 188L)
  expect_identical(result_weird_pages[["first_page"]], 72L)
  expect_identical(result_weird_pages[["last_page"]], 77L)
})

test_that("no pages are handled", {
  expect_identical(result_no_pages[["first_page"]], NA_integer_)
  expect_identical(result_no_pages[["last_page"]], NA_integer_)
})


test_that("total pages are computed correctly", {
  expect_identical(find_total_pages(first_page = 2, last_page = 5), 4)
  expect_identical(find_total_pages(first_page = 2L, last_page = 5L), 4)
  expect_identical(find_total_pages(first_page = 2, last_page = 2), 1)
  expect_identical(find_total_pages(first_page = NA, last_page = NA), NA_real_)
})

