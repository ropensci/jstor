context("extraction of page numers")
library(xml2)

# import files -----
result <- "testfiles/standard_case.xml" %>%
  jst_get_article()

result_weird_pages <- "testfiles/weird-pages.xml" %>%
  jst_get_article()

result_no_pages <- "testfiles/no-pages.xml" %>%
  jst_get_article()

 
# tests -----
test_that("pages are extracted correctly", {
  expect_identical(result[["first_page"]], "187")
  expect_identical(result[["last_page"]], "188")
  expect_identical(result_weird_pages[["first_page"]], "M72") # keep weird stuff
  expect_identical(result_weird_pages[["last_page"]], "M77")
})

test_that("no pages are handled", {
  expect_identical(result_no_pages[["first_page"]], NA_character_)
  expect_identical(result_no_pages[["last_page"]], NA_character_)
})



