# context("extraction of page numers")
# library(xml2)
# 
# # import files -----
# result <- "testfiles/test-file-single-author.xml" %>%
#   find_metadata()
# 
# result_erratum <- "testfiles/test-file-erratum.xml" %>%
#   find_metadata()
# 
# result_weird_pages <- "testfiles/test-file-weird-pages.xml" %>%
#   find_metadata()
# 
# result_no_pages <- "testfiles/test-file-no-pages.xml" %>%
#   find_metadata()
# 
# 
# 
# # tests -----
# test_that("pages are extracted correctly", {
#   expect_identical(result[["first_page"]], 3L)
#   expect_identical(result[["last_page"]], 25L)
#   expect_identical(result_erratum[["first_page"]], 437L)
#   expect_identical(result_weird_pages[["first_page"]], 72L)
#   expect_identical(result_weird_pages[["last_page"]], 77L)})
# 
# test_that("no pages are handled", {
#   expect_identical(result_no_pages[["first_page"]], NA_integer_)
#   expect_identical(result_no_pages[["last_page"]], NA_integer_)
# })
# 
# 
# test_that("number of pages is correct", {
#   expect_identical(find_total_pages(first_page = 2, last_page = 5), 4)
#   expect_identical(find_total_pages(first_page = 2L, last_page = 5L), 4)
#   expect_identical(find_total_pages(first_page = 2, last_page = 2), 1)
#   expect_identical(find_total_pages(first_page = NA, last_page = NA), NA_real_)
# })
# 
