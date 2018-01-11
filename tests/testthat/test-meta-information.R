library(xml2)
library(magrittr)
context("meta-information")


# import files -----
result <- "testfiles/test-file-single-author.xml" %>%
  find_metadata()

result_erratum <- "testfiles/test-file-erratum.xml" %>%
  find_metadata()

result_multiple_dates <- "testfiles/test-file-multiple-dates.xml" %>%
  find_metadata()

result_no_vol_issue <- "testfiles/test-file-no-vol-issue.xml" %>%
  find_metadata()

result_weird_title <- "testfiles/test-file-weird-title.xml" %>%
  find_metadata()

result_weird_title2 <- "testfiles/test-file-weird-title-2.xml" %>%
  find_metadata()

result_new_nov_17 <- "testfiles/test-file-new-format-nov-17.xml" %>%
  find_metadata()

# tests -----
test_that("Input data is checked", {
  expect_error(find_metadata("my_path.txt"))
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
  expect_s3_class(result, "data.frame")
})

test_that("jcode is correct", {
  expect_equal(result[["journal_id"]], "amerlaweconrev")
  expect_equal(result_new_nov_17[["journal_id"]], "statecrime")
})

test_that("article_id is correct", {
  expect_equal(result[["article_id"]], "42705347")
})

test_that("basename_id is correct", {
  expect_equal(result[["basename_id"]], "test-file-single-author")
})

test_that("article_type is correct", {
  expect_equal()
})

test_that("article_title is correct", {
  expect_equal(result[["article_title"]], "The Normativity of Law")
  expect_equal(result_weird_title[["article_title"]], "V‐Goods and the Role of the Urban Informal Sector in Development")
  expect_equal(result_weird_title2[["article_title"]], "Struggling over the Boundaries of Belonging: A Formal Model of Nation Building, Ethnic Closure, and Populism")
})

test_that("volume is correct", {
  expect_identical(result[["volume"]], "1")
  expect_identical(result_no_vol_issue[["volume"]], NA_character_)
})

test_that("issue is correct", {
  expect_identical(result[["issue"]], "1/2")
  expect_identical(result_no_vol_issue[["issue"]], NA_character_)
})

test_that("date is correct", {
  expect_identical(result[["pub_day"]], 1L)
  expect_identical(result[["pub_month"]], 10L)
  expect_identical(result[["pub_year"]], 1999L)
  expect_identical(result_multiple_dates[["pub_month"]], 4L)
  expect_identical(result_multiple_dates[["pub_year"]], 1990L)
})

test_that("language is correct", {
  expect_identical(result[["language"]], "eng")
  expect_identical(result_erratum[["language"]], "eng")
})
