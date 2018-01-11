library(xml2)
library(magrittr)
context("meta-information")


# import files -----
result <- "testfiles/standard_case.xml" %>%
  find_metadata()

result_empty <- "testfiles/empty_file.xml" %>% 
  find_metadata()

result_multiple_dates <- "testfiles/multiple-dates.xml" %>%
  find_metadata()
 
result_weird_title <- "testfiles/weird-title.xml" %>%
  find_metadata()

result_weird_title2 <- "testfiles/weird-title-2.xml" %>%
  find_metadata()

# result_new_nov_17 <- "testfiles/test-file-new-format-nov-17.xml" %>%
#   find_metadata()

# tests -----
test_that("Input data is checked", {
  expect_error(find_metadata("my_path.txt"))
})

test_that("class is correct", {
  expect_s3_class(result, "jstor")
  expect_s3_class(result, "data.frame")
})

test_that("jcode is correct", {
  expect_equal(result[["journal_id"]], "kewbulletin")
  expect_equal(result_empty[["journal_id"]], NA_character_)
  # expect_equal(result_new_nov_17[["journal_id"]], "statecrime")
})

test_that("article_id is correct", {
  expect_equal(result[["article_id"]], "10.2307/4117222")
  expect_equal(result_empty[["article_id"]], NA_character_)
})

test_that("basename_id is correct", {
  expect_equal(result[["basename_id"]], "standard_case")
})

test_that("article_type is correct", {
  expect_equal(result[["article_type"]], "research-article")
  expect_equal(result_empty[["article_type"]], NA_character_)
})

test_that("article_title is correct", {
  expect_equal(result[["article_title"]], "Two New Species of Ischaemum")
  expect_equal(result_weird_title[["article_title"]], "V‐Goods and the Role")
  expect_equal(result_weird_title2[["article_title"]], "Struggling over the Boundaries of Belonging: A Formal Model of Nation Building, Ethnic Closure, and Populism")
  expect_equal(result_empty[["article_title"]], NA_character_)
})

test_that("volume is correct", {
  expect_identical(result[["volume"]], "5")
  expect_identical(result_empty[["volume"]], NA_character_)
})

test_that("issue is correct", {
  expect_identical(result[["issue"]], "2")
  expect_identical(result_empty[["issue"]], NA_character_)
})

test_that("date is correct", {
  expect_identical(result[["pub_day"]], 1L)
  expect_identical(result[["pub_month"]], 1L)
  expect_identical(result[["pub_year"]], 1950L)
  expect_identical(result_multiple_dates[["pub_month"]], 4L)
  expect_identical(result_multiple_dates[["pub_year"]], 1990L)
})

test_that("language is correct", {
  expect_identical(result[["language"]], "eng")
})
