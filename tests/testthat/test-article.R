library(xml2)
library(magrittr)
context("meta-information")


# import files -----
result <- "testfiles/standard_case.xml" %>%
  find_article()

result_empty <- "testfiles/empty_file.xml" %>%
  find_article()

result_multiple_dates <- "testfiles/multiple-dates.xml" %>%
  find_article()
 
result_weird_title <- "testfiles/weird-title.xml" %>%
  find_article()

result_weird_title2 <- "testfiles/weird-title-2.xml" %>%
  find_article()

result_multiple_languages <- "testfiles/multiple-languages.xml" %>%
  find_article()

result_new_nov_17 <- "testfiles/article_complex_meta.xml" %>%
  find_article()

article_jcode_stable <- "testfiles/article-jcode-stable.xml" %>%
  find_article()

invalid_file <- "testfiles/invalid_file.xml"

# tests -----
test_that("Input data is checked", {
  expect_error(find_article("my_path.txt"))
  expect_error(find_article("testfiles/standard_book.xml"), "You are using")
})

test_that("invalid xml files raise no warning", {
  expect_silent(find_article(invalid_file))
})

test_that("class is correct", {
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("jcode is correct", {
  expect_identical(result[["journal_jcode"]], "kewbulletin")
  expect_identical(result_empty[["journal_doi"]], NA_character_)
  expect_identical(result_empty[["journal_pub_id"]], NA_character_)
  expect_identical(result_empty[["journal_jcode"]], NA_character_)
  expect_identical(result_new_nov_17[["journal_pub_id"]], "blabla")
  expect_identical(result_new_nov_17[["journal_doi"]], "10.123456")
})

test_that("article_id is correct", {
  expect_equal(result[["article_doi"]], "10.2307/4117222")
  expect_identical(result_empty[["article_doi"]], NA_character_)
  expect_identical(result_empty[["article_pub_id"]], NA_character_)
  expect_identical(result_empty[["article_jcode"]], NA_character_)
  expect_identical(result_new_nov_17[["article_doi"]],
                   "10.123456/blabla.3.6.0111")
  expect_identical(result_new_nov_17[["article_pub_id"]], "blabla.3.6.0111")
  expect_identical(result_new_nov_17[["article_jcode"]], NA_character_)
  expect_identical(article_jcode_stable[["article_jcode"]], "4122385")
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
  expect_equal(result_weird_title2[["article_title"]], "Struggling over the Boundaries of Belonging: A Formal Model of Nation Building, Ethnic Closure, and Populism") # nolint
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
  expect_identical(result[["pub_day"]], "1")
  expect_identical(result[["pub_month"]], "1")
  expect_identical(result[["pub_year"]], 1950L)
  expect_identical(result_multiple_dates[["pub_month"]], "4")
  expect_identical(result_multiple_dates[["pub_year"]], 1990L)
})

test_that("language is correct", {
  expect_identical(result[["language"]], "eng")
  expect_identical(result_multiple_languages[["language"]], "eng fre")
})
