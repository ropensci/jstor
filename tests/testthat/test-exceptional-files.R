library(xml2)
library(magrittr)
context("exceptional-files")


# import files
result <- "testfiles/standard_case.xml" %>%
  find_article() %>%
  flag_special_articles()

result_erratum <- "testfiles/erratum.xml" %>%
  find_article() %>%
  flag_special_articles()


test_that("articles are flagged correctly", {
  expect_identical(result[["special_article"]], "none")
  expect_identical(result_erratum[["special_article"]], "erratum")
})
