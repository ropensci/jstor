context("exceptional-files")
library(xml2)
library(magrittr)

# import files
result <- "testfiles/standard_case.xml" %>%
  jst_get_article() %>%
  flag_special_articles()

result_erratum <- "testfiles/erratum.xml" %>%
  jst_get_article() %>%
  flag_special_articles()


test_that("articles are flagged correctly", {
  expect_identical(result[["special_article"]], "none")
  expect_identical(result_erratum[["special_article"]], "erratum")
})
