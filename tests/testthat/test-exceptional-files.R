library(xml2)
library(magrittr)
context("exceptional-files")


# import files
result <- "testfiles/test-file-single-author.xml" %>%
  find_metadata() %>%
  flag_special_articles()

result_erratum <- "testfiles/test-file-erratum.xml" %>%
  find_metadata() %>%
  flag_special_articles()


test_that("articles are flagged correctly", {
  expect_identical(result[["special_article"]], "none")
  expect_identical(result_erratum[["special_article"]], "erratum")
})
