context("test-import-spec.R")

test_that("jst_define_import returns correct class", {
  expect_s3_class(jst_define_import(article = find_article),
                  c("jstor_import_spec", "tbl_df", "tbl",
                                         "data.frame"))
})


test_that("jst_define_import validates input", {
  expect_silent(jst_define_import(article = find_article,
                                  pamphlet = find_article,
                                  book = find_book,
                                  report = find_book))
  expect_silent(jst_define_import(article = c(find_article, find_authors)))
  expect_error(jst_define_import(bla = find_article),
               "Input type must be one of")
  expect_error(jst_define_import(article = "find_article"),
               "All inputs must be bare functions")
  expect_error(jst_define_import(article = find_article()),
               "All inputs must be bare functions")
  expect_error(jst_define_import(article = mean),
               "All supplied functions must come from")
})
