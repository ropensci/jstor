context("test-import-spec.R")

test_that("jst_define_import returns correct class", {
  expect_s3_class(jst_define_import(article = jst_get_article),
                  c("jstor_import_spec", "tbl_df", "tbl",
                                         "data.frame"))
})


test_that("jst_define_import validates input", {
  expect_silent(jst_define_import(article = jst_get_article,
                                  pamphlet = jst_get_article,
                                  book = jst_get_book,
                                  report = jst_get_book))
  expect_silent(jst_define_import(article = c(jst_get_article, jst_get_authors)))
  
  expect_error(jst_define_import(article = jst_get_book))
  expect_error(jst_define_import(book = jst_get_article))
  expect_error(jst_define_import(article = c(jst_get_article, jst_get_book),
                                 pamphlet = jst_get_article))
  
  expect_error(jst_define_import(bla = jst_get_article),
               "Input type must be one of")
  expect_error(jst_define_import(article = "jst_get_article"),
               "All inputs must be bare functions")
  expect_error(jst_define_import(article = jst_get_article()),
               "All inputs must be bare functions")
  expect_error(jst_define_import(article = mean),
               "All supplied functions must come from")
  
  chapters_w_authors <- function(x) jst_get_chapters(x, authors = TRUE)
  expect_silent(jst_define_import(book = chapters_w_authors))
})

test_that("functions from jstor can be detected", {
  chapters_w_authors <- function(x) jst_get_chapters(x, authors = TRUE)
  
  expect_identical(is_jstor(jst_get_article), TRUE)
  expect_identical(is_jstor(chapters_w_authors), TRUE)
  expect_identical(is_jstor(mean), FALSE)
})

test_that("jst_define_imports gives correct results", {
  chapters_w_authors <- function(x) jst_get_chapters(x, authors = TRUE)
  
  spec <- jst_define_import(book = chapters_w_authors)

  expect_identical(spec$meta_type, "book_chapter")
  expect_identical(spec$fun_names, list("chapters_w_authors"))
  expect_identical(spec$evaled_funs, list(book = chapters_w_authors))
  expect_identical(spec$bare_funs[[1]],
                   jstor:::capture_functions(chapters_w_authors)[[1]])
  
  
  spec2 <- jst_define_import(article = jstor::jst_get_article)
  
  expect_identical(spec2$fun_names, list("jst_get_article"))
  
})

test_that("import-spec print method works", {
  skip("This test is stupid, should be improved. It depends on the console width
       which is not a good idea.")
  out <- jst_define_import(article = c(jst_get_article, jst_get_references),
                           pamphlet = jst_get_article,
                           book = jst_get_book,
                           report = jst_get_book)
  
  out_lines <- capture.output(print(out))
  
  expect_identical(out_lines, 
                   readr::read_lines("testfiles/import_spec_print.txt"))
  
})
