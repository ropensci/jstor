context("test-test-zip.R")

test_that("jst_preview_zip works", {
  correct_res <- tribble(
    ~type, ~meta_type, ~n,
    "metadata", "book_chapter", 1L,
    "metadata", "journal_article", 1L,
    "metadata", "pamphlet", 1L,
    "ngram1", "ngram", 1L
  )
  
  expect_equal(jst_preview_zip("testfiles/pseudo_dfr.zip"), correct_res)
  
})

