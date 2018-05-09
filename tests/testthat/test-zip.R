context("test-zip.R")

test_that("jst_preview_zip works", {
  correct_res <- tribble(
    ~type, ~meta_type, ~n,
    "metadata", "book_chapter", 1L,
    "metadata", "journal_article", 1L,
    "metadata", "pamphlet", 1L,
    "ngram1", "ngram1", 1L
  )
  
  expect_equal(jst_preview_zip("testfiles/pseudo_dfr.zip"), correct_res)
  
})


# # test converting from zip-file
# # setup the correct data
# jst_import_zip("tests/testthat/testfiles/pseudo_dfr.zip",
#                import_spec = jst_define_import(article = c(find_article, 
#                                                            find_authors),
#                                                book = find_book,
#                                                ngram1 = jst_read_ngram),
#                out_file = "correct", out_path = "tests/testthat/testfiles/")

test_that("importing from zip works", {
  dir.create(test_dir <- file.path(tempdir(), "testdir"))
  
  jst_import_zip("testfiles/pseudo_dfr.zip",
                 import_spec = jst_define_import(article = c(find_article, 
                                                             find_authors),
                                                 book = find_book,
                                                 ngram1 = jst_read_ngram),
                 out_file = "correct", out_path = test_dir)
  
  
  expected_files <- c("correct_book_chapter_find_book-1.csv",
                      "correct_journal_article_find_article-1.csv",
                      "correct_journal_article_find_authors-1.csv",
                      "correct_ngram1_jst_read_ngram-1.csv")
  files <- list.files(test_dir)
  
  # test that we have the right files
  expect_identical(expected_files, files)
  
  # test for the content of the files
  test_expected_zip <- function(expected_file) {
    expect_identical(read_csv(paste0("testfiles/", expected_file)),
                     read_csv(paste0(test_dir, "/", expected_file)))
  }
  expected_files %>% 
    purrr::map(test_expected_zip)
})

test_that("too many arguments for batches throw error", {
  expect_error(jst_import_zip("testfiles/pseudo_dfr.zip", out_file = "meta_data",
                              import_spec = jst_define_import(
                                article = find_article,
                                report = find_book
                              ), out_path = test_dir,
                              n_batches = 1),
               "not available in the .zip-file: research_report"
  )
  
})

