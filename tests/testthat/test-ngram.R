context("test-ngram.R")

ngram1 <- tribble(~file_name, ~ngram, ~n,
                  "ngram1", "Common", 400L,
                  "ngram1", "Uncommon", 5L)

ngram2 <- tribble(~file_name, ~ngram, ~n,
                  "ngram2", "Common word", 400L,
                  "ngram2", "Uncommon word", 5L)

test_that("reading ngrams works", {
  expect_identical(jst_get_ngram("testfiles/ngram1-ngram1.txt"), ngram1)
  expect_identical(jst_get_ngram("testfiles/ngram2-ngram2.txt"), ngram2)
  
  expect_error(jst_get_ngram(c("path1.txt", "path2.txt")),
               "file_path should be")
  expect_error(jst_get_ngram("path.xml"), "Unknown input file")
})


test_that("subsetting ngrams works", {
  # this test is not very thorough
  # should test other scenarios too, with multiple matching files etc.

  
  correct_out <- tribble(~file_name, ~ngram, ~n,
                         "book-chapter-standard_book", "Common", 400L,
                         "book-chapter-standard_book", "Uncommon", 5L)
  
  # create sample output
  tmp <- tempdir()
  jst_import_zip(jst_example("pseudo_dfr.zip"),
                import_spec = jst_define_import(book = jst_get_book),
                out_file = "test", out_path = tmp, show_progress = F)
  
  # re-import as our selection for which we would like to import ngrams
  selection <- jst_re_import(file.path(tmp,
                                       "test_book_chapter_jst_get_book-1.csv"))
  
  # get location of file
  zip_loc <- jst_subset_ngrams(jst_example("pseudo_dfr.zip"), "ngram1", selection) 
  
  
  expect_identical(jst_get_ngram(zip_loc[[1]]), correct_out)
})
