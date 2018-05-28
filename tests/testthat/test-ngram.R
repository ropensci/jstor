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
