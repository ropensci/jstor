context("test-ngram.R")

ngram1 <- tribble(~ngram, ~n,
                  "Common", 400L,
                  "Uncommon", 5L)

ngram2 <- tribble(~ngram, ~n,
                  "Common word", 400L,
                  "Uncommon word", 5L)

test_that("reading ngrams works", {
  expect_identical(jst_read_ngram("testfiles/ngram1.txt"), ngram1)
  expect_identical(jst_read_ngram("testfiles/ngram2.txt"), ngram2)
  
  expect_error(jst_read_ngram(c("path1.txt", "path2.txt")),
               "file_path should be")
  expect_error(jst_read_ngram("path.xml"), "Unknown input file")
})
