context("test-test-ngram.R")

ngram1 <- tribble(~ngram, ~n,
                  "Common", 400L,
                  "Uncommon", 5L)

ngram2 <- tribble(~ngram, ~n,
                  "Common word", 400L,
                  "Uncommon word", 5L)

test_that("reading ngrams works", {
  expect_identical(jst_read_ngram("testfiles/ngram1.txt"), ngram1)
  expect_identical(jst_read_ngram("testfiles/ngram2.txt"), ngram2)
})
