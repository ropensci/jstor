context("full_text")

# base path for id ------
crazy_path <- "../Entpackt//soc_a-j/jpalestud/2012/41/2/10.1525%2Fjps.2012.xli.issue-2/10.1525%2Fjps.2012.xli.2.204/10.1525%2Fjps.2012.xli.2.204.txt"
standard_path <- "../Entpackt//soc_k-z/sozialewelt/1952/3/4/i40039236/40874627/40874627.txt"

test_that("getting base name of file works", {
  expect_identical(extract_basename(crazy_path, type = "txt"), "10.1525%2Fjps.2012.xli.2.204")
  expect_identical(extract_basename(standard_path, type = "txt"), "40874627")
})

# encoding -----
file_path <- "testfiles/test-file-full-file.txt"

test_that("guessing encoding works", {
  expect_identical(get_encoding(file_path), "UTF-8")
})
